# ============================================================
# VALIDACION EXPERIMENTAL MEDIANTE SIMULACION
# Autocorrelacion espacial clasica versus simbolica con datos intervalalos ASE
# ============================================================
setwd("C:/Users/eddyh/Downloads/TG-SOFIA-GOMEZ")
rm(list = ls())
gc()

# -----------------------------
# PAQUETES-LIBRERIAS 
# -----------------------------
req_pkgs <- c(
  "sf", "spdep", "spatialreg", "dplyr", "tidyr", "purrr",
  "ggplot2", "readr", "stringr", "tibble"
)

new_pkgs <- req_pkgs[!(req_pkgs %in% installed.packages()[, "Package"])]
if(length(new_pkgs) > 0) install.packages(new_pkgs)

invisible(lapply(req_pkgs, library, character.only = TRUE))

set.seed(12345)#SEMILLA

# -----------------------------
# 1. CARPETAS
# -----------------------------
dir_base <- "Simulacion_SDA_Moran"
dir_entradas <- file.path(dir_base, "01_Entradas_insumos")
dir_salidas  <- file.path(dir_base, "02_Salidas")
dir_graficas <- file.path(dir_base, "03_Graficas")
dir_tablas   <- file.path(dir_salidas, "Tablas")
dir_rdata    <- file.path(dir_salidas, "RData")
dir_mapas    <- file.path(dir_graficas, "Mapas")
dir_diag     <- file.path(dir_graficas, "Diagnosticos")

dirs <- c(dir_base, dir_entradas, dir_salidas, dir_graficas,
          dir_tablas, dir_rdata, dir_mapas, dir_diag)

invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# -----------------------------
# 2. FUNCIONES AUXILIARES PARA CREAR LA ESTRUCTURA 
# -----------------------------

# 2.1 Crear grilla sf
crear_grilla_sf <- function(n_lado = 5) {
  bbox <- st_as_sfc(st_bbox(c(xmin = 0, ymin = 0, xmax = n_lado, ymax = n_lado), crs = st_crs(4326)))
  grd <- st_make_grid(bbox, n = c(n_lado, n_lado), what = "polygons", square = TRUE)
  sf_obj <- st_sf(
    id = seq_along(grd),
    geometry = grd
  )
  sf_obj$centroide <- st_centroid(sf_obj$geometry)
  coords <- st_coordinates(sf_obj$centroide)
  sf_obj$x <- coords[,1]
  sf_obj$y <- coords[,2]
  sf_obj
}

# Convertir listw a matriz
listw_to_matrix <- function(listw_obj) {
  W <- spdep::listw2mat(listw_obj)
  return(W)
}

# 2.3 Estandarizar por filas
row_standardize <- function(W) {
  rs <- rowSums(W)
  rs[rs == 0] <- 1
  W / rs
}

# 2.4 Simular UTILIZANDO EL PROCESO ESTOCASTICO proceso SAR: x = (I - rho W)^(-1) e
simular_sar <- function(W, rho, sigma = 1) {
  n <- nrow(W)
  I_n <- diag(n)
  eps <- rnorm(n, mean = 0, sd = sigma)
  x <- solve(I_n - rho * W, eps)
  as.numeric(scale(x))
}

# 2.5 Construir intervalos sintéticos a partir de LAS configuraciones:
#   A = amplitud baja homogénea FALTA DEFINIRLAS EN EL TG
#   B = amplitud media heterogénea independiente
#   C = amplitud dependiente del nivel (heterocedástica)
crear_intervalos <- function(z, config = "A") {
  n <- length(z)
  
  if (config == "A") {
    r <- runif(n, 0.15, 0.35)
  } else if (config == "B") {
    r <- runif(n, 0.30, 0.80)
  } else if (config == "C") {
    r <- 0.20 + 0.45 * abs(scale(z))[,1] + runif(n, 0.05, 0.15)
  } else {
    stop("Configuracion no valida. Use A, B o C.")
  }
  
  c <- as.numeric(z)
  li <- c - r
  ui <- c + r
  
  tibble(
    centro = c,
    radio  = r,
    li = li,
    ui = ui
  )
}

# 2.6 Índice de Moran clásico usando el centro como resumen puntual
moran_clasico_manual <- function(x, W) {
  n <- length(x)
  x_bar <- mean(x)
  z <- x - x_bar
  S0 <- sum(W)
  num <- sum(W * (z %o% z))
  den <- sum(z^2)
  I <- (n / S0) * (num / den)
  return(I)
}

# 2.7 Índices simbólicos separados y combinado
# Ic = Moran de centros
# Ir = Moran de radios
# Is = alpha*Ic + (1-alpha)*Ir
moran_simbolico <- function(centro, radio, W, alpha = 0.5) {
  Ic <- moran_clasico_manual(centro, W)
  Ir <- moran_clasico_manual(radio, W)
  Is <- alpha * Ic + (1 - alpha) * Ir
  tibble(Ic = Ic, Ir = Ir, Is = Is, alpha = alpha)
}

# 2.8 Permutaciones para p-valor empírico
perm_moran <- function(x, W, nsim = 499) {
  I_obs <- moran_clasico_manual(x, W)
  sims <- replicate(nsim, moran_clasico_manual(sample(x), W))
  p_two <- (sum(abs(sims) >= abs(I_obs)) + 1) / (nsim + 1)
  tibble(
    I_obs = I_obs,
    p_perm = p_two,
    media_perm = mean(sims),
    sd_perm = sd(sims)
  )
}

perm_moran_simbolico <- function(centro, radio, W, alpha = 0.5, nsim = 499) {
  obs <- moran_simbolico(centro, radio, W, alpha = alpha)
  sims <- replicate(
    nsim,
    {
      idx <- sample(seq_along(centro))
      moran_simbolico(centro[idx], radio[idx], W, alpha = alpha)$Is
    }
  )
  p_two <- (sum(abs(sims) >= abs(obs$Is)) + 1) / (nsim + 1)
  tibble(
    Ic = obs$Ic,
    Ir = obs$Ir,
    Is = obs$Is,
    p_perm_Is = p_two,
    media_perm_Is = mean(sims),
    sd_perm_Is = sd(sims)
  )
}

# 2.9 W Matrices de pesos espaciales
crear_pesos <- function(sf_grid, k = 2) {
  
  # Queen
  nb_queen <- poly2nb(sf_grid, queen = TRUE)
  lw_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)
  W_queen  <- listw_to_matrix(lw_queen)
  
  # k vecinos mas cercanos
  coords <- st_coordinates(st_centroid(sf_grid))
  knn <- knearneigh(coords, k = k)
  nb_knn <- knn2nb(knn)
  lw_knn <- nb2listw(nb_knn, style = "W", zero.policy = TRUE)
  W_knn  <- listw_to_matrix(lw_knn)
  
  list(
    nb_queen = nb_queen,
    lw_queen = lw_queen,
    W_queen = W_queen,
    nb_knn = nb_knn,
    lw_knn = lw_knn,
    W_knn = W_knn
  )
}

# 2.10 Resumen estructural de la matriz
resumen_matriz_pesos <- function(nb_obj, nombre, n) {
  n_vecinos <- card(nb_obj)
  tibble(
    matriz = nombre,
    n = n,
    vecinos_min = min(n_vecinos),
    vecinos_prom = mean(n_vecinos),
    vecinos_max = max(n_vecinos),
    densidad = sum(n_vecinos) / (n * (n - 1)),
    conexa_aprox = ifelse(all(n_vecinos > 0), "si", "no")
  )
}

# 2.11 LISA local clásico
lisa_local <- function(x, lw) {
  Ii <- localmoran(x, lw, zero.policy = TRUE)
  as.data.frame(Ii)
}

# -----------------------------
# 3. PARAMETROS DE SIMULACION
# -----------------------------
grillas <- c(5, 10)
rhos <- c(-0.6, -0.3, 0, 0.3, 0.6)
configs_intervalo <- c("A", "B", "C")
matrices <- c("Queen", "KNN2")
nsim_perm <- 499

# A = amplitud baja homogénea
# B = amplitud media heterogénea
# C = amplitud dependiente del nivel

parametros <- expand.grid(
  n_lado = grillas,
  rho = rhos,
  config = configs_intervalo,
  matriz = matrices,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(escenario = row_number())

write_csv(parametros, file.path(dir_entradas, "parametros_simulacion.csv"))

# -----------------------------
# 4. SIMULACION GENERAL
# -----------------------------
resultados_globales <- list()
resultados_lisa <- list()
resumen_pesos_total <- list()

for (g in grillas) {
  
  cat("Procesando grilla:", g, "x", g, "\n")
  
  grid_sf <- crear_grilla_sf(g)
  pesos <- crear_pesos(grid_sf, k = 2)
  n_obs <- nrow(grid_sf)
  
  # Guardar resumen de matrices
  resumen_q <- resumen_matriz_pesos(pesos$nb_queen, "Queen", n_obs) %>%
    mutate(grilla = paste0(g, "x", g))
  resumen_k <- resumen_matriz_pesos(pesos$nb_knn, "KNN2", n_obs) %>%
    mutate(grilla = paste0(g, "x", g))
  
  resumen_pesos_total[[paste0("g", g, "_Q")]] <- resumen_q
  resumen_pesos_total[[paste0("g", g, "_K")]] <- resumen_k
  
  # mapas de vecindad simples
  p_base <- ggplot(grid_sf) +
    geom_sf(fill = "white", color = "grey40", linewidth = 0.2) +
    ggtitle(paste("Grilla", g, "x", g)) +
    theme_minimal()
  
  ggsave(
    filename = file.path(dir_mapas, paste0("grilla_", g, "x", g, ".png")),
    plot = p_base, width = 6, height = 6, dpi = 300
  )
  
  for (rho in rhos) {
    
    # Simulación usando Queen y KNN por separado para que la estructura
    # generadora sea coherente con la matriz que se evalúa
    z_queen <- simular_sar(row_standardize(pesos$W_queen), rho = rho, sigma = 1)
    z_knn   <- simular_sar(row_standardize(pesos$W_knn),   rho = rho, sigma = 1)
    
    for (cfg in configs_intervalo) {
      
      int_q <- crear_intervalos(z_queen, cfg)
      int_k <- crear_intervalos(z_knn, cfg)
      
      for (matriz_nombre in matrices) {
        
        if (matriz_nombre == "Queen") {
          W <- row_standardize(pesos$W_queen)
          lw <- pesos$lw_queen
          datos_int <- int_q
          z_base <- z_queen
        } else {
          W <- row_standardize(pesos$W_knn)
          lw <- pesos$lw_knn
          datos_int <- int_k
          z_base <- z_knn
        }
        
        # Moran clásico sobre el centro
        mc <- perm_moran(datos_int$centro, W, nsim = nsim_perm)
        
        # Moran simbólico
        ms <- perm_moran_simbolico(
          centro = datos_int$centro,
          radio = datos_int$radio,
          W = W,
          alpha = 0.5,
          nsim = nsim_perm
        )
        
        # Guardar resultados globales
        fila_res <- tibble(
          grilla = paste0(g, "x", g),
          n = n_obs,
          rho = rho,
          config_intervalo = cfg,
          matriz = matriz_nombre,
          I_clasico = mc$I_obs,
          p_clasico = mc$p_perm,
          Ic = ms$Ic,
          Ir = ms$Ir,
          Is = ms$Is,
          p_Is = ms$p_perm_Is,
          media_perm_clasico = mc$media_perm,
          sd_perm_clasico = mc$sd_perm,
          media_perm_Is = ms$media_perm_Is,
          sd_perm_Is = ms$sd_perm_Is,
          var_centros = var(datos_int$centro),
          var_radios = var(datos_int$radio),
          ancho_medio = mean(datos_int$ui - datos_int$li)
        )
        
        resultados_globales[[paste(g, rho, cfg, matriz_nombre, sep = "_")]] <- fila_res
        
        # Datos para mapa y LISA
        sf_escenario <- grid_sf %>%
          bind_cols(datos_int) %>%
          mutate(
            rho = rho,
            config = cfg,
            matriz = matriz_nombre,
            grilla = paste0(g, "x", g)
          )
        
        lisa_c <- lisa_local(sf_escenario$centro, lw)
        names(lisa_c) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr.z....0.")
        
        sf_escenario <- sf_escenario %>%
          bind_cols(as_tibble(lisa_c)) %>%
          mutate(sig_lisa = ifelse(`Pr.z....0.` < 0.05, "Si", "No"))
        
        resultados_lisa[[paste(g, rho, cfg, matriz_nombre, sep = "_")]] <- sf_escenario
        
        # Graficas básicas
        p_centro <- ggplot(sf_escenario) +
          geom_sf(aes(fill = centro), color = "grey30", linewidth = 0.1) +
          ggtitle(paste0(
            "Centro | Grilla ", g, "x", g,
            " | rho=", rho,
            " | ", cfg,
            " | ", matriz_nombre
          )) +
          theme_minimal()
        
        p_radio <- ggplot(sf_escenario) +
          geom_sf(aes(fill = radio), color = "grey30", linewidth = 0.1) +
          ggtitle(paste0(
            "Radio | Grilla ", g, "x", g,
            " | rho=", rho,
            " | ", cfg,
            " | ", matriz_nombre
          )) +
          theme_minimal()
        
        p_intervalo <- ggplot(sf_escenario, aes(x = centro, y = ui - li)) +
          geom_point() +
          ggtitle(paste0(
            "Centro vs amplitud | ", g, "x", g,
            " | rho=", rho,
            " | ", cfg,
            " | ", matriz_nombre
          )) +
          xlab("Centro") +
          ylab("Amplitud del intervalo") +
          theme_minimal()
        
        nom_base <- paste0(
          "g", g, "_rho", str_replace_all(as.character(rho), "-", "m"),
          "_cfg", cfg, "_", matriz_nombre
        )
        
        ggsave(file.path(dir_mapas, paste0("mapa_centro_", nom_base, ".png")),
               p_centro, width = 7, height = 6, dpi = 300)
        
        ggsave(file.path(dir_mapas, paste0("mapa_radio_", nom_base, ".png")),
               p_radio, width = 7, height = 6, dpi = 300)
        
        ggsave(file.path(dir_diag, paste0("dispersion_", nom_base, ".png")),
               p_intervalo, width = 7, height = 5, dpi = 300)
        
        # Exportar escenario puntual
        st_drop_geometry(sf_escenario) %>%
          write_csv(file.path(dir_tablas, paste0("datos_", nom_base, ".csv")))
      }
    }
  }
  
  saveRDS(grid_sf, file.path(dir_rdata, paste0("grilla_", g, "x", g, ".rds")))
  saveRDS(pesos, file.path(dir_rdata, paste0("pesos_", g, "x", g, ".rds")))
}

# -----------------------------
# 5. CONSOLIDACION DE RESULTADOS
# -----------------------------
tabla_resultados <- bind_rows(resultados_globales)
tabla_pesos <- bind_rows(resumen_pesos_total)

write_csv(tabla_resultados, file.path(dir_salidas, "resultados_globales_simulacion.csv"))
write_csv(tabla_pesos, file.path(dir_salidas, "resumen_matrices_pesos.csv"))

saveRDS(tabla_resultados, file.path(dir_rdata, "resultados_globales_simulacion.rds"))
saveRDS(resultados_lisa, file.path(dir_rdata, "resultados_lisa_lista.rds"))

# -----------------------------
# 6. TABLAS RESUMEN
# -----------------------------
resumen_por_rho <- tabla_resultados %>%
  group_by(grilla, matriz, config_intervalo, rho) %>%
  summarise(
    prom_I_clasico = mean(I_clasico, na.rm = TRUE),
    prom_Is = mean(Is, na.rm = TRUE),
    prom_Ic = mean(Ic, na.rm = TRUE),
    prom_Ir = mean(Ir, na.rm = TRUE),
    prop_sig_clasico = mean(p_clasico < 0.05, na.rm = TRUE),
    prop_sig_Is = mean(p_Is < 0.05, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(resumen_por_rho, file.path(dir_salidas, "resumen_por_rho.csv"))

# -----------------------------
# 7. GRAFICAS COMPARATIVAS ENTRE LO CLASICO Y LA PROPUESTA SIMBOLICO
# -----------------------------

# 7.1 Moran clasico vs rho
p1 <- ggplot(tabla_resultados,
             aes(x = rho, y = I_clasico, group = interaction(config_intervalo, matriz),
                 linetype = matriz, shape = config_intervalo)) +
  geom_point(size = 2) +
  geom_line() +
  facet_wrap(~ grilla) +
  theme_minimal() +
  labs(
    title = "Indice de Moran clasico segun rho",
    x = expression(rho),
    y = "I de Moran clasico"
  )

ggsave(file.path(dir_graficas, "comparacion_moran_clasico_vs_rho.png"),
       p1, width = 9, height = 5, dpi = 300)

# 7.2 Moran simbolico vs rho
p2 <- ggplot(tabla_resultados,
             aes(x = rho, y = Is, group = interaction(config_intervalo, matriz),
                 linetype = matriz, shape = config_intervalo)) +
  geom_point(size = 2) +
  geom_line() +
  facet_wrap(~ grilla) +
  theme_minimal() +
  labs(
    title = "Indice de Moran simbolico segun rho",
    x = expression(rho),
    y = "Is"
  )

ggsave(file.path(dir_graficas, "comparacion_moran_simbolico_vs_rho.png"),
       p2, width = 9, height = 5, dpi = 300)

# 7.3 Comparacion Ic e Ir
tabla_long <- tabla_resultados %>%
  select(grilla, rho, config_intervalo, matriz, Ic, Ir) %>%
  pivot_longer(cols = c(Ic, Ir), names_to = "componente", values_to = "valor")

p3 <- ggplot(tabla_long,
             aes(x = rho, y = valor, color = componente, group = componente)) +
  geom_point() +
  geom_line() +
  facet_grid(grilla ~ matriz + config_intervalo) +
  theme_minimal() +
  labs(
    title = "Componentes del Moran simbolico",
    x = expression(rho),
    y = "Valor del indice"
  )

ggsave(file.path(dir_graficas, "componentes_moran_simbolico.png"),
       p3, width = 12, height = 8, dpi = 300)

# 7.4 Potencia de deteccion
tabla_power <- tabla_resultados %>%
  mutate(
    escenario_real = ifelse(rho == 0, "Sin autocorrelacion", "Con autocorrelacion"),
    sig_clasico = p_clasico < 0.05,
    sig_simbolico = p_Is < 0.05
  ) %>%
  group_by(grilla, matriz, config_intervalo, escenario_real, rho) %>%
  summarise(
    tasa_sig_clasico = mean(sig_clasico, na.rm = TRUE),
    tasa_sig_simbolico = mean(sig_simbolico, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = starts_with("tasa_sig"),
               names_to = "indice", values_to = "tasa")

p4 <- ggplot(tabla_power, aes(x = rho, y = tasa, color = indice, group = indice)) +
  geom_point() +
  geom_line() +
  facet_grid(grilla ~ matriz + config_intervalo) +
  theme_minimal() +
  labs(
    title = "Capacidad de deteccion de autocorrelacion",
    x = expression(rho),
    y = "Proporcion de rechazo de H0"
  )

ggsave(file.path(dir_graficas, "potencia_indices.png"),
       p4, width = 12, height = 8, dpi = 300)

# -----------------------------
# 8. REPORTE INTERPRETABLE
# -----------------------------
cat("\n==================== RESUMEN ====================\n")
print(tabla_pesos)
cat("\nPrimeras filas de resultados globales:\n")
print(head(tabla_resultados, 10))

cat("\nArchivos guardados en:\n", normalizePath(dir_base), "\n")