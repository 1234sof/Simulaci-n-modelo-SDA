# ============================================================
# VALIDACION EXPERIMENTAL MEDIANTE SIMULACION
# Autocorrelacion espacial clasica versus simbolica
# con datos intervalares ASE
# ============================================================

rm(list = ls())
gc()

# -----------------------------
# PAQUETES
# -----------------------------
req_pkgs <- c(
  "sf", "spdep", "spatialreg", "dplyr", "tidyr", "purrr",
  "ggplot2", "readr", "stringr", "tibble"
)

new_pkgs <- req_pkgs[!(req_pkgs %in% installed.packages()[, "Package"])]

if(length(new_pkgs) > 0){
  install.packages(new_pkgs)
}

invisible(lapply(req_pkgs, library, character.only = TRUE))

set.seed(12345)

# ============================================================
# 1. CARPETAS
# ============================================================

dir_base <- "."

dir_entradas <- file.path(dir_base, "01_Entradas_insumos")
dir_salidas  <- file.path(dir_base, "02_Salidas")
dir_graficas <- file.path(dir_base, "03_Graficas")

dir_tablas <- file.path(dir_salidas, "Tablas")
dir_rdata  <- file.path(dir_salidas, "RData")

dir_mapas <- file.path(dir_graficas, "Mapas")
dir_diag  <- file.path(dir_graficas, "Diagnosticos")

dirs <- c(
  dir_entradas,
  dir_salidas,
  dir_graficas,
  dir_tablas,
  dir_rdata,
  dir_mapas,
  dir_diag
)

invisible(
  lapply(
    dirs,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

# ============================================================
# 2. FUNCIONES AUXILIARES
# ============================================================

# ------------------------------------------------------------
# 2.1 Crear grilla
# ------------------------------------------------------------
crear_grilla_sf <- function(n_lado = 5){
  
  bbox <- st_as_sfc(
    st_bbox(
      c(
        xmin = 0,
        ymin = 0,
        xmax = n_lado,
        ymax = n_lado
      ),
      crs = st_crs(4326)
    )
  )
  
  grd <- st_make_grid(
    bbox,
    n = c(n_lado, n_lado),
    what = "polygons",
    square = TRUE
  )
  
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

# ------------------------------------------------------------
# 2.2 listw a matriz
# ------------------------------------------------------------
listw_to_matrix <- function(listw_obj){
  
  W <- spdep::listw2mat(listw_obj)
  
  return(W)
}

# ------------------------------------------------------------
# 2.3 Estandarizar filas
# ------------------------------------------------------------
row_standardize <- function(W){
  
  rs <- rowSums(W)
  
  rs[rs == 0] <- 1
  
  W / rs
}

# ------------------------------------------------------------
# 2.4 Simulacion SAR
# x = (I - rho W)^(-1)e
# ------------------------------------------------------------
simular_sar <- function(W, rho, sigma = 1){
  
  n <- nrow(W)
  
  I_n <- diag(n)
  
  eps <- rnorm(n, mean = 0, sd = sigma)
  
  x <- solve(I_n - rho * W, eps)
  
  as.numeric(scale(x))
}

# ------------------------------------------------------------
# 2.5 Crear intervalos
# ------------------------------------------------------------
crear_intervalos <- function(z, config = "A"){
  
  n <- length(z)
  
  if(config == "A"){
    
    r <- runif(n, 0.15, 0.35)
    
  } else if(config == "B"){
    
    r <- runif(n, 0.30, 0.80)
    
  } else if(config == "C"){
    
    r <- 0.20 +
      0.45 * abs(scale(z))[,1] +
      runif(n, 0.05, 0.15)
    
  } else {
    
    stop("Configuracion no valida")
    
  }
  
  c <- as.numeric(z)
  
  li <- c - r
  ui <- c + r
  
  tibble(
    centro = c,
    radio = r,
    li = li,
    ui = ui
  )
}

# ------------------------------------------------------------
# 2.6 Moran clasico
# ------------------------------------------------------------
moran_clasico_manual <- function(x, W){
  
  n <- length(x)
  
  x_bar <- mean(x)
  
  z <- x - x_bar
  
  S0 <- sum(W)
  
  num <- sum(W * (z %o% z))
  
  den <- sum(z^2)
  
  I <- (n / S0) * (num / den)
  
  return(I)
}

# ------------------------------------------------------------
# 2.7 Moran simbolico
# ------------------------------------------------------------
moran_simbolico <- function(
    centro,
    radio,
    W,
    alpha = 0.5
){
  
  Ic <- moran_clasico_manual(centro, W)
  
  Ir <- moran_clasico_manual(radio, W)
  
  Is <- alpha * Ic + (1 - alpha) * Ir
  
  tibble(
    Ic = Ic,
    Ir = Ir,
    Is = Is,
    alpha = alpha
  )
}

# ------------------------------------------------------------
# 2.8 Permutaciones
# ------------------------------------------------------------
perm_moran <- function(x, W, nsim = 499){
  
  I_obs <- moran_clasico_manual(x, W)
  
  sims <- replicate(
    nsim,
    moran_clasico_manual(sample(x), W)
  )
  
  p_two <- (
    sum(abs(sims) >= abs(I_obs)) + 1
  ) / (nsim + 1)
  
  tibble(
    I_obs = I_obs,
    p_perm = p_two,
    media_perm = mean(sims),
    sd_perm = sd(sims)
  )
}

perm_moran_simbolico <- function(
    centro,
    radio,
    W,
    alpha = 0.5,
    nsim = 499
){
  
  obs <- moran_simbolico(
    centro,
    radio,
    W,
    alpha = alpha
  )
  
  sims <- replicate(
    nsim,
    {
      idx <- sample(seq_along(centro))
      
      moran_simbolico(
        centro[idx],
        radio[idx],
        W,
        alpha = alpha
      )$Is
    }
  )
  
  p_two <- (
    sum(abs(sims) >= abs(obs$Is)) + 1
  ) / (nsim + 1)
  
  tibble(
    Ic = obs$Ic,
    Ir = obs$Ir,
    Is = obs$Is,
    p_perm_Is = p_two,
    media_perm_Is = mean(sims),
    sd_perm_Is = sd(sims)
  )
}

# ------------------------------------------------------------
# 2.9 Crear pesos
# ------------------------------------------------------------
crear_pesos <- function(sf_grid, k = 2){
  
  nb_queen <- poly2nb(sf_grid, queen = TRUE)
  
  lw_queen <- nb2listw(
    nb_queen,
    style = "W",
    zero.policy = TRUE
  )
  
  W_queen <- listw_to_matrix(lw_queen)
  
  coords <- st_coordinates(
    st_centroid(sf_grid)
  )
  
  knn <- knearneigh(coords, k = k)
  
  nb_knn <- knn2nb(knn)
  
  lw_knn <- nb2listw(
    nb_knn,
    style = "W",
    zero.policy = TRUE
  )
  
  W_knn <- listw_to_matrix(lw_knn)
  
  list(
    nb_queen = nb_queen,
    lw_queen = lw_queen,
    W_queen = W_queen,
    nb_knn = nb_knn,
    lw_knn = lw_knn,
    W_knn = W_knn
  )
}

# ------------------------------------------------------------
# 2.10 LISA
# ------------------------------------------------------------
lisa_local <- function(x, lw){
  
  Ii <- localmoran(
    x,
    lw,
    zero.policy = TRUE
  )
  
  as.data.frame(Ii)
}

# ============================================================
# 3. PARAMETROS
# ============================================================

grillas <- c(5, 10)

rhos <- c(
  -0.6,
  -0.3,
  0,
  0.3,
  0.6
)

configs_intervalo <- c(
  "A",
  "B",
  "C"
)

matrices <- c(
  "Queen",
  "KNN2"
)

nsim_perm <- 499

# ============================================================
# NUMERO DE REPLICAS MONTE CARLO
# ============================================================

n_reps <- 500

# ============================================================
# 4. SIMULACION GENERAL
# ============================================================

resultados_globales <- list()

for(g in grillas){
  
  cat("\nProcesando grilla:", g, "x", g, "\n")
  
  grid_sf <- crear_grilla_sf(g)
  
  pesos <- crear_pesos(grid_sf, k = 2)
  
  n_obs <- nrow(grid_sf)
  
  for(rho in rhos){
    
    for(rep in 1:n_reps){
      
      set.seed(12345 + rep)
      
      cat(
        "Grilla:",
        g,
        "| rho:",
        rho,
        "| replica:",
        rep,
        "\n"
      )
      
      z_queen <- simular_sar(
        row_standardize(pesos$W_queen),
        rho = rho,
        sigma = 1
      )
      
      z_knn <- simular_sar(
        row_standardize(pesos$W_knn),
        rho = rho,
        sigma = 1
      )
      
      for(cfg in configs_intervalo){
        
        int_q <- crear_intervalos(
          z_queen,
          cfg
        )
        
        int_k <- crear_intervalos(
          z_knn,
          cfg
        )
        
        for(matriz_nombre in matrices){
          
          if(matriz_nombre == "Queen"){
            
            W <- row_standardize(
              pesos$W_queen
            )
            
            lw <- pesos$lw_queen
            
            datos_int <- int_q
            
          } else {
            
            W <- row_standardize(
              pesos$W_knn
            )
            
            lw <- pesos$lw_knn
            
            datos_int <- int_k
          }
          
          # --------------------------------------------------
          # Moran clasico
          # --------------------------------------------------
          mc <- perm_moran(
            datos_int$centro,
            W,
            nsim = nsim_perm
          )
          
          # --------------------------------------------------
          # Moran simbolico
          # --------------------------------------------------
          ms <- perm_moran_simbolico(
            centro = datos_int$centro,
            radio = datos_int$radio,
            W = W,
            alpha = 0.5,
            nsim = nsim_perm
          )
          
          # --------------------------------------------------
          # Guardar resultados globales
          # --------------------------------------------------
          fila_res <- tibble(
            
            replica = rep,
            
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
            
            ancho_medio = mean(
              datos_int$ui - datos_int$li
            )
          )
          
          resultados_globales[
            [
              paste(
                g,
                rho,
                cfg,
                matriz_nombre,
                rep,
                sep = "_"
              )
            ]
          ] <- fila_res
          
          # --------------------------------------------------
          # SOLO GUARDAR GRAFICAS
          # EN LA PRIMERA REPLICA
          # --------------------------------------------------
          if(rep == 1){
            
            sf_escenario <- grid_sf %>%
              bind_cols(datos_int)
            
            p_centro <- ggplot(sf_escenario) +
              geom_sf(
                aes(fill = centro),
                color = "grey30",
                linewidth = 0.1
              ) +
              theme_minimal()
            
            p_radio <- ggplot(sf_escenario) +
              geom_sf(
                aes(fill = radio),
                color = "grey30",
                linewidth = 0.1
              ) +
              theme_minimal()
            
            p_intervalo <- ggplot(
              sf_escenario,
              aes(
                x = centro,
                y = ui - li
              )
            ) +
              geom_point() +
              theme_minimal()
            
            nom_base <- paste0(
              "g", g,
              "_rho",
              str_replace_all(
                as.character(rho),
                "-",
                "m"
              ),
              "_cfg",
              cfg,
              "_",
              matriz_nombre
            )
            
            ggsave(
              file.path(
                dir_mapas,
                paste0(
                  "mapa_centro_",
                  nom_base,
                  ".png"
                )
              ),
              p_centro,
              width = 7,
              height = 6,
              dpi = 300
            )
            
            ggsave(
              file.path(
                dir_mapas,
                paste0(
                  "mapa_radio_",
                  nom_base,
                  ".png"
                )
              ),
              p_radio,
              width = 7,
              height = 6,
              dpi = 300
            )
            
            ggsave(
              file.path(
                dir_diag,
                paste0(
                  "dispersion_",
                  nom_base,
                  ".png"
                )
              ),
              p_intervalo,
              width = 7,
              height = 5,
              dpi = 300
            )
          }
        }
      }
    }
  }
}

# ============================================================
# 5. CONSOLIDACION
# ============================================================

tabla_resultados <- bind_rows(
  resultados_globales
)

write_csv(
  tabla_resultados,
  file.path(
    dir_salidas,
    "resultados_globales_simulacion.csv"
  )
)

saveRDS(
  tabla_resultados,
  file.path(
    dir_rdata,
    "resultados_globales_simulacion.rds"
  )
)

# ============================================================
# 6. RESUMEN FINAL
# ============================================================

resumen_final <- tabla_resultados %>%
  group_by(
    grilla,
    matriz,
    config_intervalo,
    rho
  ) %>%
  summarise(
    
    media_I_clasico = mean(I_clasico),
    
    sd_I_clasico = sd(I_clasico),
    
    media_Is = mean(Is),
    
    sd_Is = sd(Is),
    
    potencia_clasico = mean(
      p_clasico < 0.05
    ),
    
    potencia_Is = mean(
      p_Is < 0.05
    ),
    
    .groups = "drop"
  )

write_csv(
  resumen_final,
  file.path(
    dir_salidas,
    "resumen_final_simulacion.csv"
  )
)

# ============================================================
# 7. GRAFICAS FINALES
# ============================================================

p1 <- ggplot(
  resumen_final,
  aes(
    x = rho,
    y = media_I_clasico,
    color = matriz
  )
) +
  geom_point() +
  geom_line() +
  facet_grid(
    grilla ~ config_intervalo
  ) +
  theme_minimal() +
  labs(
    title = "Moran clasico promedio",
    y = "I clasico promedio"
  )

ggsave(
  file.path(
    dir_graficas,
    "moran_clasico_promedio.png"
  ),
  p1,
  width = 10,
  height = 7,
  dpi = 300
)

p2 <- ggplot(
  resumen_final,
  aes(
    x = rho,
    y = media_Is,
    color = matriz
  )
) +
  geom_point() +
  geom_line() +
  facet_grid(
    grilla ~ config_intervalo
  ) +
  theme_minimal() +
  labs(
    title = "Moran simbolico promedio",
    y = "Is promedio"
  )

ggsave(
  file.path(
    dir_graficas,
    "moran_simbolico_promedio.png"
  ),
  p2,
  width = 10,
  height = 7,
  dpi = 300
)

# ============================================================
# 8. FINAL
# ============================================================

cat("\n========================================\n")

cat("SIMULACION FINALIZADA\n")

cat("Numero total de simulaciones:\n")

print(
  2 * 5 * 3 * 2 * n_reps
)

cat("\nResultados guardados en:\n")

print(
  normalizePath(dir_base)
)

cat("\n========================================\n")