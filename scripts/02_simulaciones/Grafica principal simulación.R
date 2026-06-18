# ============================================================
# VALIDACION EXPERIMENTAL MEDIANTE SIMULACION
# Autocorrelacion espacial clasica versus simbolica 
# ============================================================

# -----------------------------
# 0. CONFIGURACIÓN E INSTALACIÓN
# -----------------------------
setwd("C:/Users/aulasingenieria/Documents/TESIS") # Ajustar según sea necesario
rm(list = ls())
gc()
set.seed(12345)

req_pkgs <- c("sf", "spdep", "spatialreg", "dplyr", "tidyr", "purrr", 
              "ggplot2", "readr", "stringr", "tibble")
new_pkgs <- req_pkgs[!(req_pkgs %in% installed.packages()[, "Package"])]
if(length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(req_pkgs, library, character.only = TRUE))

# -----------------------------
# 1. DIRECTORIOS
# -----------------------------
dir_base     <- "Simulacion_SDA_Moran"
dir_salidas  <- file.path(dir_base, "02_Salidas")
dir_tablas   <- file.path(dir_salidas, "Tablas")
dir_graficas <- file.path(dir_base, "03_Graficas")

dirs <- c(dir_base, dir_salidas, dir_tablas, dir_graficas)
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# -----------------------------
# 2. FUNCIONES AUXILIARES
# -----------------------------

# Crear grilla espacial
crear_grilla_sf <- function(n_lado = 5) {
  bbox <- st_as_sfc(st_bbox(c(xmin = 0, ymin = 0, xmax = n_lado, ymax = n_lado), crs = st_crs(4326)))
  grd  <- st_make_grid(bbox, n = c(n_lado, n_lado), what = "polygons", square = TRUE)
  sf_obj <- st_sf(id = seq_along(grd), geometry = grd)
  return(sf_obj)
}

# Simular proceso SAR: x = (I - rho W)^-1 * eps
simular_sar <- function(W, rho, sigma = 1) {
  n   <- nrow(W)
  I_n <- diag(n)
  eps <- rnorm(n, mean = 0, sd = sigma)
  x   <- solve(I_n - rho * W, eps)
  as.numeric(scale(x))
}

# Crear intervalos (Configuración A: Amplitud homogénea)
crear_intervalos <- function(z) {
  n <- length(z)
  r <- runif(n, 0.15, 0.35) # Amplitud baja
  tibble(centro = as.numeric(z), radio = r)
}

# Moran clásico manual
moran_manual <- function(x, W) {
  n <- length(x)
  z <- x - mean(x)
  S0 <- sum(W)
  return( (n / S0) * (sum(W * (z %o% z))) / (sum(z^2)) )
}

# -----------------------------
# 3. EJECUCIÓN DE LA SIMULACIÓN
# -----------------------------
grillas  <- c(5, 10)
rhos     <- c(-0.6, -0.3, 0, 0.3, 0.6)
matrices <- c("Queen", "KNN2")

res_list <- list()

for (g in grillas) {
  # Preparar geometría y pesos
  grid_sf <- crear_grilla_sf(g)
  coords  <- st_coordinates(st_centroid(grid_sf))
  
  # Matriz Queen
  nb_q <- poly2nb(grid_sf)
  W_q  <- nb2mat(nb_q, style = "W", zero.policy = TRUE)
  
  # Matriz KNN (k=2)
  nb_k <- knn2nb(knearneigh(coords, k = 2))
  W_k  <- nb2mat(nb_k, style = "W", zero.policy = TRUE)
  
  for (rho in rhos) {
    for (m_nom in matrices) {
      # Seleccionar matriz activa
      W_act <- if(m_nom == "Queen") W_q else W_k
      
      # Generar datos
      z_sim <- simular_sar(W_act, rho)
      datos <- crear_intervalos(z_sim)
      
      # Cálculos de Moran
      I_clásico <- moran_manual(datos$centro, W_act)
      I_radio   <- moran_manual(datos$radio, W_act)
      Is        <- 0.5 * I_clásico + 0.5 * I_radio # Moran Simbólico
      
      # Almacenar
      res_list[[paste(g, rho, m_nom, sep="_")]] <- tibble(
        grilla = paste0(g, "x", g),
        rho    = rho,
        matriz = m_nom,
        Moran  = I_clásico,
        Is     = Is
      )
    }
  }
}

tabla_resultados <- bind_rows(res_list)

# -----------------------------
# 4. VISUALIZACIÓN UNIFICADA
# -----------------------------

# Preparar datos para ggplot (formato largo para asignar colores)
tabla_grafica <- tabla_resultados %>%
  pivot_longer(cols = c(Moran, Is), names_to = "Indice", values_to = "Valor") %>%
  mutate(
    matriz_label = ifelse(matriz == "KNN2", "KNN (k=2)", matriz)
  )

# Crear gráfica final
p_final <- ggplot(tabla_grafica, aes(x = rho, y = Valor, color = Indice, group = Indice)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  # Facetas: 2 columnas, combinando Grilla y Matriz como en image_216274.png
  facet_wrap(~ grilla + matriz_label, ncol = 2) +
  # Colores específicos: Is = Salmón/Rojo, Moran = Turquesa
  scale_color_manual(values = c("Is" = "#F8766D", "Moran" = "#00BFC4")) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Comparación de Índices de Autocorrelación Espacial",
    x = expression(rho),
    y = "Índice",
    color = "Índice:"
  )

# Guardar resultados
print(p_final)
ggsave(file.path(dir_graficas, "grafica_unificada_final.png"), p_final, width = 10, height = 7, dpi = 300)
write_csv(tabla_resultados, file.path(dir_tablas, "resultados_finales.csv"))

cat("\n>>> Proceso finalizado con éxito. Archivos en:", dir_base)