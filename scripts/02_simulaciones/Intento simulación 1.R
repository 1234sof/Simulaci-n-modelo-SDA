# Carga de librerías necesarias
library(spdep)     # Para análisis espacial clásico
library(sp)        # Para manejo de estructuras espaciales
library(Matrix)    # Para matrices de pesos

# 1. Definición de la Estructura Espacial (Grilla)
crear_escenario <- function(dim_grilla, rho, nivel_radio) {
  n <- dim_grilla^2
  grilla <- listw2sn(nb2listw(cell2nb(dim_grilla, dim_grilla, type="queen")))
  coords <- expand.grid(x = 1:dim_grilla, y = 1:dim_grilla)
  
  # 2. Determinación del Proceso Espacial Base (Simulación de Centros)
  # Se genera un proceso autoregresivo espacial (SAR) para los centros
  W <- nb2mat(cell2nb(dim_grilla, dim_grilla, type="queen"), style="W")
  inv_matrix <- solve(diag(n) - rho * W)
  centros <- inv_matrix %*% rnorm(n, mean = 10, sd = 2)
  
  # 3. Generación de Datos de Intervalo (Radios)
  # El radio representa la incertidumbre/variabilidad interna
  sd_radio <- switch(nivel_radio, "bajo" = 0.5, "medio" = 1.5, "alto" = 3.0)
  radios <- abs(rnorm(n, mean = 2, sd = sd_radio))
  
  # Estructura Simbólica: [Límite Inferior, Límite Superior]
  lim_inf <- centros - radios
  lim_sup <- centros + radios
  
  return(data.frame(id=1:n, centro=as.numeric(centros), radio=radios, L=lim_inf, U=lim_sup))
}

# 4. Cálculo del Índice de Moran Simbólico (Is)
# Is = (Ic + Ir) / 2 (Propuesta metodológica resumida)
calcular_moran_simbolico <- function(datos, dim_grilla) {
  nb <- cell2nb(dim_grilla, dim_grilla, type="queen")
  listw <- nb2listw(nb, style="W")
  
  # Moran para centros (Ic) y para radios (Ir)
  moran_c <- moran.test(datos$centro, listw)$estimate[1]
  moran_r <- moran.test(datos$radio, listw)$estimate[1]
  
  return(list(Ic = moran_c, Ir = moran_r, Is = (moran_c + moran_r)/2))
}

# Ejemplo de ejecución para un escenario específico (Escenario 1: 5x5, rho=-0.6, bajo)
set.seed(123)
datos_sim <- crear_escenario(5, -0.6, "alto")
resultados <- calcular_moran_simbolico(datos_sim, 5)
print(resultados)