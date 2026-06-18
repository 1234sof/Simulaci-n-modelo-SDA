library(dplyr)
library(readxl)
library(stringr)

# Cragar y Anaplizar pimero el shapefile de colombia 
#paquetes necesarios
install.packages("sf")   # solo si no la tienes
library(sf)
#definir carpeta de trabajo  para departamentos 

setwd("C:/Users/aulasingenieria/Downloads/MGN2025_DPTO_POLITICO")
depto_shp <- st_read("MGN_ADM_DPTO_POLITICO.shp")

#ver que tenga los archivos necesarios 
list.files()

#ver comoe sta conofrmada ese shp
names(depto_shp)
head(depto_shp)

#despues de tener el shp, pasamos a importar 
#la base de datos con cifras de población totales por año 

Proyeccion_población <- Proyeccion_municipios
View(Proyeccion_población)

#ver las columnas con las que vamos a trabajar
names(Proyeccion_población)

#imprimir muestra para inspeccionar datos por encima 
head(Proyeccion_población)

#se suben datos limpios con variable que reunine casos de violencia físicia
#sexual 

head(`df_colombia_final.(1)`)

#ver las columnas con las que vamos a trabajar
names(`df_colombia_final.(1)`)

df_colombia <- `df_colombia_final.(1)`


names(df_colombia)

#se tiene que llamar igual todas las llaves de municipio y departamento 
#con shapeline, esta base de poryección poblacional y la data a anpalizar 

poblacion_clean <- Proyeccion_población %>%
  #renombrar variables para que se puedan hacer el join mpas facil
  rename(
    dpto_ccdgo = DP,
    dpto_cnmbr = DPNOM,
    ano = `AÑO`,
    tipo = `ÁREA GEOGRÁFICA`,
    poblacion = `Mujeres`
  ) %>%
  filter(tipo == "Total") %>%
  mutate(
    dpto_ccdgo = str_pad(dpto_ccdgo, 2, pad = "0"),
    ano = as.numeric(ano)
  ) %>%
  select(dpto_ccdgo, dpto_cnmbr, ano, poblacion)

head(poblacion_clean)

poblacion_clean <- poblacion_clean %>%
  filter(ano >= 2020 & ano <= 2025)


poblacion_dep <- poblacion_clean %>%
  group_by(dpto_ccdgo,dpto_cnmbr, ano) %>%
  summarise(
    poblacion = sum(poblacion, na.rm = TRUE),
    .groups = "drop"
  )

unique(poblacion_dep$ano)

#ahora se van a agregar los casos de la base de datos a analizar 

df_clean <- df_colombia %>%
  rename(
    dpto_ccdgo = cod_dpto_o,
    dpto_cnmbr = ndep_proce,
    ano = anio_con
  ) %>%
  # para que los codigos queden en char. convertir de numero a texto
  mutate(
    dpto_ccdgo = str_pad(as.character(dpto_ccdgo), 2, pad = "0"),
    ano = as.numeric(ano)
  )

names(df_clean)


df_dep <- df_clean %>%
  group_by(dpto_ccdgo, ano) %>%
  summarise(
    casos = sum(violencia.general, na.rm = TRUE),
    .groups = "drop"
  )

poblacion_dep %>%
  count(dpto_ccdgo, ano) %>%
  filter(n > 1)

df_dep %>%
  count(dpto_ccdgo, ano) %>%
  filter(n > 1)

df_dep <- df_dep %>%
  left_join(poblacion_dep, by = c("dpto_ccdgo", "ano"))


#Mirar si el join quedo bien. Si quedó
sum(is.na(df_dep$poblacion))

#Calcular tasa de casos por pobación por cada 100.000 habitantes.


df_dep %>%
  group_by(dpto_ccdgo) %>%
  summarise(
    anos = paste(sort(unique(ano)), collapse = ", ")
  )



todos_anos <- 2020:2025

df_dep %>%
  group_by(dpto_ccdgo) %>%
  summarise(n_anos = n_distinct(ano)) %>%
  print(n = Inf)

df_dep %>%
  group_by(dpto_ccdgo) %>%
  summarise(n_anos = n_distinct(ano)) %>%
  filter(n_anos >= 4)

df_dep %>%
  group_by(dpto_ccdgo) %>%
  summarise(n_anos = n_distinct(ano)) %>%
  filter(n_anos < 4)

#calcular tasas anuales 

df_dep <- df_dep %>%
  mutate(
    tasa_100k = (casos / poblacion) * 100000,
    tasa_log = log1p(tasa_100k)
  )

#mapa 1 primedio de tasas anuales normales
tasa_promedio <- df_dep %>%
  group_by(dpto_ccdgo) %>%
  summarise(
    tasa_prom = mean(tasa_100k, na.rm = TRUE)
    
  )

tasa_promedio_log <- df_dep %>%
  group_by(dpto_ccdgo) %>%
  summarise(
    tasa_prom_log = mean(tasa_log, na.rm = TRUE)
  )



#unir con shapefile 

depto_shp$dpto_ccdgo <- str_pad(depto_shp$dpto_ccdgo, 2, pad = "0")

mapa_prom <- depto_shp %>%
  left_join(tasa_promedio, by = "dpto_ccdgo")

mapa_prom_log <- depto_shp %>%
  left_join(tasa_promedio_log, by = "dpto_ccdgo")


install.packages("patchwork")
library(ggplot2)
library(patchwork)
install.packages("viridis")
library(viridis)

limites <- range(
  c(mapa_prom$tasa_prom, mapa_prom_log$tasa_prom_log),
  na.rm = TRUE
)


#promedio crudo

p1 <- ggplot(mapa_prom) +
  geom_sf(aes(fill = tasa_prom), color = NA) +
  scale_fill_viridis_c(option = "magma") +
  labs(title = "a. Tasa normal", fill = "Tasa") +
  theme_void() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

#estandarizado
p2 <- ggplot(mapa_prom_log) +
  geom_sf(aes(fill = tasa_prom_log), color = NA) +
  scale_fill_viridis_c(option = "magma") +
  labs(title = "b. Tasa logarítmica", fill = "Log(Tasa)") +
  theme_void() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

#juntar los dos mapas 

install.packages("patchwork")
library(patchwork)


(p1 | p2) +
  plot_annotation(
    title = "Tasas promedio de violencia física y sexual (2020–2025)",
    theme = theme(
      plot.title = element_text(
        color = "#4B0082",
        size = 16,
        face = "bold",
        hjust = 0.5 
      )
    )
  )



###AHORA LA TASA POR PERSONA-AÑO

resumen <- df_dep %>%
  group_by(dpto_ccdgo) %>%
  summarise(
    total_casos = sum(casos, na.rm = TRUE),
    total_poblacion = sum(poblacion, na.rm = TRUE),
    n_anos = n_distinct(ano),
    anos_disponibles = paste(sort(unique(ano)), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    tasa_acumulada_ponderada = (total_casos / total_poblacion) * 100000,
    tasa_acumulada_log = log1p(tasa_acumulada_ponderada)
  )

depto_shp$dpto_ccdgo <- str_pad(depto_shp$dpto_ccdgo, 2, pad = "0")

mapa_tasa_ponder <- depto_shp %>%
  left_join(resumen %>% select(dpto_ccdgo, tasa_acumulada_ponderada), 
            by = "dpto_ccdgo")

mapa_ponder_log <- depto_shp %>%
  left_join(resumen %>% select(dpto_ccdgo, tasa_acumulada_log), 
            by = "dpto_ccdgo")


p1 <- ggplot(mapa_tasa_ponder) +
  geom_sf(aes(fill = tasa_acumulada_ponderada), color = NA) +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "a. Tasa acumulada ponderada",
    fill = "Tasa"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

p2 <- ggplot(mapa_ponder_log) +
  geom_sf(aes(fill = tasa_acumulada_log), color = NA) +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "b. Tasa acumulada (log)",
    fill = "log(Tasa)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

(p1 | p2) +
  plot_annotation(
    title = "Tasa persona-año de violencia física y sexual (2020-2025)",
    theme = theme(
      plot.title = element_text(
        color = "#4B0082",
        size = 16,
        face = "bold",
        hjust = 0.5
      )
    )
  )



# ── TENDENCIA DEPARTAMENTOS ───────────────────────────────────────────────────
# En departamentos tienes cobertura alta, así que la pendiente es confiable

tasa_tendencia_dep <- df_dep %>%               # aquí df_dep es el de departamentos
  filter(!is.na(tasa_100k)) %>%
  group_by(dpto_ccdgo) %>%
  filter(n_distinct(ano) >= 3) %>%
  summarise(
    pendiente = coef(lm(tasa_100k ~ ano))[2],
    n_anos    = n_distinct(ano),
    .groups   = "drop"
  )

mapa_tend_dep <- depto_shp %>%
  left_join(tasa_tendencia_dep, by = "dpto_ccdgo")

lim_dep <- quantile(abs(mapa_tend_dep$pendiente), 0.95, na.rm = TRUE)

p3 <- ggplot(mapa_tend_dep) +
  geom_sf(aes(fill = pendiente), color = "black", size = 0.05)+
  scale_fill_gradient2(
    low      = "#185FA5",
    mid      = "white",
    high     = "#A32D2D",
    midpoint = 0,
    limits   = c(-lim_dep, lim_dep),
    oob      = scales::squish,
    na.value = "grey85",
    name     = "Puntos/año"
  ) +
  labs(title = "Tendencia anual de la tasa de violencia\n contra la mujer por departamento (2020–2025)") +
  theme_void() +
  theme(plot.title      = element_text(size = 12, face = "bold"),
        legend.position = "right")

p3

tasa_tendencia_dep %>%
  filter(n_anos < 3) %>%
  left_join(poblacion_dep %>% distinct(dpto_ccdgo, dpto_cnmbr), 
            by = "dpto_ccdgo") %>%
  select(dpto_cnmbr, n_anos)



##########################Moran clásico########

# ══════════════════════════════════════════════════════════════════════════════
# SCRIPT UNIFICADO: ROBUSTEZ DE VECINDAD + MORAN GLOBAL + LISA + MAPAS
# Variable: tasa persona-año
# Cambia solo el bloque CONFIGURACIÓN para cada nivel territorial
# ══════════════════════════════════════════════════════════════════════════════

library(spdep)
library(ggplot2)
install.packages("ggtext")
library(ggtext)
library(dplyr)
library(sf)
library(patchwork)

# ── FUNCIÓN PARA FORMATEAR p-valor EN NOTACIÓN CIENTÍFICA ────────────────────

formato_p_html <- function(p) {
  if (p < 0.001) {
    exp  <- floor(log10(p))
    base <- round(p / 10^exp, 2)
    paste0(base, " × 10<sup>", exp, "</sup>")
  } else {
    paste0("= ", round(p, 3))
  }
}

# ── CALCULAR TASA PERSONA-AÑO ─────────────────────────────────────────────────

# DEPARTAMENTOS
tasa_pa_dep <- df_dep %>%
  filter(!is.na(tasa_log)) %>%
  group_by(dpto_ccdgo) %>%
  summarise(
    total_casos      = sum(casos,     na.rm = TRUE),
    total_mujer_anos = sum(poblacion, na.rm = TRUE),
    n_anos           = n_distinct(ano),
    .groups          = "drop"
  ) %>%
  mutate(tasa_persona_ano = (total_casos / total_mujer_anos) * 100000)

mapa_dep_pa <- depto_shp %>%
  left_join(tasa_pa_dep, by = "dpto_ccdgo")

# MUNICIPIOS
tasa_pa_mun <- df_dep %>%
  filter(!is.na(tasa_100k)) %>%
  group_by(mpio_cdpmp) %>%
  summarise(
    total_casos      = sum(casos,     na.rm = TRUE),
    total_mujer_anos = sum(poblacion, na.rm = TRUE),
    n_anos           = n_distinct(ano),
    .groups          = "drop"
  ) %>%
  mutate(tasa_persona_ano = (total_casos / total_mujer_anos) * 100000)

mapa_mun_pa <- muni_shp %>%
  left_join(tasa_pa_mun, by = "mpio_cdpmp")

# BOGOTÁ
tasa_pa_loc <- df_loc %>%
  filter(!is.na(tasa_100k)) %>%
  group_by(cod_localidad) %>%
  summarise(
    total_casos      = sum(casos,     na.rm = TRUE),
    total_mujer_anos = sum(poblacion, na.rm = TRUE),
    n_anos           = n_distinct(ano),
    .groups          = "drop"
  ) %>%
  mutate(tasa_persona_ano = (total_casos / total_mujer_anos) * 100000)

mapa_loc_pa <- localidades_shp %>%
  left_join(tasa_pa_loc, by = "cod_localidad")

# ══════════════════════════════════════════════════════════════════════════════
# CONFIGURACIÓN — cambia solo estas líneas para cada nivel
# ══════════════════════════════════════════════════════════════════════════════

# DEPARTAMENTOS:
mapa_sf      <- mapa_dep_pa %>% filter(!is.na(tasa_persona_ano))
variable     <- "tasa_persona_ano"
nivel_nombre <- "departamento"
tabla_pa     <- tasa_pa_dep

# MUNICIPIOS:
mapa_sf      <- mapa_mun_pa %>% filter(!is.na(tasa_persona_ano))
variable     <- "tasa_persona_ano"
nivel_nombre <- "municipio"
tabla_pa     <- tasa_pa_mun

# BOGOTÁ:
# mapa_sf      <- mapa_loc_pa %>% filter(!is.na(tasa_persona_ano))
# variable     <- "tasa_persona_ano"
# nivel_nombre <- "localidad de Bogotá"
# tabla_pa     <- tasa_pa_loc

titulo_color <- "#4B0082"

# ══════════════════════════════════════════════════════════════════════════════
# A PARTIR DE AQUÍ EL CÓDIGO ES IGUAL PARA LOS TRES NIVELES
# ══════════════════════════════════════════════════════════════════════════════

# ── CENTROIDES ────────────────────────────────────────────────────────────────

centroides <- st_centroid(st_geometry(mapa_sf))
coords     <- st_coordinates(centroides)

# ── DISTANCIA UMBRAL MÍNIMA ───────────────────────────────────────────────────

dist_umbral <- max(unlist(nbdists(
  knn2nb(knearneigh(coords, k = 1)), coords
)))
cat("Distancia umbral mínima:", round(dist_umbral, 2), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 1. MATRICES DE VECINDAD Y MORAN GLOBAL POR CRITERIO
# ══════════════════════════════════════════════════════════════════════════════

# Rook
vecinos_rook  <- poly2nb(mapa_sf, queen = FALSE)
pesos_rook    <- nb2listw(vecinos_rook,  style = "W", zero.policy = TRUE)
moran_rook    <- moran.test(mapa_sf[[variable]], pesos_rook,
                            zero.policy = TRUE, alternative = "greater")

# Queen
vecinos_queen <- poly2nb(mapa_sf, queen = TRUE)
pesos_queen   <- nb2listw(vecinos_queen, style = "W", zero.policy = TRUE)
moran_queen   <- moran.test(mapa_sf[[variable]], pesos_queen,
                            zero.policy = TRUE, alternative = "greater")

# K = 3
vecinos_k3  <- knn2nb(knearneigh(coords, k = 3))
pesos_k3    <- nb2listw(vecinos_k3, style = "W", zero.policy = TRUE)
moran_k3    <- moran.test(mapa_sf[[variable]], pesos_k3,
                          zero.policy = TRUE, alternative = "greater")

# K = 5
vecinos_k5  <- knn2nb(knearneigh(coords, k = 5))
pesos_k5    <- nb2listw(vecinos_k5, style = "W", zero.policy = TRUE)
moran_k5    <- moran.test(mapa_sf[[variable]], pesos_k5,
                          zero.policy = TRUE, alternative = "greater")

# K = 8
vecinos_k8  <- knn2nb(knearneigh(coords, k = 8))
pesos_k8    <- nb2listw(vecinos_k8, style = "W", zero.policy = TRUE)
moran_k8    <- moran.test(mapa_sf[[variable]], pesos_k8,
                          zero.policy = TRUE, alternative = "greater")

# Distancia umbral
vecinos_dist <- dnearneigh(coords, d1 = 0, d2 = dist_umbral)
pesos_dist   <- nb2listw(vecinos_dist, style = "W", zero.policy = TRUE)
moran_dist   <- moran.test(mapa_sf[[variable]], pesos_dist,
                           zero.policy = TRUE, alternative = "greater")

# ── TABLA RESUMEN DE ROBUSTEZ ─────────────────────────────────────────────────

tabla_robustez <- data.frame(
  Criterio = c("Rook", "Queen", "K = 3", "K = 5", "K = 8", "Distancia umbral"),
  I_Moran  = round(c(
    moran_rook$estimate[1],  moran_queen$estimate[1],
    moran_k3$estimate[1],    moran_k5$estimate[1],
    moran_k8$estimate[1],    moran_dist$estimate[1]
  ), 4),
  p_valor = round(c(
    moran_rook$p.value,  moran_queen$p.value,
    moran_k3$p.value,    moran_k5$p.value,
    moran_k8$p.value,    moran_dist$p.value
  ), 4),
  Significativo = ifelse(c(
    moran_rook$p.value,  moran_queen$p.value,
    moran_k3$p.value,    moran_k5$p.value,
    moran_k8$p.value,    moran_dist$p.value
  ) < 0.05, "Sí", "No"),
  Vecinos_prom = round(c(
    mean(card(vecinos_rook)),  mean(card(vecinos_queen)),
    3, 5, 8,
    mean(card(vecinos_dist))
  ), 1)
)

print(tabla_robustez)

# ── GRÁFICO DE ROBUSTEZ ───────────────────────────────────────────────────────

fig_robustez <- ggplot(tabla_robustez,
                       aes(x    = reorder(Criterio, I_Moran),
                           y    = I_Moran,
                           fill = Significativo)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("I = ", I_Moran, "\np = ", p_valor)),
            hjust = -0.1, size = 3.2, color = "grey30") +
  scale_fill_manual(
    values = c("Sí" = "#4B0082", "No" = "grey70"),
    name   = "p < 0.05"
  ) +
  coord_flip(ylim = c(0, max(tabla_robustez$I_Moran) * 1.4)) +
  labs(
    title = paste("Robustez del I de Moran por criterio de vecindad\n",
                  nivel_nombre, "— tasa persona-año (2020–2025)"),
    x = "Criterio",
    y = "I de Moran"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(color = titulo_color, size = 13,
                                   face = "bold", hjust = 0.5),
    legend.position = "right"
  )

fig_robustez


# ══════════════════════════════════════════════════════════════════════════════
# 2. SELECCIONAR CRITERIO PARA LISA
# ══════════════════════════════════════════════════════════════════════════════

pesos_final        <- pesos_k3    # <- cambia según tabla de robustez
criterio_seleccion <- "K=3"         # <- para títulos

# ══════════════════════════════════════════════════════════════════════════════
# 3. MORAN GLOBAL FINAL CON MONTE CARLO
# ══════════════════════════════════════════════════════════════════════════════

moran_final <- moran.mc(
  mapa_sf[[variable]],
  pesos_final,
  nsim        = 9999,
  zero.policy = TRUE,
  alternative = "greater"
)

I_final <- round(moran_final$statistic, 4)
p_final <- round(moran_final$p.value,   4)

cat("\n── Moran Global (criterio:", criterio_seleccion, ", Monte Carlo 9999) ──\n")
cat("I de Moran:", I_final, "\n")
cat("p-valor:   ", p_final, "\n")
cat("n años promedio por unidad:", round(mean(tabla_pa$n_anos), 1), "\n")
if (p_final < 0.05) {
  cat("→ Hay autocorrelación espacial positiva significativa\n")
} else {
  cat("→ No se detecta autocorrelación espacial significativa\n")
}

# ── ESTANDARIZAR Y LAG ESPACIAL ───────────────────────────────────────────────

mapa_sf <- mapa_sf %>%
  mutate(
    z_var = scale(.data[[variable]])[, 1],
    lag_z = lag.listw(pesos_final, z_var, zero.policy = TRUE)
  )

# ── DIAGRAMA DE MORAN CON p EN NOTACIÓN CIENTÍFICA ───────────────────────────
install.packages("ggtext")
library(ggtext)
fig_moran <- ggplot(mapa_sf, aes(x = z_var, y = lag_z)) +
  geom_point(color = "#4B0082", alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", color = "#A32D2D",
              se = FALSE, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_richtext(
    aes(x     = Inf, y = Inf,
        label = paste0("*I* = ", I_final,
                       "<br>*p* ", formato_p_html(p_final))),
    hjust       = 1.1,
    vjust       = 1.5,
    size        = 4,
    color       = "#4B0082",
    fill        = NA,
    label.color = NA,
    inherit.aes = FALSE
  ) +
  labs(
    title = paste("Diagrama de Moran —", criterio_seleccion,
                  "\npor", nivel_nombre),
    x = "Tasa estandarizada (z)",
    y = "Lag espacial (z)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(color = titulo_color, size = 13,
                                  face = "bold", hjust = 0.5))

fig_moran

# ══════════════════════════════════════════════════════════════════════════════
# 4. LISA
# ══════════════════════════════════════════════════════════════════════════════

lisa <- localmoran(mapa_sf[[variable]], pesos_final, zero.policy = TRUE)

mapa_sf <- mapa_sf %>%
  mutate(
    Ii       = lisa[, 1],
    p_lisa   = lisa[, 5],
    lisa_cat = case_when(
      p_lisa > 0.05          ~ "No significativo",
      z_var > 0 & lag_z > 0  ~ "Alto-Alto (hot spot)",
      z_var < 0 & lag_z < 0  ~ "Bajo-Bajo (cold spot)",
      z_var > 0 & lag_z < 0  ~ "Alto-Bajo (outlier)",
      z_var < 0 & lag_z > 0  ~ "Bajo-Alto (outlier)",
      TRUE                   ~ "No significativo"
    ),
    lisa_cat = factor(lisa_cat, levels = c(
      "Alto-Alto (hot spot)",
      "Bajo-Bajo (cold spot)",
      "Alto-Bajo (outlier)",
      "Bajo-Alto (outlier)",
      "No significativo"
    )),
    sig_cat = case_when(
      p_lisa <= 0.01 ~ "p ≤ 0.01",
      p_lisa <= 0.05 ~ "p ≤ 0.05",
      TRUE           ~ "No significativo"
    ),
    sig_cat = factor(sig_cat,
                     levels = c("p ≤ 0.01", "p ≤ 0.05", "No significativo"))
  )

cat("\nDistribución LISA (", criterio_seleccion, "):\n")
print(table(mapa_sf$lisa_cat))

# ── MAPA LISA ─────────────────────────────────────────────────────────────────

fig_lisa <- ggplot(mapa_sf) +
  geom_sf(aes(fill = lisa_cat), color = "white", size = 0.2) +  # 👈 bordes blancos
  scale_fill_manual(
    values = c(
      "Alto-Alto (hot spot)"  = "#A32D2D",
      "Bajo-Bajo (cold spot)" = "#185FA5",
      "Alto-Bajo (outlier)"   = "#F09595",
      "Bajo-Alto (outlier)"   = "#85B7EB",
      "No significativo"      = "grey85"
    ),
    name = "",  # 👈 como en tu imagen
    drop = TRUE
  ) +
  labs(
    title = paste("Clústeres espaciales locales de la tasa de \n violencia contra la mujer (2020–2025)")
  ) +
  theme_void() +  # 👈 quita grilla automáticamente
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5
    ),
    legend.position = "right"
  )


fig_lisa

# ── MAPA DE SIGNIFICANCIA ─────────────────────────────────────────────────────

fig_pval <- ggplot(mapa_sf) +
  geom_sf(aes(fill = sig_cat), color = NA) +
  scale_fill_manual(
    values = c(
      "p ≤ 0.01"         = "#4B0082",
      "p ≤ 0.05"         = "#9F6EC0",
      "No significativo"  = "grey85"
    ),
    name = "Significancia"
  ) +
  labs(title = paste("Significancia LISA\npor", nivel_nombre)) +
  theme_void() +
  theme(plot.title      = element_text(color = titulo_color, size = 13,
                                       face = "bold", hjust = 0.5),
        legend.position = "right")

fig_pval

# ══════════════════════════════════════════════════════════════════════════════
# 5. PANEL FINAL
# ══════════════════════════════════════════════════════════════════════════════

fig_robustez /
  (fig_moran | fig_lisa | fig_pval) +
  plot_layout(heights = c(1, 1.2)) +
  plot_annotation(
    title = paste(
      "Autocorrelación espacial — violencia contra la mujer\npor",
      nivel_nombre, "(tasa persona-año, 2020–2025)"
    ),
    theme = theme(
      plot.title = element_text(
        color = titulo_color, size = 15, face = "bold", hjust = 0.5)
    )
  )


# ── MORAN MONTE CARLO PARA TODOS LOS CRITERIOS ───────────────────────────────

mc_rook  <- moran.mc(mapa_sf[[variable]], pesos_rook,  nsim = 9999, zero.policy = TRUE)
mc_queen <- moran.mc(mapa_sf[[variable]], pesos_queen, nsim = 9999, zero.policy = TRUE)
mc_k3    <- moran.mc(mapa_sf[[variable]], pesos_k3,    nsim = 9999, zero.policy = TRUE)
mc_k5    <- moran.mc(mapa_sf[[variable]], pesos_k5,    nsim = 9999, zero.policy = TRUE)
mc_k8    <- moran.mc(mapa_sf[[variable]], pesos_k8,    nsim = 9999, zero.policy = TRUE)
mc_dist  <- moran.mc(mapa_sf[[variable]], pesos_dist,  nsim = 9999, zero.policy = TRUE)


tabla_mc <- data.frame(
  Criterio = c("Rook", "Queen", "K=3", "K=5", "K=8", "Distancia"),
  
  I_clasico = round(c(
    moran_rook$estimate[1],
    moran_queen$estimate[1],
    moran_k3$estimate[1],
    moran_k5$estimate[1],
    moran_k8$estimate[1],
    moran_dist$estimate[1]
  ), 4),
  
  p_clasico = round(c(
    moran_rook$p.value,
    moran_queen$p.value,
    moran_k3$p.value,
    moran_k5$p.value,
    moran_k8$p.value,
    moran_dist$p.value
  ), 4),
  
  I_mc = round(c(
    mc_rook$statistic,
    mc_queen$statistic,
    mc_k3$statistic,
    mc_k5$statistic,
    mc_k8$statistic,
    mc_dist$statistic
  ), 4),
  
  p_mc = round(c(
    mc_rook$p.value,
    mc_queen$p.value,
    mc_k3$p.value,
    mc_k5$p.value,
    mc_k8$p.value,
    mc_dist$p.value
  ), 4)
)

print(tabla_mc)


top_criterios <- tabla_mc %>%
  arrange(p_mc) %>%
  slice(1:3)

print(top_criterios)

lista_pesos <- list(
  "Rook" = pesos_rook,
  "Queen" = pesos_queen,
  "K=3" = pesos_k3,
  "K=5" = pesos_k5,
  "K=8" = pesos_k8,
  "Distancia" = pesos_dist
)

crear_mapa_lisa <- function(pesos, nombre) {
  
  lisa_tmp <- localmoran(mapa_sf[[variable]], pesos, zero.policy = TRUE)
  
  df_tmp <- mapa_sf %>%
    mutate(
      p_lisa = lisa_tmp[, 5],
      lisa_cat = case_when(
        p_lisa > 0.05          ~ "No significativo",
        z_var > 0 & lag_z > 0  ~ "Alto-Alto (hot spot)",
        z_var < 0 & lag_z < 0  ~ "Bajo-Bajo (cold spot)",
        z_var > 0 & lag_z < 0  ~ "Alto-Bajo (outlier)",
        z_var < 0 & lag_z > 0  ~ "Bajo-Alto (outlier)",
        TRUE                   ~ "No significativo"
      )
    )
  
  df_tmp$lisa_cat <- droplevels(factor(df_tmp$lisa_cat))
  
  ggplot(df_tmp) +
    geom_sf(aes(fill = lisa_cat), color = "white", size = 0.2) +
    scale_fill_manual(
      values = c(
        "Alto-Alto (hot spot)"  = "#A32D2D",
        "Bajo-Bajo (cold spot)" = "#185FA5",
        "Alto-Bajo (outlier)"   = "#F09595",
        "Bajo-Alto (outlier)"   = "#85B7EB",
        "No significativo"      = "grey85"
      ),
      drop = TRUE
    ) +
    labs(title = nombre) +
    theme_void() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5)
    )
}

mapas_lisa <- lapply(top_criterios$Criterio, function(c) {
  crear_mapa_lisa(lista_pesos[[c]], c)
})

fig_lisa_comp <- mapas_lisa[[1]] | mapas_lisa[[2]] | mapas_lisa[[3]] +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Comparación de clústeres espaciales (LISA)\ncriterios más significativos"
  )

fig_lisa_comp



##########################LOGARITMO CON CLASICO ##################3

tasa_pa_dep <- df_dep %>%
  group_by(dpto_ccdgo) %>%
  summarise(
    total_casos      = sum(casos, na.rm = TRUE),
    total_mujer_anos = sum(poblacion, na.rm = TRUE),
    n_anos           = n_distinct(ano),
    .groups = "drop"
  ) %>%
  mutate(
    tasa_persona_ano = (total_casos / total_mujer_anos) * 100000,
    tasa_log = log1p(tasa_persona_ano)
  )

mapa_dep_pa <- depto_shp %>%
  left_join(tasa_pa_dep, by = "dpto_ccdgo")

mapa_sf      <- mapa_dep_pa %>% filter(!is.na(tasa_log))
variable     <- "tasa_log"
nivel_nombre <- "departamento"
tabla_pa     <- tasa_pa_dep
titulo_color <- "#4B0082"


centroides <- st_centroid(st_geometry(mapa_sf))
coords     <- st_coordinates(centroides)

dist_umbral <- max(unlist(nbdists(
  knn2nb(knearneigh(coords, k = 1)), coords
)))


vecinos_rook  <- poly2nb(mapa_sf, queen = FALSE)
pesos_rook    <- nb2listw(vecinos_rook, style = "W", zero.policy = TRUE)
moran_rook    <- moran.test(mapa_sf[[variable]], pesos_rook)

vecinos_queen <- poly2nb(mapa_sf, queen = TRUE)
pesos_queen   <- nb2listw(vecinos_queen, style = "W", zero.policy = TRUE)
moran_queen   <- moran.test(mapa_sf[[variable]], pesos_queen)

vecinos_k3  <- knn2nb(knearneigh(coords, k = 3))
pesos_k3    <- nb2listw(vecinos_k3, style = "W", zero.policy = TRUE)
moran_k3    <- moran.test(mapa_sf[[variable]], pesos_k3)

vecinos_k5  <- knn2nb(knearneigh(coords, k = 5))
pesos_k5    <- nb2listw(vecinos_k5, style = "W", zero.policy = TRUE)
moran_k5    <- moran.test(mapa_sf[[variable]], pesos_k5)

vecinos_k8  <- knn2nb(knearneigh(coords, k = 8))
pesos_k8    <- nb2listw(vecinos_k8, style = "W", zero.policy = TRUE)
moran_k8    <- moran.test(mapa_sf[[variable]], pesos_k8)

vecinos_dist <- dnearneigh(coords, 0, dist_umbral)
pesos_dist   <- nb2listw(vecinos_dist, style = "W", zero.policy = TRUE)
moran_dist   <- moran.test(mapa_sf[[variable]], pesos_dist)


tabla_robustez <- data.frame(
  Criterio = c("Rook", "Queen", "K = 3", "K = 5", "K = 8", "Distancia"),
  I_Moran  = round(c(
    moran_rook$estimate[1],
    moran_queen$estimate[1],
    moran_k3$estimate[1],
    moran_k5$estimate[1],
    moran_k8$estimate[1],
    moran_dist$estimate[1]
  ), 4),
  p_valor = round(c(
    moran_rook$p.value,
    moran_queen$p.value,
    moran_k3$p.value,
    moran_k5$p.value,
    moran_k8$p.value,
    moran_dist$p.value
  ), 4),
  Significativo = ifelse(c(
    moran_rook$p.value,
    moran_queen$p.value,
    moran_k3$p.value,
    moran_k5$p.value,
    moran_k8$p.value,
    moran_dist$p.value
  ) < 0.05, "Sí", "No")
)

print(tabla_robustez)


lista_pesos <- list(
  "Rook"       = pesos_rook,
  "Queen"      = pesos_queen,
  "K=3"        = pesos_k3,
  "K=5"        = pesos_k5,
  "K=8"        = pesos_k8,
  "Distancia"  = pesos_dist
)

run_mc <- function(variable, pesos_lista) {
  
  res <- lapply(names(pesos_lista), function(nm) {
    
    mc <- moran.mc(
      variable,
      pesos_lista[[nm]],
      nsim = 9999,
      zero.policy = TRUE
    )
    
    data.frame(
      Criterio = nm,
      I_mc     = as.numeric(mc$statistic),
      p_mc     = mc$p.value
    )
  })
  
  do.call(rbind, res)
}



tabla_mc_cruda <- run_mc(
  variable = mapa_sf$tasa_100k,
  pesos_lista = lista_pesos
)

tabla_mc_cruda <- tabla_mc_cruda %>%
  mutate(
    Significativo = ifelse(p_mc < 0.05, "Sí", "No")
  )

print(tabla_mc_cruda)

tabla_mc_log <- run_mc(
  variable = mapa_sf$tasa_log,
  pesos_lista = lista_pesos
)

tabla_mc_log <- tabla_mc_log %>%
  mutate(
    Significativo = ifelse(p_mc < 0.05, "Sí", "No")
  )

print(tabla_mc_log)

mapa_sf <- mapa_sf %>%
  mutate(
    z_var = scale(.data[[variable]])[,1],
    lag_z = lag.listw(pesos_final, z_var, zero.policy = TRUE)
  )

lisa <- localmoran(mapa_sf[[variable]], pesos_final)

mapa_sf <- mapa_sf %>%
  mutate(
    Ii = lisa[,1],
    p_lisa = lisa[,5],
    lisa_cat = case_when(
      p_lisa > 0.05          ~ "No significativo",
      z_var > 0 & lag_z > 0  ~ "Alto-Alto (hot spot)",
      z_var < 0 & lag_z < 0  ~ "Bajo-Bajo (cold spot)",
      z_var > 0 & lag_z < 0  ~ "Alto-Bajo (outlier)",
      z_var < 0 & lag_z > 0  ~ "Bajo-Alto (outlier)",
      TRUE ~ "No significativo"
    )
  )

fig_lisa1 <- ggplot(mapa_sf) +
  geom_sf(aes(fill = lisa_cat), color = "white", size = 0.2) +
  scale_fill_manual(values = c(
    "Alto-Alto (hot spot)"  = "#A32D2D",
    "Bajo-Bajo (cold spot)" = "#185FA5",
    "Alto-Bajo (outlier)"   = "#F09595",
    "Bajo-Alto (outlier)"   = "#85B7EB",
    "No significativo"      = "grey85"
  )) +
  labs(title = "LISA - tasa log") +
  theme_void()

fig_lisa1





#construccion de intervalos símbolicos por departamentos 
#intervalos de tasas en los años registrados 


intervalos <- df_dep %>%
  group_by(dpto_cnmbr, dpto_ccdgo) %>%
  summarise(
    tasa_min = min(tasa_log, na.rm = TRUE),
    tasa_max = max(tasa_log, na.rm = TRUE),
    casos_min = min(casos, na.rm = TRUE),
    casos_max = max(casos, na.rm = TRUE),
    
    # años disponibles por departamento
    anios = paste(sort(unique(ano)), collapse = ", "),
    
    # opcional: año mínimo y máximo
    anio_min = min(ano, na.rm = TRUE),
    anio_max = max(ano, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    periodo = paste0("[", anio_min, " - ", anio_max, "]"),
    intervalo_tasa = paste0("[", tasa_min, ", ", tasa_max, "]"),
    intervalo_casos = paste0("[", casos_min, ", ", casos_max, "]"),
    
  )

#convertir ambos formatos a character y unir a sahpefile
intervalos$dpto_ccdgo <- sprintf("%02s", intervalos$dpto_ccdgo)
depto_shp$dpto_ccdgo <- sprintf("%02s", depto_shp$dpto_ccdgo)

#union espacial
mapa_df <- depto_shp %>%
  left_join(intervalos,  by = "dpto_ccdgo")

setdiff(intervalos$dpto_ccdgo, depto_shp$dpto_ccdgo)
setdiff(depto_shp$dpto_ccdgo, intervalos$dpto_ccdgo)

table(is.na(mapa_df$intervalo_tasa))



dist_wasserstein <- function(c1, r1, c2, r2){
  sqrt((c1 - c2)^2 + (1/3)*(r1 - r2)^2)
}

dist_centro <- function(c1, r1, c2, r2){
  sqrt((c1 - c2)^2 + (r1 - r2)^2)
}

lista_pesos <- list(
  "Queen" = pesos_queen,
  "Rook"  = pesos_rook,
  "KNN3"  = pesos_k3
)

moran_simbolico <- function(pesos, intervalos, tipo_dist = "wasserstein", alpha = 1){
  
  n <- nrow(intervalos)
  W <- matrix(0, n, n)
  
  # convertir listw a matriz
  W_bin <- listw2mat(pesos)
  
  for(i in 1:n){
    for(j in 1:n){
      if(i != j){
        
        if(tipo_dist == "wasserstein"){
          d <- dist_wasserstein(intervalos$c[i], intervalos$r[i],
                                intervalos$c[j], intervalos$r[j])
        } else {
          d <- dist_centro(intervalos$c[i], intervalos$r[i],
                           intervalos$c[j], intervalos$r[j])
        }
        
        W[i,j] <- W_bin[i,j] / (d^alpha)
      }
    }
  }
  
  c_bar <- mean(intervalos$c)
  r_bar <- mean(intervalos$r)
  
  Ic_num <- 0
  Ir_num <- 0
  
  for(i in 1:n){
    for(j in 1:n){
      Ic_num <- Ic_num + W[i,j] * (intervalos$c[i] - c_bar) * (intervalos$c[j] - c_bar)
      Ir_num <- Ir_num + W[i,j] * (intervalos$r[i] - r_bar) * (intervalos$r[j] - r_bar)
    }
  }
  
  Ic <- (n / sum(W)) * (Ic_num / sum((intervalos$c - c_bar)^2))
  Ir <- (n / sum(W)) * (Ir_num / sum((intervalos$r - r_bar)^2))
  
  Is <- 0.5 * Ic + 0.5 * Ir
  
  return(c(Ic = Ic, Ir = Ir, Is = Is))
}

resultados <- data.frame()

combinaciones <- list(
  list(vec = "Queen", dist = "wasserstein"),
  list(vec = "Queen", dist = "centro"),
  list(vec = "Rook",  dist = "wasserstein"),
  list(vec = "KNN3",  dist = "wasserstein")
)

for(comb in combinaciones){
  
  res <- moran_simbolico(
    pesos = lista_pesos[[comb$vec]],
    intervalos = intervalos,
    tipo_dist = comb$dist
  )
  
  resultados <- rbind(resultados, data.frame(
    Vecindad = comb$vec,
    Distancia = comb$dist,
    Ic = round(res["Ic"], 4),
    Ir = round(res["Ir"], 4),
    Is = round(res["Is"], 4)
  ))
}

print(resultados)



#######MORAN SIMBÓLICO 

# =========================================================
# 🔥 MORAN SIMBÓLICO COMPLETO (MODELO + ROBUSTEZ)
# =========================================================

library(dplyr)
library(sf)
library(spdep)

# =========================================================
# 1. INTERVALOS SIMBÓLICOS
# =========================================================

intervalos <- df_dep %>%
  group_by(dpto_cnmbr, dpto_ccdgo) %>%
  summarise(
    tasa_min = quantile(tasa_log, 0.25, na.rm = TRUE),
    tasa_max = quantile(tasa_log, 0.75, na.rm = TRUE),
    n_anos   = n_distinct(ano),
    .groups  = "drop"
  ) %>%
  mutate(
    dpto_ccdgo = sprintf("%02s", dpto_ccdgo),
    centro = (tasa_min + tasa_max) / 2,
    radio  = (tasa_max - tasa_min) / 2
  ) %>%
  filter(!is.na(centro), !is.na(radio))

summary(intervalos$radio)
# =========================================================
# 2. UNIÓN CON SHAPEFILE
# =========================================================

depto_shp$dpto_ccdgo <- sprintf("%02s", depto_shp$dpto_ccdgo)

mapa_sf <- depto_shp %>%
  left_join(intervalos, by = "dpto_ccdgo") %>%
  filter(!is.na(centro))

# =========================================================
# 3. MATRICES ESPACIALES
# =========================================================

coords <- st_coordinates(st_centroid(mapa_sf))

# Queen (principal)
nb_queen <- poly2nb(mapa_sf, queen = TRUE)
pesos_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)

# Rook
nb_rook <- poly2nb(mapa_sf, queen = FALSE)
pesos_rook <- nb2listw(nb_rook, style = "W", zero.policy = TRUE)

# KNN (k=3)
nb_knn3 <- knn2nb(knearneigh(coords, k = 3))
pesos_knn3 <- nb2listw(nb_knn3, style = "W", zero.policy = TRUE)

# =========================================================
# 4. DISTANCIAS SIMBÓLICAS
# =========================================================

dist_hausdorff <- function(c1, r1, c2, r2){
  max(abs(c1 - c2), abs(r1 - r2))
}

dist_centro <- function(c1, r1, c2, r2){
  sqrt((c1 - c2)^2 + (r1 - r2)^2)
}

# =========================================================
# 5. FUNCIÓN MORAN SIMBÓLICO
# =========================================================

moran_simbolico <- function(listw, intervalos, tipo_dist = "hausdorff"){
  
  W_bin <- listw2mat(listw)
  n <- nrow(intervalos)
  W <- matrix(0, n, n)
  
  for(i in 1:n){
    for(j in 1:n){
      
      if(i != j && W_bin[i,j] > 0){
        
        if(tipo_dist == "hausdorff"){
          d <- dist_hausdorff(intervalos$centro[i], intervalos$radio[i],
                              intervalos$centro[j], intervalos$radio[j])
        } else {
          d <- dist_centro(intervalos$centro[i], intervalos$radio[i],
                           intervalos$centro[j], intervalos$radio[j])
        }
        
        if(!is.na(d) && d > 0){
          W[i,j] <- W_bin[i,j] / d
        }
      }
    }
  }
  
  c <- intervalos$centro
  r <- intervalos$radio
  
  c_bar <- mean(c)
  r_bar <- mean(r)
  
  Ic <- (n / sum(W)) *
    (t(c - c_bar) %*% W %*% (c - c_bar)) /
    sum((c - c_bar)^2)
  
  Ir <- (n / sum(W)) *
    (t(r - r_bar) %*% W %*% (r - r_bar)) /
    sum((r - r_bar)^2)
  
  Is <- 0.5 * Ic + 0.5 * Ir
  
  list(Ic = Ic, Ir = Ir, Is = Is, W = W)
}

# =========================================================
# 6. TEST DE PERMUTACIÓN
# =========================================================
perm_test <- function(W, c, r, alpha = 0.5, B = 999){
  
  obs <- as.numeric(
    alpha * (
      (t(c-mean(c)) %*% W %*% (c-mean(c))) / sum((c-mean(c))^2)
    ) + (1-alpha) * (
      (t(r-mean(r)) %*% W %*% (r-mean(r))) / sum((r-mean(r))^2)
    )
  )
  
  perm <- numeric(B)
  
  for(b in 1:B){
    
    perm_c <- sample(c)
    perm_r <- sample(r)
    
    perm[b] <- as.numeric(
      alpha * (
        (t(perm_c-mean(perm_c)) %*% W %*% (perm_c-mean(perm_c))) /
          sum((perm_c-mean(perm_c))^2)
      ) + (1-alpha) * (
        (t(perm_r-mean(perm_r)) %*% W %*% (perm_r-mean(perm_r))) /
          sum((perm_r-mean(perm_r))^2)
      )
    )
  }
  
  mean(abs(perm) >= abs(obs))
}
# =========================================================
# 7. CONFIGURACIONES (MODELO + ROBUSTEZ)
# =========================================================

configuraciones <- list(
  list(nombre = "Queen + Hausdorff", pesos = pesos_queen, dist = "hausdorff"),
  list(nombre = "Queen + Centro",    pesos = pesos_queen, dist = "centro"),
  list(nombre = "Rook + Hausdorff",  pesos = pesos_rook,  dist = "hausdorff"),
  list(nombre = "KNN3 + Hausdorff",  pesos = pesos_knn3,  dist = "hausdorff")
)

# =========================================================
# 8. EJECUCIÓN FINAL
# =========================================================

resultados <- data.frame()

for(cfg in configuraciones){
  
  res <- moran_simbolico(cfg$pesos, intervalos, tipo_dist = cfg$dist)
  
  pval <- perm_test(res$W, intervalos$centro, intervalos$radio)
  
  resultados <- rbind(resultados, data.frame(
    Modelo = cfg$nombre,
    Ic = round(res$Ic, 4),
    Ir = round(res$Ir, 4),
    Is = round(res$Is, 4),
    p_value = round(pval, 4)
  ))
}

print(resultados)



#######MORAN SIMBÓLICO TASA NORMAL

# =========================================================
# 🔥 MORAN SIMBÓLICO COMPLETO (MODELO + ROBUSTEZ)
# =========================================================

library(dplyr)
library(sf)
library(spdep)

# =========================================================
# 1. INTERVALOS SIMBÓLICOS
# =========================================================

intervalos <- df_dep %>%
  group_by(dpto_cnmbr, dpto_ccdgo) %>%
  summarise(
    tasa_min = quantile(tasa_100k, 0.25, na.rm = TRUE),
    tasa_max = quantile(tasa_100k, 0.75, na.rm = TRUE),
    n_anos   = n_distinct(ano),
    .groups  = "drop"
  ) %>%
  mutate(
    dpto_ccdgo = sprintf("%02s", dpto_ccdgo),
    centro = (tasa_min + tasa_max) / 2,
    radio  = (tasa_max - tasa_min) / 2
  ) %>%
  filter(!is.na(centro), !is.na(radio))

summary(intervalos$radio)
# =========================================================
# 2. UNIÓN CON SHAPEFILE
# =========================================================

depto_shp$dpto_ccdgo <- sprintf("%02s", depto_shp$dpto_ccdgo)

mapa_sf <- depto_shp %>%
  left_join(intervalos, by = "dpto_ccdgo") %>%
  filter(!is.na(centro))

# =========================================================
# 3. MATRICES ESPACIALES
# =========================================================

coords <- st_coordinates(st_centroid(mapa_sf))

# Queen (principal)
nb_queen <- poly2nb(mapa_sf, queen = TRUE)
pesos_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)

# Rook
nb_rook <- poly2nb(mapa_sf, queen = FALSE)
pesos_rook <- nb2listw(nb_rook, style = "W", zero.policy = TRUE)

# KNN (k=3)
nb_knn3 <- knn2nb(knearneigh(coords, k = 3))
pesos_knn3 <- nb2listw(nb_knn3, style = "W", zero.policy = TRUE)

# =========================================================
# 4. DISTANCIAS SIMBÓLICAS
# =========================================================

dist_hausdorff <- function(c1, r1, c2, r2){
  max(abs(c1 - c2), abs(r1 - r2))
}

dist_centro <- function(c1, r1, c2, r2){
  sqrt((c1 - c2)^2 + (r1 - r2)^2)
}

# =========================================================
# 5. FUNCIÓN MORAN SIMBÓLICO
# =========================================================

moran_simbolico <- function(listw, intervalos, tipo_dist = "hausdorff"){
  
  W_bin <- listw2mat(listw)
  n <- nrow(intervalos)
  W <- matrix(0, n, n)
  
  for(i in 1:n){
    for(j in 1:n){
      
      if(i != j && W_bin[i,j] > 0){
        
        if(tipo_dist == "hausdorff"){
          d <- dist_hausdorff(intervalos$centro[i], intervalos$radio[i],
                              intervalos$centro[j], intervalos$radio[j])
        } else {
          d <- dist_centro(intervalos$centro[i], intervalos$radio[i],
                           intervalos$centro[j], intervalos$radio[j])
        }
        
        if(!is.na(d) && d > 0){
          W[i,j] <- W_bin[i,j] / d
        }
      }
    }
  }
  
  c <- intervalos$centro
  r <- intervalos$radio
  
  c_bar <- mean(c)
  r_bar <- mean(r)
  
  Ic <- (n / sum(W)) *
    (t(c - c_bar) %*% W %*% (c - c_bar)) /
    sum((c - c_bar)^2)
  
  Ir <- (n / sum(W)) *
    (t(r - r_bar) %*% W %*% (r - r_bar)) /
    sum((r - r_bar)^2)
  
  Is <- 0.5 * Ic + 0.5 * Ir
  
  list(Ic = Ic, Ir = Ir, Is = Is, W = W)
}

# =========================================================
# 6. TEST DE PERMUTACIÓN
# =========================================================
perm_test <- function(W, c, r, alpha = 0.5, B = 999){
  
  obs <- as.numeric(
    alpha * (
      (t(c-mean(c)) %*% W %*% (c-mean(c))) / sum((c-mean(c))^2)
    ) + (1-alpha) * (
      (t(r-mean(r)) %*% W %*% (r-mean(r))) / sum((r-mean(r))^2)
    )
  )
  
  perm <- numeric(B)
  
  for(b in 1:B){
    
    perm_c <- sample(c)
    perm_r <- sample(r)
    
    perm[b] <- as.numeric(
      alpha * (
        (t(perm_c-mean(perm_c)) %*% W %*% (perm_c-mean(perm_c))) /
          sum((perm_c-mean(perm_c))^2)
      ) + (1-alpha) * (
        (t(perm_r-mean(perm_r)) %*% W %*% (perm_r-mean(perm_r))) /
          sum((perm_r-mean(perm_r))^2)
      )
    )
  }
  
  mean(abs(perm) >= abs(obs))
}
# =========================================================
# 7. CONFIGURACIONES (MODELO + ROBUSTEZ)
# =========================================================

configuraciones <- list(
  list(nombre = "Queen + Hausdorff", pesos = pesos_queen, dist = "hausdorff"),
  list(nombre = "Queen + Centro",    pesos = pesos_queen, dist = "centro"),
  list(nombre = "Rook + Hausdorff",  pesos = pesos_rook,  dist = "hausdorff"),
  list(nombre = "KNN3 + Hausdorff",  pesos = pesos_knn3,  dist = "hausdorff")
)

# =========================================================
# 8. EJECUCIÓN FINAL
# =========================================================

resultados <- data.frame()

for(cfg in configuraciones){
  
  res <- moran_simbolico(cfg$pesos, intervalos, tipo_dist = cfg$dist)
  
  pval <- perm_test(res$W, intervalos$centro, intervalos$radio)
  
  resultados <- rbind(resultados, data.frame(
    Modelo = cfg$nombre,
    Ic = round(res$Ic, 4),
    Ir = round(res$Ir, 4),
    Is = round(res$Is, 4),
    p_value = round(pval, 4)
  ))
}

print(resultados)



# =========================================================
# 1. FUNCIONES BASE (SIMBÓLICAS)
# =========================================================

# Distancia de Hausdorff para intervalos
dist_hausdorff <- function(c1, r1, c2, r2){
  max(abs(c1 - c2), abs(r1 - r2))
}

# Moran Simbólico Global
moran_simbolico <- function(listw, intervalos_data, tipo_dist = "hausdorff"){
  W_bin <- listw2mat(listw)
  n <- nrow(intervalos_data)
  W <- matrix(0, n, n)
  
  for(i in 1:n){
    for(j in 1:n){
      if(i != j && W_bin[i,j] > 0){
        d <- dist_hausdorff(intervalos_data$centro[i], intervalos_data$radio[i],
                            intervalos_data$centro[j], intervalos_data$radio[j])
        if(!is.na(d) && d > 0) W[i,j] <- W_bin[i,j] / d
      }
    }
  }
  
  c <- intervalos_data$centro
  r <- intervalos_data$radio
  c_bar <- mean(c); r_bar <- mean(r)
  
  Ic <- (n / sum(W)) * (t(c - c_bar) %*% W %*% (c - c_bar)) / sum((c - c_bar)^2)
  Ir <- (n / sum(W)) * (t(r - r_bar) %*% W %*% (r - r_bar)) / sum((r - r_bar)^2)
  Is <- 0.5 * Ic + 0.5 * Ir
  
  list(Ic = Ic, Ir = Ir, Is = Is, W = W)
}

# LISA Simbólico Local
lisa_simbolico <- function(listw, intervalos_data) {
  res_global <- moran_simbolico(listw, intervalos_data)
  W <- res_global$W
  n <- nrow(intervalos_data)
  
  # Estandarización combinada (Centro + Radio)
  zc <- (intervalos_data$centro - mean(intervalos_data$centro)) / sd(intervalos_data$centro)
  zr <- (intervalos_data$radio - mean(intervalos_data$radio)) / sd(intervalos_data$radio)
  z_simb <- 0.5 * zc + 0.5 * zr
  
  lag_simb <- W %*% z_simb
  lisa_i <- z_simb * lag_simb
  
  cuadrante <- rep("Insignificante", n)
  cuadrante[z_simb > 0 & lag_simb > 0] <- "Alto-Alto (HH)"
  cuadrante[z_simb < 0 & lag_simb < 0] <- "Bajo-Bajo (LL)"
  cuadrante[z_simb > 0 & lag_simb < 0] <- "Alto-Bajo (HL)"
  cuadrante[z_simb < 0 & lag_simb > 0] <- "Bajo-Alto (LH)"
  
  data.frame(dpto_ccdgo = intervalos_data$dpto_ccdgo, lisa_i = lisa_i, cuadrante = cuadrante)
}

# =========================================================
# 2. PROCESAMIENTO: TASA LOGARÍTMICA
# =========================================================

inter_log <- df_dep %>%
  group_by(dpto_ccdgo) %>%
  summarise(tasa_min = quantile(tasa_log, 0.25, na.rm = TRUE),
            tasa_max = quantile(tasa_log, 0.75, na.rm = TRUE), .groups = "drop") %>%
  mutate(dpto_ccdgo = sprintf("%02s", dpto_ccdgo),
         centro = (tasa_min + tasa_max) / 2, radio = (tasa_max - tasa_min) / 2) %>%
  filter(!is.na(centro))

mapa_sf_log <- depto_shp %>% 
  mutate(dpto_ccdgo = sprintf("%02s", dpto_ccdgo)) %>%
  left_join(inter_log, by = "dpto_ccdgo") %>% filter(!is.na(centro))

pesos_log <- nb2listw(poly2nb(mapa_sf_log, queen = TRUE), style = "W", zero.policy = TRUE)
lisa_res_log <- lisa_simbolico(pesos_log, inter_log)
mapa_final_log <- mapa_sf_log %>% left_join(lisa_res_log, by = "dpto_ccdgo")

# =========================================================
# 3. PROCESAMIENTO: TASA NORMAL
# =========================================================

inter_norm <- df_dep %>%
  group_by(dpto_ccdgo) %>%
  summarise(tasa_min = quantile(tasa_100k, 0.25, na.rm = TRUE),
            tasa_max = quantile(tasa_100k, 0.75, na.rm = TRUE), .groups = "drop") %>%
  mutate(dpto_ccdgo = sprintf("%02s", dpto_ccdgo),
         centro = (tasa_min + tasa_max) / 2, radio = (tasa_max - tasa_min) / 2) %>%
  filter(!is.na(centro))

mapa_sf_norm <- depto_shp %>% 
  mutate(dpto_ccdgo = sprintf("%02s", dpto_ccdgo)) %>%
  left_join(inter_norm, by = "dpto_ccdgo") %>% filter(!is.na(centro))

pesos_norm <- nb2listw(poly2nb(mapa_sf_norm, queen = TRUE), style = "W", zero.policy = TRUE)
lisa_res_norm <- lisa_simbolico(pesos_norm, inter_norm)
mapa_final_norm <- mapa_sf_norm %>% left_join(lisa_res_norm, by = "dpto_ccdgo")

# =========================================================
# 4. GRÁFICAS (LISA MAPS)
# =========================================================

library(ggplot2)
library(patchwork)

paleta_lisa <- c("Alto-Alto (HH)" = "#d7191c", "Bajo-Bajo (LL)" = "#2b83ba", 
                 "Alto-Bajo (HL)" = "#fdae61", "Bajo-Alto (LH)" = "#abdda4", 
                 "Insignificante" = "#eeeeee")

g1 <- ggplot(mapa_final_log) + geom_sf(aes(fill = cuadrante)) +
  scale_fill_manual(values = paleta_lisa) + labs(title = "LISA Tasa Logarítmica") + theme_minimal()

g2 <- ggplot(mapa_final_norm) + geom_sf(aes(fill = cuadrante)) +
  scale_fill_manual(values = paleta_lisa) + labs(title = "LISA Tasa Normal") + theme_minimal()

# Mostrar ambos juntos
g1 + g2 + plot_layout(guides = "collect")



# =========================================================
# 1. FUNCIÓN PARA GENERAR EL MAPA LISA SIMBÓLICO
# =========================================================

generar_lisa_simbolico <- function(shp, intervalos_data, titulo) {
  
  # Calcular LISA usando las funciones previas
  lisa_res <- lisa_simbolico(pesos_queen, intervalos_data)
  
  # Clasificación con los nuevos nombres de etiquetas
  lisa_res <- lisa_res %>%
    mutate(cuadrante = case_when(
      cuadrante == "Alto-Alto (HH)" ~ "Alto-Alto",
      cuadrante == "Bajo-Bajo (LL)" ~ "Bajo-Bajo",
      cuadrante == "Alto-Bajo (HL)" ~ "Alto-Bajo",
      cuadrante == "Bajo-Alto (LH)" ~ "Bajo-Alto",
      TRUE ~ "No sig"
    ))
  
  # Unir al shapefile
  mapa_data <- shp %>%
    mutate(dpto_ccdgo = sprintf("%02s", dpto_ccdgo)) %>%
    left_join(lisa_res, by = "dpto_ccdgo")
  
  # Paleta de colores solicitada
  colores_lisa <- c(
    "Alto-Alto"   = "#A32D2D",
    "Bajo-Bajo"  = "#185FA5",
    "Alto-Bajo"    = "#F09595",
    "Bajo-Alto"    = "#85B7EB",
    "No sig"       = "grey85"
  )
  
  # Generar gráfica
  ggplot(mapa_data) +
    geom_sf(aes(fill = cuadrante), color = "white", size = 0.2) + # Líneas blancas
    scale_fill_manual(values = colores_lisa) +
    labs(title = titulo, fill = "") +
    theme_void() + # Elimina grilla, ejes y fondo
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      legend.position = "right"
    )
}

# =========================================================
# 2. GENERAR MAPAS Y COMPARACIÓN FINAL
# =========================================================

# Asegúrate de tener definidos: intervalos_100k e intervalos_log
fig_simb_100k <- generar_lisa_simbolico(depto_shp, inter_norm, "Tasa 100.000 habitantes")
fig_simb_log  <- generar_lisa_simbolico(depto_shp, inter_log, "Tasa Logarítmica")

# Unir gráficas con Patchwork
library(patchwork)

comparativa_lisa <- (fig_simb_100k | fig_simb_log) + 
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Comparación de Clústeres Espaciales Simbólicos",
    subtitle = "Basado en Centro e Intervalos (Hausdorff)",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right"
    )
  )

# Visualizar
print(comparativa_lisa)
