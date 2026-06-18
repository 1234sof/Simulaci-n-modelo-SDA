#Análisis bogotá 

# ── CARGAR SHAPEFILE DE LOCALIDADES ──────────────────────────────────────────

setwd("C:/Users/aulasingenieria/Documents/TESIS/Fuentes/Violencia de Genero/SHAPEFILES-LOCALIDADES-BOGOTA/Shapefiles Localidades")  

localidades_shp <- st_read("~/TESIS/Fuentes/Violencia de Genero/SHAPEFILES-LOCALIDADES-BOGOTA/Shapefiles Localidades/poligonos-localidades.shp")  # cambia por el nombre real

# Revisar cómo vienen los campos
names(localidades_shp)
head(localidades_shp)

names(df_colombia)

names(localidades_shp)

names(Proyeccion_bogota)
# ── FILTRAR BOGOTÁ DE TU BASE PRINCIPAL ───────────────────────────────────────
# Ajusta cod_localidad y nom_localidad según los nombres reales de tus columnas

# ── FILTRAR BOGOTÁ DE TU BASE PRINCIPAL ───────────────────────────────────────

df_bogota <- df_colombia %>%
  filter(str_starts(as.character(cod_mpio_o), "11")) %>%
  mutate(
    cod_localidad = str_pad(as.character(identificad), 2, pad = "0"),
    ano           = as.numeric(anio_con)
  )

# Verificar
nrow(df_bogota)
unique(df_bogota$cod_localidad)

# ── AGREGAR CASOS POR LOCALIDAD Y AÑO ────────────────────────────────────────

df_loc <- df_bogota %>%
  group_by(cod_localidad, ano) %>%
  summarise(
    casos = sum(violencia.general, na.rm = TRUE),
    .groups = "drop"
  )

# ── POBLACIÓN BOGOTÁ POR LOCALIDAD ───────────────────────────────────────────

poblacion_loc <- Proyeccion_bogota %>%
  rename(
    cod_localidad = COD_LOC,
    nom_localidad = NOM_LOC,
    ano           = `AÑO`,
    poblacion     = `Total Mujeres`
  ) %>%
  mutate(
    cod_localidad = str_pad(as.character(cod_localidad), 2, pad = "0"),
    ano           = as.numeric(ano)
  ) %>%
  filter(ano >= 2020 & ano <= 2025)

# Verificar
head(poblacion_loc)
unique(poblacion_loc$ano)

# ── JOIN CASOS + POBLACIÓN ────────────────────────────────────────────────────

df_loc <- df_loc %>%
  left_join(poblacion_loc, by = c("cod_localidad", "ano")) %>%
  mutate(
    tasa_100k = (casos / poblacion) * 100000,
    tasa_log  = log1p(tasa_100k)
  )

# Verificar join
sum(is.na(df_loc$poblacion))
head(df_loc)

# ── PREPARAR SHAPEFILE ────────────────────────────────────────────────────────

localidades_shp <- localidades_shp %>%
  mutate(cod_localidad = str_pad(as.character(Identificad), 2, pad = "0"))

# ── INDICADORES POR LOCALIDAD ─────────────────────────────────────────────────

# 1. Tasa promedio
tasa_prom_loc <- df_loc %>%
  filter(!is.na(tasa_100k)) %>%
  group_by(cod_localidad) %>%
  summarise(
    tasa_prom     = mean(tasa_100k, na.rm = TRUE),
    tasa_prom_log = mean(tasa_log,  na.rm = TRUE),
    n_anos        = n_distinct(ano),
    .groups       = "drop"
  )

# 2. Tendencia
tasa_tend_loc <- df_loc %>%
  filter(!is.na(tasa_100k)) %>%
  group_by(cod_localidad) %>%
  filter(n_distinct(ano) >= 3) %>%
  summarise(
    pendiente = coef(lm(tasa_100k ~ ano))[2],
    n_anos    = n_distinct(ano),
    .groups   = "drop"
  )

# 3. Coeficiente de variación
cv_loc <- df_loc %>%
  filter(!is.na(tasa_100k)) %>%
  group_by(cod_localidad) %>%
  filter(n_distinct(ano) >= 3) %>%
  summarise(
    media_tasa = mean(tasa_100k, na.rm = TRUE),
    sd_tasa    = sd(tasa_100k,   na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    cv     = (sd_tasa / media_tasa) * 100,
    cv_cat = case_when(
      cv < 30  ~ "Estable (< 30%)",
      cv < 60  ~ "Moderado (30–60%)",
      cv >= 60 ~ "Errático (> 60%)",
      TRUE     ~ "Sin dato"
    )
  )

# 4. Riesgo acumulado
riesgo_loc <- df_loc %>%
  filter(!is.na(tasa_100k)) %>%
  group_by(cod_localidad) %>%
  summarise(
    riesgo_acum = sum(tasa_100k, na.rm = TRUE),
    n_anos      = n_distinct(ano),
    .groups     = "drop"
  )

# ── UNIR TODO AL SHAPEFILE ────────────────────────────────────────────────────

mapa_loc <- localidades_shp %>%
  left_join(tasa_prom_loc,                                by = "cod_localidad") %>%
  left_join(tasa_tend_loc %>% select(cod_localidad, pendiente), by = "cod_localidad") %>%
  left_join(cv_loc        %>% select(cod_localidad, cv, cv_cat), by = "cod_localidad") %>%
  left_join(riesgo_loc    %>% select(cod_localidad, riesgo_acum), by = "cod_localidad")

# Verificar que el join quedó bien
mapa_loc %>% st_drop_geometry() %>% summary()

# ── 6 MAPAS ───────────────────────────────────────────────────────────────────

m1 <- ggplot(mapa_loc) +
  geom_sf(aes(fill = tasa_prom), color = NA) +
  scale_fill_viridis_c(option = "magma", na.value = "grey85", name = "Tasa") +
  labs(title = "a. Tasa promedio") +
  theme_void() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    legend.position = "right"
  )

m2 <- ggplot(mapa_loc) +
  geom_sf(aes(fill = tasa_prom_log), color = NA) +
  scale_fill_viridis_c(option = "magma", na.value = "grey85", name = "log(Tasa)") +
  labs(title = "b. Tasa promedio (log)") +
  theme_void() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    legend.position = "right"
  )

lim_loc <- quantile(abs(mapa_loc$pendiente), 0.95, na.rm = TRUE)

m3 <- ggplot(mapa_loc) +
  geom_sf(aes(fill = pendiente), color = "grey60", size = 0.1) +
  scale_fill_gradient2(
    low      = "#185FA5",
    mid      = "white",
    high     = "#A32D2D",
    midpoint = 0,
    limits   = c(-lim_loc, lim_loc),
    oob      = scales::squish,
    na.value = "grey85",
    name     = "Puntos/año"
  ) +
  labs(title = "c. Tendencia 2020–2025") +
  theme_void() +
  theme(plot.title      = element_text(size = 11, face = "bold"),
        legend.position = "right")

m4 <- ggplot(mapa_loc) +
  geom_sf(aes(fill = cv_cat), color = "grey60", size = 0.1) +
  scale_fill_manual(
    values = c(
      "Estable (< 30%)"   = "#0F6E56",
      "Moderado (30–60%)" = "#FAC775",
      "Errático (> 60%)"  = "#A32D2D",
      "Sin dato"          = "grey85"
    ),
    na.value = "grey85",
    name = "Variabilidad"
  ) +
  labs(title = "d. Variabilidad interanual (CV)") +
  theme_void() +
  theme(plot.title      = element_text(size = 11, face = "bold"),
        legend.position = "right")

m5 <- ggplot(mapa_loc) +
  geom_sf(aes(fill = riesgo_acum), color = "grey60", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey85", name = "Tasa acum.") +
  labs(title = "e. Riesgo acumulado") +
  theme_void() +
  theme(plot.title      = element_text(size = 11, face = "bold"),
        legend.position = "right")

m6 <- ggplot(mapa_loc) +
  geom_sf(aes(fill = factor(n_anos)), color = "grey60", size = 0.1) +
  scale_fill_manual(
    values = c(
      "6" = "#0F6E56",
      "5" = "#5DCAA5",
      "4" = "#9FE1CB",
      "3" = "#FAC775",
      "2" = "#D85A30",
      "1" = "#993C1D"
    ),
    na.value = "grey85",
    name = "Años con registro"
  ) +
  labs(title = "f. Cobertura del registro") +
  theme_void() +
  theme(plot.title      = element_text(size = 11, face = "bold"),
        legend.position = "right")

# ── PANEL FINAL ───────────────────────────────────────────────────────────────

(m1 | m2 ) +
  plot_annotation(
    title    = "Tasa promedio de violencia física y sexual \n por localidades de Bogotá (2020-2025)",
    theme = theme(
      plot.title = element_text(
        color = "#4B0082",
        size  = 15,
        face  = "bold",
        hjust = 0.5
      )
    )
  )


# ── UNIR TASAS ANUALES AL SHAPEFILE ──────────────────────────────────────────

# df_loc ya tiene tasa_100k por localidad y año
# solo necesitas unirlo al shapefile para cada año

mapa_anual <- localidades_shp %>%
  left_join(
    df_loc %>% filter(!is.na(tasa_100k)) %>% 
      select(cod_localidad, ano, tasa_100k),
    by = "cod_localidad"
  )

# Verificar que quedaron los 6 años
unique(mapa_anual$ano)

# ── ESCALA COMÚN PARA TODOS LOS AÑOS ─────────────────────────────────────────
# Esto es clave: todos los mapas deben usar exactamente el mismo rango

escala_min <- min(mapa_anual$tasa_100k, na.rm = TRUE)
escala_max <- max(mapa_anual$tasa_100k, na.rm = TRUE)

# ── FUNCIÓN DE MAPAS ──────────────────────────────────────────────────────────

mapa_por_ano <- function(año) {
  ggplot(mapa_anual %>% filter(ano == año)) +
    geom_sf(aes(fill = tasa_100k), color = NA) +
    scale_fill_viridis_c(
      option   = "magma",
      limits   = c(escala_min, escala_max),
      na.value = "grey85",
      name     = "Tasa x\n100.000"
    ) +
    labs(title = as.character(año)) +
    theme_void() +
    theme(
      plot.title      = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.position = "none"
    )
}

# ── 6 MAPAS ───────────────────────────────────────────────────────────────────

a2020 <- mapa_por_ano(2020)
a2021 <- mapa_por_ano(2021)
a2022 <- mapa_por_ano(2022)
a2023 <- mapa_por_ano(2023)
a2024 <- mapa_por_ano(2024)
a2025 <- mapa_por_ano(2025)

# ── MAPA CON LEYENDA (pegada al panel) ───────────────────────────────────────

mapa_con_leyenda <- mapa_por_ano(2020) +
  theme(
    legend.position = "right",
    legend.margin   = margin(0, 0, 0, -20),   # jala la leyenda hacia la izquierda
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 9)
  )

# ── PANEL FINAL ───────────────────────────────────────────────────────────────


panel_final <- (a2020 | a2021 | a2022) / (a2023 | a2024 | a2025)

panel_final + mapa_con_leyenda +
  plot_layout(widths = c(3, 0.12)) +
  plot_annotation(
    title = "Tasa de violencia contra la mujer\n por localidad de Bogotá (2020–2025)",
    theme = theme(
      plot.title = element_text(
        color = "#4B0082",
        size  = 15,
        face  = "bold",
        hjust = 0.5
      )
    )
  )


lim_loc <- quantile(abs(mapa_loc$pendiente), 0.95, na.rm = TRUE)

fig2 <- ggplot(mapa_loc) +
  geom_sf(aes(fill = pendiente), color = NA) +
  scale_fill_gradient2(
    low      = "#185FA5",
    mid      = "white",
    high     = "#A32D2D",
    midpoint = 0,
    limits   = c(-lim_loc, lim_loc),
    oob      = scales::squish,
    na.value = "grey85",
    name     = "Puntos/año"
  ) +
  labs(title = "Tendencia anual de la tasa de violencia contra la\n mujer por localidad de Bogotá (2020–2025)") +
  theme_void() +
  theme(
    plot.title      = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

fig2


# ── UNIR TASAS ANUALES AL SHAPEFILE ──────────────────────────────────────────

# df_loc ya tiene tasa_100k por localidad y año
# solo necesitas unirlo al shapefile para cada año

mapa_anual <- localidades_shp %>%
  left_join(
    df_loc %>% filter(!is.na(tasa_100k)) %>% 
      select(cod_localidad, ano, tasa_100k),
    by = "cod_localidad"
  )

# Verificar que quedaron los 6 años
unique(mapa_anual$ano)

# ── ESCALA COMÚN PARA TODOS LOS AÑOS ─────────────────────────────────────────
# Esto es clave: todos los mapas deben usar exactamente el mismo rango

escala_min <- min(mapa_anual$tasa_100k, na.rm = TRUE)
escala_max <- max(mapa_anual$tasa_100k, na.rm = TRUE)

# ── FUNCIÓN PARA GENERAR CADA MAPA ───────────────────────────────────────────

mapa_por_ano <- function(año) {
  ggplot(mapa_anual %>% filter(ano == año)) +
    geom_sf(aes(fill = tasa_100k), color = "Nan", size = 0.1) +
    scale_fill_viridis_c(
      option  = "magma",
      limits  = c(escala_min, escala_max),
      na.value = "grey85",
      name    = "Tasa x\n100.000"
    ) +
    labs(title = as.character(año)) +
    theme_void() +
    theme(
      plot.title      = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.position = "right"
    )
}

# ── GENERAR LOS 6 MAPAS ───────────────────────────────────────────────────────

a2020 <- mapa_por_ano(2020)
a2021 <- mapa_por_ano(2021)
a2022 <- mapa_por_ano(2022)
a2023 <- mapa_por_ano(2023)
a2024 <- mapa_por_ano(2024)
a2025 <- mapa_por_ano(2025)

# ── PANEL FINAL ───────────────────────────────────────────────────────────────

(a2020 | a2021 | a2022) / (a2023 | a2024 | a2025) +
  plot_annotation(
    title = "Tasa de violencia contra la mujer por localidad de Bogotá (2020–2025)",
    theme = theme(
      plot.title = element_text(
        color = "#4B0082",
        size  = 15,
        face  = "bold",
        hjust = 0.5
      )
    )
  )

m6

m5
m4
