##############AHORA MUNICIIPOIOS#########

library(dplyr)
library(readxl)
library(stringr)

# Cragar y Anaplizar pimero el shapefile de colombia 
#paquetes necesarios
install.packages("sf")   # solo si no la tienes
library(sf)
#definir carpeta de trabajo  para departamentos 


setwd("C:/Users/aulasingenieria/Downloads/MGN2025_MPIO_GRAFICO")

muni_shp <- st_read("MGN_ADM_MPIO_GRAFICO.shp")


#ver comoe sta conofrmada ese shp
names(muni_shp)
head(muni_shp)


muni_shp$mpio_cdpmp <- str_pad(muni_shp$mpio_cdpmp, 5, pad = "0")

Proyeccion_población <- Proyeccion_municipios
View(Proyeccion_población)

head(`df_colombia_final.(1)`)

#ver las columnas con las que vamos a trabajar
names(`df_colombia_final.(1)`)

df_colombia <- `df_colombia_final.(1)`


names(df_colombia)

poblacion_clean <- Proyeccion_población %>%
  #renombrar variables para que se puedan hacer el join mpas facil
  rename(
    mpio_cdpmp = MPIO,
    mpio_cnmbr = DPMP,
    ano = `AÑO`,
    tipo = `ÁREA GEOGRÁFICA`,
    poblacion = `Mujeres`
  ) %>%
  filter(tipo == "Total") %>%
  mutate(
    mpio_cdpmp = str_pad(mpio_cdpmp, 5, pad = "0"),
    ano = as.numeric(ano)
  ) %>%
  select(mpio_cdpmp, mpio_cnmbr, ano, poblacion)

head(poblacion_clean)


poblacion_clean <- poblacion_clean %>%
  filter(ano >= 2020 & ano <= 2025)


poblacion_mun <- poblacion_clean %>%
  group_by(mpio_cdpmp,mpio_cnmbr, ano) %>%
  summarise(
    poblacion = sum(poblacion, na.rm = TRUE),
    .groups = "drop"
  )

unique(poblacion_mun$ano)

df_clean <- df_colombia %>%
  rename(
    mpio_cdpmp = cod_mpio_o,
    mpio_cnmbr = ndep_proce,
    ano = anio_con
  ) %>%
  # para que los codigos queden en char. convertir de numero a texto
  mutate(
    mpio_cdpmp = str_pad(as.character(mpio_cdpmp), 5, pad = "0"),
    ano = as.numeric(ano)
  )


names(df_clean)



df_dep <- df_clean %>%
  group_by(mpio_cdpmp, ano) %>%
  summarise(
    casos = sum(violencia.general, na.rm = TRUE),
    .groups = "drop"
  )

poblacion_mun %>%
  count(mpio_cdpmp, ano) %>%
  filter(n > 1)

df_dep %>%
  count(mpio_cdpmp, ano) %>%
  filter(n > 1)

df_dep <- df_dep %>%
  left_join(poblacion_mun, by = c("mpio_cdpmp", "ano"))


#Mirar si el join quedo bien. Si quedó
sum(is.na(df_dep$poblacion))

#Calcular tasa de casos por pobación por cada 100.000 habitantes.


df_dep %>%
  group_by(mpio_cdpmp) %>%
  summarise(
    anos = paste(sort(unique(ano)), collapse = ", ")
  )



todos_anos <- 2020:2025

df_dep %>%
  group_by(mpio_cdpmp) %>%
  summarise(n_anos = n_distinct(ano)) %>%
  print(n = Inf)

df_dep %>%
  group_by(mpio_cdpmp) %>%
  summarise(n_anos = n_distinct(ano)) %>%
  filter(n_anos >= 4)

df_dep %>%
  group_by(mpio_cdpmp) %>%
  summarise(n_anos = n_distinct(ano)) %>%
  filter(n_anos < 3)


#calcular tasas anuales 

df_dep <- df_dep %>%
  mutate(
    tasa_100k = (casos / poblacion) * 100000,
    tasa_log = log1p(tasa_100k)
  )

#mapa 1 primedio de tasas anuales normales
tasa_promedio <- df_dep %>%
  group_by(mpio_cdpmp) %>%
  summarise(
    tasa_prom = mean(tasa_100k, na.rm = TRUE)
    
  )

tasa_promedio_log <- df_dep %>%
  group_by(mpio_cdpmp) %>%
  summarise(
    tasa_prom_log = mean(tasa_log, na.rm = TRUE)
  )


#unir con shapefile 

muni_shp$mpio_cdpmp <- str_pad(muni_shp$mpio_cdpmp, 5, pad = "0")

mapa_prom <- muni_shp %>%
  left_join(tasa_promedio, by = "mpio_cdpmp")

mapa_prom_log <- muni_shp %>%
  left_join(tasa_promedio_log, by = "mpio_cdpmp")


anti_join(muni_shp, tasa_promedio, by = "mpio_cdpmp")

sum(is.na(mapa_prom$tasa_prom))

mapa_prom %>%
  filter(is.na(tasa_prom)) %>%
  select(mpio_cnmbr)





library(ggplot2)
library(patchwork)
library(viridis)


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

## ver que no machea 

tabla_municipios <- mapa_prom %>%
  distinct(mpio_cdpmp)

tabla_municipios <- mapa_prom %>%
  distinct(mpio_cdpmp) %>%
  arrange(mpio_cdpmp)


nrow(tabla_municipios)

write.csv(tabla_municipios2, "municipios_unicos2.csv", row.names = FALSE)
getwd()


# ponderda

resumen <- df_dep %>%
  group_by(mpio_cdpmp) %>%
  summarise(
    total_casos = sum(casos, na.rm = TRUE),
    poblacion_ref = mean(poblacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    riesgo_periodo = (total_casos / poblacion_ref) * 100000,
    riesgo_periodo_log = log1p(riesgo_periodo)
  )




muni_shp$mpio_cdpmp <- str_pad(muni_shp$mpio_cdpmp, 5, pad = "0")

mapa_tasa_ponder <- muni_shp %>%
  left_join(resumen %>% select(mpio_cdpmp, riesgo_periodo), 
            by = "mpio_cdpmp")

mapa_ponder_log <- muni_shp %>%
  left_join(resumen %>% select(mpio_cdpmp, riesgo_periodo_log), 
            by = "mpio_cdpmp")


p1 <- ggplot(mapa_tasa_ponder) +
  geom_sf(aes(fill =  riesgo_periodo), color = NA) +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "a. Riesgo acumulado",
    fill = "Tasa"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

p2 <- ggplot(mapa_ponder_log) +
  geom_sf(aes(fill = riesgo_periodo_log), color = NA) +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "b. Riesgo acumulado log",
    fill = "log(Tasa)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

(p1 | p2) +
  plot_annotation(
    title = "Tasa acumulada de violencia física y sexual por municipios (2020-2025)",
    theme = theme(
      plot.title = element_text(
        color = "#4B0082",
        size = 16,
        face = "bold",
        hjust = 0.5
      )
    )
  )

# ── MAPA DE TENDENCIA ──────────────────────────────────────────────────────────

# 1. Calcular pendiente de regresión por municipio
#    (usa los mismos datos df_dep que ya tienes con tasa_100k)

tasa_tendencia <- df_dep %>%
  filter(!is.na(tasa_100k)) %>%
  group_by(mpio_cdpmp) %>%
  filter(n_distinct(ano) >= 3) %>%          # mínimo 3 años para regresar
  summarise(
    pendiente = coef(lm(tasa_100k ~ ano))[2],
    n_anos    = n_distinct(ano),
    .groups   = "drop"
  )

# 2. Unir con shapefile (mismo join que hiciste con tasa_promedio)
mapa_tend <- muni_shp %>%
  left_join(tasa_tendencia, by = "mpio_cdpmp")

# 3. Ver cuántos municipios quedaron sin tendencia
sum(is.na(mapa_tend$pendiente))

# ── MAPA ──────────────────────────────────────────────────────────────────────

# Límite simétrico para que el 0 quede en el centro de la paleta
# Límite en percentil 95 para no dejar que Bogotá aplaste todo
lim <- quantile(abs(mapa_tend$pendiente), 0.95, na.rm = TRUE)

p3 <- ggplot(mapa_tend) +
  geom_sf(aes(fill = pendiente), color = NA) +
  scale_fill_gradient2(
    low      = "#185FA5",
    mid      = "white",
    high     = "#A32D2D",
    midpoint = 0,
    limits   = c(-lim, lim),
    oob      = scales::squish,   # <- valores fuera del límite se saturan al extremo
    na.value = "grey80",
    name     = "Puntos/año"
  ) +
  labs(title = "c. Tendencia 2020–2025") +
  theme_void() +
  theme(
    plot.title      = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )
# ── PANEL CONJUNTO con tus mapas existentes ───────────────────────────────────
p3 +
  plot_annotation(
    title = "Tendencia de la violencia contra la mujer por municipio (2020–2025)",
    theme = theme(
      plot.title = element_text(
        color = "#4B0082",
        size  = 16,
        face  = "bold",
        hjust = 0.5
      )
    )
  )


cobertura <- df_dep %>%
  filter(!is.na(tasa_100k)) %>%
  group_by(mpio_cdpmp) %>%
  summarise(n_anos = n_distinct(ano), .groups = "drop") %>%
  mutate(
    cobertura = case_when(
      n_anos == 6      ~ "6 años (completo)",
      n_anos %in% 4:5  ~ "4–5 años",
      n_anos %in% 2:3  ~ "2–3 años",
      n_anos == 1      ~ "1 año",
      TRUE             ~ "Sin dato"
    )
  )


mapa_cob <- muni_shp %>%
  left_join(cobertura, by = "mpio_cdpmp") %>%
  mutate(
    cobertura = replace_na(cobertura, "Sin dato")
  )


mapa_cob$cobertura <- factor(
  mapa_cob$cobertura,
  levels = c(
    "6 años (completo)",
    "4–5 años",
    "2–3 años",
    "1 año",
    "Sin dato"
  )
)


p_cob <- ggplot(mapa_cob) +
  geom_sf(aes(fill = cobertura), color = NA) +
  scale_fill_manual(
    values = c(
      "6 años (completo)" = "#0F6E56",  # verde fuerte
      "4–5 años"          = "#5DCAA5",  # verde claro
      "2–3 años"          = "#FAC775",  # amarillo
      "1 año"             = "#D85A30",  # naranja
      "Sin dato"          = "grey85"    # gris
    ),
    name = "Años con registro"
  ) +
  labs(
    title = "Cobertura temporal del registro de violencia\npor municipio (2020–2025)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )


# ================================
p_cob



# ── RIESGO ACUMULADO MUNICIPIOS ───────────────────────────────────────────────
# Mismo cálculo que hiciste en departamentos con resumen/tasa_acumulada_ponderada

resumen_mun <- df_dep %>%             # df_dep ya tiene tasa_100k y casos
  group_by(mpio_cdpmp) %>%
  summarise(
    total_casos      = sum(casos, na.rm = TRUE),
    total_poblacion  = sum(poblacion, na.rm = TRUE),
    n_anos           = n_distinct(ano),
    anos_disponibles = paste(sort(unique(ano)), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    tasa_acumulada_ponderada = (total_casos / total_poblacion) * 100000,
    tasa_acumulada_log       = log1p(tasa_acumulada_ponderada)
  )

mapa_acum_mun <- muni_shp %>%
  left_join(resumen_mun %>% select(mpio_cdpmp, tasa_acumulada_ponderada),
            by = "mpio_cdpmp")

mapa_acum_log_mun <- muni_shp %>%
  left_join(resumen_mun %>% select(mpio_cdpmp, tasa_acumulada_log),
            by = "mpio_cdpmp")

p_acum <- ggplot(mapa_acum_mun) +
  geom_sf(aes(fill = tasa_acumulada_ponderada), color = NA) +
  scale_fill_viridis_c(option = "magma", na.value = "grey85") +
  labs(title = "a. Tasa acumulada ponderada", fill = "Tasa") +
  theme_void() +
  theme(plot.title      = element_text(size = 12, face = "bold"),
        legend.position = "right")

p_acum_log <- ggplot(mapa_acum_log_mun) +
  geom_sf(aes(fill = tasa_acumulada_log), color = NA) +
  scale_fill_viridis_c(option = "magma", na.value = "grey85") +
  labs(title = "b. Tasa acumulada (log)", fill = "log(Tasa)") +
  theme_void() +
  theme(plot.title      = element_text(size = 12, face = "bold"),
        legend.position = "right")


# Moran 




