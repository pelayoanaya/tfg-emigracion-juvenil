# =============================================================================
# Script: 12_mapas.R
# Objetivo: Crear mapas coroplépticos de la tasa de emigración juvenil
#           por CCAA para diferentes años
# =============================================================================

# ---- 1. Cargar paquetes -----------------------------------------------------
library(tidyverse)
library(sf)
library(mapSpain)

# ---- 2. Cargar panel --------------------------------------------------------
panel <- read_csv2("datos_procesados/panel_tfg.csv")

# ---- 3. Descargar mapa de CCAA ----------------------------------------------
# mapSpain tiene los polígonos precargados de España
# Pedimos las CCAA con Canarias desplazadas (común en mapas españoles)
mapa_ccaa <- esp_get_ccaa(moveCAN = TRUE)

# Ver qué estructura tiene
class(mapa_ccaa)
names(mapa_ccaa)
head(mapa_ccaa[, c("codauto", "ine.ccaa.name")])


# ---- 4. Verificar que los nombres de CCAA coinciden -------------------------

ccaa_mapa  <- sort(unique(mapa_ccaa$ine.ccaa.name))
ccaa_panel <- sort(unique(panel$ccaa))

cat("CCAA en el mapa pero no en el panel:\n")
print(setdiff(ccaa_mapa, ccaa_panel))

cat("\nCCAA en el panel pero no en el mapa:\n")
print(setdiff(ccaa_panel, ccaa_mapa))


# ---- 5. Crear el primer mapa: tasa de emigración juvenil en 2024 ------------

# Unir los datos del panel (año 2024) con el mapa
mapa_2024 <- mapa_ccaa %>%
  left_join(
    panel %>% filter(año == 2024) %>% select(ccaa, tasa_emig_2034),
    by = c("ine.ccaa.name" = "ccaa")
  )

# Comprobar que hay datos en todas las CCAA
cat("Valores NA en tasa_emig_2034:\n")
sum(is.na(mapa_2024$tasa_emig_2034))

# ---- 6. Dibujar el mapa -----------------------------------------------------

mapa_g1 <- ggplot(mapa_2024) +
  geom_sf(aes(fill = tasa_emig_2034), color = "white", size = 0.3) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Tasa (‰)",
    breaks = c(2, 4, 6, 8, 10, 12, 14)
  ) +
  labs(
    title = "Tasa de emigración juvenil por CCAA en 2024",
    subtitle = "Jóvenes 20-34 con nacionalidad española, por mil habitantes",
    caption = "Fuente: elaboración propia a partir de INE (EMCR y Padrón)"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "grey40", hjust = 0.5),
    plot.caption = element_text(color = "grey50", hjust = 0),
    legend.position = "right"
  )

print(mapa_g1)

# Guardar
ggsave("figuras/fig6_mapa_2024.png", plot = mapa_g1,
       width = 10, height = 7, dpi = 300)

cat("Mapa guardado en figuras/fig6_mapa_2024.png\n")


# ---- 7. Mapa comparativo: 2008 vs 2014 vs 2024 ------------------------------

# Filtrar los tres años y unir al mapa
panel_3años <- panel %>%
  filter(año %in% c(2008, 2014, 2024)) %>%
  select(ccaa, año, tasa_emig_2034)

# Unir con el mapa (cada año genera su propia copia de las 19 geometrías)
mapa_3años <- mapa_ccaa %>%
  left_join(panel_3años, by = c("ine.ccaa.name" = "ccaa"),
            relationship = "many-to-many")

# Verificar
nrow(mapa_3años)  # 19 CCAA × 3 años = 57 filas

# Dibujar con facet_wrap (una subfigura por año)
mapa_g2 <- ggplot(mapa_3años) +
  geom_sf(aes(fill = tasa_emig_2034), color = "white", size = 0.2) +
  facet_wrap(~ año, ncol = 3) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Tasa (‰)",
    limits = c(0, 16),  # escala común para los 3 mapas
    breaks = c(2, 4, 6, 8, 10, 12, 14)
  ) +
  labs(
    title = "Evolución de la tasa de emigración juvenil por CCAA",
    subtitle = "2008 (pre-crisis), 2014 (inicio recuperación), 2024 (actualidad)",
    caption = "Fuente: elaboración propia a partir de INE (EM/EMCR y Padrón)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(color = "grey40", hjust = 0.5),
    plot.caption = element_text(color = "grey50", hjust = 0),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

print(mapa_g2)

ggsave("figuras/fig7_mapas_comparativos.png", plot = mapa_g2,
       width = 14, height = 6, dpi = 300,
       bg = "white")

cat("Mapa guardado en figuras/fig7_mapas_comparativos.png\n")


# Ver qué funciones tiene mapSpain relacionadas con mover CCAA
ls("package:mapSpain")[grepl("move|can|cm", ls("package:mapSpain"), ignore.case = TRUE)]


install.packages("ggrepel")
# ---- 9bis. Versión simple: mapa con anotación de Ceuta y Melilla ------------

library(ggrepel)

# Centroides para etiquetas
mapa_con_centroides <- mapa_ccaa %>%
  mutate(
    centroide = st_centroid(geometry),
    lon = st_coordinates(centroide)[,1],
    lat = st_coordinates(centroide)[,2]
  )

# Unir con datos de 2024
mapa_2024 <- mapa_con_centroides %>%
  left_join(
    panel %>% filter(año == 2024) %>% select(ccaa, tasa_emig_2034),
    by = c("ine.ccaa.name" = "ccaa")
  ) %>%
  mutate(
    etiqueta = case_when(
      ine.ccaa.name == "Ceuta"    ~ paste0("Ceuta\n(", round(tasa_emig_2034, 1), "‰)"),
      ine.ccaa.name == "Melilla"  ~ paste0("Melilla\n(", round(tasa_emig_2034, 1), "‰)"),
      TRUE                        ~ ""
    )
  )

mapa_g4 <- ggplot(mapa_2024) +
  geom_sf(aes(fill = tasa_emig_2034), color = "white", size = 0.3) +
  geom_label_repel(
    aes(x = lon, y = lat, label = etiqueta),
    size = 3,
    fontface = "bold",
    box.padding = 1.5,
    point.padding = 0.5,
    segment.color = "grey20",
    segment.size = 0.6,
    min.segment.length = 0,
    nudge_x = 2,
    nudge_y = 1,
    na.rm = TRUE
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Tasa (‰)",
    breaks = c(2, 4, 6, 8, 10, 12, 14)
  ) +
  labs(
    title = "Tasa de emigración juvenil por CCAA en 2024",
    subtitle = "Jóvenes 20-34 con nacionalidad española, por mil habitantes",
    caption = "Fuente: elaboración propia a partir de INE (EMCR y Padrón)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(color = "grey40", hjust = 0.5),
    plot.caption = element_text(color = "grey50", hjust = 0),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

print(mapa_g4)

ggsave("figuras/fig6_mapa_2024.png", plot = mapa_g4,
       width = 10, height = 7, dpi = 300,
       bg = "white")


# ---- 10. Mapa comparativo: 2008, 2013, 2019, 2024 --------------------------

# Filtrar los 4 años clave
panel_4años <- panel %>%
  filter(año %in% c(2008, 2013, 2019, 2024)) %>%
  select(ccaa, año, tasa_emig_2034)

# Unir con el mapa (se duplican las geometrías por año)
mapa_4años <- mapa_con_centroides %>%
  left_join(panel_4años, by = c("ine.ccaa.name" = "ccaa"),
            relationship = "many-to-many")

# Añadir columna de etiqueta solo para Ceuta y Melilla
mapa_4años <- mapa_4años %>%
  mutate(
    etiqueta = case_when(
      ine.ccaa.name == "Ceuta"    ~ paste0("Ceuta (", round(tasa_emig_2034, 1), "‰)"),
      ine.ccaa.name == "Melilla"  ~ paste0("Melilla (", round(tasa_emig_2034, 1), "‰)"),
      TRUE                        ~ ""
    )
  )

# Dibujar con facet_wrap
mapa_g5 <- ggplot(mapa_4años) +
  geom_sf(aes(fill = tasa_emig_2034), color = "white", size = 0.2) +
  geom_label_repel(
    aes(x = lon, y = lat, label = etiqueta),
    size = 2.5,
    fontface = "bold",
    box.padding = 1.2,
    point.padding = 0.3,
    segment.color = "grey30",
    segment.size = 0.4,
    min.segment.length = 0,
    nudge_x = 1.5,
    nudge_y = 0.8,
    na.rm = TRUE,
    label.padding = 0.15
  ) +
  facet_wrap(~ año, ncol = 2) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Tasa (‰)",
    limits = c(0, 16),
    breaks = c(2, 4, 6, 8, 10, 12, 14)
  ) +
  labs(
    title = "Evolución de la tasa de emigración juvenil por CCAA",
    subtitle = "Cuatro momentos clave: pre-crisis, pico crisis, bonanza, actualidad",
    caption = "Fuente: elaboración propia a partir de INE (EM/EMCR y Padrón)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(color = "grey40", hjust = 0.5),
    plot.caption = element_text(color = "grey50", hjust = 0),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

print(mapa_g5)

ggsave("figuras/fig7_mapas_comparativos.png", plot = mapa_g5,
       width = 12, height = 10, dpi = 300,
       bg = "white")

cat("Mapa guardado (sobrescribiendo el de 3 años)\n")


# ---- 11. Mapa: paro juvenil vs emigración juvenil en 2024 ------------------

# Vamos a hacer 2 mapas: el de emigración (ya lo tenemos) y el del paro
# Los dibujamos juntos usando patchwork para ponerlos lado a lado
install.packages("patchwork")
library(patchwork)  # si no lo tienes: install.packages("patchwork")

# Unir el panel con el mapa para 2024, con paro y emigración
mapa_comparativo <- mapa_con_centroides %>%
  left_join(
    panel %>% filter(año == 2024) %>% 
      select(ccaa, tasa_emig_2034, paro_joven),
    by = c("ine.ccaa.name" = "ccaa")
  ) %>%
  mutate(
    etiq_emig = case_when(
      ine.ccaa.name == "Ceuta"   ~ paste0("Ceuta (", round(tasa_emig_2034, 1), "‰)"),
      ine.ccaa.name == "Melilla" ~ paste0("Melilla (", round(tasa_emig_2034, 1), "‰)"),
      TRUE                       ~ ""
    ),
    etiq_paro = case_when(
      ine.ccaa.name == "Ceuta"   ~ paste0("Ceuta (", round(paro_joven, 1), "%)"),
      ine.ccaa.name == "Melilla" ~ paste0("Melilla (", round(paro_joven, 1), "%)"),
      TRUE                       ~ ""
    )
  )

# Mapa 1: paro juvenil
mapa_paro <- ggplot(mapa_comparativo) +
  geom_sf(aes(fill = paro_joven), color = "white", size = 0.3) +
  geom_label_repel(
    aes(x = lon, y = lat, label = etiq_paro),
    size = 2.5, fontface = "bold",
    box.padding = 1.2, point.padding = 0.3,
    segment.color = "grey30", segment.size = 0.4,
    min.segment.length = 0,
    nudge_x = 1.5, nudge_y = 0.8,
    na.rm = TRUE, label.padding = 0.15
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    name = "Paro (%)"
  ) +
  labs(
    title = "Tasa de paro juvenil 2024",
    subtitle = "20-34 años, ambos sexos"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 10),
    panel.grid = element_blank(),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()
  )

# Mapa 2: emigración juvenil
mapa_emig <- ggplot(mapa_comparativo) +
  geom_sf(aes(fill = tasa_emig_2034), color = "white", size = 0.3) +
  geom_label_repel(
    aes(x = lon, y = lat, label = etiq_emig),
    size = 2.5, fontface = "bold",
    box.padding = 1.2, point.padding = 0.3,
    segment.color = "grey30", segment.size = 0.4,
    min.segment.length = 0,
    nudge_x = 1.5, nudge_y = 0.8,
    na.rm = TRUE, label.padding = 0.15
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Emig. (‰)"
  ) +
  labs(
    title = "Tasa de emigración juvenil 2024",
    subtitle = "20-34 años con nacionalidad española"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 10),
    panel.grid = element_blank(),
    axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()
  )

# Combinar lado a lado con patchwork
mapa_g6 <- mapa_paro + mapa_emig +
  plot_annotation(
    title = "Paro juvenil y emigración juvenil por CCAA (2024)",
    subtitle = "¿Coinciden territorialmente las CCAA con más paro y más emigración?",
    caption = "Fuente: elaboración propia a partir de INE (EPA y EMCR)",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
      plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 11),
      plot.caption = element_text(color = "grey50", hjust = 0)
    )
  )

print(mapa_g6)

ggsave("figuras/fig8_mapa_paro_vs_emig.png", plot = mapa_g6,
       width = 14, height = 7, dpi = 300,
       bg = "white")


# ---- 12. Mapa 8 paneles: paro y emigración en 4 años -----------------------

# Filtrar los 4 años y quedarnos con paro y emigración
panel_paro_emig <- panel %>%
  filter(año %in% c(2008, 2013, 2019, 2024)) %>%
  select(ccaa, año, tasa_emig_2034, paro_joven) %>%
  # Pasar a formato largo: una columna con el nombre de la variable y otra con el valor
  pivot_longer(
    cols = c(tasa_emig_2034, paro_joven),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  # Renombrar para que salga bonito en el gráfico
  mutate(
    variable = case_when(
      variable == "tasa_emig_2034" ~ "Tasa de emigración (‰)",
      variable == "paro_joven"     ~ "Tasa de paro juvenil (%)"
    ),
    variable = factor(variable, levels = c("Tasa de paro juvenil (%)",
                                           "Tasa de emigración (‰)"))
  )

# Unir al mapa (generará 19 × 4 × 2 = 152 filas)
mapa_8paneles <- mapa_con_centroides %>%
  left_join(panel_paro_emig, by = c("ine.ccaa.name" = "ccaa"),
            relationship = "many-to-many")

# Quitar filas con NA en variable (las CCAA duplicadas sin datos)
mapa_8paneles <- mapa_8paneles %>%
  filter(!is.na(variable))

# Dibujar con facet_grid (variable en filas, año en columnas)
mapa_g7 <- ggplot(mapa_8paneles) +
  geom_sf(aes(fill = valor), color = "white", size = 0.2) +
  facet_grid(variable ~ año, switch = "y") +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Valor"
  ) +
  labs(
    title = "Paro juvenil y emigración juvenil por CCAA",
    subtitle = "Cuatro momentos clave: pre-crisis (2008), pico crisis (2013), bonanza (2019), actualidad (2024)",
    caption = "Fuente: elaboración propia a partir de INE (EPA, EM/EMCR y Padrón)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(color = "grey40", hjust = 0.5),
    plot.caption = element_text(color = "grey50", hjust = 0),
    strip.text.x = element_text(face = "bold", size = 11),
    strip.text.y = element_text(face = "bold", size = 11),
    strip.placement = "outside",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

print(mapa_g7)

ggsave("figuras/fig8_mapa_paro_vs_emig_4años.png", plot = mapa_g7,
       width = 14, height = 9, dpi = 300,
       bg = "white")

cat("Mapa guardado\n")


# ---- 12bis. Mapa 8 paneles con escalas independientes ----------------------

library(patchwork)  # install.packages("patchwork") si no lo tienes

# Dataset de paro (4 años)
mapa_paro_4años <- mapa_con_centroides %>%
  left_join(
    panel %>% filter(año %in% c(2008, 2013, 2019, 2024)) %>% 
      select(ccaa, año, paro_joven),
    by = c("ine.ccaa.name" = "ccaa"),
    relationship = "many-to-many"
  ) %>%
  filter(!is.na(año))

# Dataset de emigración (4 años)
mapa_emig_4años <- mapa_con_centroides %>%
  left_join(
    panel %>% filter(año %in% c(2008, 2013, 2019, 2024)) %>% 
      select(ccaa, año, tasa_emig_2034),
    by = c("ine.ccaa.name" = "ccaa"),
    relationship = "many-to-many"
  ) %>%
  filter(!is.na(año))

# Gráfico de paro (fila superior)
g_paro <- ggplot(mapa_paro_4años) +
  geom_sf(aes(fill = paro_joven), color = "white", size = 0.2) +
  facet_wrap(~ año, nrow = 1) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Paro (%)",
    limits = c(0, 55)
  ) +
  labs(title = "Tasa de paro juvenil (%)") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

# Gráfico de emigración (fila inferior)
g_emig <- ggplot(mapa_emig_4años) +
  geom_sf(aes(fill = tasa_emig_2034), color = "white", size = 0.2) +
  facet_wrap(~ año, nrow = 1) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Emig. (‰)",
    limits = c(0, 16)
  ) +
  labs(title = "Tasa de emigración juvenil (‰)") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

# Combinar las dos filas con patchwork
mapa_g7 <- (g_paro / g_emig) +
  plot_annotation(
    title = "Paro juvenil y emigración juvenil por CCAA",
    subtitle = "Cuatro momentos clave: pre-crisis (2008), pico crisis (2013), bonanza (2019), actualidad (2024)",
    caption = "Fuente: elaboración propia a partir de INE (EPA, EM/EMCR y Padrón)",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 11),
      plot.caption = element_text(color = "grey50", hjust = 0)
    )
  )

print(mapa_g7)

ggsave("figuras/fig8_mapa_paro_vs_emig_4años.png", plot = mapa_g7,
       width = 14, height = 9, dpi = 300,
       bg = "white")

cat("Mapa guardado (sobrescrito con escalas independientes)\n")


# ---- 12ter. Mapa 8 paneles con etiquetas Ceuta/Melilla ----------------------

# Dataset de paro (4 años) con etiquetas
mapa_paro_4años <- mapa_con_centroides %>%
  left_join(
    panel %>% filter(año %in% c(2008, 2013, 2019, 2024)) %>% 
      select(ccaa, año, paro_joven),
    by = c("ine.ccaa.name" = "ccaa"),
    relationship = "many-to-many"
  ) %>%
  filter(!is.na(año)) %>%
  mutate(
    etiqueta = case_when(
      ine.ccaa.name == "Ceuta"   ~ paste0("Ceuta ", round(paro_joven, 0), "%"),
      ine.ccaa.name == "Melilla" ~ paste0("Melilla ", round(paro_joven, 0), "%"),
      TRUE                       ~ ""
    )
  )

# Dataset de emigración (4 años) con etiquetas
mapa_emig_4años <- mapa_con_centroides %>%
  left_join(
    panel %>% filter(año %in% c(2008, 2013, 2019, 2024)) %>% 
      select(ccaa, año, tasa_emig_2034),
    by = c("ine.ccaa.name" = "ccaa"),
    relationship = "many-to-many"
  ) %>%
  filter(!is.na(año)) %>%
  mutate(
    etiqueta = case_when(
      ine.ccaa.name == "Ceuta"   ~ paste0("Ceuta ", round(tasa_emig_2034, 1), "‰"),
      ine.ccaa.name == "Melilla" ~ paste0("Melilla ", round(tasa_emig_2034, 1), "‰"),
      TRUE                       ~ ""
    )
  )

# Gráfico de paro (fila superior)
g_paro <- ggplot(mapa_paro_4años) +
  geom_sf(aes(fill = paro_joven), color = "white", size = 0.2) +
  geom_label_repel(
    aes(x = lon, y = lat, label = etiqueta),
    size = 2, fontface = "bold",
    box.padding = 1, point.padding = 0.2,
    segment.color = "grey30", segment.size = 0.3,
    min.segment.length = 0,
    nudge_x = 1.2, nudge_y = 0.6,
    na.rm = TRUE, label.padding = 0.12
  ) +
  facet_wrap(~ año, nrow = 1) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Paro (%)",
    limits = c(0, 55)
  ) +
  labs(title = "Tasa de paro juvenil (%)") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

# Gráfico de emigración (fila inferior)
g_emig <- ggplot(mapa_emig_4años) +
  geom_sf(aes(fill = tasa_emig_2034), color = "white", size = 0.2) +
  geom_label_repel(
    aes(x = lon, y = lat, label = etiqueta),
    size = 2, fontface = "bold",
    box.padding = 1, point.padding = 0.2,
    segment.color = "grey30", segment.size = 0.3,
    min.segment.length = 0,
    nudge_x = 1.2, nudge_y = 0.6,
    na.rm = TRUE, label.padding = 0.12
  ) +
  facet_wrap(~ año, nrow = 1) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Emig. (‰)",
    limits = c(0, 16)
  ) +
  labs(title = "Tasa de emigración juvenil (‰)") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

# Combinar
mapa_g7 <- (g_paro / g_emig) +
  plot_annotation(
    title = "Paro juvenil y emigración juvenil por CCAA",
    subtitle = "Cuatro momentos clave: pre-crisis (2008), pico crisis (2013), bonanza (2019), actualidad (2024)",
    caption = "Fuente: elaboración propia a partir de INE (EPA, EM/EMCR y Padrón)",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 11),
      plot.caption = element_text(color = "grey50", hjust = 0)
    )
  )

print(mapa_g7)

ggsave("figuras/fig8_mapa_paro_vs_emig_4años.png", plot = mapa_g7,
       width = 14, height = 9, dpi = 300,
       bg = "white")

# ---- 13. Heatmap: tasa de emigración por CCAA y año ------------------------

# Ordenar las CCAA por su tasa media 2008-2024 (así se ven patrones)
orden_ccaa <- panel %>%
  group_by(ccaa) %>%
  summarise(media = mean(tasa_emig_2034, na.rm = TRUE)) %>%
  arrange(media) %>%
  pull(ccaa)

# Convertir ccaa a factor con ese orden (para que salgan ordenadas en el eje Y)
panel_heatmap <- panel %>%
  mutate(ccaa = factor(ccaa, levels = orden_ccaa))

# Dibujar el heatmap
g8 <- ggplot(panel_heatmap, aes(x = año, y = ccaa, fill = tasa_emig_2034)) +
  geom_tile(color = "white", size = 0.3) +
  geom_text(
    aes(label = sprintf("%.1f", tasa_emig_2034)),
    size = 2.5,
    color = "white"
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Tasa (‰)",
    breaks = c(2, 4, 6, 8, 10, 12, 14, 16)
  ) +
  scale_x_continuous(breaks = seq(2008, 2024, 2), expand = c(0, 0)) +
  labs(
    title = "Tasa de emigración juvenil por CCAA y año (2008-2024)",
    subtitle = "CCAA ordenadas por tasa media del período (abajo = menor emigración)",
    x = NULL,
    y = NULL,
    caption = "Fuente: elaboración propia a partir de INE (EM/EMCR y Padrón)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 10),
    plot.caption = element_text(color = "grey50", hjust = 0),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    legend.position = "right"
  )

print(g8)

ggsave("figuras/fig9_heatmap_ccaa_año.png", plot = g8,
       width = 14, height = 8, dpi = 300,
       bg = "white")

cat("Heatmap guardado en figuras/fig9_heatmap_ccaa_año.png\n")

# ---- 14. Multipanel temporal: evolución España de todas las variables ------

# Datos: serie nacional (media simple entre CCAA por año)
series_nacional <- panel %>%
  group_by(año) %>%
  summarise(
    `Tasa de emigración juvenil (‰)`        = mean(tasa_emig_2034, na.rm = TRUE),
    `Tasa de paro juvenil (%)`              = mean(paro_joven, na.rm = TRUE),
    `Ratio paro joven / paro adulto`        = mean(ratio_paro_jov_adult, na.rm = TRUE),
    `Crecimiento del PIB regional (%)`      = mean(pib_crec, na.rm = TRUE),
    `Índice de precios de vivienda (IPV)`   = mean(ipv, na.rm = TRUE),
    `Salario real mensual (€)`              = mean(salario_real, na.rm = TRUE),
    .groups = "drop"
  )

# Pasar a formato largo para facetar
series_long <- series_nacional %>%
  pivot_longer(
    cols = -año,
    names_to = "variable",
    values_to = "valor"
  ) %>%
  # Ordenar las variables en el orden que queramos mostrar
  mutate(
    variable = factor(variable, levels = c(
      "Tasa de emigración juvenil (‰)",
      "Tasa de paro juvenil (%)",
      "Ratio paro joven / paro adulto",
      "Crecimiento del PIB regional (%)",
      "Índice de precios de vivienda (IPV)",
      "Salario real mensual (€)"
    ))
  )

# Dibujar
g9 <- ggplot(series_long, aes(x = año, y = valor)) +
  geom_line(color = "steelblue", size = 1.1) +
  geom_point(color = "steelblue", size = 1.8) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "grey60", size = 0.4) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "grey60", size = 0.4) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(2008, 2024, 4)) +
  labs(
    title = "Evolución de las variables del modelo en España (2008-2024)",
    subtitle = "Promedio no ponderado entre CCAA; líneas verticales: 2014 (recuperación) y 2020 (COVID)",
    x = "Año",
    y = NULL,
    caption = "Fuente: elaboración propia a partir de INE (EM/EMCR, EPA, CRE, IPV, DSEP, IPC)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption = element_text(color = "grey50", hjust = 0),
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(1, "lines")
  )

print(g9)

ggsave("figuras/fig10_series_variables.png", plot = g9,
       width = 12, height = 8, dpi = 300,
       bg = "white")

cat("Figura guardada en figuras/fig10_series_variables.png\n")


# ---- 15. Multipanel de mapas 2024: variables explicativas -------------------

# Unir panel 2024 con el mapa
mapa_2024_all <- mapa_con_centroides %>%
  left_join(
    panel %>% filter(año == 2024) %>%
      select(ccaa, ratio_paro_jov_adult, pib_crec, ipv, salario_real),
    by = c("ine.ccaa.name" = "ccaa")
  )

# Función auxiliar para crear cada mapa con etiquetas CM
crear_mapa <- function(data, var, titulo, unidad, paleta = "plasma", formato = "%.1f") {
  data <- data %>%
    mutate(
      etiqueta = case_when(
        ine.ccaa.name == "Ceuta"   ~ paste0("Ceuta ", sprintf(formato, .data[[var]]), unidad),
        ine.ccaa.name == "Melilla" ~ paste0("Melilla ", sprintf(formato, .data[[var]]), unidad),
        TRUE                       ~ ""
      )
    )
  
  ggplot(data) +
    geom_sf(aes(fill = .data[[var]]), color = "white", size = 0.2) +
    geom_label_repel(
      aes(x = lon, y = lat, label = etiqueta),
      size = 2, fontface = "bold",
      box.padding = 1, point.padding = 0.2,
      segment.color = "grey30", segment.size = 0.3,
      min.segment.length = 0,
      nudge_x = 1.2, nudge_y = 0.6,
      na.rm = TRUE, label.padding = 0.12
    ) +
    scale_fill_viridis_c(
      option = paleta,
      direction = -1,
      name = unidad
    ) +
    labs(title = titulo) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      panel.grid = element_blank(),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "right"
    )
}

# Crear los 4 mapas
m_ratio    <- crear_mapa(mapa_2024_all, "ratio_paro_jov_adult", 
                         "Ratio paro joven / paro adulto", "", "viridis")
m_pib      <- crear_mapa(mapa_2024_all, "pib_crec",
                         "Crecimiento del PIB regional", "%", "plasma")
m_ipv      <- crear_mapa(mapa_2024_all, "ipv",
                         "Índice de precios de vivienda (IPV)", "", "plasma",
                         formato = "%.0f")
m_salario  <- crear_mapa(mapa_2024_all, "salario_real",
                         "Salario real mensual", "€", "viridis",
                         formato = "%.0f")

# Combinar con patchwork en 2×2
mapa_g8 <- (m_ratio + m_pib) / (m_ipv + m_salario) +
  plot_annotation(
    title = "Variables explicativas del modelo por CCAA en 2024",
    subtitle = "Contexto regional al final del periodo analizado",
    caption = "Fuente: elaboración propia a partir de INE (EPA, CRE, IPV, DSEP, IPC)",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 11),
      plot.caption = element_text(color = "grey50", hjust = 0)
    )
  )

print(mapa_g8)

ggsave("figuras/fig11_mapas_variables_2024.png", plot = mapa_g8,
       width = 14, height = 10, dpi = 300,
       bg = "white")

cat("Figura guardada en figuras/fig11_mapas_variables_2024.png\n")


# ---- 16. Mapas evolutivos por variable (4 figuras de 4 años cada una) ------

# Función generadora: crea una figura con 4 mapas (2008, 2013, 2019, 2024) para una variable
crear_mapa_evolutivo <- function(var_name, var_label, unidad, paleta, 
                                 formato = "%.1f", archivo) {
  
  # Unir panel con el mapa (4 años)
  datos <- mapa_con_centroides %>%
    left_join(
      panel %>% filter(año %in% c(2008, 2013, 2019, 2024)) %>%
        select(ccaa, año, all_of(var_name)),
      by = c("ine.ccaa.name" = "ccaa"),
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(año)) %>%
    mutate(
      etiqueta = case_when(
        ine.ccaa.name == "Ceuta"   ~ paste0("Ceuta ", sprintf(formato, .data[[var_name]]), unidad),
        ine.ccaa.name == "Melilla" ~ paste0("Melilla ", sprintf(formato, .data[[var_name]]), unidad),
        TRUE                       ~ ""
      )
    )
  
  # Gráfico
  g <- ggplot(datos) +
    geom_sf(aes(fill = .data[[var_name]]), color = "white", size = 0.2) +
    geom_label_repel(
      aes(x = lon, y = lat, label = etiqueta),
      size = 2, fontface = "bold",
      box.padding = 1, point.padding = 0.2,
      segment.color = "grey30", segment.size = 0.3,
      min.segment.length = 0,
      nudge_x = 1.2, nudge_y = 0.6,
      na.rm = TRUE, label.padding = 0.12
    ) +
    facet_wrap(~ año, nrow = 1) +
    scale_fill_viridis_c(option = paleta, direction = -1, name = unidad) +
    labs(
      title = paste0(var_label, " por CCAA (2008, 2013, 2019, 2024)"),
      subtitle = "Cuatro momentos clave: pre-crisis, pico crisis, bonanza pre-COVID, actualidad",
      caption = "Fuente: elaboración propia a partir de INE"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 10),
      plot.caption = element_text(color = "grey50", hjust = 0),
      strip.text = element_text(face = "bold", size = 11),
      panel.grid = element_blank(),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "right"
    )
  
  print(g)
  
  ggsave(paste0("figuras/", archivo), plot = g,
         width = 14, height = 5, dpi = 300,
         bg = "white")
  
  cat("Guardado:", archivo, "\n")
  
  return(g)
}

# ---- Generar las 4 figuras --------------------------------------------------

# Ratio paro joven/adulto
g_ratio_evo <- crear_mapa_evolutivo(
  var_name = "ratio_paro_jov_adult",
  var_label = "Ratio paro joven / paro adulto",
  unidad = "",
  paleta = "viridis",
  formato = "%.1f",
  archivo = "fig12_ratio_evolutivo.png"
)

# Crecimiento PIB
g_pib_evo <- crear_mapa_evolutivo(
  var_name = "pib_crec",
  var_label = "Crecimiento del PIB regional",
  unidad = "%",
  paleta = "plasma",
  formato = "%.1f",
  archivo = "fig13_pib_evolutivo.png"
)

# IPV
g_ipv_evo <- crear_mapa_evolutivo(
  var_name = "ipv",
  var_label = "Índice de precios de vivienda (IPV)",
  unidad = "",
  paleta = "plasma",
  formato = "%.0f",
  archivo = "fig14_ipv_evolutivo.png"
)

# Salario real
g_salario_evo <- crear_mapa_evolutivo(
  var_name = "salario_real",
  var_label = "Salario real mensual",
  unidad = "€",
  paleta = "viridis",
  formato = "%.0f",
  archivo = "fig15_salario_evolutivo.png"
)