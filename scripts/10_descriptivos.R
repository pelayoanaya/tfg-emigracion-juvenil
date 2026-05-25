# =============================================================================
# Script: 10_descriptivos.R
# Objetivo: Análisis descriptivo del panel para el TFG
# =============================================================================

# ---- 1. Cargar paquetes -----------------------------------------------------
library(tidyverse)
library(modelsummary)   # para tablas de estadísticos descriptivos

# ---- 2. Cargar panel --------------------------------------------------------
panel <- read_csv2("datos_procesados/panel_tfg.csv")

# Ver estructura del panel
cat("Panel: ", nrow(panel), "filas,", ncol(panel), "columnas\n\n")
glimpse(panel)


# ---- 3. Tabla de estadísticos descriptivos ----------------------------------
# Nos quedamos solo con las variables que usaremos en el modelo

panel_modelo <- panel %>%
  select(
    tasa_emig_2034,
    paro_joven,
    ratio_paro_jov_adult,
    pib_crec,
    ipv,
    salario_real
  )

# Tabla de estadísticos con modelsummary
datasummary_skim(
  panel_modelo,
  output = "markdown"
)


# ---- 4. Series temporales: evolución nacional de variables clave ------------

# Calculamos el promedio simple entre CCAA para cada año
series_nacional <- panel %>%
  group_by(año) %>%
  summarise(
    tasa_emig_prom = mean(tasa_emig_2034, na.rm = TRUE),
    paro_joven_prom = mean(paro_joven, na.rm = TRUE),
    ratio_prom = mean(ratio_paro_jov_adult, na.rm = TRUE),
    pib_crec_prom = mean(pib_crec, na.rm = TRUE),
    ipv_prom = mean(ipv, na.rm = TRUE),
    salario_real_prom = mean(salario_real, na.rm = TRUE),
    .groups = "drop"
  )

print(series_nacional)

dir.create("figuras", showWarnings = FALSE)


# ---- 5. Gráfico: evolución de la tasa de emigración juvenil ----------------

library(ggplot2)

g1 <- ggplot(series_nacional, aes(x = año, y = tasa_emig_prom)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2.5) +
  # Líneas verticales para marcar eventos importantes
  geom_vline(xintercept = 2014, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "grey50") +
  # Anotaciones de los eventos
  annotate("text", x = 2014, y = 6, label = "Inicio\nrecuperación", 
           hjust = -0.1, size = 3, color = "grey30") +
  annotate("text", x = 2020, y = 6, label = "COVID-19", 
           hjust = -0.1, size = 3, color = "grey30") +
  # Etiquetas y título
  labs(
    title = "Evolución de la tasa de emigración juvenil en España (2008-2024)",
    subtitle = "Promedio no ponderado entre CCAA, jóvenes 20-34 con nacionalidad española",
    x = "Año",
    y = "Tasa de emigración (‰)",
    caption = "Fuente: elaboración propia a partir de la Estadística de Migraciones (INE)"
  ) +
  scale_x_continuous(breaks = seq(2008, 2024, 2)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption = element_text(color = "grey50", hjust = 0)
  )

print(g1)

# Guardar como imagen para el TFG
ggsave("figuras/fig1_emigracion_temporal.png", plot = g1, 
       width = 9, height = 5, dpi = 300)

cat("Gráfico guardado en figuras/fig1_emigracion_temporal.png\n")


# ---- 6. Gráfico: emigración vs paro juvenil (doble eje) --------------------

# Para escalar el paro y la emigración en el mismo gráfico
# Usamos sec_axis para tener dos ejes Y

# Factor de escala: queremos que ambas series sean visibles
# Paro ~ 14-35%, Emigración ~ 1-6‰. Dividimos paro entre 5.
factor_escala <- 5

g2 <- ggplot(series_nacional, aes(x = año)) +
  # Tasa de emigración (eje izquierdo)
  geom_line(aes(y = tasa_emig_prom, color = "Tasa de emigración (‰)"), 
            size = 1.2) +
  geom_point(aes(y = tasa_emig_prom, color = "Tasa de emigración (‰)"), 
             size = 2.5) +
  # Paro juvenil (eje derecho, escalado)
  geom_line(aes(y = paro_joven_prom / factor_escala, 
                color = "Paro juvenil (%, eje der.)"), 
            size = 1.2, linetype = "longdash") +
  geom_point(aes(y = paro_joven_prom / factor_escala, 
                 color = "Paro juvenil (%, eje der.)"), 
             size = 2) +
  # Línea vertical 2014
  geom_vline(xintercept = 2014, linetype = "dashed", color = "grey60") +
  # Doble eje Y
  scale_y_continuous(
    name = "Tasa de emigración (‰)",
    sec.axis = sec_axis(~ . * factor_escala, 
                        name = "Tasa de paro juvenil (%)")
  ) +
  scale_x_continuous(breaks = seq(2008, 2024, 2)) +
  scale_color_manual(values = c("Tasa de emigración (‰)" = "steelblue",
                                "Paro juvenil (%, eje der.)" = "firebrick")) +
  labs(
    title = "Emigración juvenil y paro juvenil en España (2008-2024)",
    subtitle = "¿Se mueven juntos o se han desacoplado?",
    x = "Año",
    color = NULL,
    caption = "Fuente: elaboración propia a partir de INE (Estadística de Migraciones y EPA)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption = element_text(color = "grey50", hjust = 0),
    legend.position = "top"
  )

print(g2)

ggsave("figuras/fig2_emigracion_vs_paro.png", plot = g2,
       width = 9, height = 5, dpi = 300)

cat("Gráfico guardado en figuras/fig2_emigracion_vs_paro.png\n")


# ---- 7. Heterogeneidad por CCAA: tasa emigración por CCAA (2024) ------------

# Ordenamos las CCAA de mayor a menor tasa de emigración en 2024
panel_2024 <- panel %>%
  filter(año == 2024) %>%
  arrange(desc(tasa_emig_2034))

g3 <- panel_2024 %>%
  mutate(ccaa = fct_reorder(ccaa, tasa_emig_2034)) %>%
  ggplot(aes(x = tasa_emig_2034, y = ccaa)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f", tasa_emig_2034)), 
            hjust = -0.2, size = 3.3) +
  labs(
    title = "Tasa de emigración juvenil por CCAA en 2024",
    subtitle = "Por mil habitantes jóvenes (20-34 años) con nacionalidad española",
    x = "Tasa de emigración (‰)",
    y = NULL,
    caption = "Fuente: elaboración propia a partir de INE (EMCR y Padrón)"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption = element_text(color = "grey50", hjust = 0),
    panel.grid.major.y = element_blank()
  )

print(g3)

ggsave("figuras/fig3_emigracion_ccaa_2024.png", plot = g3,
       width = 9, height = 6, dpi = 300)

cat("Gráfico guardado en figuras/fig3_emigracion_ccaa_2024.png\n")


install.packages("corrplot")

# ---- 8. Matriz de correlaciones ---------------------------------------------

library(corrplot)

# Seleccionamos las variables del modelo
cor_data <- panel %>%
  select(tasa_emig_2034, paro_joven, ratio_paro_jov_adult,
         pib_crec, ipv, salario_real) %>%
  as.data.frame()

# Calcular matriz de correlación (Pearson)
cor_matrix <- cor(cor_data, use = "complete.obs")

# Mostrar en consola
print(round(cor_matrix, 3))

# Gráfico de correlación
png("figuras/fig4_correlaciones.png", width = 2000, height = 1800, res = 300)
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.9,
  col = colorRampPalette(c("firebrick", "white", "steelblue"))(200),
  diag = FALSE
)
dev.off()

cat("Gráfico guardado en figuras/fig4_correlaciones.png\n")


# ---- 9. Scatter: paro juvenil vs emigración (por CCAA-año) ------------------

g5 <- panel %>%
  ggplot(aes(x = paro_joven, y = tasa_emig_2034)) +
  geom_point(aes(color = as.factor(año >= 2014)), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("FALSE" = "firebrick", "TRUE" = "steelblue"),
    labels = c("FALSE" = "2008-2013", "TRUE" = "2014-2024"),
    name = "Periodo"
  ) +
  labs(
    title = "Relación entre paro juvenil y emigración juvenil",
    subtitle = "Cada punto es una observación CCAA-año (N=323)",
    x = "Tasa de paro juvenil (%)",
    y = "Tasa de emigración juvenil (‰)",
    caption = "Fuente: elaboración propia a partir de INE (EPA, EM/EMCR, Padrón)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption = element_text(color = "grey50", hjust = 0),
    legend.position = "top"
  )

print(g5)

ggsave("figuras/fig5_scatter_paro_emig.png", plot = g5,
       width = 9, height = 6, dpi = 300)

cat("Gráfico guardado en figuras/fig5_scatter_paro_emig.png\n")

