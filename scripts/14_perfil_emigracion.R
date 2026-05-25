# =============================================================================
# Capítulo 5 — Perfil de la emigración juvenil española
# Carga inicial de tablas EM (24300) + EMCR (69705)
# =============================================================================

library(tidyverse)
library(janitor)

# ---- 1. Cargar tabla EM antigua (24300): periodo 2008-2021 ------------------

em <- read_delim(
  "datos_brutos/ine_perfil/24300.csv",
  delim = ";",
  locale = locale(encoding = "UTF-8", decimal_mark = ",", grouping_mark = "."),
  show_col_types = FALSE
) %>%
  clean_names()

# Vemos la estructura
cat("=== Tabla 24300 (EM) ===\n")
dim(em)
glimpse(em)
head(em)


# ---- 2. Cargar tabla EMCR (69705): periodo 2021-2024 ------------------------

emcr <- read_delim(
  "datos_brutos/ine_perfil/69705.csv",
  delim = ";",
  locale = locale(encoding = "ISO-8859-1", decimal_mark = ",", grouping_mark = "."),
  show_col_types = FALSE
) %>%
  clean_names()

cat("=== Tabla 69705 (EMCR) ===\n")
dim(emcr)
glimpse(emcr)
head(emcr)


# ---- 3. Filtrar EM para quedarnos con las mismas dimensiones que EMCR -------

em_simplificada <- em %>% 
  filter(
    pais_de_nacimiento == "Total",
    grupo_quinquenal_de_edad == "Total"
  ) %>% 
  select(nacionalidad, sexo, 
         pais_de_destino = pais_destino,
         periodo, total) %>% 
  filter(periodo <= 2020)   # Quitamos 2021 porque lo cogemos de EMCR

# Comprobamos
cat("=== EM simplificada (2008-2020) ===\n")
dim(em_simplificada)
range(em_simplificada$periodo)

# ---- 4. Encadenar EM + EMCR -------------------------------------------------

panel_destinos <- bind_rows(
  em_simplificada %>% mutate(fuente = "EM"),
  emcr %>% mutate(fuente = "EMCR")
)

cat("=== Panel encadenado ===\n")
dim(panel_destinos)
range(panel_destinos$periodo)
table(panel_destinos$periodo, panel_destinos$fuente)


# ---- 5. Filtrar a españoles ambos sexos -------------------------------------

espanoles_destinos <- panel_destinos %>% 
  filter(
    nacionalidad == "Española",
    sexo == "Ambos sexos"
  )

cat("=== Solo españoles, ambos sexos ===\n")
dim(espanoles_destinos)

# ¿Qué países de destino aparecen?
unique(espanoles_destinos$pais_de_destino) %>% sort()


# ---- 6. Verificación: total de españoles emigrados por año ------------------

total_anual <- espanoles_destinos %>% 
  filter(pais_de_destino == "Total") %>% 
  arrange(periodo) %>% 
  select(periodo, total, fuente)

print(total_anual, n = 17)


# ---- 7. Guardar el panel limpio ---------------------------------------------

# Crear carpeta si no existe
dir.create("datos_procesados", showWarnings = FALSE)

# Guardar
write_csv(panel_destinos, "datos_procesados/panel_destinos.csv")

cat("Panel guardado en datos_procesados/panel_destinos.csv\n")
cat("Filas:", nrow(panel_destinos), "\n")
cat("Periodo:", min(panel_destinos$periodo), "-", max(panel_destinos$periodo), "\n")


# =============================================================================
# FIGURAS DEL CAP 5
# =============================================================================

# ---- Figura 5.1: Evolución temporal de la emigración española --------------

# Creamos la carpeta de figuras del cap 5
dir.create("figuras/cap5", showWarnings = FALSE, recursive = TRUE)

# Datos: total de españoles emigrados por año
fig1_data <- espanoles_destinos %>% 
  filter(pais_de_destino == "Total") %>% 
  arrange(periodo)

# Gráfico
fig1 <- ggplot(fig1_data, aes(x = periodo, y = total, color = fuente)) +
  # Líneas
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  
  # Línea vertical marcando el cambio metodológico
  geom_vline(xintercept = 2020.5, linetype = "dashed", color = "grey40") +
  annotate("text", x = 2020.5, y = max(fig1_data$total) * 0.95,
           label = "Cambio metodológico\n(EM → EMCR)",
           hjust = -0.05, size = 3.2, color = "grey30") +
  
  # Colores
  scale_color_manual(values = c("EM" = "#1f77b4", "EMCR" = "#d62728"),
                     labels = c("EM (2008-2020)", "EMCR (2021-2024)")) +
  
  # Escalas
  scale_x_continuous(breaks = seq(2008, 2024, 2)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  
  # Etiquetas
  labs(
    title = "Evolución de la emigración española al extranjero (2008-2024)",
    subtitle = "Españoles emigrados al extranjero, todas las edades y ambos sexos",
    x = NULL,
    y = "Número de personas",
    color = "Fuente",
    caption = "Fuente: elaboración propia a partir de INE (tablas 24300 y 69705)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.minor = element_blank()
  )

# Vemos la figura
print(fig1)

# Guardamos
ggsave("figuras/cap5/fig1_evolucion_emigracion.png",
       fig1, width = 9, height = 5.5, dpi = 300, bg = "white")

cat("Figura 1 guardada\n")


# ---- Figura 3: Principales destinos en 2024 ---------------------------------

# Lista de "no países" a excluir (agregados regionales)
no_paises <- c(
  "Total",
  "África", "América del Norte", "Asia", "Centro América y Caribe",
  "Europa menos UE27_2020", "Oceanía", "Sudamérica",
  "Otro país de África", "Otro país de Asia",
  "Otro país de la Unión Europea sin España",
  "Otro país de Sudamérica", "Otro país del resto de Europa",
  "Otros países de Centro América y Caribe",
  "País de Europa menos UE28",
  "UE27_2020 sin España", "UE28 sin España"
)

# Filtramos: solo países concretos, año 2024, españoles ambos sexos
fig3_data <- espanoles_destinos %>% 
  filter(
    !pais_de_destino %in% no_paises,
    periodo == 2024
  ) %>% 
  arrange(desc(total)) %>% 
  slice_head(n = 15)

# Vemos qué hay
print(fig3_data)


# Gráfico de barras horizontales
fig3 <- ggplot(fig3_data, 
               aes(x = total, 
                   y = reorder(pais_de_destino, total))) +
  
  # Barras
  geom_col(fill = "#2E5984", width = 0.7) +
  
  # Etiquetas dentro/fuera de las barras
  geom_text(aes(label = scales::label_number(big.mark = ".", decimal.mark = ",")(total)),
            hjust = -0.1, size = 3.2, color = "grey20") +
  
  # Escalas
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.15))) +
  
  # Etiquetas
  labs(
    title = "Principales destinos de la emigración española (2024)",
    subtitle = "Top 15 países de destino, ambos sexos, todas las edades",
    x = "Número de emigrantes",
    y = NULL,
    caption = "Fuente: elaboración propia a partir de INE (tabla 69705)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10)
  )

print(fig3)

# Guardar
ggsave("figuras/cap5/fig3_top_destinos_2024.png",
       fig3, width = 9, height = 6, dpi = 300, bg = "white")

cat("Figura 3 guardada\n")


# ---- Figura 4: Evolución de los principales destinos europeos --------------

# Top 5 destinos europeos para hacer la figura
top_destinos <- c("Francia", "Reino Unido", "Alemania", "Suiza", "Bélgica")

fig4_data <- espanoles_destinos %>% 
  filter(pais_de_destino %in% top_destinos)

# Gráfico
fig4 <- ggplot(fig4_data, 
               aes(x = periodo, y = total, 
                   color = pais_de_destino, 
                   group = interaction(pais_de_destino, fuente))) +
  
  # Línea vertical del cambio metodológico
  geom_vline(xintercept = 2020.5, linetype = "dashed", color = "grey60", alpha = 0.5) +
  
  # Líneas y puntos
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  
  # Colores
  scale_color_brewer(palette = "Set1") +
  
  # Escalas
  scale_x_continuous(breaks = seq(2008, 2024, 2)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  
  # Etiquetas
  labs(
    title = "Evolución de los principales destinos europeos (2008-2024)",
    subtitle = "Españoles emigrados a los 5 principales países europeos",
    x = NULL,
    y = "Número de emigrantes",
    color = "Destino",
    caption = "Fuente: elaboración propia a partir de INE (tablas 24300 y 69705).\nLa línea vertical marca el cambio metodológico EM → EMCR en 2021."
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.minor = element_blank()
  )

print(fig4)

ggsave("figuras/cap5/fig4_evolucion_destinos.png",
       fig4, width = 9.5, height = 6, dpi = 300, bg = "white")

cat("Figura 4 guardada\n")


# ---- Figura 5: Cambio en composición de destinos ---------------------------

# Top 10 destinos en 2024 + 5 latinoamericanos clave
destinos_clave <- c("Francia", "Reino Unido", "Alemania", "Suiza", "Bélgica",
                    "Estados Unidos de América", "Países Bajos", "Italia",
                    "Ecuador", "Colombia", "Argentina", "Venezuela", "México")

fig5_data <- espanoles_destinos %>% 
  filter(
    pais_de_destino %in% destinos_clave,
    periodo %in% c(2008, 2015, 2024)
  ) %>% 
  mutate(
    region = case_when(
      pais_de_destino %in% c("Francia", "Reino Unido", "Alemania", "Suiza",
                             "Bélgica", "Países Bajos", "Italia") ~ "Europa",
      pais_de_destino == "Estados Unidos de América" ~ "EEUU",
      TRUE ~ "Latinoamérica"
    )
  )

# Gráfico
fig5 <- ggplot(fig5_data, 
               aes(x = factor(periodo), y = total, 
                   fill = region)) +
  
  geom_col(position = "stack", width = 0.6) +
  
  scale_fill_manual(values = c("Europa" = "#2E5984", 
                               "EEUU" = "#5C8B5C",
                               "Latinoamérica" = "#C97D60")) +
  
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  
  labs(
    title = "Composición regional de los destinos de la emigración española",
    subtitle = "Comparativa de los principales destinos en tres momentos clave",
    x = NULL,
    y = "Número de emigrantes",
    fill = "Región",
    caption = "Fuente: elaboración propia a partir de INE (tablas 24300 y 69705).\n2024 utiliza datos EMCR; 2008 y 2015 utilizan datos EM."
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(fig5)

ggsave("figuras/cap5/fig5_composicion_destinos.png",
       fig5, width = 8, height = 5.5, dpi = 300, bg = "white")

cat("Figura 5 guardada\n")


# ---- Figura 3 mejorada: Top destinos en 4 años clave -----------------------

# Top destinos a mostrar (dejamos fijo el orden basado en 2024)
top_destinos_4y <- fig3_data$pais_de_destino  # los 15 que ya teníamos

# Datos: solo esos países, en los 4 años clave
fig3b_data <- espanoles_destinos %>% 
  filter(
    pais_de_destino %in% top_destinos_4y,
    periodo %in% c(2008, 2013, 2019, 2024)
  ) %>% 
  mutate(
    pais_de_destino = factor(pais_de_destino, levels = rev(top_destinos_4y))
  )

# Gráfico facetado
fig3b <- ggplot(fig3b_data, 
                aes(x = total, y = pais_de_destino)) +
  
  geom_col(fill = "#2E5984", width = 0.7) +
  
  geom_text(aes(label = scales::label_number(big.mark = ".", decimal.mark = ",")(total)),
            hjust = -0.1, size = 2.7, color = "grey20") +
  
  facet_wrap(~ periodo, nrow = 1, scales = "free_x") +
  
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.25))) +
  
  labs(
    title = "Principales destinos de la emigración española en cuatro momentos clave",
    subtitle = "Top 15 destinos según ranking de 2024",
    x = "Número de emigrantes",
    y = NULL,
    caption = "Fuente: elaboración propia a partir de INE (tablas 24300 y 69705)."
  ) +
  
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey30", size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.y = element_text(size = 8.5)
  )

print(fig3b)

ggsave("figuras/cap5/fig3_top_destinos_comparativa.png",
       fig3b, width = 12, height = 6.5, dpi = 300, bg = "white")

cat("Figura 3 (comparativa) guardada\n")


# ---- Figura 5: Composición regional de destinos en 4 momentos clave --------

destinos_clave <- c("Francia", "Reino Unido", "Alemania", "Suiza", "Bélgica",
                    "Estados Unidos de América", "Países Bajos", "Italia",
                    "Ecuador", "Colombia", "Argentina", "Venezuela", "México",
                    "República Dominicana")

fig5_data <- espanoles_destinos %>% 
  filter(
    pais_de_destino %in% destinos_clave,
    periodo %in% c(2008, 2013, 2019, 2024)
  ) %>% 
  mutate(
    region = case_when(
      pais_de_destino %in% c("Francia", "Reino Unido", "Alemania", "Suiza",
                             "Bélgica", "Países Bajos", "Italia") ~ "Europa",
      pais_de_destino == "Estados Unidos de América" ~ "EEUU",
      TRUE ~ "Latinoamérica"
    ),
    region = factor(region, levels = c("Europa", "EEUU", "Latinoamérica"))
  ) %>% 
  group_by(periodo, region) %>% 
  summarise(total = sum(total), .groups = "drop")

# Calculamos el total por año para etiquetar
totales_anuales <- fig5_data %>% 
  group_by(periodo) %>% 
  summarise(total_anual = sum(total))

# Versión 1: BARRAS APILADAS (volumen absoluto)
fig5a <- ggplot(fig5_data, 
                aes(x = factor(periodo), y = total, fill = region)) +
  
  geom_col(position = "stack", width = 0.6) +
  
  # Etiqueta del total encima de cada barra
  geom_text(data = totales_anuales,
            aes(x = factor(periodo), y = total_anual, 
                label = scales::label_number(big.mark = ".")(total_anual)),
            inherit.aes = FALSE,
            vjust = -0.5, size = 3.5, fontface = "bold", color = "grey20") +
  
  scale_fill_manual(values = c("Europa" = "#2E5984", 
                               "EEUU" = "#5C8B5C",
                               "Latinoamérica" = "#C97D60")) +
  
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Composición regional de la emigración española",
    subtitle = "Volumen absoluto en cuatro momentos clave",
    x = NULL,
    y = "Número de emigrantes",
    fill = "Región",
    caption = "Fuente: elaboración propia a partir de INE (tablas 24300 y 69705)."
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(fig5a)

ggsave("figuras/cap5/fig5_composicion_destinos.png",
       fig5a, width = 8, height = 5.5, dpi = 300, bg = "white")

cat("Figura 5 guardada\n")


# ---- Figura 5b: Composición porcentual --------------------------------------

fig5_pct <- fig5_data %>% 
  group_by(periodo) %>% 
  mutate(porcentaje = total / sum(total) * 100) %>% 
  ungroup()

fig5b <- ggplot(fig5_pct, 
                aes(x = factor(periodo), y = porcentaje, fill = region)) +
  
  geom_col(position = "stack", width = 0.6) +
  
  geom_text(aes(label = paste0(round(porcentaje), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  
  scale_fill_manual(values = c("Europa" = "#2E5984", 
                               "EEUU" = "#5C8B5C",
                               "Latinoamérica" = "#C97D60")) +
  
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     breaks = seq(0, 100, 25)) +
  
  labs(
    title = "Composición porcentual de la emigración española por región",
    subtitle = "Distribución relativa en cuatro momentos clave",
    x = NULL,
    y = "Porcentaje del total",
    fill = "Región",
    caption = "Fuente: elaboración propia a partir de INE (tablas 24300 y 69705).\nLa distribución porcentual reduce la influencia del cambio metodológico EM → EMCR de 2021."
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(fig5b)

ggsave("figuras/cap5/fig5b_composicion_porcentual.png",
       fig5b, width = 8, height = 5.5, dpi = 300, bg = "white")

cat("Figura 5b guardada\n")



# =============================================================================
# CORRECCIÓN: filtrar por jóvenes 20-34 años
# =============================================================================

# ---- Reconstruir el panel filtrando por jóvenes ----------------------------

# La tabla EM (24300) sí tiene edad. Filtramos 20-34.
em_jovenes <- em %>% 
  filter(
    pais_de_nacimiento == "Total",
    grupo_quinquenal_de_edad %in% c("De 20 a 24 años", 
                                    "De 25 a 29 años", 
                                    "De 30 a 34 años")
  ) %>% 
  # Sumamos los 3 grupos quinquenales para obtener el total 20-34
  group_by(sexo, nacionalidad, pais_destino, periodo) %>% 
  summarise(total = sum(total), .groups = "drop") %>% 
  rename(pais_de_destino = pais_destino) %>% 
  filter(periodo <= 2020)   # quitamos 2021 (lo cogemos de EMCR)

# La tabla EMCR no tiene edad. La dejamos solo para la figura 1 (visión general)
# Pero NO la usaremos para las figuras de 20-34 (3, 4, 5)

cat("=== EM jóvenes 20-34 (2008-2020) ===\n")
dim(em_jovenes)
range(em_jovenes$periodo)

# Filtramos a españoles, ambos sexos
espanoles_jovenes <- em_jovenes %>% 
  filter(
    nacionalidad == "Española",
    sexo == "Ambos sexos"
  )

cat("=== Solo españoles 20-34, ambos sexos ===\n")
dim(espanoles_jovenes)

# Verificación: total de españoles 20-34 emigrados por año
total_jovenes_anual <- espanoles_jovenes %>% 
  filter(pais_de_destino == "Total") %>% 
  arrange(periodo) %>% 
  select(periodo, total)

print(total_jovenes_anual)



# =============================================================================
# FIGURAS DE JÓVENES 20-34 (2008-2020)
# =============================================================================

# ---- Figura 1 jóvenes: evolución 2008-2020 ---------------------------------

fig1j_data <- espanoles_jovenes %>% 
  filter(pais_de_destino == "Total") %>% 
  arrange(periodo)

fig1j <- ggplot(fig1j_data, aes(x = periodo, y = total)) +
  geom_line(linewidth = 1.1, color = "#1f77b4") +
  geom_point(size = 2.5, color = "#1f77b4") +
  
  # Anotación del pico 2015
  geom_text(data = fig1j_data %>% filter(periodo == 2015),
            aes(label = paste0("Pico crisis: ", scales::label_number(big.mark = ".")(total))),
            vjust = -1.5, hjust = 0.5, size = 3.2, color = "grey30") +
  
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     limits = c(0, NA)) +
  
  labs(
    title = "Evolución de la emigración juvenil española (2008-2020)",
    subtitle = "Españoles emigrados al extranjero, 20-34 años, ambos sexos",
    x = NULL,
    y = "Número de personas",
    caption = "Fuente: elaboración propia a partir de INE (tabla 24300)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.minor = element_blank()
  )

print(fig1j)
ggsave("figuras/cap5/fig1_jovenes_evolucion.png",
       fig1j, width = 9, height = 5.5, dpi = 300, bg = "white")

# ---- Figura 3 jóvenes: top destinos 4 años ---------------------------------

# Top 15 destinos en 2020 (último año disponible)
no_paises <- c(
  "Total", "África", "América del Norte", "Asia", "Centro América y Caribe",
  "Europa menos UE27_2020", "Oceanía", "Sudamérica",
  "Otro país de África", "Otro país de Asia",
  "Otro país de la Unión Europea sin España",
  "Otro país de Sudamérica", "Otro país del resto de Europa",
  "Otros países de Centro América y Caribe",
  "País de Europa menos UE28",
  "UE27_2020 sin España", "UE28 sin España"
)

top_destinos_jovenes <- espanoles_jovenes %>% 
  filter(
    !pais_de_destino %in% no_paises,
    periodo == 2020
  ) %>% 
  arrange(desc(total)) %>% 
  slice_head(n = 15) %>% 
  pull(pais_de_destino)

fig3j_data <- espanoles_jovenes %>% 
  filter(
    pais_de_destino %in% top_destinos_jovenes,
    periodo %in% c(2008, 2013, 2017, 2020)
  ) %>% 
  mutate(
    pais_de_destino = factor(pais_de_destino, levels = rev(top_destinos_jovenes))
  )

fig3j <- ggplot(fig3j_data, 
                aes(x = total, y = pais_de_destino)) +
  geom_col(fill = "#2E5984", width = 0.7) +
  geom_text(aes(label = scales::label_number(big.mark = ".", decimal.mark = ",")(total)),
            hjust = -0.1, size = 2.7, color = "grey20") +
  facet_wrap(~ periodo, nrow = 1, scales = "free_x") +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Principales destinos de la emigración juvenil española",
    subtitle = "Top 15 destinos según ranking de 2020. Españoles 20-34 años",
    x = "Número de emigrantes",
    y = NULL,
    caption = "Fuente: elaboración propia a partir de INE (tabla 24300)."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey30", size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.y = element_text(size = 8.5)
  )

print(fig3j)
ggsave("figuras/cap5/fig3_jovenes_top_destinos.png",
       fig3j, width = 12, height = 6.5, dpi = 300, bg = "white")

# ---- Figura 4 jóvenes: evolución destinos europeos ------------------------

top_destinos_eu <- c("Francia", "Reino Unido", "Alemania", "Suiza", "Bélgica")

fig4j_data <- espanoles_jovenes %>% 
  filter(pais_de_destino %in% top_destinos_eu)

fig4j <- ggplot(fig4j_data, 
                aes(x = periodo, y = total, color = pais_de_destino)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Evolución de los principales destinos europeos",
    subtitle = "Españoles 20-34 años emigrados a los 5 principales países europeos (2008-2020)",
    x = NULL,
    y = "Número de emigrantes",
    color = "Destino",
    caption = "Fuente: elaboración propia a partir de INE (tabla 24300)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.minor = element_blank()
  )

print(fig4j)
ggsave("figuras/cap5/fig4_jovenes_evolucion_destinos.png",
       fig4j, width = 9.5, height = 6, dpi = 300, bg = "white")

# ---- Figura 5 jóvenes: composición porcentual ------------------------------

destinos_clave <- c("Francia", "Reino Unido", "Alemania", "Suiza", "Bélgica",
                    "Estados Unidos de América", "Países Bajos", "Italia",
                    "Ecuador", "Colombia", "Argentina", "Venezuela", "México",
                    "República Dominicana")

fig5j_data <- espanoles_jovenes %>% 
  filter(
    pais_de_destino %in% destinos_clave,
    periodo %in% c(2008, 2013, 2017, 2020)
  ) %>% 
  mutate(
    region = case_when(
      pais_de_destino %in% c("Francia", "Reino Unido", "Alemania", "Suiza",
                             "Bélgica", "Países Bajos", "Italia") ~ "Europa",
      pais_de_destino == "Estados Unidos de América" ~ "EEUU",
      TRUE ~ "Latinoamérica"
    ),
    region = factor(region, levels = c("Europa", "EEUU", "Latinoamérica"))
  ) %>% 
  group_by(periodo, region) %>% 
  summarise(total = sum(total), .groups = "drop") %>% 
  group_by(periodo) %>% 
  mutate(porcentaje = total / sum(total) * 100) %>% 
  ungroup()

fig5j <- ggplot(fig5j_data, 
                aes(x = factor(periodo), y = porcentaje, fill = region)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(aes(label = paste0(round(porcentaje), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Europa" = "#2E5984", 
                               "EEUU" = "#5C8B5C",
                               "Latinoamérica" = "#C97D60")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     breaks = seq(0, 100, 25)) +
  labs(
    title = "Composición regional de la emigración juvenil española",
    subtitle = "Distribución relativa por región. Españoles 20-34 años",
    x = NULL,
    y = "Porcentaje del total",
    fill = "Región",
    caption = "Fuente: elaboración propia a partir de INE (tabla 24300)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(fig5j)
ggsave("figuras/cap5/fig5_jovenes_composicion_porcentual.png",
       fig5j, width = 8, height = 5.5, dpi = 300, bg = "white")

cat("Las 4 figuras de jóvenes guardadas\n")


# ---- Figura 1 jóvenes: corregida (etiqueta del pico bien colocada) ---------

fig1j <- ggplot(fig1j_data, aes(x = periodo, y = total)) +
  geom_line(linewidth = 1.1, color = "#1f77b4") +
  geom_point(size = 2.5, color = "#1f77b4") +
  
  # Anotación del pico 2015 (movida más abajo)
  geom_text(data = fig1j_data %>% filter(periodo == 2015),
            aes(label = paste0("Pico crisis 2015\n", scales::label_number(big.mark = ".")(total))),
            vjust = 1.3, hjust = -0.1, size = 3.2, color = "grey30") +
  
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     limits = c(0, 38000)) +
  
  labs(
    title = "Evolución de la emigración juvenil española (2008-2020)",
    subtitle = "Españoles emigrados al extranjero, 20-34 años, ambos sexos",
    x = NULL,
    y = "Número de personas",
    caption = "Fuente: elaboración propia a partir de INE (tabla 24300)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.minor = element_blank()
  )

print(fig1j)
ggsave("figuras/cap5/fig1_jovenes_evolucion.png",
       fig1j, width = 9, height = 5.5, dpi = 300, bg = "white")


# ---- Figura 1 jóvenes: corregida (etiqueta separada del punto) -------------

fig1j <- ggplot(fig1j_data, aes(x = periodo, y = total)) +
  geom_line(linewidth = 1.1, color = "#1f77b4") +
  geom_point(size = 2.5, color = "#1f77b4") +
  
  # Marcamos el punto del pico con un círculo distinto
  geom_point(data = fig1j_data %>% filter(periodo == 2015),
             color = "#1f77b4", size = 4, shape = 21, fill = "white", stroke = 1.5) +
  
  # Anotación del pico 2015 con flecha y caja
  annotate("segment", 
           x = 2017.5, xend = 2015.2,
           y = 33000, yend = 32183,
           color = "grey40", linewidth = 0.4,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  
  annotate("label",
           x = 2017.5, y = 33500,
           label = "Pico crisis (2015): 32.183",
           hjust = 0, size = 3.3, color = "grey20",
           label.size = 0.3, fill = "white") +
  
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     limits = c(0, 38000)) +
  
  labs(
    title = "Evolución de la emigración juvenil española (2008-2020)",
    subtitle = "Españoles emigrados al extranjero, 20-34 años, ambos sexos",
    x = NULL,
    y = "Número de personas",
    caption = "Fuente: elaboración propia a partir de INE (tabla 24300)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.minor = element_blank()
  )

print(fig1j)
ggsave("figuras/cap5/fig1_jovenes_evolucion.png",
       fig1j, width = 9, height = 5.5, dpi = 300, bg = "white")

# Verificar que el panel sigue en memoria
cat("=== ¿Panel em_jovenes existe? ===\n")
exists("em_jovenes")
cat("Filas:", nrow(em_jovenes), "\n")
cat("Periodo:", range(em_jovenes$periodo), "\n")


# =============================================================================
# APARTADO 5.4 — PERFIL DEMOGRÁFICO
# =============================================================================

# ---- Figura 6.1: emigración por sexo ----------------------------------------

# Datos: filtramos solo españoles, separamos hombre/mujer (NO ambos sexos)
sexo_anual <- em_jovenes %>% 
  filter(
    nacionalidad == "Española",
    sexo %in% c("Hombre", "Mujer"),
    pais_de_destino == "Total"
  ) %>% 
  arrange(periodo, sexo)

# Vemos los datos para verificar
print(sexo_anual)

# Ver qué valores tiene la columna sexo
unique(em_jovenes$sexo)


# ---- Figura 6.1: emigración por sexo (corregido) ---------------------------

sexo_anual <- em_jovenes %>% 
  filter(
    nacionalidad == "Española",
    sexo %in% c("Hombres", "Mujeres"),
    pais_de_destino == "Total"
  ) %>% 
  arrange(periodo, sexo)

# Verificamos
print(sexo_anual)

# Gráfico de líneas: hombres vs mujeres
fig6_1 <- ggplot(sexo_anual, 
                 aes(x = periodo, y = total, color = sexo)) +
  
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.3) +
  
  # Colores: rosa fuerte para mujeres, azul oscuro para hombres
  scale_color_manual(values = c("Hombres" = "#1f4e79", 
                                "Mujeres" = "#c71585")) +
  
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     limits = c(0, NA)) +
  
  labs(
    title = "Emigración juvenil española por sexo (2008-2020)",
    subtitle = "Españoles 20-34 años emigrados al extranjero",
    x = NULL,
    y = "Número de personas",
    color = "Sexo",
    caption = "Fuente: elaboración propia a partir de INE (tabla 24300)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.minor = element_blank()
  )

print(fig6_1)
ggsave("figuras/cap5/fig6_1_emigracion_sexo.png",
       fig6_1, width = 9, height = 5.5, dpi = 300, bg = "white")

cat("Figura 6.1 guardada\n")


# ---- Figura 6.2: emigración por subgrupo de edad ---------------------------

edad_anual <- em %>% 
  filter(
    nacionalidad == "Española",
    sexo == "Ambos sexos",
    pais_de_nacimiento == "Total",
    pais_destino == "Total",
    grupo_quinquenal_de_edad %in% c("De 20 a 24 años", 
                                    "De 25 a 29 años", 
                                    "De 30 a 34 años")
  ) %>% 
  select(grupo_quinquenal_de_edad, periodo, total) %>% 
  arrange(periodo, grupo_quinquenal_de_edad)

# Verificamos
print(edad_anual, n = 12)

# Gráfico
fig6_2 <- ggplot(edad_anual, 
                 aes(x = periodo, y = total, color = grupo_quinquenal_de_edad)) +
  
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.3) +
  
  scale_color_manual(values = c("De 20 a 24 años" = "#2E5984", 
                                "De 25 a 29 años" = "#5C8B5C",
                                "De 30 a 34 años" = "#C97D60")) +
  
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     limits = c(0, NA)) +
  
  labs(
    title = "Emigración juvenil española por subgrupo de edad (2008-2020)",
    subtitle = "Españoles emigrados al extranjero, ambos sexos",
    x = NULL,
    y = "Número de personas",
    color = "Grupo de edad",
    caption = "Fuente: elaboración propia a partir de INE (tabla 24300)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.minor = element_blank()
  )

print(fig6_2)
ggsave("figuras/cap5/fig6_2_emigracion_edad.png",
       fig6_2, width = 9, height = 5.5, dpi = 300, bg = "white")

cat("Figura 6.2 guardada\n")


# Corregir: añadir filtro de periodo <= 2020
edad_anual <- em %>% 
  filter(
    nacionalidad == "Española",
    sexo == "Ambos sexos",
    pais_de_nacimiento == "Total",
    pais_destino == "Total",
    grupo_quinquenal_de_edad %in% c("De 20 a 24 años", 
                                    "De 25 a 29 años", 
                                    "De 30 a 34 años"),
    periodo <= 2020   # AÑADIDO
  ) %>% 
  select(grupo_quinquenal_de_edad, periodo, total) %>% 
  arrange(periodo, grupo_quinquenal_de_edad)

# Volvemos a generar la figura (mismo código de antes)
fig6_2 <- ggplot(edad_anual, 
                 aes(x = periodo, y = total, color = grupo_quinquenal_de_edad)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.3) +
  scale_color_manual(values = c("De 20 a 24 años" = "#2E5984", 
                                "De 25 a 29 años" = "#5C8B5C",
                                "De 30 a 34 años" = "#C97D60")) +
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     limits = c(0, NA)) +
  labs(
    title = "Emigración juvenil española por subgrupo de edad (2008-2020)",
    subtitle = "Españoles emigrados al extranjero, ambos sexos",
    x = NULL,
    y = "Número de personas",
    color = "Grupo de edad",
    caption = "Fuente: elaboración propia a partir de INE (tabla 24300)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.minor = element_blank()
  )

print(fig6_2)
ggsave("figuras/cap5/fig6_2_emigracion_edad.png",
       fig6_2, width = 9, height = 5.5, dpi = 300, bg = "white")



# ---- Figura 6.3: edad × sexo en 2015 ----------------------------------------

edad_sexo_2015 <- em %>% 
  filter(
    nacionalidad == "Española",
    sexo %in% c("Hombres", "Mujeres"),
    pais_de_nacimiento == "Total",
    pais_destino == "Total",
    grupo_quinquenal_de_edad %in% c("De 20 a 24 años", 
                                    "De 25 a 29 años", 
                                    "De 30 a 34 años"),
    periodo == 2015
  ) %>% 
  select(grupo_quinquenal_de_edad, sexo, total)

print(edad_sexo_2015)

# Gráfico: barras agrupadas
fig6_3 <- ggplot(edad_sexo_2015, 
                 aes(x = grupo_quinquenal_de_edad, y = total, fill = sexo)) +
  
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  geom_text(aes(label = scales::label_number(big.mark = ".")(total)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.3, color = "grey20") +
  
  scale_fill_manual(values = c("Hombres" = "#1f4e79", 
                               "Mujeres" = "#c71585")) +
  
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title = "Emigración juvenil española por edad y sexo (2015)",
    subtitle = "Año del pico emigratorio. Españoles 20-34 años",
    x = NULL,
    y = "Número de personas",
    fill = "Sexo",
    caption = "Fuente: elaboración propia a partir de INE (tabla 24300)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(fig6_3)
ggsave("figuras/cap5/fig6_3_edad_sexo_2015.png",
       fig6_3, width = 9, height = 5.5, dpi = 300, bg = "white")

cat("Figura 6.3 guardada\n")



# ---- Figura 6.4: edad × sexo en 4 años clave (2008, 2013, 2017, 2020) ------

edad_sexo_4y <- em %>% 
  filter(
    nacionalidad == "Española",
    sexo %in% c("Hombres", "Mujeres"),
    pais_de_nacimiento == "Total",
    pais_destino == "Total",
    grupo_quinquenal_de_edad %in% c("De 20 a 24 años", 
                                    "De 25 a 29 años", 
                                    "De 30 a 34 años"),
    periodo %in% c(2008, 2013, 2017, 2020)
  ) %>% 
  select(grupo_quinquenal_de_edad, sexo, periodo, total)

# Verificación
print(edad_sexo_4y)

# Gráfico facetado por año
fig6_4 <- ggplot(edad_sexo_4y, 
                 aes(x = grupo_quinquenal_de_edad, y = total, fill = sexo)) +
  
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  geom_text(aes(label = scales::label_number(big.mark = ".")(total)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 2.7, color = "grey20") +
  
  facet_wrap(~ periodo, nrow = 1) +
  
  scale_fill_manual(values = c("Hombres" = "#1f4e79", 
                               "Mujeres" = "#c71585")) +
  
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.18))) +
  
  scale_x_discrete(labels = c("De 20 a 24 años" = "20-24",
                              "De 25 a 29 años" = "25-29",
                              "De 30 a 34 años" = "30-34")) +
  
  labs(
    title = "Emigración juvenil española por edad y sexo en cuatro momentos clave",
    subtitle = "Españoles 20-34 años emigrados al extranjero",
    x = "Grupo de edad",
    y = "Número de personas",
    fill = "Sexo",
    caption = "Fuente: elaboración propia a partir de INE (tabla 24300)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

print(fig6_4)
ggsave("figuras/cap5/fig6_4_edad_sexo_4anios.png",
       fig6_4, width = 12, height = 5.5, dpi = 300, bg = "white")

cat("Figura 6.4 guardada\n")


# ---- Figura 6.4: hombres vs mujeres 20-34 en 4 años clave ------------------

sexo_4y <- em_jovenes %>% 
  filter(
    nacionalidad == "Española",
    sexo %in% c("Hombres", "Mujeres"),
    pais_de_destino == "Total",
    periodo %in% c(2008, 2013, 2017, 2020)
  ) %>% 
  select(sexo, periodo, total)

print(sexo_4y)

# Gráfico de barras agrupadas
fig6_4 <- ggplot(sexo_4y, 
                 aes(x = factor(periodo), y = total, fill = sexo)) +
  
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  geom_text(aes(label = scales::label_number(big.mark = ".")(total)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.3, color = "grey20") +
  
  scale_fill_manual(values = c("Hombres" = "#1f4e79", 
                               "Mujeres" = "#c71585")) +
  
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title = "Emigración juvenil española por sexo en cuatro momentos clave",
    subtitle = "Españoles 20-34 años emigrados al extranjero",
    x = NULL,
    y = "Número de personas",
    fill = "Sexo",
    caption = "Fuente: elaboración propia a partir de INE (tabla 24300)"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(fig6_4)
ggsave("figuras/cap5/fig6_4_sexo_4anios.png",
       fig6_4, width = 9, height = 5.5, dpi = 300, bg = "white")

cat("Figura 6.4 guardada\n")





