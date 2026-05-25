# =============================================================================
# Script: 03_limpieza_parados.R
# Objetivo: Limpiar el fichero de parados y generar un dataset limpio
#           con parados 20-34 y 35-54 por CCAA y año (media anual de 4 trimestres)
# =============================================================================

# ---- 1. Cargar paquetes -----------------------------------------------------
library(tidyverse)
library(janitor)

# ---- 2. Cargar fichero -------------------------------------------------------
parados <- read_delim(
  file = "datos_brutos/04_parados.csv",
  delim = ";",
  locale = locale(
    decimal_mark = ",",
    grouping_mark = ".",
    encoding = "latin1"
  )
) %>%
  clean_names()

# Ver estructura
names(parados)
head(parados)


# ---- 3. Limpiar y filtrar ---------------------------------------------------

parados <- parados %>%
  # Quitar Total Nacional
  filter(comunidades_y_ciudades_autonomas != "Total Nacional") %>%
  # Quitar el número de delante del nombre de la CCAA
  mutate(
    ccaa = str_remove(comunidades_y_ciudades_autonomas, "^\\d+ ")
  ) %>%
  # Filtrar solo los años 2008-2024
  filter(periodo >= 2008 & periodo <= 2024) %>%
  # Renombrar
  rename(
    año = periodo,
    parados = total
  ) %>%
  # Crear grupo joven/adulto
  mutate(
    grupo = case_when(
      edad %in% c("De 20 a 24 años", "De 25 a 34 años") ~ "joven",
      edad %in% c("De 35 a 44 años", "De 45 a 54 años") ~ "adulto",
      TRUE ~ NA_character_
    )
  ) %>%
  # Seleccionar columnas
  select(ccaa, año, grupo, parados)

head(parados)
unique(parados$grupo)


# ---- 4. Agregar por grupo de edad -------------------------------------------

parados <- parados %>%
  group_by(ccaa, año, grupo) %>%
  summarise(
    parados = sum(parados, na.rm = TRUE),
    .groups = "drop"
  )

head(parados)
nrow(parados)


# ---- 5. Pivotar a formato ancho y guardar -----------------------------------

parados_limpia <- parados %>%
  pivot_wider(
    names_from = grupo,
    values_from = parados,
    names_prefix = "parados_"
  ) %>%
  arrange(ccaa, año)

head(parados_limpia)
nrow(parados_limpia)

# Guardar
write_csv2(parados_limpia, "datos_procesados/parados_limpia.csv")
cat("Fichero guardado en datos_procesados/parados_limpia.csv\n")

