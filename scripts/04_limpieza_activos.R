# =============================================================================
# Script: 04_limpieza_activos.R
# Objetivo: Limpiar el fichero de activos y generar un dataset limpio
#           con activos 20-34 y 35-54 por CCAA y año
# NOTA: activos viene en miles de personas (coherente con parados)
# =============================================================================

# ---- 1. Cargar paquetes -----------------------------------------------------
library(tidyverse)
library(janitor)

# ---- 2. Cargar fichero -------------------------------------------------------
activos <- read_delim(
  file = "datos_brutos/05_activos.csv",
  delim = ";",
  locale = locale(
    decimal_mark = ",",
    grouping_mark = ".",
    encoding = "latin1"
  )
) %>%
  clean_names()

# Comprobar estructura
names(activos)
unique(activos$edad)
unique(activos$periodo)


# ---- 3. Limpiar, agregar, pivotar y guardar ---------------------------------

activos_limpia <- activos %>%
  # Quitar Total Nacional
  filter(comunidades_y_ciudades_autonomas != "Total Nacional") %>%
  # Quitar número de delante
  mutate(
    ccaa = str_remove(comunidades_y_ciudades_autonomas, "^\\d+ ")
  ) %>%
  # Filtrar años 2008-2024
  filter(periodo >= 2008 & periodo <= 2024) %>%
  # Renombrar
  rename(
    año = periodo,
    activos = total
  ) %>%
  # Crear grupo joven/adulto
  mutate(
    grupo = case_when(
      edad %in% c("De 20 a 24 años", "De 25 a 34 años") ~ "joven",
      edad %in% c("De 35 a 44 años", "De 45 a 54 años") ~ "adulto",
      TRUE ~ NA_character_
    )
  ) %>%
  # Agregar por grupo
  group_by(ccaa, año, grupo) %>%
  summarise(
    activos = sum(activos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Pivotar a ancho: columnas activos_joven, activos_adulto
  pivot_wider(
    names_from = grupo,
    values_from = activos,
    names_prefix = "activos_"
  ) %>%
  arrange(ccaa, año)

head(activos_limpia)
nrow(activos_limpia)

# Guardar
write_csv2(activos_limpia, "datos_procesados/activos_limpia.csv")
cat("Fichero guardado en datos_procesados/activos_limpia.csv\n")