# =============================================================================
# Script: 06_limpieza_salario.R
# Objetivo: Limpiar el fichero de salarios y generar un dataset con salario
#           mediano por CCAA y año
# =============================================================================

# ---- 1. Cargar paquetes -----------------------------------------------------
library(tidyverse)
library(janitor)

# ---- 2. Cargar fichero -------------------------------------------------------
salario <- read_delim(
  file = "datos_brutos/08_salario.csv",
  delim = ";",
  locale = locale(
    decimal_mark = ",",
    grouping_mark = ".",
    encoding = "latin1"
  )
) %>%
  clean_names()

# Explorar
names(salario)
head(salario)

# ---- 3. Limpiar, filtrar y guardar ------------------------------------------

salario_limpia <- salario %>%
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
    salario = total
  ) %>%
  # Seleccionar columnas finales
  select(ccaa, año, salario) %>%
  arrange(ccaa, año)

head(salario_limpia)
nrow(salario_limpia)

# Guardar
write_csv2(salario_limpia, "datos_procesados/salario_limpia.csv")
cat("Fichero guardado en datos_procesados/salario_limpia.csv\n")