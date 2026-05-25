# =============================================================================
# Script: 05_limpieza_ipv.R
# Objetivo: Limpiar el fichero de IPV (Índice de Precios de Vivienda) y 
#           generar un dataset con IPV anual por CCAA (media de 4 trimestres)
# =============================================================================

# ---- 1. Cargar paquetes -----------------------------------------------------
library(tidyverse)
library(janitor)

# ---- 2. Cargar fichero -------------------------------------------------------
ipv <- read_delim(
  file = "datos_brutos/07_ipv.csv",
  delim = ";",
  locale = locale(
    decimal_mark = ",",
    grouping_mark = ".",
    encoding = "latin1"
  )
) %>%
  clean_names()

# Explorar
names(ipv)
head(ipv)


# ---- 3. Limpiar, extraer año, agregar y guardar -----------------------------

ipv_limpia <- ipv %>%
  # Quedarse solo con filas a nivel CCAA
  filter(!is.na(comunidades_y_ciudades_autonomas)) %>%
  # Quitar número de delante
  mutate(
    ccaa = str_remove(comunidades_y_ciudades_autonomas, "^\\d+ ")
  ) %>%
  # Extraer el año del periodo ("2024T3" -> 2024)
  mutate(
    año = as.integer(str_extract(periodo, "^\\d{4}"))
  ) %>%
  # Filtrar años 2008-2024
  filter(año >= 2008 & año <= 2024) %>%
  # Renombrar valor
  rename(indice = total) %>%
  # Agregar a anual: media de los 4 trimestres por CCAA-año
  group_by(ccaa, año) %>%
  summarise(
    ipv = mean(indice, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ccaa, año)

head(ipv_limpia)
nrow(ipv_limpia)

# Guardar
write_csv2(ipv_limpia, "datos_procesados/ipv_limpia.csv")
cat("Fichero guardado en datos_procesados/ipv_limpia.csv\n")