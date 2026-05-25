# =============================================================================
# Script: 07_limpieza_ipc.R
# Objetivo: Limpiar el fichero de IPC y generar un dataset con IPC anual
#           por CCAA (media de 12 meses) para deflactar el salario
# =============================================================================

# ---- 1. Cargar paquetes -----------------------------------------------------
library(tidyverse)
library(janitor)

# ---- 2. Cargar fichero -------------------------------------------------------
ipc <- read_delim(
  file = "datos_brutos/09_ipc.csv",
  delim = ";",
  locale = locale(
    decimal_mark = ",",
    grouping_mark = ".",
    encoding = "latin1"
  )
) %>%
  clean_names()

# Explorar estructura
names(ipc)
head(ipc)


# ---- 3. Limpiar, extraer año, agregar y guardar -----------------------------

ipc_limpia <- ipc %>%
  # Quedarse solo con las filas a nivel CCAA (quitar Nacional)
  filter(comunidades_y_ciudades_autonomas != "Nacional") %>%
  # Quitar número de delante
  mutate(
    ccaa = str_remove(comunidades_y_ciudades_autonomas, "^\\d+ ")
  ) %>%
  # Extraer el año del periodo ("2024M12" -> 2024)
  mutate(
    año = as.integer(str_extract(periodo, "^\\d{4}"))
  ) %>%
  # Filtrar años 2008-2024
  filter(año >= 2008 & año <= 2024) %>%
  # Renombrar valor
  rename(indice = total) %>%
  # Agregar a anual: media de los 12 meses por CCAA-año
  group_by(ccaa, año) %>%
  summarise(
    ipc = mean(indice, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ccaa, año)

head(ipc_limpia)
nrow(ipc_limpia)

# Guardar
write_csv2(ipc_limpia, "datos_procesados/ipc_limpia.csv")
cat("Fichero guardado en datos_procesados/ipc_limpia.csv\n")