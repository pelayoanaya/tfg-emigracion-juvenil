# =============================================================================
# Script: 02_limpieza_poblacion.R
# Objetivo: Limpiar el fichero de población y generar un dataset limpio
#           con población 20-34 española por CCAA y año a 1 de enero
# =============================================================================

# ---- 1. Cargar paquetes -----------------------------------------------------
library(tidyverse)
library(janitor)

# ---- 2. Cargar fichero -------------------------------------------------------
poblacion <- read_delim(
  file = "datos_brutos/03_poblacion.csv",
  delim = ";",
  locale = locale(
    decimal_mark = ",",
    grouping_mark = ".",
    encoding = "latin1"
  )
) %>%
  clean_names()

# Ver nombres y primeras filas
names(poblacion)
head(poblacion)


# ---- 3. Limpiar y filtrar ---------------------------------------------------

poblacion <- poblacion %>%
  # Quitar Total Nacional
  filter(comunidades_y_ciudades_autonomas != "Total Nacional") %>%
  # Quedarnos solo con fechas de 1 de enero
  filter(str_starts(periodo, "1 de enero")) %>%
  # Extraer el año de la cadena "1 de enero de 2008" -> 2008
  mutate(
    año = as.integer(str_extract(periodo, "\\d{4}")),
    ccaa = str_remove(comunidades_y_ciudades_autonomas, "^\\d+ ")
  ) %>%
  # Filtrar solo los años que nos interesan (2008-2024)
  filter(año >= 2008 & año <= 2024) %>%
  # Renombrar columna de edad
  rename(
    edad = grupo_quinquenal_de_edad,
    habitantes = total
  ) %>%
  # Seleccionar solo las columnas que vamos a usar
  select(ccaa, año, edad, habitantes)

# Comprobar
head(poblacion)
unique(poblacion$año)


# ---- 4. Agregar las tres edades en 20-34 y guardar --------------------------

poblacion_limpia <- poblacion %>%
  group_by(ccaa, año) %>%
  summarise(
    pob_2034 = sum(habitantes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ccaa, año)

# Comprobar
head(poblacion_limpia)
nrow(poblacion_limpia)

# Guardar en datos_procesados/
write_csv2(poblacion_limpia, "datos_procesados/poblacion_limpia.csv")

cat("Fichero guardado en datos_procesados/poblacion_limpia.csv\n")
