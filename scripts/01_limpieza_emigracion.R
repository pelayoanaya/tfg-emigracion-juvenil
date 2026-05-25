# =============================================================================
# Script: 01_limpieza_emigracion.R
# Objetivo: Limpiar los ficheros de emigración (2008-2021 y 2021-2024)
#           y generar un dataset limpio con emigración 20-34 por CCAA y año
# =============================================================================

# ---- 1. Cargar paquetes -----------------------------------------------------
library(tidyverse)  # manejo de datos (filter, mutate, group_by, etc.)
library(janitor)    # limpieza de nombres de columnas

# ---- 2. Cargar fichero de emigración 2008-2021 ------------------------------

# read_delim permite especificar separador, decimales y codificación
emig_0821 <- read_delim(
  file = "datos_brutos/02_emig_2008_2021.csv",
  delim = ";",                      # separador punto y coma
  locale = locale(
    decimal_mark = ",",             # coma decimal (estándar español)
    grouping_mark = ".",            # punto como separador de miles
    encoding = "latin1"             # codificación para leer eñes y acentos bien
  )
)

# Ver las primeras filas del dataframe recién cargado
head(emig_0821)


# ---- 3. Limpiar nombres de columnas -----------------------------------------
emig_0821 <- emig_0821 %>%
  clean_names()

# Ver cómo han quedado los nombres
names(emig_0821)

# ---- 4. Limpiar y filtrar ---------------------------------------------------

emig_0821 <- emig_0821 %>%
  # Quitar Total Nacional (solo queremos CCAA, no agregado)
  filter(comunidades_y_ciudades_autonomas != "Total Nacional") %>%
  # Quitar el número de delante del nombre de la CCAA
  mutate(
    ccaa = str_remove(comunidades_y_ciudades_autonomas, "^\\d+ ")
  ) %>%
  # Renombrar columnas para que sean más claras
  rename(
    año = periodo,
    edad = grupo_quinquenal_de_edad,
    emigrantes = total
  ) %>%
  # Seleccionar solo las columnas que vamos a usar
  select(ccaa, año, edad, emigrantes)

# Comprobar el resultado
head(emig_0821)

# ---- 5. Agregar los tres grupos de edad en uno solo (20-34) -----------------

emig_0821 <- emig_0821 %>%
  group_by(ccaa, año) %>%
  summarise(
    emig_2034 = sum(emigrantes, na.rm = TRUE),
    .groups = "drop"
  )

# Comprobar el resultado
head(emig_0821)

# Comprobar el número de filas: debería ser 19 CCAA × 13 años = 247
nrow(emig_0821)


# ---- 6. Cargar fichero de emigración 2021-2024 (serie EMCR nueva) -----------

emig_2124 <- read_delim(
  file = "datos_brutos/01_emig_2021_2024.csv",
  delim = ";",
  locale = locale(
    decimal_mark = ",",
    grouping_mark = ".",
    encoding = "latin1"
  )
) %>%
  clean_names()

# Ver nombres de columnas y primeras filas
names(emig_2124)
head(emig_2124)


# ---- 7. Limpiar el fichero 2021-2024 ----------------------------------------

emig_2124 <- emig_2124 %>%
  # Quedarnos solo con las filas que tienen dato a nivel CCAA
  filter(!is.na(comunidades_y_ciudades_autonomas)) %>%
  # Quitar el número de delante del nombre de la CCAA
  mutate(
    ccaa = str_remove(comunidades_y_ciudades_autonomas, "^\\d+ ")
  ) %>%
  # Renombrar columnas
  rename(
    año = periodo,
    emigrantes = total
  ) %>%
  # Seleccionar solo las columnas que vamos a usar
  select(ccaa, año, edad, emigrantes)

# Comprobar
head(emig_2124)
unique(emig_2124$año)

# ---- 8. Agregar los tres grupos de edad (20-34) en el fichero 2021-2024 -----

emig_2124 <- emig_2124 %>%
  group_by(ccaa, año) %>%
  summarise(
    emig_2034 = sum(emigrantes, na.rm = TRUE),
    .groups = "drop"
  )

# Comprobar
head(emig_2124)
nrow(emig_2124)

# ---- 9. Unir ambos ficheros y guardar ---------------------------------------

# Unir: 2008-2020 (247 filas) + 2021-2024 (76 filas) = 323 filas
emigracion_limpia <- bind_rows(emig_0821, emig_2124) %>%
  arrange(ccaa, año)

# Comprobar
nrow(emigracion_limpia)
head(emigracion_limpia, 20)

# Guardar en datos_procesados/
write_csv2(emigracion_limpia, "datos_procesados/emigracion_limpia.csv")

cat("Fichero guardado correctamente en datos_procesados/emigracion_limpia.csv\n")
