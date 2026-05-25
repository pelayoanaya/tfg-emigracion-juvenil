# =============================================================================
# Script: 08_merge_panel.R
# Objetivo: Unir los 8 ficheros limpios en un panel único CCAA × año
#           y construir las variables derivadas del modelo
# =============================================================================

# ---- 1. Cargar paquetes -----------------------------------------------------
library(tidyverse)

# ---- 2. Cargar los 8 CSVs limpios -------------------------------------------
emigracion <- read_csv2("datos_procesados/emigracion_limpia.csv")
poblacion  <- read_csv2("datos_procesados/poblacion_limpia.csv")
parados    <- read_csv2("datos_procesados/parados_limpia.csv")
activos    <- read_csv2("datos_procesados/activos_limpia.csv")
pib        <- read_csv2("datos_brutos/06_pib.csv")
ipv        <- read_csv2("datos_procesados/ipv_limpia.csv")
salario    <- read_csv2("datos_procesados/salario_limpia.csv")
ipc        <- read_csv2("datos_procesados/ipc_limpia.csv")

# ---- 3. Arreglar el PIB (nombres en mayúsculas, pib_crec como texto) --------
pib <- pib %>%
  mutate(
    ccaa = str_to_title(ccaa),
    ccaa = case_when(
      ccaa == "Castilla Y León"             ~ "Castilla y León",
      ccaa == "Madrid, Comunidad De"        ~ "Madrid, Comunidad de",
      ccaa == "Murcia, Región De"           ~ "Murcia, Región de",
      ccaa == "Navarra, Comunidad Foral De" ~ "Navarra, Comunidad Foral de",
      ccaa == "Asturias, Principado De"     ~ "Asturias, Principado de",
      TRUE                                  ~ ccaa
    ),
    pib_crec = as.numeric(pib_crec)
  )

# ---- 4. Merge de los 8 ficheros ---------------------------------------------
panel <- emigracion %>%
  left_join(poblacion, by = c("ccaa", "año")) %>%
  left_join(parados,   by = c("ccaa", "año")) %>%
  left_join(activos,   by = c("ccaa", "año")) %>%
  left_join(pib,       by = c("ccaa", "año")) %>%
  left_join(ipv,       by = c("ccaa", "año")) %>%
  left_join(salario,   by = c("ccaa", "año")) %>%
  left_join(ipc,       by = c("ccaa", "año"))

# ---- 5. Construir variables derivadas ---------------------------------------
panel <- panel %>%
  mutate(
    # Variable dependiente: tasa de emigración juvenil por mil habitantes
    tasa_emig_2034 = (emig_2034 / pob_2034) * 1000,
    
    # Tasas de paro (parados y activos en miles, la unidad se cancela)
    paro_joven  = (parados_joven  / activos_joven ) * 100,
    paro_adulto = (parados_adulto / activos_adulto) * 100,
    
    # Ratio paro joven / paro adulto
    ratio_paro_jov_adult = paro_joven / paro_adulto,
    
    # Salario real (deflactado por IPC)
    salario_real = salario / (ipc / 100),
    
    # Dummies temporales
    post2013 = if_else(año >= 2013, 1L, 0L),
    post2014 = if_else(año >= 2014, 1L, 0L),
    post2021 = if_else(año >= 2021, 1L, 0L)
  )

# ---- 6. Verificación --------------------------------------------------------
cat("Dimensiones del panel:", nrow(panel), "filas,", ncol(panel), "columnas\n\n")
cat("Nombres de columnas:\n")
print(names(panel))
cat("\nValores faltantes por columna:\n")
print(sapply(panel, function(x) sum(is.na(x))))

# ---- 7. Guardar -------------------------------------------------------------
write_csv2(panel, "datos_procesados/panel_tfg.csv")
cat("\nPanel guardado en datos_procesados/panel_tfg.csv\n")