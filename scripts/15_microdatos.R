# =============================================================================
# APARTADO 5.6 — PERFIL EDUCATIVO (statistical matching)
# Validación con 3 años: 2008, 2013, 2015
# =============================================================================

library(tidyverse)
library(janitor)

# ---- 1. Cargar el fichero combinado -----------------------------------------

emigrantes_edu <- read_csv("datos_brutos/Emigrantes_TODOS_Con_Estudios.csv",
                           show_col_types = FALSE)

cat("=== Total filas ===\n")
nrow(emigrantes_edu)

cat("\n=== Filas por año ===\n")
table(emigrantes_edu$ANIO)

cat("\n=== Distribución de NFORMA_IMPUTADO ===\n")
table(emigrantes_edu$NFORMA_IMPUTADO, useNA = "ifany")



# ---- 2. Filtrar a españoles 25-34 -------------------------------------------

espanoles_jovenes_edu <- emigrantes_edu %>% 
  filter(
    CNAC == 108,           # 108 = Española
    EDAD >= 25,            # mínimo 25 años
    EDAD <= 34,            # máximo 34 años
    !is.na(NFORMA_IMPUTADO),  # quitamos los que no tienen estudios imputados
    NFORMA_IMPUTADO != "  "    # quitamos los que tenían "  " como código
  )

cat("=== Españoles 25-34 con estudios imputados ===\n")
cat("Total filas:", nrow(espanoles_jovenes_edu), "\n\n")

cat("=== Por año ===\n")
table(espanoles_jovenes_edu$ANIO)

cat("\n=== Distribución de estudios ===\n")
table(espanoles_jovenes_edu$NFORMA_IMPUTADO)


# ---- 3. Comprobación: % de universitarios por año ---------------------------

resumen_universitarios <- espanoles_jovenes_edu %>% 
  group_by(ANIO) %>% 
  summarise(
    total = n(),
    universitarios = sum(NFORMA_IMPUTADO == "SU"),
    pct_universitarios = round(universitarios / total * 100, 1)
  )

print(resumen_universitarios)



# ---- 4. Traducir códigos de país de destino ---------------------------------

espanoles_jovenes_edu <- espanoles_jovenes_edu %>% 
  mutate(
    pais_destino = case_when(
      MUNIALTA == 110 ~ "Francia",
      MUNIALTA == 125 ~ "Reino Unido",
      MUNIALTA == 126 ~ "Alemania",
      MUNIALTA == 127 ~ "Italia",
      MUNIALTA == 132 ~ "Suiza",
      MUNIALTA == 109 ~ "Portugal",
      MUNIALTA == 121 ~ "Países Bajos",
      MUNIALTA == 105 ~ "Bélgica",
      MUNIALTA == 130 ~ "Irlanda",
      MUNIALTA == 302 ~ "Estados Unidos",
      MUNIALTA == 305 ~ "Canadá",
      MUNIALTA == 340 ~ "Argentina",
      MUNIALTA == 332 ~ "Ecuador",
      MUNIALTA == 333 ~ "Colombia",
      MUNIALTA == 343 ~ "Chile",
      MUNIALTA == 344 ~ "Venezuela",
      MUNIALTA == 320 ~ "México",
      MUNIALTA == 322 ~ "República Dominicana",
      MUNIALTA == 425 ~ "Marruecos",
      MUNIALTA == 718 ~ "China",
      MUNIALTA == 750 ~ "Japón",
      MUNIALTA == 805 ~ "Australia",
      TRUE ~ NA_character_
    )
  )

cat("=== Españoles 25-34 por país de destino ===\n")
espanoles_jovenes_edu %>% 
  filter(!is.na(pais_destino)) %>% 
  count(pais_destino, sort = TRUE) %>% 
  print(n = 25)


# Ver los códigos de país de destino más frecuentes
cat("=== Top 30 códigos de MUNIALTA (país de destino) ===\n")
espanoles_jovenes_edu %>% 
  count(MUNIALTA, sort = TRUE) %>% 
  print(n = 30)

