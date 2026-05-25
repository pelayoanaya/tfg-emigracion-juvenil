install.packages("fixest")
# =============================================================================
# Script: 11_modelo.R
# Objetivo: Estimar modelos econométricos del panel CCAA × año
#           para explicar la tasa de emigración juvenil
# =============================================================================

# ---- 1. Cargar paquetes -----------------------------------------------------
library(tidyverse)
library(fixest)          # modelos con efectos fijos (TWFE)
library(modelsummary)    # tablas académicas de regresión

# ---- 2. Cargar panel --------------------------------------------------------
panel <- read_csv2("datos_procesados/panel_tfg.csv")

# Convertir ccaa a factor y año a entero
panel <- panel %>%
  mutate(
    ccaa = as.factor(ccaa),
    año  = as.integer(año)
  )

# Comprobar
glimpse(panel)
cat("\nPanel:", nrow(panel), "filas,", ncol(panel), "columnas\n")

# ---- 3. Modelo 1: OLS pooled (sin efectos fijos) ----------------------------

modelo_1 <- feols(
  tasa_emig_2034 ~ paro_joven + ratio_paro_jov_adult + 
    pib_crec + ipv + salario_real,
  data = panel,
  se = "iid"    # errores estándar clásicos
)

summary(modelo_1)


# ---- 4. Modelo 2: Efectos fijos de CCAA -------------------------------------

modelo_2 <- feols(
  tasa_emig_2034 ~ paro_joven + ratio_paro_jov_adult + 
    pib_crec + ipv + salario_real | ccaa,
  data = panel,
  cluster = ~ ccaa   # errores estándar clusterizados por CCAA
)

summary(modelo_2)

# ---- 5. Modelo 3: Efectos fijos de año --------------------------------------

modelo_3 <- feols(
  tasa_emig_2034 ~ paro_joven + ratio_paro_jov_adult + 
    pib_crec + ipv + salario_real | año,
  data = panel,
  cluster = ~ ccaa
)

summary(modelo_3)


# ---- 6. Modelo 4: TWFE (efectos fijos CCAA + año) --------------------------

modelo_4 <- feols(
  tasa_emig_2034 ~ paro_joven + ratio_paro_jov_adult + 
    pib_crec + ipv + salario_real | ccaa + año,
  data = panel,
  cluster = ~ ccaa
)

summary(modelo_4)


# ---- 7. Modelo 5: TWFE con interacciones post-2014 --------------------------

modelo_5 <- feols(
  tasa_emig_2034 ~ paro_joven * post2014 + 
    ratio_paro_jov_adult * post2014 + 
    pib_crec * post2014 + 
    ipv * post2014 + 
    salario_real * post2014 | ccaa + año,
  data = panel,
  cluster = ~ ccaa
)

summary(modelo_5)


# ---- 8. Tabla conjunta de los 5 modelos ------------------------------------

# Lista con los 5 modelos
modelos_lista <- list(
  "(1) OLS pooled"       = modelo_1,
  "(2) FE CCAA"          = modelo_2,
  "(3) FE año"           = modelo_3,
  "(4) TWFE"             = modelo_4,
  "(5) TWFE × post2014"  = modelo_5
)

# Mapear nombres técnicos a nombres bonitos para la tabla
coef_map <- c(
  "paro_joven"                    = "Paro juvenil (%)",
  "ratio_paro_jov_adult"          = "Ratio paro jov/adulto",
  "pib_crec"                      = "Crecimiento PIB (%)",
  "ipv"                           = "IPV (índice)",
  "salario_real"                  = "Salario real (€)",
  "paro_joven:post2014"           = "Paro juv. × post2014",
  "post2014:ratio_paro_jov_adult" = "Ratio × post2014",
  "post2014:pib_crec"             = "PIB × post2014",
  "post2014:ipv"                  = "IPV × post2014",
  "post2014:salario_real"         = "Salario real × post2014",
  "(Intercept)"                   = "Constante"
)

# Tabla en formato markdown (se ve bien en consola y se puede copiar a Word)
tabla_modelos <- modelsummary(
  modelos_lista,
  output = "markdown",
  coef_map = coef_map,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title = "Tabla 1. Determinantes de la tasa de emigración juvenil en España (2008-2024)",
  notes = "Errores estándar clusterizados por CCAA entre paréntesis. * p<0.1, ** p<0.05, *** p<0.01."
)

print(tabla_modelos)

# Guardar también como HTML (por si quieres abrirla en el navegador)
modelsummary(
  modelos_lista,
  output = "figuras/tabla_modelos.html",
  coef_map = coef_map,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title = "Tabla 1. Determinantes de la tasa de emigración juvenil en España (2008-2024)"
)

cat("\nTabla guardada en figuras/tabla_modelos.html\n")


# =============================================================================
# ROBUSTEZ
# =============================================================================

# ---- 9. Robustez 1: excluir Ceuta y Melilla ---------------------------------

modelo_4_sin_cm <- feols(
  tasa_emig_2034 ~ paro_joven + ratio_paro_jov_adult + 
    pib_crec + ipv + salario_real | ccaa + año,
  data = panel %>% filter(!ccaa %in% c("Ceuta", "Melilla")),
  cluster = ~ ccaa
)

summary(modelo_4_sin_cm)

cat("\nObservaciones modelo original:", nobs(modelo_4), "\n")
cat("Observaciones sin CM:", nobs(modelo_4_sin_cm), "\n")

# ---- 10. Robustez 2: añadir dummy COVID (año 2020) --------------------------

panel <- panel %>%
  mutate(covid_2020 = if_else(año == 2020, 1L, 0L))

modelo_4_covid <- feols(
  tasa_emig_2034 ~ paro_joven + ratio_paro_jov_adult + 
    pib_crec + ipv + salario_real + covid_2020 | ccaa + año,
  data = panel,
  cluster = ~ ccaa
)

summary(modelo_4_covid)


# ---- 11. Robustez 3: paro total (20-54) en vez de paro juvenil --------------

panel <- panel %>%
  mutate(
    parados_total  = parados_joven + parados_adulto,
    activos_total  = activos_joven + activos_adulto,
    paro_total     = (parados_total / activos_total) * 100
  )

modelo_4_parototal <- feols(
  tasa_emig_2034 ~ paro_total + ratio_paro_jov_adult + 
    pib_crec + ipv + salario_real | ccaa + año,
  data = panel,
  cluster = ~ ccaa
)

summary(modelo_4_parototal)

# ---- 12. Tabla de robustez --------------------------------------------------

modelos_robustez <- list(
  "(4) TWFE\nprincipal"         = modelo_4,
  "(R1) Sin\nCeuta/Melilla"     = modelo_4_sin_cm,
  "(R2) Con dummy\nCOVID"       = modelo_4_covid,
  "(R3) Paro total\n(20-54)"    = modelo_4_parototal
)

coef_map_robustez <- c(
  "paro_joven"           = "Paro juvenil (%)",
  "paro_total"           = "Paro total 20-54 (%)",
  "ratio_paro_jov_adult" = "Ratio paro jov/adulto",
  "pib_crec"             = "Crecimiento PIB (%)",
  "ipv"                  = "IPV (índice)",
  "salario_real"         = "Salario real (€)"
)

# Tabla en consola
tabla_robustez <- modelsummary(
  modelos_robustez,
  output = "markdown",
  coef_map = coef_map_robustez,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title = "Tabla 2. Tests de robustez del modelo TWFE",
  notes = "Todas las columnas usan efectos fijos de CCAA y año. Errores estándar clusterizados por CCAA entre paréntesis. * p<0.1, ** p<0.05, *** p<0.01."
)

print(tabla_robustez)

# Guardar también en HTML
modelsummary(
  modelos_robustez,
  output = "figuras/tabla_robustez.html",
  coef_map = coef_map_robustez,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title = "Tabla 2. Tests de robustez del modelo TWFE"
)

cat("\nTabla guardada en figuras/tabla_robustez.html\n")

