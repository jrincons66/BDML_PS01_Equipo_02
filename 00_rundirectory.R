################################################################################
##                                                                            ##
##                      Problem Set 1                                         ##
##                    GEIH 2018 - Bogotá                                      ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##          Integrantes:                                                      ##
##          - Jose Alejandro Rincon                                           ## 
##          - Juan Camilo Riano                                               ##
##          - Lucas Rodriguez                                                 ##
##                                                                            ##
################################################################################

## Descripción:
##   Este es el script maestro que reproduce todos los resultados del 
##   Problem Set 1. Ejecutar este único archivo genera todas las figuras,
##   tablas y outputs del proyecto para que se pueda replicar.

################################################################################

# ==============================================================================
# CONFIGURACIÓN INICIAL
# ==============================================================================

rm(list = ls())
set.seed(6666)

# Paquetes
require(pacman)
p_load(tidyverse, boot, stargazer, ggplot2, scales, gridExtra, knitr, kableExtra, rvest, httr,here,modelsummary)

# Tema para gráficos
theme_set(theme_minimal(base_size = 12))


# Creación de carpetas para cada punto

setwd(here())
dir.create("01_Section_1_Age_Labor_Income_Profile", showWarnings = FALSE)
dir.create("02_Section_2_Gender_Labor_Income_Gap", showWarnings = FALSE)
dir.create("03_Section_3_Predicting_Income", showWarnings = FALSE)

# Colores
col_primary <- "#1E3A5F"
col_secondary <- "#C41E3A"
col_accent <- "#4A90A4"

cat("Iniciando reproducción de resultados\n")
cat("Fecha:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ==============================================================================
# EJERCICIO 1
# ==============================================================================

# Working directory

here()
setwd(here("01_Section_1_Age_Labor_Income_Profile"))

# Crear carpetas
dir.create("01_code", showWarnings = FALSE)
dir.create("02_output", showWarnings = FALSE)
dir.create("02_output/tables", showWarnings = FALSE)
dir.create("02_output/figures", showWarnings = FALSE)

# ==============================================================================
# PARTE 1: CARGA Y PREPARACIÓN DE DATOS (PARA TODAS LAS SECCIONES)
# ==============================================================================

cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("   WEB SCRAPING - GEIH 2018                                      \n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

source("01_code/01_data_scraping.R")

cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  LIMPIEZA Y CONSTRUCCIÓN DE MUESTRA                            \n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

source("01_code/02_data_cleaning_workers.R")

# ==============================================================================
# PARTE 2: SECTION 1 - AGE-LABOR INCOME PROFILE
# ==============================================================================

cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  SECTION 1: ESTADÍSTICAS DESCRIPTIVAS                          \n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

source("01_code/03_age_analysis_descriptive_stats.R")

cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  SECTION 1: ESTIMACIÓN PERFIL EDAD-INGRESO                     \n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

source("01_code/04_age_income_models.R")

cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("   SECTION 1: VISUALIACIÓN DE LOS MODELOS                  \n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

source("01_code/05_age_income_models_visualization.R")

cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  SECTION 1: RESUMEN DE RESULTADOS                 \n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

source("01_code/06_age_model_results_sum.R")

# ==============================================================================
#  FINAL PUNTO 1
# ==============================================================================


# ==============================================================================
#  EJERCICIO 2
# ==============================================================================

# Working directory

here()
setwd(here("02_Section_2_Gender_Labor_Income_Gap"))

# Crear carpetas
dir.create("01_code", showWarnings = FALSE)
dir.create("02_output", showWarnings = FALSE)
dir.create("02_output/tables", showWarnings = FALSE)
dir.create("02_output/figures", showWarnings = FALSE)

# ==============================================================================
# PARTE 1: SECTION 2 - UNCONDITIONAL MODEL
# ==============================================================================

source("01_code/01_s2_Descriptive_Statistics.R")

# ==============================================================================
# PARTE 2: SECTION 2 - UNCONDITIONAL MODEL
# ==============================================================================

source("01_code/02_s2_Unconditional_Model.R")

# ==============================================================================
# PARTE 3: SECTION 2 - MODELO CONDICIONAL POR FWL
# ==============================================================================

source("01_code/03_s2_FWL_Model.R")

# ==============================================================================
# PARTE 4: SECTION 2 - Bootstrap de errores estandar
# ==============================================================================

source("01_code/04_s2_FWL_SE_Bootstrap.R")

# ==============================================================================
# PARTE 5: SECTION 2 - Presentación de resultados
# ==============================================================================

source("01_code/05_s2_FWL_Presentacion_Resultados.R")

# ==============================================================================
# PARTE 6: SECTION 2 - Gráfico y diference de edades pico
# ==============================================================================

source("01_code/06_s2_Diferencia_Edades.R")
