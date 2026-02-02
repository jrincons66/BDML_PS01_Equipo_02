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
p_load(tidyverse, boot, stargazer, ggplot2, scales, gridExtra, knitr, kableExtra, rvest, httr)

# Crear carpetas
dir.create("output", showWarnings = FALSE)
dir.create("output/tables", showWarnings = FALSE)
dir.create("output/figures", showWarnings = FALSE)

# Tema para gráficos
theme_set(theme_minimal(base_size = 12))

# Colores
col_primary <- "#1E3A5F"
col_secondary <- "#C41E3A"
col_accent <- "#4A90A4"

# Verificar directorio de trabajo
if (!file.exists("01_code/00_rundirectory.R")) {
  stop("Error: Ejecutar desde la raíz del repositorio BDML-PS01/")
}

cat("Iniciando reproducción de resultados\n")
cat("Fecha:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

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
# PARTE 3: SECTION 2 - GENDER WAGE GAP
# ==============================================================================

cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("   SECTION 2: ESTADÍSTICAS DESCRIPTIVAS                          \n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

source("01_code/06_section2_descriptive_stats.R")

cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("   SECTION 2: ESTIMACIÓN BRECHA SALARIAL                         \n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

source("01_code/07_section2_wage_gap_models.R")

# ==============================================================================
# PARTE 4: SECTION 3 - INCOME PREDICTION
# ==============================================================================

cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  SECTION 3: ESTADÍSTICAS DESCRIPTIVAS                          \n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

source("01_code/08_section3_descriptive_stats.R")

cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("   SECTION 3: MODELOS DE PREDICCIÓN                              \n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

source("01_code/09_section3_prediction_models.R")



# ==============================================================================
#  FINAL
# ==============================================================================

