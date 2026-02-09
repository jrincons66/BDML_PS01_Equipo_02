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
##   Problem Set 2. Ejecutar este único archivo genera todas las figuras,
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
dir.create("02_Section_2_Gender_Labor_Income_Gap/02_output", showWarnings = FALSE)
dir.create("02_Section_2_Gender_Labor_Income_Gap/02_output/tables", showWarnings = FALSE)
dir.create("02_Section_2_Gender_Labor_Income_Gap/02_output/figures", showWarnings = FALSE)

# Tema para gráficos
theme_set(theme_minimal(base_size = 12))

# Colores
col_primary <- "#1E3A5F"
col_secondary <- "#C41E3A"
col_accent <- "#4A90A4"

# Verificar directorio de trabajo
if (!file.exists("02_Section_2_Gender_Labor_Income_Gap/01_code/00_rundirectory.R")) {
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

source("02_Section_2_Gender_Labor_Income_Gap/01_code/01_data_scraping.R")

cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  LIMPIEZA Y CONSTRUCCIÓN DE MUESTRA                            \n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

source("02_Section_2_Gender_Labor_Income_Gap/01_code/02_data_cleaning_workers.R")


# ==============================================================================
#  FINAL
# ==============================================================================

