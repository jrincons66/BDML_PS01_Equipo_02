################################################################################
##                                                                            ##
##                     DIVISIÓN TRAIN / VALIDATION                            ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

## Descripción:
##   Divide el dataset limpio (geih_analysis) en dos muestras mutuamente 
##   excluyentes:
##     - Training set:    Chunks 1–7  (estimación de modelos)
##     - Validation set:  Chunks 8–10 (evaluación out-of-sample)
##
##   SOLUCIÓN AL PROBLEMA DE NIVELES DE FACTORES:
##   Al filtrar el training set, algunos niveles de factores (ej: categorías 
##   de indus) pueden quedar sin observaciones. lm() los descarta internamente,
##   pero predict() falla si los encuentra en el validation set ("factor has 
##   new levels"). 
##
##   La solución es:
##     1. Aplicar droplevels() al training → elimina niveles vacíos
##     2. Alinear los niveles del validation con los del training
##   Observaciones del validation con niveles no vistos en training quedan
##   como NA al predecir y se excluyen del cálculo de RMSE.

# ==============================================================================
# 1. VERIFICACIÓN DE PREREQUISITOS
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("   SECTION 3: DIVISIÓN TRAIN / VALIDATION                       \n")
cat("================================================================\n\n")

if (!exists("geih_analysis")) {
  stop("ERROR: El objeto 'geih_analysis' no existe. 
       Asegúrese de haber corrido 02_data_cleaning_workers.R primero.")
}

cat(sprintf("Dataset de entrada: geih_analysis\n"))
cat(sprintf("  Observaciones totales: %s\n", format(nrow(geih_analysis), big.mark = ",")))
cat(sprintf("  Chunks disponibles:    %s\n",
            paste(sort(unique(geih_analysis$chunk_id)), collapse = ", ")))

# ==============================================================================
# 2. DIVISIÓN DE LA MUESTRA
# ==============================================================================

# Training: chunks 1-7
# droplevels() elimina niveles de factores sin observaciones en training,
# que son los que causan el error "factor has new levels" en predict().

geih_train <- geih_analysis %>%
  filter(chunk_id %in% 1:7) %>%
  droplevels()

# Validation: chunks 8-10
# Los factores se re-definen usando los niveles del training.
# Observaciones con niveles no vistos en training quedan como NA al predecir.

factor_vars <- c("relab", "educ", "indus", "firm_size", "formal")

geih_validation <- geih_analysis %>%
  filter(chunk_id %in% 8:10)

for (v in factor_vars) {
  geih_validation[[v]] <- factor(geih_validation[[v]],
                                 levels = levels(geih_train[[v]]))
}

# ==============================================================================
# 3. VERIFICACIÓN DE LA DIVISIÓN
# ==============================================================================

cat("\n--- Verificación de la división ---\n\n")

cat(sprintf("Training set   (chunks 1–7):   %s observaciones  (%.1f%%)\n",
            format(nrow(geih_train), big.mark = ","),
            100 * nrow(geih_train) / nrow(geih_analysis)))

cat(sprintf("Validation set (chunks 8–10):  %s observaciones  (%.1f%%)\n",
            format(nrow(geih_validation), big.mark = ","),
            100 * nrow(geih_validation) / nrow(geih_analysis)))

# Sin pérdida de observaciones
n_check <- nrow(geih_train) + nrow(geih_validation)
if (n_check == nrow(geih_analysis)) {
  cat(sprintf("\n✓ Sin pérdida de observaciones: %s + %s = %s\n",
              format(nrow(geih_train), big.mark = ","),
              format(nrow(geih_validation), big.mark = ","),
              format(nrow(geih_analysis), big.mark = ",")))
} else {
  warning("✗ La suma no coincide. Revisar chunk_id.")
}

# Sin superposición de chunks
if (length(intersect(unique(geih_train$chunk_id), unique(geih_validation$chunk_id))) == 0) {
  cat("✓ Sin superposición entre training y validation\n")
} else {
  warning("✗ Hay chunks en común. Revisar filtros.")
}

# Reportar niveles descartados por droplevels()
cat("\n--- Niveles descartados del training (no vistos en chunks 1-7) ---\n\n")
for (v in factor_vars) {
  niveles_full  <- levels(geih_analysis[[v]])
  niveles_train <- levels(geih_train[[v]])
  descartados   <- setdiff(niveles_full, niveles_train)
  if (length(descartados) > 0) {
    cat(sprintf("  %s: %d nivel(es) descartado(s) → %s\n",
                v, length(descartados), paste(descartados, collapse = ", ")))
  } else {
    cat(sprintf("  %s: sin niveles descartados\n", v))
  }
}

# Observaciones en validation afectadas (NAs introducidos)
cat("\n--- Observaciones en validation con niveles no vistos en training ---\n\n")
for (v in factor_vars) {
  n_na <- sum(is.na(geih_validation[[v]]))
  if (n_na > 0) {
    cat(sprintf("  %s: %d observaciones con nivel desconocido (NA al predecir)\n", v, n_na))
  } else {
    cat(sprintf("  %s: ninguna observación afectada\n", v))
  }
}

# ==============================================================================
# 4. COMPARACIÓN DE ESTADÍSTICAS DESCRIPTIVAS
# ==============================================================================

cat("\n--- Comparación de estadísticas clave ---\n\n")

stats_comparacion <- data.frame(
  Variable = c("Log Ingreso (media)",
               "Log Ingreso (sd)",
               "Edad (media)",
               "Horas trabajadas (media)",
               "% Mujeres"),
  Training = c(
    round(mean(geih_train$log_income,       na.rm = TRUE), 3),
    round(sd(geih_train$log_income,         na.rm = TRUE), 3),
    round(mean(geih_train$age,              na.rm = TRUE), 1),
    round(mean(geih_train$totalHoursWorked, na.rm = TRUE), 1),
    round(100 * mean(geih_train$female,     na.rm = TRUE), 1)
  ),
  Validation = c(
    round(mean(geih_validation$log_income,       na.rm = TRUE), 3),
    round(sd(geih_validation$log_income,         na.rm = TRUE), 3),
    round(mean(geih_validation$age,              na.rm = TRUE), 1),
    round(mean(geih_validation$totalHoursWorked, na.rm = TRUE), 1),
    round(100 * mean(geih_validation$female,     na.rm = TRUE), 1)
  )
)

print(stats_comparacion, row.names = FALSE)
