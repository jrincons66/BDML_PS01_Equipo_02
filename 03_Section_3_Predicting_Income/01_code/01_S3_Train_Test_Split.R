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
##   IMPORTANTE: Este cuaderno debe correrse DESPUÉS de 02_data_cleaning_workers.R
##   y ANTES de cualquier script de estimación de la Sección 3. Todos los 
##   cuadernos posteriores de esta sección consumen los objetos que se generan 
##   aquí: geih_train y geih_validation.
##
##   La división se basa en chunk_id (variable creada en el scraping), lo que
##   garantiza que la separación sea determinística y reproducible sin depender
##   de aleatorización. Esto refleja el diseño del Problem Set, donde los chunks
##   8–10 actúan como un hold-out real, nunca visto durante la estimación.

# ==============================================================================
# 1. VERIFICACIÓN DE PREREQUISITOS
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("   SECTION 3: DIVISIÓN TRAIN / VALIDATION                       \n")
cat("================================================================\n\n")

# Verificar que geih_analysis existe y tiene chunk_id
if (!exists("geih_analysis")) {
  stop("ERROR: El objeto 'geih_analysis' no existe. 
       Asegúrese de haber corrido 02_data_cleaning_workers.R primero.")
}

if (!"chunk_id" %in% names(geih_analysis)) {
  stop("ERROR: La variable 'chunk_id' no está en geih_analysis. 
       Verifique el script de limpieza.")
}

cat("Dataset de entrada: geih_analysis\n")
cat(sprintf("  Observaciones totales: %s\n", format(nrow(geih_analysis), big.mark = ",")))
cat(sprintf("  Chunks disponibles:    %s\n", 
            paste(sort(unique(geih_analysis$chunk_id)), collapse = ", ")))

# ==============================================================================
# 2. DIVISIÓN DE LA MUESTRA
# ==============================================================================

# Training:   chunks 1–7  → estimación de todos los modelos
# Validation: chunks 8–10 → evaluación out-of-sample (RMSE)

geih_train <- geih_analysis %>%
  filter(chunk_id %in% 1:7)

geih_validation <- geih_analysis %>%
  filter(chunk_id %in% 8:10)

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

# Confirmación de que la unión es exactamente igual al total
n_total_check <- nrow(geih_train) + nrow(geih_validation)
if (n_total_check == nrow(geih_analysis)) {
  cat(sprintf("\n✓ Verificación OK: %s + %s = %s (sin pérdida de observaciones)\n",
              format(nrow(geih_train), big.mark = ","),
              format(nrow(geih_validation), big.mark = ","),
              format(nrow(geih_analysis), big.mark = ",")))
} else {
  warning(sprintf("✗ Advertencia: La suma no coincide. Revisar chunk_id.
                  Total esperado: %d | Total obtenido: %d",
                  nrow(geih_analysis), n_total_check))
}

# Confirmar que no hay superposición entre conjuntos
chunks_train <- unique(geih_train$chunk_id)
chunks_val   <- unique(geih_validation$chunk_id)

if (length(intersect(chunks_train, chunks_val)) == 0) {
  cat("✓ Sin superposición entre training y validation\n")
} else {
  warning("✗ Hay chunks en común entre training y validation. Revisar filtros.")
}

# ==============================================================================
# 4. COMPARACIÓN DE ESTADÍSTICAS DESCRIPTIVAS
# ==============================================================================
# Verifica que training y validation sean comparables en distribución,
# lo que valida que el hold-out sea representativo de la muestra general.

cat("\n--- Comparación de estadísticas clave ---\n\n")

stats_comparacion <- data.frame(
  Variable = c("Log Ingreso (media)", 
               "Log Ingreso (sd)",
               "Edad (media)", 
               "Horas trabajadas (media)",
               "% Mujeres"),
  Training = c(
    round(mean(geih_train$log_income,          na.rm = TRUE), 3),
    round(sd(geih_train$log_income,            na.rm = TRUE), 3),
    round(mean(geih_train$age,                 na.rm = TRUE), 1),
    round(mean(geih_train$totalHoursWorked,    na.rm = TRUE), 1),
    round(100 * mean(geih_train$female,        na.rm = TRUE), 1)
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
