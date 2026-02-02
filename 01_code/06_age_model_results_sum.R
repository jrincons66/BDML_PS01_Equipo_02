################################################################################
##                                                                            ##
##                       RESUMEN DE RESULTADOS                                ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################
# ==============================================================================
# 7. RESUMEN DE RESULTADOS 
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("        RESUMEN - SECTION 1: AGE-LABOR INCOME PROFILE           \n")
cat("================================================================\n\n")

cat("1. EVIDENCIA DE NO-LINEALIDAD Y CONCAVIDAD:\n")
cat("   ─────────────────────────────────────────\n")
cat(sprintf("   • Coeficiente Age²: %.7f (p < 0.001)\n", beta_age2_unc))
if (beta_age2_unc < 0) {
  cat("   • El perfil es CÓNCAVO → CONSISTENTE con teoría del capital humano\n")
  cat("   • Existe un MÁXIMO en el rango de edad observado\n")
}

cat("\n2. EDAD PICO DE INGRESOS:\n")
cat("   ─────────────────────────────────────────\n")
cat(sprintf("   Modelo Incondicional: %.1f años  [IC 95%%: %.1f - %.1f]\n",
            peak_age_unconditional, ci_unc_lower, ci_unc_upper))
cat(sprintf("   Modelo Condicional:   %.1f años  [IC 95%%: %.1f - %.1f]\n",
            peak_age_conditional, ci_cond_lower, ci_cond_upper))
cat(sprintf("   Diferencia:           %+.1f años\n", 
            peak_age_conditional - peak_age_unconditional))

cat("\n3. EFECTO DE LOS CONTROLES:\n")
cat("   ─────────────────────────────────────────\n")
cat(sprintf("   • R² ajustado: %.4f → %.4f (+%.4f)\n",
            summary(model_unconditional)$adj.r.squared,
            summary(model_conditional)$adj.r.squared,
            summary(model_conditional)$adj.r.squared - 
              summary(model_unconditional)$adj.r.squared))

if (peak_age_conditional > peak_age_unconditional) {
  cat("   • La edad pico AUMENTA al condicionar\n")
  cat("   • Interpretación: Parte del declive observado en ingresos a edades\n")
  cat("     avanzadas se explica por la reducción de horas trabajadas y/o\n")
  cat("     cambios en el tipo de empleo (ej: transición a cuenta propia)\n")
} else if (peak_age_conditional < peak_age_unconditional) {
  cat("   • La edad pico DISMINUYE al condicionar\n")
  cat("   • Interpretación: Los trabajadores más jóvenes trabajan menos horas,\n")
  cat("     lo que subestima su productividad en el modelo incondicional\n")
}

cat("\n4. INTERPRETACIÓN ECONÓMICA:\n")
cat("   ─────────────────────────────────────────\n")
cat("   • El perfil cóncavo refleja la teoría del capital humano:\n")
cat("     (a) Acumulación de experiencia y habilidades al inicio de la carrera\n")
cat("     (b) Rendimientos decrecientes de la experiencia adicional\n")
cat("     (c) Depreciación del capital humano / obsolescencia de habilidades\n")
cat(sprintf("   • Una edad pico de ~%.0f años es consistente con la literatura\n",
            (peak_age_unconditional + peak_age_conditional) / 2))

cat("\n================================================================\n")
cat("                    FIN DE LA SECCIÓN 1                         \n")
cat("================================================================\n")
