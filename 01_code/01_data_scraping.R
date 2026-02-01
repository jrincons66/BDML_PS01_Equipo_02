################################################################################
##                                                                            ##
##                    CARGA DE DATOS - WEB SCRAPING                           ##
##                         GEIH 2018 - Bogotá                                 ##
##                                                                            ##
##          Big Data and Machine Learning para Economía Aplicada              ##
##          MECA 4107 - Universidad de los Andes                              ##
##                                                                            ##
################################################################################

# ==============================================================================
# 1. WEB SCRAPING DE LOS 10 CHUNKS
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("     WEB SCRAPING - GEIH 2018 BOGOTÁ                            \n")
cat("     Fuente: ignaciomsarmiento.github.io/GEIH2018_sample/       \n")
cat("================================================================\n\n")


base_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"

chunks_list <- list()

for (i in 1:10) {

  url <- paste0(base_url, i, ".html")

  tryCatch({
    
    pagina <- read_html(url)
    tablas <- pagina %>% html_table(fill = TRUE)
    
    if (length(tablas) > 0) {
      chunk_df <- tablas[[1]]
      chunk_df$chunk_id <- i
      chunks_list[[i]] <- chunk_df
      cat(sprintf("        ✓ %s observaciones\n", format(nrow(chunk_df), big.mark = ",")))
    } else {
      cat("        ✗ No se encontró tabla\n")
    }
    
    # Dejamos "dormir" por 40 s para asegurarnos que las tabas cargan
    Sys.sleep(40)
    
  }, error = function(e) {
  })
}

# ==============================================================================
# 2. COMBINAR CHUNKS
# ==============================================================================

chunks_validos <- chunks_list[!sapply(chunks_list, is.null)]
geih_raw <- bind_rows(chunks_validos)


cat("================================================================\n")
cat(sprintf("TOTAL: %s observaciones de %d chunks\n", 
            format(nrow(geih_raw), big.mark = ","), 
            length(chunks_validos)))
cat("================================================================\n\n")

# ==============================================================================
# 3. VERIFICAR ESTRUCTURA DE LOS DATOS
# ==============================================================================


cat("================================================================\n")
cat("     ESTRUCTURA DE LOS DATOS                                    \n")
cat("================================================================\n")

cat("Dimensiones:", nrow(geih_raw), "filas x", ncol(geih_raw), "columnas\n\n")

# Verificar variables clave
vars_clave <- c("age", "y_total_m", "totalHoursWorked", "sex", "relab")

# ==============================================================================
# 4. RESUMEN ESTADÍSTICO 
# ==============================================================================

cat("================================================================\n")
cat("     RESUMEN ESTADÍSTICO                                        \n")
cat("================================================================\n\n")

# Convertir a numérico las variables clave
geih_raw <- geih_raw %>%
  mutate(
    age = as.numeric(age),
    y_total_m = as.numeric(y_total_m),
    totalHoursWorked = as.numeric(totalHoursWorked)
  )

cat("EDAD:\n")
cat(sprintf("  Rango: %d - %d años\n", 
            min(geih_raw$age, na.rm = TRUE),
            max(geih_raw$age, na.rm = TRUE)))

cat(sprintf("  Media: %.1f años\n", mean(geih_raw$age, na.rm = TRUE)))

cat(sprintf("  Missing: %d (%.1f%%)\n", 
            sum(is.na(geih_raw$age)),
            sum(is.na(geih_raw$age))/nrow(geih_raw)*100))

cat("\nINGRESO MENSUAL (y_total_m):\n")
cat(sprintf("  Observaciones con ingreso > 0: %s\n",
            format(sum(geih_raw$y_total_m > 0, na.rm = TRUE), big.mark = ",")))

cat(sprintf("  Observaciones con ingreso = 0: %s\n",
            format(sum(geih_raw$y_total_m == 0, na.rm = TRUE), big.mark = ",")))

# Encontramos que no hay observaciones con cero de ingresos al año, eso dice que pueden estar cobijadas bajo NAs

cat(sprintf("  Missing: %s\n",
            format(sum(is.na(geih_raw$y_total_m)), big.mark = ",")))

cat("\nHORAS TRABAJADAS:\n")
cat(sprintf("  Media (entre quienes trabajan): %.1f horas/semana\n",
            mean(geih_raw$totalHoursWorked[geih_raw$totalHoursWorked > 0], na.rm = TRUE)))

cat(sprintf("  Media total base: %.1f horas/semana\n",
            mean(geih_raw$totalHoursWorked, na.rm = TRUE)))

# Encontramos que no hay observaciones con cero de horas trabajadas, eso dice que pueden estar cobijadas bajo NAs

cat(sprintf(" Missing: %s\n",
            format(sum(is.na(geih_raw$totalHoursWorked)), big.mark = ",")))
cat("\n")