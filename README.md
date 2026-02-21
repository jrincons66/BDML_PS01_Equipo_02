# Problem Set 1: Predicting Income

# Grupo 2

## Big Data and Machine Learning para Economía Aplicada

**MECA 4107** -- Universidad de los Andes -- 2026-10

------------------------------------------------------------------------

## Autores

| Nombre             | Código    |
|--------------------|-----------|
| Jose A. Rincon S   | 202013328 |
| Juan C. Riaño      | 202013305 |
| Lucas Rodriguez    | 202021985 |
| Santiago González  | 202110234 |

------------------------------------------------------------------------

## Descripción

Este repositorio contiene el análisis del **Problem Set 1** del curso de Big Data y Machine Learning para Economía Aplicada. El objetivo es estimar y predecir el ingreso laboral utilizando datos de la Gran Encuesta Integrada de Hogares (GEIH) 2018 para Bogotá.

El análisis se divide en tres secciones:

1.  **Section 1: Age-Labor Income Profile** - Estimación del perfil edad-ingreso para testear las predicciones de la teoría del capital humano
2.  **Section 2: Gender Wage Gap** - Medición y descomposición de la brecha salarial de género via FWL
3.  **Section 3: Income Prediction** - Modelos de predicción de ingreso laboral
------------------------------------------------------------------------

## Instrucciones de Replicación

Para reproducir todos los resultados, ejecutar:

``` r
source("01_code/00_rundirectory.R")
```

Este script maestro ejecuta secuencialmente:

1.  Configuración del entorno y carga de paquetes
2.  Web scraping de los 10 chunks de datos de la GEIH 2018
3.  Limpieza de datos y construcción de la muestra de análisis
4.  Estadísticas descriptivas para cada sección
5.  Estimación de modelos econométricos
6.  Bootstrap para intervalos de confianza
7.  Generación de todas las figuras y tablas

------------------------------------------------------------------------

## Estructura del Repositorio

```         
grupo_1_BDML/
│
├── README.md
├── .gitignore
├── 00_rundirectory.R                        # Script maestro
├── grupo_1_BDML.Rproj                       # Proyecto de RStudio
│
├── 01_Section_1_Age_Labor_Income/           # SECTION 1: Age-Income Profile
│   ├── 01_code/
│   │   ├── 01_S1_data_scraping.R            # Web scraping GEIH 2018
│   │   ├── 02_S1_data_cleaning_workers.R    # Limpieza y construcción de muestra
│   │   ├── 03_S1_age_analysis_descriptive.R # Estadísticas descriptivas
│   │   ├── 04_S1_age_income_models.R        # Modelos y bootstrap edad pico
│   │   ├── 05_S1_age_income_models_vis.R    # Visualizaciones
│   │   └── 06_S1_age_model_results_sum.R    # Resumen de resultados
│   └── 02_output/
│       ├── figures/
│       └── tables/
│
├── 02_Section_2_Gender_Labor_Income/        # SECTION 2: Gender Wage Gap
│   ├── 01_code/
│   │   ├── 01_s2_Descriptive_Statistics.R   # Estadísticas por género
│   │   ├── 02_S2_Unconditional_Model.R      # Modelo incondicional
│   │   ├── 03_s2_FWL_Model.R                # Modelos FWL (con/sin industria)
│   │   ├── 04_s2_FWL_SE_Bootstrap.R         # Bootstrap para errores estándar
│   │   ├── 05_s2_FWL_Presentacion_Resul.R   # Tabla de resultados
│   │   └── 06_s2_Diferencia_Edades.R        # Perfiles edad-ingreso por género
│   └── 02_output/
│       ├── figures/
│       └── tables/
│
└── 03_Section_3_Predicting_Income/          # SECTION 3: Income Prediction
    ├── 01_code/
    │   ├── 01_S3_Train_Test_Split.R         # División training/testing
    │   ├── 02_S3_Baseline_Models.R          # Modelos baseline (Secciones 1 y 2)
    │   └── 03_S3_Extended_Models.R          # Nuevas especificaciones (5+ modelos)
    │   └── 04_S3_Model_Selection.R          # Se definió el mejor modelo
    │   └── 05_S3_LOOCV_Error.R              # Calculo del LOOVC
    │   └── 06_S3_Leverage_Analysis.R        # Realizamos el Leverage del modelo
    │   └── 07_S3_Prediction_Errors.R        # Se definió el mejor modelo
    └── 02_output/
        ├── figures/
        └── tables/


```  
------------------------------------------------------------------------
## Datos

### Fuente

[GEIH 2018 - Bogotá](https://ignaciomsarmiento.github.io/GEIH2018_sample/)

Los datos provienen de la Medición de Pobreza Monetaria y Desigualdad 2018 del DANE, basada en la Gran Encuesta Integrada de Hogares (GEIH). El dataset contiene **32,177 observaciones** distribuidas en 10 chunks.

### Variables Principales

| Variable | Descripción |
|----------|-------------|
| `age` | Edad del individuo (años) |
| `y_total_m` | Ingreso laboral mensual total (pesos colombianos) |
| `totalHoursWorked` | Horas trabajadas por semana |
| `sex` | Sexo (1 = Hombre, 2 = Mujer) |
| `relab` | Tipo de relación laboral (1-7) |
| `edu` | Nivel educativo de los individuos |
| `SizeFirm` | Tamaño de la firma empleadora |

### Construcción de la Muestra

| Criterio | Justificación Económica |
|----------|-------------------------|
| `age >= 18` | Mercado laboral formal de adultos |
| `y_total_m > 0` | Individuos empleados con ingreso reportado |
| `totalHoursWorked > 0` | Consistencia con condición de empleo |
| `!is.na(relab)` | Tipo de empleo identificable |

### Variables Derivadas

| Variable | Definición | Uso |
|----------|------------|-----|
| `log_income` | `log(y_total_m)` | Variable dependiente |
| `age2` | `age^2` | Término cuadrático para perfil edad-ingreso |
| `female` | `1` si `sex == 2`, `0` si no | Indicador de género |
| `age_group` | Categorías: 18-24, 25-34, 35-44, 45-54, 55-64, 65+ | Descriptivas |

---

## Metodología

### Section 1: Age-Labor Income Profile

**Objetivo:** Testear si el perfil edad-ingreso es cóncavo, como predice la teoría del capital humano.

**Especificaciones:**

1. **Modelo Incondicional:**
```

log(w) = β₁ + β₂·Age + β₃·Age² + u \`\`\`
```

2.  **Modelo Condicional:**

```         
    log(w) = β₁ + β₂·Age + β₃·Age² + β₄·Hours + β₅·Relab + u
 ```

**Edad Pico:** Se calcula como `Age* = -β₂ / (2·β₃)` a partir de la condición de primer orden.

**Inferencia:** Los intervalos de confianza para la edad pico se calculan mediante **bootstrap** (B = 1000 réplicas) usando el método percentil, dado que la edad pico es una función no lineal de los coeficientes.

### Section 2: Gender Wage Gap

**Objetivo:** Medir la brecha salarial de género y analizar cómo cambia al controlar por características observables.

1. **Modelo Incondicional:**
```
log(w) = b0 + b1*female + u
```
2. **Modelo Condicional (FWL) con industrias:**
```
log(w) = b0 + b1*female + b2*Age + b3*Age^2 + b4*educ + b5*Hours 
         + b6*tenure + b7*industry + b8*firm_size + u
```
3. **Modelo Condicional (FWL) sin industrias:**
```
log(w) = b0 + b1*female + b2*Age + b3*Age^2 + b4*educ + b5*Hours 
         + b6*tenure + b7*firm_size + u
```
**Inferencia:** Bootstrap (B = 1000) para errores estándar de los coeficientes FWL.


### Section 3: Income Prediction

**Objetivo:** Construir modelos predictivos de ingreso para identificar individuos con posible subdeclaración tributaria.
**División de datos:**

- Training: 70% (o Chunks 1-7)
- Testing: 30% (o Chunks 8-10)

**Modelos Baseline:**

- Modelos de Sección 1 y 2

**Nuevas Especificaciones (5+):**

1. Retornos a educación heterogéneos por género
2. Perfil edad-ingreso heterogéneo por género (ciclo de vida)
3. No-linealidad en tenure con heterogeneidad por formalidad
4. Estructura de horas (usuales vs efectivas)
5. Interacciones ocupación con características

Evaluación:

- RMSE out-of-sample
- LOOCV para modelo seleccionado
- Análisis de influencia via FWL

------------------------------------------------------------------------

## Software

```         
R version 4.3.0 o superior
```

### Paquetes Requeridos

Los paquetes se instalan y cargan automáticamente mediante `pacman::p_load()`:

| Categoría     | Paquetes                                    |
|---------------|---------------------------------------------|
| Datos         | `tidyverse`, `rvest`                        |
| Modelado      | `boot`, `broom`, `caret`                    |
| Visualización | `ggplot2`, `scales`, `gridExtra`, `viridis` |
| Tablas        | `stargazer`, `knitr`, `kableExtra`          |

## Referencias

-   Sarmiento-Barbieri, I. (2026). *Big Data and Machine Learning for Applied Economics*. Universidad de los Andes.
-   DANE. *Gran Encuesta Integrada de Hogares (GEIH) 2018*.
-   Mincer, J. (1974). *Schooling, Experience, and Earnings*. NBER.
-   Blau, F. & Kahn, L. (2017). The Gender Wage Gap. Journal of Economic Literature.
-   Kleven, H. et al. (2019). Child Penalties Across Countries. AER Insights.

------------------------------------------------------------------------

## Contacto

Para preguntas sobre este repositorio, contactar a los autores menciondos al inicio.
