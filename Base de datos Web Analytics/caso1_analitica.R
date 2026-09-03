#SOLUCIÓN CASO: Web Analytics
#Instalar paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
getwd()
list.files()
excel_sheets("Web_Analytics.xls")




archivo <- "Web_Analytics.xls"




# Hoja "Weekly Visits": encabezados en la fila 5, datos desde la fila 6
# (66 semanas: 25-mayo-2008 a 29-agosto-2009)
weekly_visits <- read_excel(archivo, sheet = "Weekly Visits", skip = 4) %>%
  slice(1:66) %>%
  transmute(
    Semana        = `Week (2008-2009)`,
    Visitas       = Visits,
    VisitasUnicas = `Unique Visits`
  )
View(weekly_visits)




# Hoja "Financials": mismas 66 semanas
financials <- read_excel(archivo, sheet = "Financials", skip = 4) %>%
  slice(1:66) %>%
  transmute(
    Semana         = `Week (2008-2009)`,
    Ingresos       = Revenue,
    Utilidad       = Profit,
    LibrasVendidas = `Lbs. Sold`
  )




# Hoja "Lbs. Sold": serie más larga (ene-2005 a jul-2010), para el punto 8
lbs_sold <- read_excel(archivo, sheet = "Lbs. Sold", skip = 4) %>%
  transmute(
    Semana         = Week,
    LibrasVendidas = `Lbs. Sold`
  ) %>%
  filter(!is.na(LibrasVendidas))




# Unimos visitas + financieros y asignamos el periodo de cada semana.
datos <- weekly_visits %>%
  left_join(financials, by = "Semana") %>%
  mutate(
    indice_semana = row_number(),
    periodo = case_when(
      indice_semana <= 14 ~ "Inicial",
      indice_semana <= 35 ~ "Pre-Promocion",
      indice_semana <= 52 ~ "Promocion",
      TRUE                ~ "Post-Promocion"
    ),
    periodo = factor(periodo,
                      levels = c("Inicial", "Pre-Promocion",
                                 "Promocion", "Post-Promocion"))
  )




variables <- c("Visitas", "VisitasUnicas", "Ingresos", "Utilidad", "LibrasVendidas")








# PUNTO 1: Gráficos de columnas en el tiempo
# (visitas únicas, ingresos, utilidad, libras vendidas)
graficar_serie <- function(data, variable, titulo, y_lab) {
  ggplot(data, aes(x = indice_semana, y = .data[[variable]])) +
    geom_col(fill = "steelblue") +
    labs(title = titulo, x = "Semana", y = y_lab) +
    theme_minimal()
}




g_visitas_unicas <- graficar_serie(datos, "VisitasUnicas",
                                    "Visitas únicas por semana", "Visitas únicas")
g_ingresos <- graficar_serie(datos, "Ingresos",
                              "Ingresos por semana", "Ingresos ($)")
g_utilidad <- graficar_serie(datos, "Utilidad",
                              "Utilidad por semana", "Utilidad ($)")
g_libras <- graficar_serie(datos, "LibrasVendidas",
                            "Libras vendidas por semana", "Libras vendidas")




print(g_visitas_unicas)
print(g_ingresos)
print(g_utilidad)
print(g_libras)




ggsave("p1_visitas_unicas.png", g_visitas_unicas, width = 8, height = 4)
ggsave("p1_ingresos.png", g_ingresos, width = 8, height = 4)
ggsave("p1_utilidad.png", g_utilidad, width = 8, height = 4)
ggsave("p1_libras_vendidas.png", g_libras, width = 8, height = 4)








# PUNTO 2: Estadísticas descriptivas por periodo
# (media, mediana, desv. estándar, mínimo, máximo)
# Una tabla de 5x5 por cada uno de los 4 periodos.
resumen_variable <- function(x) {
  c(
    media    = mean(x, na.rm = TRUE),
    mediana  = median(x, na.rm = TRUE),
    desv_est = sd(x, na.rm = TRUE),
    minimo   = min(x, na.rm = TRUE),
    maximo   = max(x, na.rm = TRUE)
  )
}




tablas_por_periodo <- lapply(levels(datos$periodo), function(p) {
  sub <- datos %>% filter(periodo == p)
  tabla <- sapply(variables, function(v) resumen_variable(sub[[v]]))
  round(tabla, 2)
})
names(tablas_por_periodo) <- levels(datos$periodo)




cat("\n--- Periodo Inicial ---\n");        print(tablas_por_periodo$Inicial)
cat("\n--- Periodo Pre-Promocion ---\n");  print(tablas_por_periodo$`Pre-Promocion`)
cat("\n--- Periodo Promocion ---\n");      print(tablas_por_periodo$Promocion)
cat("\n--- Periodo Post-Promocion ---\n"); print(tablas_por_periodo$`Post-Promocion`)




# Exportar las 4 tablas a un solo csv:
tabla_larga <- bind_rows(
  lapply(names(tablas_por_periodo), function(p) {
    df <- as.data.frame(tablas_por_periodo[[p]])
    df$estadistico <- rownames(df)
    df$periodo <- p
    df
  })
)
write.csv(tabla_larga, "p2_estadisticas_por_periodo.csv", row.names = FALSE)




# PUNTO 3: Gráficos de medias a través de los 4 periodos
# (uno por cada variable: visitas, visitas únicas, ingresos, utilidad, libras vendidas)




medias_por_periodo <- datos %>%
  group_by(periodo) %>%
  summarise(across(all_of(variables), ~ mean(.x, na.rm = TRUE)))




write.csv(medias_por_periodo, "p3_medias_por_periodo.csv", row.names = FALSE)




graficar_medias <- function(data, variable, titulo, y_lab) {
  ggplot(data, aes(x = periodo, y = .data[[variable]])) +
    geom_col(fill = "darkorange") +
    labs(title = titulo, x = "Periodo", y = y_lab) +
    theme_minimal()
}




g_media_visitas        <- graficar_medias(medias_por_periodo, "Visitas",
                              "Media de visitas por periodo", "Visitas promedio")
g_media_visitas_unicas <- graficar_medias(medias_por_periodo, "VisitasUnicas",
                              "Media de visitas únicas por periodo", "Visitas únicas promedio")
g_media_ingresos       <- graficar_medias(medias_por_periodo, "Ingresos",
                              "Media de ingresos por periodo", "Ingresos promedio ($)")
g_media_utilidad       <- graficar_medias(medias_por_periodo, "Utilidad",
                              "Media de utilidad por periodo", "Utilidad promedio ($)")
g_media_libras         <- graficar_medias(medias_por_periodo, "LibrasVendidas",
                              "Media de libras vendidas por periodo", "Libras vendidas promedio")




print(g_media_visitas)
print(g_media_visitas_unicas)
print(g_media_ingresos)
print(g_media_utilidad)
print(g_media_libras)




ggsave("p3_media_visitas.png", g_media_visitas, width = 6, height = 4)
ggsave("p3_media_visitas_unicas.png", g_media_visitas_unicas, width = 6, height = 4)
ggsave("p3_media_ingresos.png", g_media_ingresos, width = 6, height = 4)
ggsave("p3_media_utilidad.png", g_media_utilidad, width = 6, height = 4)
ggsave("p3_media_libras.png", g_media_libras, width = 6, height = 4)




# Apoyo para Punto 4: variación porcentual entre periodos consecutivos
cambios_periodo <- medias_por_periodo %>%
  mutate(across(all_of(variables),
                ~ round((.x - lag(.x)) / lag(.x) * 100, 1),
                .names = "cambio_pct_{.col}"))
print(cambios_periodo)








# PUNTO 5: Diagrama de dispersión Ingresos vs. Libras Vendidas
g_ingresos_libras <- ggplot(datos, aes(x = LibrasVendidas, y = Ingresos)) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Ingresos vs. Libras Vendidas",
       x = "Libras Vendidas", y = "Ingresos ($)") +
  theme_minimal()
print(g_ingresos_libras)
ggsave("p5_ingresos_vs_libras.png", g_ingresos_libras, width = 6, height = 4)








# Correlación
cor_ingresos_libras <- cor(datos$Ingresos, datos$LibrasVendidas, use = "complete.obs")
cat("Correlación Ingresos-Libras Vendidas:", cor_ingresos_libras, "\n")








# PUNTO 6: Diagrama de dispersión Ingresos vs. Visitas
g_ingresos_visitas <- ggplot(datos, aes(x = Visitas, y = Ingresos)) +
  geom_point(color = "darkorange", size = 2) +
  labs(title = "Ingresos vs. Visitas",
       x = "Visitas", y = "Ingresos ($)") +
  theme_minimal()
print(g_ingresos_visitas)
ggsave("p6_ingresos_vs_visitas.png", g_ingresos_visitas, width = 6, height = 4)








# PUNTO 7: Matriz de correlación entre todas las variables
cor_matrix <- cor(datos[variables], use = "complete.obs")
print(round(cor_matrix, 3))
write.csv(round(cor_matrix, 3), "p7_matriz_correlacion.csv", row.names = TRUE)




# Recordatorio de las dos correlaciones clave ya calculadas:
cat("Correlación Ingresos-LibrasVendidas:", cor_ingresos_libras, "\n")
cat("Correlación Ingresos-Visitas:", cor_ingresos_visitas, "\n")






# PUNTO 8: Libras de material vendido (ene-2005 a jul-2010)


# 8a) Estadísticas resumen
stats_libras <- resumen_variable(lbs_sold$LibrasVendidas)
cat("\n--- Punto 8a: Estadisticas de Libras Vendidas (2005-2010) ---\n")
print(round(stats_libras, 2))




write.csv(as.data.frame(t(round(stats_libras, 2))),
          "p8a_estadisticas_libras.csv", row.names = FALSE)




# 8b) Histograma
n_obs <- nrow(lbs_sold)
n_bins <- round(sqrt(n_obs))




g_histograma <- ggplot(lbs_sold, aes(x = LibrasVendidas)) +
  geom_histogram(bins = n_bins, fill = "seagreen", color = "black") +
  labs(
    title = "Histograma de libras de material vendidas por semana",
    subtitle = paste0("Enero 2005 - Julio 2010 (n = ", n_obs, ", bins = ", n_bins, ")"),
    x = "Libras vendidas", y = "Frecuencia"
  ) +
  theme_minimal()




print(g_histograma)
ggsave("p8b_histograma_libras.png", g_histograma, width = 8, height = 5)




# 8c) Descripción del histograma
cat("\n--- Ayuda para el punto 8c ---\n")
cat("Media:   ", round(stats_libras["media"], 2), "\n")
cat("Mediana: ", round(stats_libras["mediana"], 2), "\n")
cat("Si media y mediana son similares y el histograma es simétrico",
    "alrededor de ese valor, hay evidencia a favor de forma acampanada.\n")
cat("Asimetría fuerte (media muy distinta de la mediana) o colas",
    "pesadas de un solo lado sugieren que NO es normal.\n")




# ============================================================
# PUNTO 8d - REGLA EMPIRICA
# ============================================================


# Calcular media de libras vendidas
media <- mean(lbs_sold$LibrasVendidas, na.rm = TRUE)


# Calcular desviacion estandar
desv <- sd(lbs_sold$LibrasVendidas, na.rm = TRUE)


# Numero total de observaciones
n <- sum(!is.na(lbs_sold$LibrasVendidas))


# Calcular Z-score para cada observacion
lbs_sold$z_score <- (lbs_sold$LibrasVendidas - media) / desv




# ------------------------------------------------------------
# Numero teorico de observaciones
# ------------------------------------------------------------


teorico_1 <- n * 0.68
teorico_2 <- n * 0.95
teorico_3 <- n * 0.99




# ------------------------------------------------------------
# Numero real de observaciones
# ------------------------------------------------------------


# Dentro de 1 desviacion estandar
real_1 <- sum(
  lbs_sold$z_score >= -1 &
  lbs_sold$z_score <= 1,
  na.rm = TRUE
)


# Dentro de 2 desviaciones estandar
real_2 <- sum(
  lbs_sold$z_score >= -2 &
  lbs_sold$z_score <= 2,
  na.rm = TRUE
)


# Dentro de 3 desviaciones estandar
real_3 <- sum(
  lbs_sold$z_score >= -3 &
  lbs_sold$z_score <= 3,
  na.rm = TRUE
)




# ------------------------------------------------------------
# Crear tabla 8d
# ------------------------------------------------------------


tabla_d <- data.frame(
  Intervalo = c(
    "Media +/- 1 desviacion estandar",
    "Media +/- 2 desviaciones estandar",
    "Media +/- 3 desviaciones estandar"
  ),
 
  Porcentaje_Teorico = c(
    "68%",
    "95%",
    "99%"
  ),
 
  Observaciones_Teoricas = round(c(
    teorico_1,
    teorico_2,
    teorico_3
  ), 2),
 
  Observaciones_Reales = c(
    real_1,
    real_2,
    real_3
  )
)




# Mostrar tabla 8d
print(tabla_d)
View(tabla_d)




# Exportar tabla 8d a CSV
write.csv(
  tabla_d,
  "p8d_regla_empirica.csv",
  row.names = FALSE
)




# 8e) - ANALISIS POR INTERVALOS


# Media a +1 desviacion
teorico_media_mas1 <- n * 0.34


# Media a -1 desviacion
teorico_media_menos1 <- n * 0.34


# +1 a +2 desviaciones
teorico_1a2 <- n * 0.135


# -1 a -2 desviaciones
teorico_menos1a2 <- n * 0.135


# +2 a +3 desviaciones
teorico_2a3 <- n * 0.02


# -2 a -3 desviaciones
teorico_menos2a3 <- n * 0.02


# Numero real de observaciones por intervalo
# Media a +1 desviacion
real_media_mas1 <- sum(
  lbs_sold$z_score >= 0 &
  lbs_sold$z_score <= 1,
  na.rm = TRUE
)


# Media a -1 desviacion
real_media_menos1 <- sum(
  lbs_sold$z_score >= -1 &
  lbs_sold$z_score < 0,
  na.rm = TRUE
)


# +1 a +2 desviaciones
real_1a2 <- sum(
  lbs_sold$z_score > 1 &
  lbs_sold$z_score <= 2,
  na.rm = TRUE
)


# -1 a -2 desviaciones
real_menos1a2 <- sum(
  lbs_sold$z_score >= -2 &
  lbs_sold$z_score < -1,
  na.rm = TRUE
)


# +2 a +3 desviaciones
real_2a3 <- sum(
  lbs_sold$z_score > 2 &
  lbs_sold$z_score <= 3,
  na.rm = TRUE
)


# -2 a -3 desviaciones
real_menos2a3 <- sum(
  lbs_sold$z_score >= -3 &
  lbs_sold$z_score < -2,
  na.rm = TRUE
)




# ------------------------------------------------------------
# Crear tabla 8e
# ------------------------------------------------------------


tabla_e <- data.frame(
 
  Intervalo = c(
    "Media a +1 desviacion",
    "Media a -1 desviacion",
    "+1 a +2 desviaciones",
    "-1 a -2 desviaciones",
    "+2 a +3 desviaciones",
    "-2 a -3 desviaciones"
  ),
 
  Porcentaje_Teorico = c(
    "34%",
    "34%",
    "13.5%",
    "13.5%",
    "2%",
    "2%"
  ),
 
  Observaciones_Teoricas = round(c(
    teorico_media_mas1,
    teorico_media_menos1,
    teorico_1a2,
    teorico_menos1a2,
    teorico_2a3,
    teorico_menos2a3
  ), 2),
 
  Observaciones_Reales = c(
    real_media_mas1,
    real_media_menos1,
    real_1a2,
    real_menos1a2,
    real_2a3,
    real_menos2a3
  )
)




# Mostrar tabla 8e
print(tabla_e)
View(tabla_e)




# Exportar tabla 8e a CSV
write.csv(
  tabla_e,
  "p8e_analisis_intervalos.csv",
  row.names = FALSE
)


getwd()


list.files(pattern = "p8")


# 8f)
# Los datos de libras de material vendidas parecen seguir razonablemente
# bien una distribución normal. El histograma muestra una forma aproximadamente
# simétrica y con forma de campana, sin colas extremas evidentes. Esto se confirma
# al revisar la regla empírica: el 69.3% de las observaciones cae dentro de ±1
# desviación estándar de la media (vs. 68% teórico), el 95.2% dentro de ±2 desviaciones
# (vs. 95% teórico) y el 99.3% dentro de ±3 desviaciones (vs. 99% teórico) — en los tres
# casos, los valores reales están muy cerca de los teóricos. En conjunto, tanto la forma
# visual del histograma como el ajuste a la regla empírica sugieren que el supuesto de
# normalidad es razonable para esta variable.


# 8g)




# PUNTO 9: Estadísticos descriptivos, histograma y regla empírica para Lbs Sold
library(moments)


# a) Estadísticos descriptivos
resumen_lbs <- resumen_variable(lbs_sold$LibrasVendidas)
print(round(resumen_lbs, 2))
write.csv(as.data.frame(t(resumen_lbs)), "p9_resumen_lbs_sold.csv", row.names = FALSE)


media_lbs <- resumen_lbs["media"]
sd_lbs    <- resumen_lbs["desv_est"]


# b) Histograma
g_hist_lbs <- ggplot(lbs_sold, aes(x = LibrasVendidas)) +
  geom_histogram(bins = round(sqrt(nrow(lbs_sold))), fill = "steelblue", color = "black") +
  labs(title = "Histograma de Libras Vendidas (2005-2010)",
       x = "Libras vendidas", y = "Frecuencia") +
  theme_minimal()
print(g_hist_lbs)
ggsave("p9_histograma_lbs.png", g_hist_lbs, width = 8, height = 4)


# c) Se describe visualmente comparando con el histograma de Daily Visits del caso


# d) y e) Regla empírica: z-score y conteo por intervalo
lbs_sold <- lbs_sold %>%
  mutate(z = (LibrasVendidas - media_lbs) / sd_lbs)


n_lbs <- nrow(lbs_sold)


tabla_empirica <- data.frame(
  Intervalo          = c("mean ± 1sd", "mean ± 2sd", "mean ± 3sd"),
  Teorico_pct        = c(0.68, 0.95, 0.99),
  Teorico_No_Obs     = round(c(0.68, 0.95, 0.99) * n_lbs, 0),
  Actual_No_Obs      = c(
    sum(abs(lbs_sold$z) <= 1, na.rm = TRUE),
    sum(abs(lbs_sold$z) <= 2, na.rm = TRUE),
    sum(abs(lbs_sold$z) <= 3, na.rm = TRUE)
  )
)
print(tabla_empirica)
write.csv(tabla_empirica, "p9_regla_empirica_lbs.csv", row.names = FALSE)


tabla_detallada <- data.frame(
  Intervalo = c("mean+1sd", "mean-1sd", "1sd a 2sd", "-1sd a -2sd", "2sd a 3sd", "-2sd a -3sd"),
  Teorico_No_Obs = round(c(0.34, 0.34, 0.135, 0.135, 0.02, 0.02) * n_lbs, 0),
  Actual_No_Obs = c(
    sum(lbs_sold$z > 0 & lbs_sold$z <= 1, na.rm = TRUE),
    sum(lbs_sold$z < 0 & lbs_sold$z >= -1, na.rm = TRUE),
    sum(lbs_sold$z > 1 & lbs_sold$z <= 2, na.rm = TRUE),
    sum(lbs_sold$z < -1 & lbs_sold$z >= -2, na.rm = TRUE),
    sum(lbs_sold$z > 2 & lbs_sold$z <= 3, na.rm = TRUE),
    sum(lbs_sold$z < -2 & lbs_sold$z >= -3, na.rm = TRUE)
  )
)
print(tabla_detallada)
write.csv(tabla_detallada, "p9_regla_empirica_detallada_lbs.csv", row.names = FALSE)


# g) Skewness y kurtosis
skew_lbs <- skewness(lbs_sold$LibrasVendidas, na.rm = TRUE)
kurt_lbs <- kurtosis(lbs_sold$LibrasVendidas, na.rm = TRUE) - 3  # -3 para comparar con el "exceso" que da Excel


cat("Skewness Lbs Sold:", round(skew_lbs, 3), "\n")
cat("Kurtosis (exceso) Lbs Sold:", round(kurt_lbs, 3), "\n")


# PUNTO 10 - Paso 1: inspeccionar la hoja Demographics
demograficos <- read_excel(archivo, sheet = "Demographics", skip = 4)
print(names(demograficos))
print(head(demograficos, 10))
