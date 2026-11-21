# Ejecutar desde el proyecto RentCar-Marketing-Analytics en RStudio.
# Base simulada: los resultados no son evidencia causal real.
datos <- read.csv("data/RentCar_Dataset.csv")
# Los nombres originales del CSV se convierten a minusculas para evitar errores.
names(datos) <- tolower(names(datos))
stopifnot(all(c("ventas", "ordenes", "amazon_dsp", "search", "sponsored", "precio_promedio", "indice_demanda", "mes", "anio") %in% names(datos)))
dir.create("outputs", showWarnings = FALSE)
# COMPAÑERO 1 - COMMIT 2: comparar dos controles de estacionalidad
base <- ventas ~ amazon_dsp + search + sponsored + precio_promedio + indice_demanda + anio
m_sin <- lm(base, data = datos)
m_mes <- lm(ventas ~ amazon_dsp + search + sponsored + precio_promedio + indice_demanda + anio + factor(mes), data = datos)
m_alta <- lm(ventas ~ amazon_dsp + search + sponsored + precio_promedio + indice_demanda + anio + temporada_alta, data = datos)
tabla <- data.frame(estrategia = c("Sin control mensual", "Dummies por mes", "Temporada alta"), r2_ajustado = round(c(summary(m_sin)$adj.r.squared, summary(m_mes)$adj.r.squared, summary(m_alta)$adj.r.squared), 4), AIC = round(c(AIC(m_sin), AIC(m_mes), AIC(m_alta)), 2))
print(tabla)
write.csv(tabla, "outputs/08_Comparacion_Estacionalidad.csv", row.names = FALSE)
cat("Mejor R2 ajustado:", tabla$estrategia[which.max(tabla$r2_ajustado)], "\n")
cat("Nota: comparar tambien supuestos y capacidad predictiva; el mejor R2 no prueba causalidad.\n")
