# Ejecutar desde el proyecto RentCar-Marketing-Analytics en RStudio.
# Base simulada: los resultados no son evidencia causal real.
datos <- read.csv("data/RentCar_Dataset.csv")
# Los nombres originales del CSV se convierten a minusculas para evitar errores.
names(datos) <- tolower(names(datos))
stopifnot(all(c("ventas", "ordenes", "amazon_dsp", "search", "sponsored", "precio_promedio", "indice_demanda", "mes", "anio") %in% names(datos)))
dir.create("outputs", showWarnings = FALSE)
# COMPAÑERO 1 - COMMIT 3: diagnostico sencillo
m <- lm(ventas ~ amazon_dsp + search + sponsored + precio_promedio + indice_demanda + factor(mes) + anio, data = datos)
res <- residuals(m)
cat("Error medio absoluto:", round(mean(abs(res)), 2), "\n")
cat("Raiz del error cuadratico medio:", round(sqrt(mean(res^2)), 2), "\n")
cat("R2 ajustado:", round(summary(m)$adj.r.squared, 4), "\n")
png("outputs/09_Diagnostico_Residuos.png", width = 1000, height = 500)
par(mfrow = c(1, 2))
plot(fitted(m), res, main = "Residuos vs estimados", xlab = "Ventas estimadas", ylab = "Residuos", pch = 19, col = "steelblue")
abline(h = 0, lty = 2, col = "red")
qqnorm(res, main = "Normalidad de residuos", pch = 19)
qqline(res, col = "red")
par(mfrow = c(1, 1))
dev.off()
write.csv(data.frame(venta_estimada = fitted(m), residuo = res), "outputs/09_Residuos_Detalle.csv", row.names = FALSE)
cat("Revisar patrones, dispersion y desviaciones de la diagonal antes de confiar en inferencias.\n")
