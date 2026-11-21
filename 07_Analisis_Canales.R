# Ejecutar desde el proyecto RentCar-Marketing-Analytics en RStudio.
# Base simulada: los resultados no son evidencia causal real.
datos <- read.csv("data/RentCar_Dataset.csv")
# Los nombres originales del CSV se convierten a minusculas para evitar errores.
names(datos) <- tolower(names(datos))
stopifnot(all(c("ventas", "ordenes", "amazon_dsp", "search", "sponsored", "precio_promedio", "indice_demanda", "mes", "anio") %in% names(datos)))
dir.create("outputs", showWarnings = FALSE)
# COMPAÑERO 2 - COMMIT 1: comparar canales de publicidad
m <- lm(ventas ~ amazon_dsp + search + sponsored + precio_promedio + indice_demanda + factor(mes) + anio, data = datos)
canales <- c("amazon_dsp", "search", "sponsored")
coefs <- summary(m)$coefficients
tabla <- data.frame(canal = canales, coeficiente = round(coefs[canales,1], 4), p_value = round(coefs[canales,4], 4), gasto_total = round(sapply(canales, function(x) sum(datos[[x]], na.rm = TRUE)), 2))
print(tabla)
write.csv(tabla, "outputs/10_Canales_Publicitarios.csv", row.names = FALSE)
png("outputs/10_Gasto_Canales.png", width = 900, height = 600)
barplot(tabla$gasto_total, names.arg = c("Amazon DSP", "Search", "Sponsored"), main = "Inversion acumulada por canal", ylab = "Gasto total (unidad de la base)", col = "steelblue")
dev.off()
cat("Los coeficientes son asociaciones condicionadas, NO retorno de inversion ni efecto causal.\n")
