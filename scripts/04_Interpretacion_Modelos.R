# Ejecutar desde el proyecto RentCar-Marketing-Analytics en RStudio.
# Base simulada: los resultados no son evidencia causal real.
datos <- read.csv("data/RentCar_Dataset.csv")
# Los nombres originales del CSV se convierten a minusculas para evitar errores.
names(datos) <- tolower(names(datos))
stopifnot(all(c("ventas", "ordenes", "amazon_dsp", "search", "sponsored", "precio_promedio", "indice_demanda", "mes", "anio") %in% names(datos)))
dir.create("outputs", showWarnings = FALSE)
# COMPAÑERO 1 - COMMIT 1: coeficientes y significancia
modelo <- lm(ventas ~ amazon_dsp + search + sponsored + precio_promedio + indice_demanda + factor(mes) + anio, data = datos)
resultado <- summary(modelo)$coefficients
tabla <- data.frame(variable = rownames(resultado), coeficiente = round(resultado[,1], 4), error_estandar = round(resultado[,2], 4), p_value = round(resultado[,4], 4), significativo_5pct = resultado[,4] < 0.05, row.names = NULL)
write.csv(tabla, "outputs/07_Coeficientes_Ventas.csv", row.names = FALSE)
print(tabla)
cat("Interpretacion: un coeficiente positivo se asocia con mayores ventas, manteniendo las demas variables constantes. Un p-value menor a 0.05 indica significancia estadistica al 5%. No implica causalidad.\n")
