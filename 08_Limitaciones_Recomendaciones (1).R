# Ejecutar desde el proyecto RentCar-Marketing-Analytics en RStudio.
# Base simulada: los resultados no son evidencia causal real.
datos <- read.csv("data/RentCar_Dataset.csv")
# Los nombres originales del CSV se convierten a minusculas para evitar errores.
names(datos) <- tolower(names(datos))
stopifnot(all(c("ventas", "ordenes", "amazon_dsp", "search", "sponsored", "precio_promedio", "indice_demanda", "mes", "anio") %in% names(datos)))
dir.create("outputs", showWarnings = FALSE)
# COMPAÑERO 2 - COMMIT 2: limitaciones y recomendaciones prudentes
m <- lm(ventas ~ amazon_dsp + search + sponsored + precio_promedio + indice_demanda + factor(mes) + anio, data = datos)
texto <- c(
"LIMITACIONES DEL CASO RENT CAR",
"1. La base es simulada: no representa resultados reales de una empresa.",
"2. La regresion identifica asociaciones, no demuestra causalidad.",
"3. Puede haber factores omitidos como promociones, competencia y eventos.",
"4. La publicidad puede responder a la demanda, no solo anticiparla.",
"5. Las semanas consecutivas pueden presentar dependencia temporal.",
"6. El R2 ajustado no garantiza buen pronostico fuera de muestra.",
"",
"RECOMENDACIONES",
"1. Validar los resultados con datos reales antes de decidir presupuestos.",
"2. Revisar residuos y errores de prediccion.",
"3. Comparar meses y temporada alta al planear campañas.",
"4. Considerar pruebas A/B para estudiar efectos causales.",
"",
paste("R2 ajustado del modelo de ventas:", round(summary(m)$adj.r.squared, 4))
)
writeLines(texto, "outputs/11_Limitaciones_Recomendaciones.txt")
cat(paste(texto, collapse = "\n"), "\n")
