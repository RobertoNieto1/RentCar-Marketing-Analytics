# Ejecutar desde el proyecto RentCar-Marketing-Analytics en RStudio.
# Base simulada: los resultados no son evidencia causal real.
datos <- read.csv("data/RentCar_Dataset.csv")
# Los nombres originales del CSV se convierten a minusculas para evitar errores.
names(datos) <- tolower(names(datos))
stopifnot(all(c("ventas", "ordenes", "amazon_dsp", "search", "sponsored", "precio_promedio", "indice_demanda", "mes", "anio") %in% names(datos)))
dir.create("outputs", showWarnings = FALSE)
# COMPAÑERO 2 - COMMIT 3: resumen reproducible para entrega
m1 <- lm(ventas ~ amazon_dsp + search + sponsored, data = datos)
m2 <- lm(ventas ~ amazon_dsp + search + sponsored + precio_promedio + indice_demanda, data = datos)
m3 <- lm(ventas ~ amazon_dsp + search + sponsored + precio_promedio + indice_demanda + factor(mes) + anio, data = datos)
o1 <- lm(ordenes ~ amazon_dsp + search + sponsored, data = datos)
o2 <- lm(ordenes ~ amazon_dsp + search + sponsored + precio_promedio + indice_demanda, data = datos)
o3 <- lm(ordenes ~ amazon_dsp + search + sponsored + precio_promedio + indice_demanda + factor(mes) + anio, data = datos)
modelos <- list(m1,m2,m3,o1,o2,o3)
comparacion <- data.frame(modelo = c("Ventas 1", "Ventas 2", "Ventas 3", "Ordenes 1", "Ordenes 2", "Ordenes 3"), r2_ajustado = round(sapply(modelos, function(x) summary(x)$adj.r.squared), 4))
lineas <- c("INFORME RESUMEN - RENT CAR MARKETING", "", "Objetivo: estudiar la relacion entre publicidad digital, ventas y ordenes.", "Datos: 156 semanas simuladas. No corresponden a una empresa real.", "", "MODELOS Y RESULTADOS:", capture.output(print(comparacion, row.names = FALSE)), "", "METODO: regresion lineal multiple con controles economicos y estacionales.", "", "INTERPRETACION: el R2 ajustado compara capacidad explicativa dentro de la muestra.", "Los coeficientes muestran asociaciones manteniendo constantes otras variables.", "", "LIMITACIONES: datos simulados, variables omitidas y posible dependencia temporal.", "RECOMENDACIONES: validar con datos reales y experimentos antes de invertir.", "", "ARCHIVOS: revisar tablas y graficas de la carpeta outputs.")
writeLines(lineas, "outputs/12_Informe_Resumen.txt")
cat(paste(lineas, collapse = "\n"), "\n")
