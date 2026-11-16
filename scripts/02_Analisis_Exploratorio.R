
# CASO 4 - RENT CAR DATASET
# ANALISIS EXPLORATORIO DE DATOS
# Base de datos simulada

# 1. Cargar la base de datos
rentcar <- read.csv("data/RentCar_Dataset.csv")
rentcar$Fecha <- as.Date(rentcar$Fecha)

# 2. Revisar la informacion general
cat("DIMENSIONES DE LA BASE\n")
dim(rentcar)

cat("\nPRIMERAS OBSERVACIONES\n")
head(rentcar)

cat("\nESTRUCTURA DE LAS VARIABLES\n")
str(rentcar)

cat("\nVALORES FALTANTES\n")
colSums(is.na(rentcar))

# 3. Estadisticas descriptivas
variables <- c(
  "Amazon_DSP", "Search", "Sponsored",
  "Precio_Promedio", "Indice_Demanda",
  "Ordenes", "Ventas"
)

estadisticas <- data.frame(
  Variable = variables,
  Media = sapply(rentcar[variables], mean),
  Mediana = sapply(rentcar[variables], median),
  Desviacion = sapply(rentcar[variables], sd),
  Minimo = sapply(rentcar[variables], min),
  Maximo = sapply(rentcar[variables], max)
)

estadisticas[-1] <- round(estadisticas[-1], 2)
rownames(estadisticas) <- NULL

print(estadisticas)

# 4. Matriz de correlaciones
correlaciones <- cor(rentcar[variables])

cat("\nMATRIZ DE CORRELACIONES\n")
print(round(correlaciones, 3))

# 5. Crear carpetas para resultados
dir.create("outputs", showWarnings = FALSE)

# Guardar tablas
write.csv(
  estadisticas,
  "outputs/Estadisticas_Descriptivas.csv",
  row.names = FALSE
)

write.csv(
  correlaciones,
  "outputs/Correlaciones.csv"
)

# 6. Grafica de ventas semanales
png("outputs/01_Ventas_Semanales.png",
    width = 1100, height = 650)

plot(
  rentcar$Fecha, rentcar$Ventas,
  type = "l",
  col = "steelblue",
  lwd = 2,
  main = "Ventas semanales - Rent Car",
  xlab = "Fecha",
  ylab = "Ventas (USD)"
)

dev.off()

# 7. Grafica de ordenes semanales
png("outputs/02_Ordenes_Semanales.png",
    width = 1100, height = 650)

plot(
  rentcar$Fecha, rentcar$Ordenes,
  type = "l",
  col = "darkgreen",
  lwd = 2,
  main = "Ordenes semanales - Rent Car",
  xlab = "Fecha",
  ylab = "Numero de ordenes"
)

dev.off()

# 8. Publicidad digital y ventas
png("outputs/03_Publicidad_Ventas.png",
    width = 1100, height = 650)

par(mfrow = c(1, 3))

plot(rentcar$Amazon_DSP, rentcar$Ventas,
     main = "Amazon DSP",
     xlab = "Inversion (USD)", ylab = "Ventas",
     col = "steelblue", pch = 19)

plot(rentcar$Search, rentcar$Ventas,
     main = "Search",
     xlab = "Inversion (USD)", ylab = "Ventas",
     col = "darkgreen", pch = 19)

plot(rentcar$Sponsored, rentcar$Ventas,
     main = "Sponsored",
     xlab = "Inversion (USD)", ylab = "Ventas",
     col = "darkorange", pch = 19)

dev.off()

# 9. Comparacion de temporada
png("outputs/04_Temporada_Alta.png",
    width = 900, height = 650)

boxplot(
  Ventas ~ Temporada_Alta,
  data = rentcar,
  names = c("Temporada baja", "Temporada alta"),
  col = c("lightblue", "lightgreen"),
  main = "Ventas segun temporada",
  ylab = "Ventas (USD)"
)

dev.off()

# 10. Inversion total en publicidad
rentcar$Publicidad_Total <- (
  rentcar$Amazon_DSP +
    rentcar$Search +
    rentcar$Sponsored
)

png("outputs/05_Inversion_Publicitaria.png",
    width = 1100, height = 650)

plot(
  rentcar$Fecha,
  rentcar$Publicidad_Total,
  type = "l",
  col = "purple",
  lwd = 2,
  main = "Inversion publicitaria semanal",
  xlab = "Fecha",
  ylab = "Inversion total (USD)"
)

dev.off()

cat("\nAnalisis exploratorio completado correctamente\n")
