
# CASO 4 - RENT CAR DATASET
# MODELOS DE REGRESION Y ESTACIONALIDAD
# Datos simulados

# 1. Cargar base de datos
rentcar <- read.csv("data/RentCar_Dataset.csv")

# 2. MODELOS PARA VENTAS

# Modelo 1: solo publicidad digital
modelo_ventas_1 <- lm(
  Ventas ~ Amazon_DSP + Search + Sponsored,
  data = rentcar
)

# Modelo 2: publicidad y variables economicas
modelo_ventas_2 <- lm(
  Ventas ~ Amazon_DSP + Search + Sponsored +
    Precio_Promedio + Indice_Demanda,
  data = rentcar
)

# Modelo 3: agregamos controles de estacionalidad
modelo_ventas_3 <- lm(
  Ventas ~ Amazon_DSP + Search + Sponsored +
    Precio_Promedio + Indice_Demanda +
    factor(Mes) + Anio,
  data = rentcar
)

# 3. MODELOS PARA ORDENES

modelo_ordenes_1 <- lm(
  Ordenes ~ Amazon_DSP + Search + Sponsored,
  data = rentcar
)

modelo_ordenes_2 <- lm(
  Ordenes ~ Amazon_DSP + Search + Sponsored +
    Precio_Promedio + Indice_Demanda,
  data = rentcar
)

modelo_ordenes_3 <- lm(
  Ordenes ~ Amazon_DSP + Search + Sponsored +
    Precio_Promedio + Indice_Demanda +
    factor(Mes) + Anio,
  data = rentcar
)

# 4. Revisar resultados
cat("\nMODELOS DE VENTAS\n")

summary(modelo_ventas_1)
summary(modelo_ventas_2)
summary(modelo_ventas_3)

cat("\nMODELOS DE ORDENES\n")

summary(modelo_ordenes_1)
summary(modelo_ordenes_2)
summary(modelo_ordenes_3)

# 5. Comparar R2 ajustado
comparacion <- data.frame(
  Modelo = c(
    "Ventas 1", "Ventas 2", "Ventas 3",
    "Ordenes 1", "Ordenes 2", "Ordenes 3"
  ),
  R2 = c(
    summary(modelo_ventas_1)$r.squared,
    summary(modelo_ventas_2)$r.squared,
    summary(modelo_ventas_3)$r.squared,
    summary(modelo_ordenes_1)$r.squared,
    summary(modelo_ordenes_2)$r.squared,
    summary(modelo_ordenes_3)$r.squared
  ),
  R2_Ajustado = c(
    summary(modelo_ventas_1)$adj.r.squared,
    summary(modelo_ventas_2)$adj.r.squared,
    summary(modelo_ventas_3)$adj.r.squared,
    summary(modelo_ordenes_1)$adj.r.squared,
    summary(modelo_ordenes_2)$adj.r.squared,
    summary(modelo_ordenes_3)$adj.r.squared
  )
)

comparacion$R2 <- round(comparacion$R2, 4)
comparacion$R2_Ajustado <- round(
  comparacion$R2_Ajustado, 4
)

print(comparacion)

# 6. Guardar resultados
dir.create("outputs", showWarnings = FALSE)

write.csv(
  comparacion,
  "outputs/Comparacion_Modelos.csv",
  row.names = FALSE
)

# Guardar resumen de regresiones
sink("outputs/Resultados_Regresiones.txt")

cat("MODELOS DE VENTAS\n\n")
print(summary(modelo_ventas_1))
print(summary(modelo_ventas_2))
print(summary(modelo_ventas_3))

cat("\nMODELOS DE ORDENES\n\n")
print(summary(modelo_ordenes_1))
print(summary(modelo_ordenes_2))
print(summary(modelo_ordenes_3))

sink()

# 7. Grafica de residuos del modelo de ventas
png(
  "outputs/06_Residuos_Ventas.png",
  width = 900,
  height = 650
)

plot(
  fitted(modelo_ventas_3),
  residuals(modelo_ventas_3),
  main = "Residuos del modelo de ventas",
  xlab = "Ventas estimadas",
  ylab = "Residuos",
  col = "steelblue",
  pch = 19
)

abline(h = 0, col = "red", lty = 2)

dev.off()

cat("\nModelos de regresion completados correctamente\n")
