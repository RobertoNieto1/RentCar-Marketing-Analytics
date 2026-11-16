
# CASO 4 - RENT CAR DATASET
# Creacion de base de datos simulada

# 1. Configuracion
set.seed(123)

# 2. Crear fechas semanales
fechas <- seq(
  as.Date("2023-01-02"),
  by = "week",
  length.out = 156
)

n <- length(fechas)

# 3. Variables de tiempo
semana <- seq_len(n)
mes <- as.integer(format(fechas, "%m"))
anio <- as.integer(format(fechas, "%Y"))

temporada_alta <- ifelse(mes %in% c(6, 7, 8, 12), 1, 0)

# 4. Variables economicas y de demanda
precio_promedio <- round(
  65 + 0.04 * semana + 8 * temporada_alta +
    rnorm(n, 0, 4), 2
)

indice_demanda <- round(
  100 + 15 * temporada_alta +
    8 * sin(2 * pi * semana / 52) +
    rnorm(n, 0, 5), 2
)

# 5. Inversion semanal en publicidad (USD)
amazon_dsp <- round(
  pmax(0, 2500 + 600 * temporada_alta +
         5 * semana + rnorm(n, 0, 350)), 2
)

search <- round(
  pmax(0, 3200 + 800 * temporada_alta +
         6 * semana + rnorm(n, 0, 400)), 2
)

sponsored <- round(
  pmax(0, 1800 + 450 * temporada_alta +
         4 * semana + rnorm(n, 0, 250)), 2
)

# 6. Simular ordenes semanales
ordenes <- round(
  180 + 0.012 * amazon_dsp +
    0.020 * search +
    0.015 * sponsored +
    1.5 * indice_demanda -
    1.2 * precio_promedio +
    35 * temporada_alta +
    rnorm(n, 0, 25)
)

ordenes <- pmax(ordenes, 0)

# 7. Simular ventas semanales (USD)
ventas <- round(
  ordenes * precio_promedio * 3 +
    rnorm(n, 0, 1500), 2
)

ventas <- pmax(ventas, 0)

# 8. Construir base final
rentcar <- data.frame(
  Fecha = fechas,
  Semana = semana,
  Anio = anio,
  Mes = mes,
  Temporada_Alta = temporada_alta,
  Amazon_DSP = amazon_dsp,
  Search = search,
  Sponsored = sponsored,
  Precio_Promedio = precio_promedio,
  Indice_Demanda = indice_demanda,
  Ordenes = ordenes,
  Ventas = ventas
)

# 9. Guardar base de datos
dir.create("data", showWarnings = FALSE)

write.csv(
  rentcar,
  "data/RentCar_Dataset.csv",
  row.names = FALSE
)

# 10. Verificar resultados
head(rentcar)
dim(rentcar)
summary(rentcar)

cat("Base de datos creada correctamente\n")
