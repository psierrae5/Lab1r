### --------------------------
###  PASO 1
### --------------------------

energia <- c(
  rep("renovable", 10),
  rep("no renovable", 10)
)

consumo <- c(
  11,34,56,78,44,23,NA,22,89,98,
  NA,NA,22,11,47,100,NA,23,98,77
)

costo_kwh <- c(
  rep(10, 10),
  rep(20, 10)
)

### --------------------------
###  PASO 2: Limpieza de Datos
### --------------------------

# Calcular medianas por tipo
mediana_renovable <- median(consumo[energia == "renovable"], na.rm = TRUE)
mediana_no_renovable <- median(consumo[energia == "no renovable"], na.rm = TRUE)

# Crear copia del consumo
consumo_limpio <- consumo

# Reemplazar NA según tipo
consumo_limpio[is.na(consumo_limpio) & energia == "renovable"] <- mediana_renovable
consumo_limpio[is.na(consumo_limpio) & energia == "no renovable"] <- mediana_no_renovable


### --------------------------
###  PASO 3: Crear dataframe
### --------------------------

df_consumo <- data.frame(
  energia = energia,
  consumo = consumo_limpio,
  costo_kwh = costo_kwh
)


### --------------------------
###  PASO 4: Cálculos
### --------------------------

# Costo total por día
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# Totales por tipo
total_consumo <- aggregate(consumo ~ energia, data = df_consumo, sum)
total_costo <- aggregate(costo_total ~ energia, data = df_consumo, sum)

# Media de consumo
media_consumo <- aggregate(consumo ~ energia, data = df_consumo, mean)

# Simulación aumento 10%
df_consumo$ganancia <- df_consumo$costo_total * 1.10


### --------------------------
###  PASO 5: Resumen
### --------------------------

# Orden descendente por costo_total
df_ordenado <- df_consumo[order(-df_consumo$costo_total), ]

# Top 3 de costos
top_3_costos <- head(df_ordenado, 3)

# Lista final solicitada
resumen_energia <- list(
  dataframe_ordenado = df_ordenado,
  total_consumo = total_consumo,
  total_costo = total_costo,
  top_3_costos = top_3_costos
)

resumen_energia

