#Paso 1
energia <- c(
  rep("renovable",10),
  rep("no renovable",10)
)
consumo <- c(
  11,34,56,78,44,23,NA,22,89,98
  NA,NA,22,11,47,100,NA,23,98,77
)
costo_kwh <- c(
  rep(10,10),
  rep(20,10)
)
#Paso 2: Limpieza de Datos

mediana_renovable <- median(consumo[energia == "Renovable"], na.rm = TRUE)
mediana_norenovable <- median(consumo[energia == "No renovable"], na.rm = TRUE)
consumo_limpio <- consumo
consumo_limpio[is.na(consumo_limpio) & energia == "Renovable"] <- mediana_renovable
consumo_limpio[is.na(consumo_limpio) & energia == "No renovable"] <- mediana_norenovable

#Paso 3:Dataframe
df <- data.frame(
    energia = energia,
    consumo = consumo_limpio,
    costo_kwh = costo_kwh
)

#Paso 4 : Calculos

df$costo_total = df$consumo * df$costo_kwh

total_consumo <- aggregate(consumo ~ energia, data = df, sum)
total_costo <- aggregate(costo_total ~ energia, data = df, sum)

media_consumo <- aggregate(consumo ~ energia, data = df, mean)

df$ganancia <- df$costo_total * 1.10

#Paso 5 : Resumen

df_ordenado <- df[order(-df$costo_total), ]

top_3_costos <- head(df_ordenado, 3)

resumen_energia <- list(
  dataframe_ordenado = df_ordenado,
  total_consumo = total_consumo,
  total_costo = total_costo,
  top_3_costos = top_3_costos
)

resumen_energia
  
  
  
