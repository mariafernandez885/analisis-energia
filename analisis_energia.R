# Paso 1: Configuración inicial
energia <- c(rep("renovable", 10), rep("no renovable", 10))
consumo <- c(120, 100, NA, 90, 150, 130, NA, 85, 95, NA, 
             200, 220, 180, NA, 210, 195, NA, 205, 215, NA)
costo_kwh <- c(rep(0.12, 10), rep(0.15, 10))

# Paso 2: Limpieza de datos
mediana_renovable <- median(consumo[1:10], na.rm = TRUE)
mediana_no_renovable <- median(consumo[11:20], na.rm = TRUE)

consumo[is.na(consumo) & energia == "renovable"] <- mediana_renovable
consumo[is.na(consumo) & energia == "no renovable"] <- mediana_no_renovable

# Paso 3: Creación del dataframe
df_consumo <- data.frame(energia, consumo, costo_kwh)

# Paso 4: Cálculos
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# Cálculo de totales y medias
total_consumo <- tapply(df_consumo$consumo, df_consumo$energia, sum)
total_costo <- tapply(df_consumo$costo_total, df_consumo$energia, sum)
media_consumo <- tapply(df_consumo$consumo, df_consumo$energia, mean)

# Simulación de aumento de precio
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 5: Resumen
df_consumo_ordenado <- df_consumo[order(-df_consumo$costo_total), ]
top_3_costos <- head(df_consumo_ordenado, 3)

resumen_energia <- list(
  "Consumo total por tipo de energía" = total_consumo,
  "Costo total por tipo de energía" = total_costo,
  "Top 3 costos más altos" = top_3_costos
)

# Mostrar el resumen
print(resumen_energia)

