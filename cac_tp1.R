########## Computacion Cientifica Acturial
################## Primer trabajo practico







############## Ejercicio 2

# Imports
library(readxl)
library(dplyr)
library(tidyr)
library("ggplot2")

# Datos
data <- read_excel("Facultad/Computacion Cientifica /retail_data_short.xlsx")
df <- data.frame(data)

class(df)
dim(df)
View(df)
summary(df)

# Definimos PAISES
PAISES <- c(unique(df[c("Country")])$Country)
length(PAISES)

# Rango de la variable cantidad
range(df$Quantity)

# Vemos que tenemos unidades positivas y negativas. Vamos a suponer que representan ventas
# aquellas que tengan signo positivo y devoluciones aquellas con signo negativo.


# Clientes unicos c/ pais
clientesPais <- function(){
  df$CustomerID <- as.numeric(df$CustomerID)
  sum(is.na(df))
  
  uniqUsers <- function(PAIS){
    df_pais <- filter(df, Country== PAIS)
    usrs <- dim(unique(df_pais[c("CustomerID")]))[1]  
    return(usrs)
  }
  
  store = c()
  for(PAIS in PAISES){
    uniqs <- uniqUsers(PAIS)
    store <- c(store, uniqs)
  }
  
  clientes <- data.frame(PAISES, store)
  return(clientes[order(clientes$store, decreasing=TRUE),])
}

clientes_por_pais <- clientesPais()
clientes_por_pais

#### Mejores Clientes 
### Nuestros mejores clientes serán aquellos que tengan mayor cantidad de (compras-devoluciones)

# Mejores clientes c/ país y globales
bestClientesPais = function(){
  
  # Solo datos de aquellos que sabemos quien es el cliente
  df_clientes <- drop_na(df, CustomerID)
  
  # Mejores clientes de un pais
  bestClis <- function(PAIS){
    df_pais = filter(df_clientes, Country == PAIS)
    gdf <- group_by(df_pais, CustomerID)
    totales = summarise(gdf, total=sum(Quantity*UnitPrice))
    return(totales)
  }
  
  # Mejores clientes para cada pais
  store <- c()
  for(PAIS in PAISES){
    bests <- bestClis(PAIS)
    max_val <- max(bests$total)
    best <- bests$CustomerID[bests$total == max_val]
    store <- cbind(store, PAIS, best, max_val)
  }
  
  data_clientes <- data.frame(matrix(data=store,nrow=length(PAISES), ncol=3, byrow=TRUE))
  data_clientes <- rename(data_clientes, Pais=X1, MejorCliente=X2, BalanceCliente=X3)
  return(data_clientes)
}
bestClientesGlobal = function(){
  # Solo datos de aquellos que sabemos quien es el cliente
  df_clientes <- drop_na(df, CustomerID)
  gdf <- group_by(df_clientes, CustomerID)
  totales = summarise(gdf, BalanceCliente=sum(Quantity*UnitPrice))
  totales = totales[order(totales$BalanceCliente,decreasing=TRUE), ]
  return(totales)
}

mejores_clientes_p = bestClientesPais()
mejores_clientes_p
View(mejores_clientes_p)

mejores_clientes_g = bestClientesGlobal()
mejores_clientes_g
View(mejores_clientes_g)

### País con mejores ventas
### Dos análisis uno para ventas netas y otro ventas totales
bestVentas <- function(){
  ### Ventas totales
  ventasTotales <- function(){
    # Solo valores de cantidad positivos (ventas)
    df_ventas <- df[df$Quantity>0, ]
    gdf <- group_by(df_ventas, Country)
    ventasporpais = summarize(gdf, VentasTotales=sum(Quantity)) 
    ventasporpais = ventasporpais[order(ventasporpais$VentasTotales, decreasing=TRUE), ]
    return(ventasporpais)
  }
  ventas_pais_t = ventasTotales()
  
  ### Ventas netas
  ventasNetas <- function(){
    # Conservamos las devoluciones y las neteamos
    gdf <- group_by(df, Country)
    ventasporpais <- summarize(gdf, VentasTotales=sum(Quantity)) 
    ventasporpais <- ventasporpais[order(ventasporpais$VentasTotales, decreasing=TRUE), ]
    ventasporpais <- rename(ventasporpais, VentasNetas=VentasTotales)
    return(ventasporpais)
  }
  ventas_pais_n <- ventasNetas()
  
  ### Porcentaje devoluciones devoluciones/(devoluciones+ventas)
  ratioDevoluciones <- function(){
    store <- c()
    for(PAIS in PAISES){
      aux <- df[df$Country==PAIS, ]
      total <- dim(aux)[1]
      aux <- aux[aux$Quantity < 0, ]
      devoluciones <- dim(aux)[1]
      avg <- devoluciones/total
      store <- c(store, PAIS, avg)
    }
    ratios <- data.frame(matrix(store, nrow=length(PAISES), ncol=2, byrow=T))
    ratios <- rename(ratios, Country=X1, RatioDevolucion=X2)
    return(ratios)
  }
  ratios_devoluciones <- ratioDevoluciones()
  
  # Merge
  analisis_ventas <- merge(ventas_pais_t, ventas_pais_n, ratios_devoluciones, by.x = 'Country' ,by.y = 'Country')
  analisis_ventas <- merge(analisis_ventas, ratios_devoluciones, by.x='Country', by.y='Country')
  analisis_ventas <- analisis_ventas[order(analisis_ventas$VentasTotales, decreasing=T), ]
  
  return(analisis_ventas)
}

analisis_ventas <- bestVentas()
View(analisis_ventas)

### Articulo con más ventas (global, país, top 10 compradores)
whichArt <- function(StockCode){
  return(df[df$StockCode==StockCode, 'Description'][1])
}

bestArticulo <- function(df){
  gdf <- group_by(df, StockCode)
  ventas_art <- summarize(gdf, Ventas=sum(Quantity))
  best_art <- ventas_art[ventas_art$Ventas==max(ventas_art$Ventas), ]
  return(best_art)
}
bestArticuloPais <- function(){
  store <- c()
  for(PAIS in PAISES){
    df_pais <- df[df$Country==PAIS, ]
    articulo <- bestArticulo(df_pais)
    store <- c(store, PAIS, articulo$StockCode[1], articulo$Ventas[1])
  }
  best_arts <- data.frame(matrix(store, nrow=length(PAISES), ncol=3, byrow=T))
  best_arts <- rename(best_arts, Country=X1, StockCode=X2, Ventas=X3)
  best_arts$Ventas <- as.numeric(best_arts$Ventas)
  best_arts <- best_arts[order(best_arts$Ventas, decreasing=TRUE), ]
  return(best_arts)
}
bestArticuloTop10 <- function(){
  mejores_clientes_g <- bestClientesGlobal()
  top_10 <- mejores_clientes_g[0:10, ]
  ids <- mejores_clientes_g[0:10, 'CustomerID']
  df_top10 <- df[df$CustomerID %in%  c(ids$CustomerID), ]
  best_art <- bestArticulo(df_top10)
  return(best_art)
}

best_art_global <- bestArticulo(df)
best_art_pais <- bestArticuloPais()
best_art_top10 <- bestArticuloTop10()

best_art_pais
paste('Mejor articulo global:',whichArt(best_art_global$StockCode[1]))
paste('Mejor articulo top 10:',whichArt(best_art_top10$StockCode[1]))


### Articulo con menos ventas (global, país, top 10 compradores)
worstArticulo <- function(df){
  gdf <- group_by(df, StockCode)
  ventas_art <- summarize(gdf, Ventas=sum(Quantity))
  worst_art <- ventas_art[ventas_art$Ventas==min(ventas_art$Ventas), ]
  return(worst_art)
}
worstArticuloPais <- function(){
  store <- c()
  for(PAIS in PAISES){
    df_pais <- df[df$Country==PAIS, ]
    articulo <- worstArticulo(df_pais)
    store <- c(store, PAIS, articulo$StockCode[1], articulo$Ventas[1])
  }
  worst_arts <- data.frame(matrix(store, nrow=length(PAISES), ncol=3, byrow=T))
  worst_arts <- rename(worst_arts, Country=X1, StockCode=X2, Ventas=X3)
  worst_arts$Ventas <- as.numeric(worst_arts$Ventas)
  worst_arts <- worst_arts[order(worst_arts$Ventas, decreasing=TRUE), ]
  return(worst_arts)
}
bestArticuloTop10 <- function(){
  mejores_clientes_g <- bestClientesGlobal()
  top_10 <- mejores_clientes_g[0:10, ]
  ids <- mejores_clientes_g[0:10, 'CustomerID']
  df_top10 <- df[df$CustomerID %in%  c(ids$CustomerID), ]
  worst_art <- worstArticulo(df_top10)
  return(worst_art)
}

worst_art_global <- worstArticulo(df)
worst_art_pais <- worstArticuloPais()
worst_art_top10 <- worstArticuloTop10()


### Mayor cantidad de devoluciones
masDevoluciones <- function(){
  df_devoluciones <- df[df$Quantity < 0, ]
  gdf <- group_by(df_devoluciones, StockCode)
  mas_devoluciones <- summarise(gdf, Devoluciones=sum(abs(Quantity)))
  mas_devoluciones <- mas_devoluciones[order(mas_devoluciones$Devoluciones, decreasing=TRUE), ]
  return(mas_devoluciones[1, ])
}

mas_devoluciones <- masDevoluciones()
mas_devoluciones
whichArt(mas_devoluciones$StockCode)

### Mayor monto devuelto
montoDevuelto <- function(){
  df_devoluciones <- df[df$Quantity < 0, ]
  df_devoluciones$MontoDevuelto <- df_devoluciones$Quantity*df_devoluciones$UnitPrice
  gdf <- group_by(df_devoluciones, StockCode)
  monto_devoluciones <- summarise(gdf, MontosDevolucion=sum(abs(MontoDevuelto)))
  monto_devoluciones <- monto_devoluciones[order(monto_devoluciones$MontosDevolucion, decreasing = TRUE), ]
  return(monto_devoluciones[1, ])
}

m_montoDevuelto <- montoDevuelto()
m_montoDevuelto
whichArt(m_montoDevuelto$StockCode)



### Top 10 articulos mas caros
masCaros <- function(){
  df_ventas <- df[df$Quantity>0, ]
  gdf <- group_by(df_ventas, StockCode)
  precios <- summarise(gdf, PrecioUnitario=max(UnitPrice))
  precios <- precios[order(precios$PrecioUnitario, decreasing = TRUE), ]
  return(precios[0:10, ])
}

top_mas_caros <- masCaros()
top_mas_caros


### Articulo mayor ingreso (global, pais, top10)
### Contaremos solo lo que ingresa
mayorIngreso <- function(df){
  df_ingreso <- df[df$UnitPrice>0, ]
  gdf <- group_by(df_ingreso, StockCode)
  aux <- summarise(gdf, FlujoBalance=sum(Quantity*UnitPrice))
  aux <- aux[order(aux$FlujoBalance, decreasing = TRUE), ]
  return(aux[1, ])
}
mayorIngresoPais <- function(){
  store <- c()
  for(PAIS in PAISES){
    df_pais <- df[df$Country==PAIS, ]
    articulo <- mayorIngreso(df_pais)
    store <- c(store, PAIS, articulo$StockCode[1], articulo$FlujoBalance[1])
  }
  aux <- data.frame(matrix(store, nrow=length(PAISES), ncol=3, byrow=T))
  aux <- rename(aux, Country=X1, StockCode=X2, FlujoBalance=X3)
  aux$FlujoBalance <- as.numeric(aux$FlujoBalance)
  aux <- aux[order(aux$FlujoBalance, decreasing=TRUE), ]
  return(aux)
}
mayorIngresoTop10 <- function(){
  mejores_clientes_g <- bestClientesGlobal()
  top_10 <- mejores_clientes_g[0:10, ]
  ids <- mejores_clientes_g[0:10, 'CustomerID']
  df_top10 <- df[df$CustomerID %in%  c(ids$CustomerID), ]
  best_art <- mayorIngreso(df_top10)
  return(best_art)
}

mayorIngreso(df)
mayorIngresoTop10()
mayorIngresoPais()



### Distribucion de volumen de ventas a lo largo del día
getHour <- function(fechas){
  store <- c()
  for(i in 1:length(fechas)){
    hora <- substr(fechas[i], 12, 13)
    store <- c(store, hora)
  }
  return(store)
}
ventasHour <- function(){
  df_ventas <- df[df$Quantity>0, ]
  df_ventas$Hora <- getHour(df_ventas$InvoiceDate)
  gdf <- group_by(df_ventas, Hora)
  aux <- summarise(gdf, vol_cash=sum(Quantity*UnitPrice), vol_unit=sum(Quantity))
  return(aux)
}
ventas_p_hora <- ventasHour()


# Graficamos las distribuciones
barplot(ventas_p_hora$vol_cash,
        main = "Volumen de ventas por hora ($)",
        xlab = "Hora",
        ylab = "$",
        names.arg = ventas_p_hora$Hora,
        col = "darkred")

barplot(ventas_p_hora$vol_unit,
        main = "Volumen de ventas por hora (unidades)",
        xlab = "Hora",
        ylab = "Unidades Vendidas",
        names.arg = ventas_p_hora$Hora,
        col = "darkred")










