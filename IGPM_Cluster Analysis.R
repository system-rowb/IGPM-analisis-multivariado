# Análisis de conglomerados para las observaciones dentro de un
# Índice de Poder Militar (IPM)
library(ggplot2)
library(tidyverse)
# Semilla de resultados reproducibles
set.seed(491)

# NOTA: Se retoman las variables más representativas y con mayor comunalidad
# ... dentro de la base de datos para seguir con consistencia dentro del 
# ... cluster analysis.
# NOTA: Las variables eliminadas para la estimación óptima del modelo
# de la base original son:
# - "x"
# - "Country"
# - "Active.Reserves"
# - "Paramilitary"
# - "Merchant.Marine.Strength"
# - "Towed.Artillery"

# Carga la base de datos
ipm <- read.csv('Global Fire Power.csv')
names(ipm)
eliminadas <- c(1,2,6,7,12,23)
# Estandarizar los datos para evitar problemas de varianza
#   NOTA: Se toma desde la columna 3 porque las primeras dos son innecesarias
# ... considera la columna de índice y el nombre del país.
ipm.s <- data.frame(scale(ipm[,-eliminadas]))
colnames(ipm.s)
# Verifica que no tenga datos faltantes
sum(is.na(ipm.s))
#   NO hay datos faltantes; Continuamos.

# ===DETERMINAR NÚMERO DE CLUSTERS===
# Determinación de clústers mediante distancia eucladiana y
# método algorítmico para "Elbow"
# Se crea un vector para guardar los datos para la gráfica del Elbow
# Within group sum of squares
wss <- vector()
wss
# Se efectuará un análisis con 15 centroides o clústers.
for (i in 1:15) wss[i] <- sum(kmeans(ipm.s, centers = i)$withinss)
wss
# Se desarrolla una observación en el cambio porcentual de los SS
# weights of Sum squares Porcentual
wsspc <- (wss[1] - wss[2])/wss[1]
for (i in 2:15) {
  wsspc[i] <- (wss[i] - wss[i+1])/wss[i]}
wsspc

# Gráfica el screeplot
# Gráficas
plot(1:15, wss, type="b",
     xlab="Número de clusters",
     ylab="Suma de cuadrados internos por conglomerado",
     main = "Cambio en heterogeneidad") +
  mtext("Fuente: Elaboración propia",
        side = 1, line=3.5, at=1, cex=1) 

barplot(wsspc,
        xlab="Número de Conglomerados",
        ylab="Cambio porcentual",
        main="Cambios % en la Suma de Cuadrados de Conglomerados") +
  mtext("Fuente: Elaboración propia",
        side = 1, line=3.5, at=1, cex=1)

#   CONCLUSIÓN: El método del codo presenta que dos con dos clústers...
# ...puede bastar, sin embargo, el dividirlo en 2 no supone gran diferencia.
# ...Similarmente, hasta 5 clusters parece haber cambio considerable,
# ...aún pasando el codo. De forma similar, se expresa que se muestra un
# ...patrón de repunte cada tres K-clusters. De esta manera, se optma
# ...por un método de conglomerados con 5 centroides.
# ...De forma teórica se presentaria como poder: "Muy bajo", "Bajo", 
# ..."Intermedio", "Alto" y "Muy alto".
# NOTA: Al análizar 5 conglomerados hay algunos clusters con 1 o 3 países;
# entonces para mejorar, de cierta manera, el conglomerado. Se optó
# por solo tener 3 clusters.


# Análisis 5 clusters
ipm.s.c4 <- kmeans(ipm.s, centers = 4, nstart=1)
# Resultados generales
ipm.s.c4
# Muestra solo el tamaño de los conglomerados
ipm.s.c4$size
# Muestra la medida de evalución
ipm.s.c4$betweenss / ipm.s.c5$totss

# Análisis 3 clusters
ipm.s.c3 <- kmeans(ipm.s, centers=3)
ipm.s.c3$size
round(ipm.s.c3$betweenss / ipm.s.c3$totss, 2)

# Análisis de los clusters
ipm.s.cluster3 <- data.frame(ipm$Country, ipm.s.c3$cluster, ipm.s)
ipm.s.cluster3

ipm.s.cluster3.1 <- subset(ipm.s.cluster3, ipm.s.c3$cluster == 1)
ipm.s.cluster3.2 <- subset(ipm.s.cluster3, ipm.s.c3$cluster == 2)
ipm.s.cluster3.3 <- subset(ipm.s.cluster3, ipm.s.c3$cluster == 3)

# Revisa los paises
ipm.s.cluster3.1$ipm.Country
ipm.s.cluster3.2$ipm.Country
ipm.s.cluster3.3$ipm

# Revisa los promedios de los países
apply(ipm.s.cluster3.1[,3:24], 2, mean)
apply(ipm.s.cluster3.2[,3:24], 2, mean)
apply(ipm.s.cluster3.3[,3:24], 2, mean)

importantes <- c('Defense.Budgets','Airports',
                 'Reaching.Military.Age', 'Total.population',
                 'Rocket.Projectors', 'Corvettes')
# Análisis de promedios de clusters por primeras 2 variables más importantes
# de cada factor
# Factor 1: Defense.Budgets & Airports
# Factor 2: Reaching.Military.Age & Total.Population
# Factor 3: Rocket.Projectors & Corvettes
# Factores importantes
# Revisa los promedios de los países
round(apply(ipm.s.cluster3.1[,importantes], 2, mean), 3)
round(apply(ipm.s.cluster3.2[,importantes], 2, mean), 3)
round(apply(ipm.s.cluster3.3[,importantes], 2, mean), 3)


# ===GRÄFICA LOS RESULTADOS===
# Cambiale el nombre de "ipm.Country" a "region" para combinar con 
# el database de mapdata
colnames(ipm.s.cluster3)[1] <- "region"

# Cambia los nombres de los países para que puedan ser coloreadas
ipm.s.cluster3[ipm.s.cluster3 == "United States"] <- "USA"
ipm.s.cluster3[ipm.s.cluster3 == "United Kingdom"] <- "UK"
ipm.s.cluster3[ipm.s.cluster3 == "Czechia"] <- "Czech Republic"
mapdata <- map_data('world')
mapdata <- left_join(mapdata, ipm.s.cluster3[, c('region',
                                                  'ipm.s.c3.cluster')],
                     by="region")

ggplot(data = mapdata, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = ipm.s.c3.cluster)) +
  scale_fill_gradient(name = "IGPM",
                      low="darkgreen", high="lightgreen", na.value="gray") +
  ggtitle('Mapa de propuesta de Conglomerado', 
          subtitle = "Índice Global de Poder Militar (IGPM)") +
  ylab('Latitud') + xlab('Longitud') + 
  labs(caption = "Fuente: Elaboración propia")
# NOTA: Países como Groenlandia, Papúa Nueva Guínea, Belice, y algunos del 
# continente africano com Guínea, Guínea Ecuatorial y Togo ,no cuentan con registros.
