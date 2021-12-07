# Análisis de Factores con información militar
library(psych)
library(ggplot2)
set.seed(491)

# ===BASE DE DATOS===
# Abre la base de datos
ipm <- read.csv("Global Fire Power.csv")
head(ipm)
# Elimina la columna de "X" y "Country"
ipm <- ipm[, -c(1, 2)]
# Verifica que se haya eliminado
head(ipm)
# Verifica que no hayan valores ausentes
sum(is.na(ipm))
#   No hay valores ausentes; Continuamos
# Escala las unidades para evitar afectaciones en la varianzada
# por diferentes unidades
ipm.s <- data.frame(scale(ipm))
head(ipm.s)
# ===COMPRUEBA COVARIANZA PARA UN ANÁLISIS MULTIVARIADO===
# Análisis Exploratorio
# Prueba de barlett (Basada en normalidad)
bartlett.test(ipm)
#   La prueba de Barlett es significativa. Si hay covarianza suficiente. 
# Prueba de Fligner-Killeen (La normalidad no es necesaria para esta prueba)
fligner.test(ipm)
#   La hipotesis nula es significativa bajo un alpha de 0.05. Si hay covarianza suficiente.
# Prueba Kaiser-Meyer-Olkin (KMO): Measure of Sampling Adequacy (MSA) > .5 para ser una...
# ... muestra individual y conjunta adecuada
ipm.KMO = KMO(ipm.s)
round(ipm.KMO$MSA, 2)
#   Overall MSA = 0.89; Es una muestra adecuada al inicio
which(ipm.KMO$MSAi < 0.5)
#   NO hay variables individuales con MSA < .5; Todas parecen ser adecuadas.
# Consistencia de los datos
psych::alpha(ipm.s)
# raw alpha = 0.97; Excelente
psych::alpha(ipm, check.keys = TRUE)
# raw alpha = 0.97; Excelente

# ===SELECCIÓN DE NÚMERO DE FACTORES===
# Principal Component Analysis
# Principal Component Analysisis (PCA)
ipm.pca <- prcomp(ipm, scale=TRUE)
ipm.s.pca <- prcomp(ipm.s)
summary(ipm.pca)
summary(ipm.s.pca)
# Screeplot
scree(ipm.s)
#   Sugiere, visualmente, 4 factores
# fa.parallel
fa.parallel(ipm.s)
#   Numero de factores = 3; Numero de componentes = 3.
#   Múltiples mensajes de advertencia:
#     - Los ponderadores son probablemente incorrectos. Intenta otra métrica.
#     - Caso ultra-Heywood detectado. Revisa los resultados.
# Factor Analysis con método de rotación = "none"
#   NO corre con nstart=1 ¿Qué es nstart?; Corre con nstart >= 8
ipm.s.fa.none <- factanal(ipm.s, factors=3, nstart=8, rotation="none")
ipm.s.fa.none
# Factor Analysis con método de rotación = "varimax"; corre con nstart >=9
ipm.s.fa.varimax <- factanal(ipm, factors=3, nstart=10, rotation="varimax")
ipm.s.fa.varimax
# Factor Analysis con método de rotación = "promax";
# Favorable en la disciplina psicometria (factor ortogonal)
ipms.fa.promax <- factanal(ipm, factors=3, nstart=6, rotation="promax")
ipms.fa.promax
# Conclusión:
#   Se opta por "varimax" por ser el método de rotación recomendado
#   y por mantener una varianza acumulativa de 0.768

# ===VERIFICACIÓN, ANÁLISIS E INTERPRETACIÓN===
# Comunalidades
which((1 - ipm.s.fa.varimax$uniquenesses) < 0.5)
#   Variables como "Active.Reserves", "Paramilitary" y "Merchant.Marine.S..."
#   muestran poca comunalidad dentro de la base; Se eliminan.

# ===OPTIMIZA MODELO SIN 3 VARIABLES===
names(ipm)
ipm.s2 <- ipm.s[, -c(4,5,21)]
# Factor Analysis con ipm.s2
#   NOTA: NO hubo de necesidad de especificar nstart=" "
ipm.s2.fa.varimax <- factanal(ipm.s2, factors=3, rotation="varimax")
ipm.s2.fa.varimax$loadings
#   Los factores parecen explicar hasta el 83.2% de la varianza acumulada
# Verificar comunuladidad
which((1 - ipm.s2.fa.varimax$uniquenesses) < 0.5)
#   "Towed Artillery" muestra una comunalidad < 0.5; Se elimina y optimiza

# ===OPTIMIZA MODELO SIN 4 VARIABLES===
ipm.s3 <- ipm.s2[, -c(8)]
scree(ipm.s3)
fa.parallel(ipm.s3) + mtext("Fuente: Elaboración propia",
                            side = 1, line=3.5, at=1, cex=1) 
#   Se mantienen los 3 factores y componentes para el análisis, según fa.par
# Factor Analysis con ipm.s3
ipm.s3.fa.varimax <- factanal(ipm.s3, factors=3, rotation="varimax")
ipm.s3.fa.varimax$loadings
#   Los factores parecen explicar hasta el 84.8% de la varianza acumulada
# Verificar comunalidad
which((1 - ipm.s3.fa.varimax$uniquenesses) < 0.5)
#   NO hay variables con comunalidad < 0.5; El modelo ha sido OPTIMIZADO.

# Pruebas finales
# Principal Component Analysis
ipm.s3.pca <- prcomp(ipm.s3)
ipm.s3.pc1 <- ipm.s3.pca$rotation[,1]
ipm.s3.pc2 <- ipm.s3.pca$rotation[,2]
plot(ipm.s3.pca$rotation[,1:2], type="n",
     xlab = "Componente Principal 1",
     ylab = "Componente Principal 2",
     main = "Componentes Principales de la base para IPM") +
  abline(h = median(ipm.s3.pc2), v=median(ipm.s3.pc1)) +
  text(ipm.s3.pca$rotation[,1:2], labels=names(ipm.s3), cex=1)

# KMO ipm.s3 
ipm.s3.KMO <- KMO(ipm.s3)
round(ipm.s3.KMO$MSA, 2)
#   MSA conjunto de 0.9; Es adecuado 
which(ipm.s3.KMO$MSAi < 0.5)
#   NO hay ninguna variable con MSA < 0.5
sort(ipm.s3.KMO$MSAi, decreasing = TRUE)
# alpha de Cronbach
psych::alpha(ipm.s3)
# raw alpha = 0.97; Excelente.
ipm.s3.fa.varimax$loadings
# Ordena los resultados por impacto en factor
fa.sort(ipm.s3.fa.varimax$loadings)
# Crea una lista con el nombre de las variables relevantes
variables <- list('Total.population','Reaching.Military.Age','Active.Service',
                  'Fighters.Interceptors','Attack.Strike','Helicopter.Fleets',
                  'Armored.Fighting.Vehicles','Rocket.Projectors','Submarines',
                  'Frigates','Corvettes','Defense.Budgets',
                  'External.Debt..USD.','Purchasing.Power.Parity..USD',
                  'Reserves.of.Foreign.Exchange..Gold..USD',
                  'Airports','Labor.Force','Railway.Coverage..km',
                  'Roadway.Coverage..km','Oil.Production..bl',
                  'Oil.Consumption..bbl','Square.Land.Area..km2')
# Gráfica los primeros dos factores
plot(ipm.s3.fa.varimax$loadings[,1:2], type="n",
     main = "Análisis de Factores: Índice de Poder Militar (IPM)",
     xlab = "Factor 1: Financiamiento, Infraestructura para la movilidad y fuerza aérea",
     ylab = "Factor 2: Población y riqueza") +
  text(ipm.s3.fa.varimax$loadings[,1:2], labels=names(ipm.s3), cex=.8) +
  abline(h = 0.5, v = 0.5)

#Loadings:
#  Factor1 Factor2 Factor3
# ===FACTOR 1=== (8 variables)
# Financiamiento, Infraestructura para la movilidad y equipo aérea
#Defense.Budgets                           0.962   0.159   0.190  
#Airports                                  0.938                  
#Helicopter.Fleets                         0.912   0.119   0.335  
#External.Debt..USD.                       0.874                  
#Railway.Coverage..km.                     0.832   0.297   0.350  
#Oil.Consumption..bbl.                     0.803   0.432   0.344  
#Roadway.Coverage..km.                     0.795   0.532   0.212  
#Fighters.Interceptors                     0.733   0.358   0.538  

# ===FACTOR 2=== (7 variables)
# Población y riqueza
#Reaching.Military.Age                     0.143   0.974   0.124  
#Total.population                          0.182   0.962   0.194  
#Labor.Force                               0.174   0.934   0.260  
#Purchasing.Power.Parity..USD.             0.601   0.652   0.369  
#Active.Service                            0.355   0.643   0.580  
#Reserves.of.Foreign.Exchange...Gold..USD.         0.628   0.443  
#Frigates                                          0.546   0.492

# ===FACTOR 3=== (8 variables)
# Equipo de ataque y gran alcance
#Rocket.Projectors                         0.142   0.190   0.873  
#Corvettes                                 0.139   0.465   0.757  
#Submarines                                0.478   0.381   0.749  
#Armored.Fighting.Vehicles                 0.567   0.339   0.679  
#Attack.Strike                             0.623   0.199   0.664  
#Square.Land.Area..km2.                    0.368   0.264   0.571  
#Oil.Production..bbl.                      0.506           0.533  

# NOTA: Las variables eliminadas para la estimación óptima del modelo
# de la base original son:
# - "x"
# - "Country"
# - "Active.Reserves"
# - "Paramilitary"
# - "Merchant.Marine.Strength"
# - "Towed.Artillery"
