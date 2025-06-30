# Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025")

datos <- read.csv("plays_team_PHI.csv")

#sum(is.na(datos))
#datos <- na.omit(datos)

etiquetas <- datos$isDropback
colnames(datos)
#datos <- datos[,-c(39)]


datos <- datos[,c(2:7,10:13,38:39)] # Modelo 1 kansas
#datos <- datos[,c(2:7,10:39)] # Modelo 2 kansas
#datos <- datos[,c(2:39)] # Modelo 3 kansas
#datos <- datos[,c(2:13,38,39)] # Modelo 4 kansas

modelos <- "modelo_4"
equipo <- "phily_"

nombre <- paste0(equipo, "resultados_lightgbm_", modelos, "_defecto.csv")


datos$offenseFormation <- as.numeric(factor(datos$offenseFormation))
datos$receiverAlignment <- as.numeric(factor(datos$receiverAlignment))
