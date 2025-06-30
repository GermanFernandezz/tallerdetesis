library(tidyr)
library(dplyr)
library(caret)

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025")

# carga de datos
games <- read.csv("games.csv")
plays <- read.csv("plays.csv")
player_play <- read.csv("player_play.csv") 
players <- read.csv("players.csv")

# seleccion de quipo de interés
equipo <- "PHI"

# posiciones ofensivas
ofensivos <- c("QB", "T", "TE", "WR", "G", "RB", "C", "FB")

# unifico gameid y playid en un unico id
plays$game_play_id <- paste0(plays$gameId, plays$playId)
player_play$game_play_id <- paste0(player_play$gameId, player_play$playId)

# selecciono solo los jugadores ofensivos
players_ofensivos <- players[players$position %in% ofensivos,]

# selecciono las jugadas de los jugadores ofensivos
player_play_ofensivos <- player_play[player_play$nflId %in% players_ofensivos$nflId, ]


# join de plays y games
plays <- plays %>%
  right_join(games, by = "gameId")

# Pasar gameclock a decimales
plays$gameClock_mod <- sapply(plays$gameClock, function(x) {
  min_seg <- strsplit(as.character(x), ":")[[1]]
  minutos <- as.numeric(min_seg[1])
  segundos <- as.numeric(min_seg[2])
  # Convertir los segundos a minutos y sumarlos
  total_minutos <- minutos + segundos / 60
  return(total_minutos)
})

# Nueva variable para categorizar gameclock
plays <- plays %>%
  mutate(
    timeCategory = case_when(
      gameClock_mod >= 0 & gameClock_mod < 3  ~ "12-15 min",
      gameClock_mod >= 3 & gameClock_mod <  6 ~ "9-12 min",
      gameClock_mod >= 12 & gameClock_mod <= 15 ~ "0-3 min",
      gameClock_mod >= 9 & gameClock_mod <  12 ~ "3-6 min",
      gameClock_mod >= 6 & gameClock_mod <  9 ~ "6-9 min"
    ),
    timeCategory = factor(timeCategory, levels = c("0-3 min", 
                                                   "3-6 min", 
                                                   "6-9 min",
                                                   "9-12 min",
                                                   "12-15 min")) # Orden lógico
  )

# selecciono jugadas de equipo de interés
plays_team <- plays[plays$possessionTeam == equipo,]

# selecciono jugadas de jugadores ofensivos de equipo de interés
player_play_ofensivos_team <- player_play_ofensivos[player_play_ofensivos$teamAbbr == equipo,]


# agrego nueva variable que indica si el equipo es visitante o local
plays_team <- plays_team %>%
  mutate(
    estadio = case_when(
      homeTeamAbbr == equipo ~ "local",
      homeTeamAbbr != equipo  ~ "visitante"),
    estadio = factor(estadio, levels = c("local", "visitante")) # Orden lógico
  )


# Crear tabla de presencia de jugadores por playId
one_hot <- player_play_ofensivos_team %>%
  distinct(game_play_id, nflId, teamAbbr) %>%  # Eliminar duplicados primero
  mutate(value = 1) %>%        # Crear la columna con 1s después
  pivot_wider(
    names_from = nflId,
    values_from = value,
    values_fill = 0,
    names_prefix = "nflId_"
  )

# agrego jugadores a plays_team
plays_team <- plays_team %>%
  left_join(one_hot, by = "game_play_id")


# Nueva variable para categorizar quien está ganando
plays_team <- plays_team %>%
  mutate(
    diferencia = case_when(
      estadio == "local"  ~ preSnapHomeScore - preSnapVisitorScore,
      estadio == "visitante" ~ preSnapVisitorScore - preSnapHomeScore,
      ))


# Cambio yardlineSide
plays_team <- plays_team %>%
  mutate(
    yardlineSide_mod = case_when(
      yardlineSide == equipo  ~ equipo,
      yardlineSide != equipo ~ "rival",
      is.na(yardlineSide) ~ "centro"
    ))


# seleccion de variables predictores
plays_team <- plays_team[ ,c(1,4,5,6,10,15,19,20,55,60,61,62,64:89,45)]

sum(is.na(plays_team))
plays_team <- na.omit(plays_team)

# para red neuronal
str(plays_team)

# paso a character para luego poder pasar a dummies
plays_team_red <- plays_team %>% mutate(isDropback = ifelse(isDropback == TRUE, 1, 0))
plays_team_red$gameId <- as.character(plays_team_red$gameId)
plays_team_red$gameTimeEastern <- as.character(plays_team_red$gameTimeEastern)
plays_team_red$timeCategory <- as.character(plays_team_red$timeCategory)
plays_team_red$estadio <- as.character(plays_team_red$estadio)

dummies <- dummyVars(isDropback ~ ., data = plays_team_red)
X <- predict(dummies, newdata = plays_team_red) %>% as.data.frame()

str(X)

# escalado 1-0
min_max_scaler <- function(x) {
  if (sum(x) == 0 ) {
    return(0)
  } else if (sum(x) == nrow(X)) {
    return(1)
  } else {
    return((x - min(x)) / (max(x) - min(x)))
  }
}

X_scaled <- as.data.frame(lapply(X, min_max_scaler))
str(X_scaled)

isDropback <- plays_team_red$isDropback
plays_team_red <- cbind(X_scaled, isDropback)
str(plays_team_red)


# guardo datos finales para usar
write.csv(plays_team, paste0("plays_team_",equipo,".csv"))
write.csv(plays_team_red, paste0("plays_team_red_",equipo,".csv"))
