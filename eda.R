library(gridExtra)
library(grid)
library(ggplot2)
library(dplyr)

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025")

# Cargo archivos
games <- read.csv("games.csv")
plays <- read.csv("plays.csv")

#Selecciono equipos que pasaron a playoffs
finalistas <- c("JAX", "KC", "NYG", "PHI", "CIN", "BUF", "DAL", "SF")

sum(games$homeTeamAbbr %in% finalistas | games$visitorTeamAbbr %in% finalistas)


plays_finalistas <- plays[plays$possessionTeam %in% finalistas,]

# grafico de barras dropback para KC
ggplot(plays %>% filter(possessionTeam == "KC"), aes(isDropback))+
  geom_bar()


# Crear data de equipos finalistas
finalistas_data <- plays %>%
  filter(possessionTeam %in% finalistas) %>%
  mutate(grupo = possessionTeam)

# Crear data de "Todos"
todos_data <- plays %>%
  mutate(grupo = "Todos")

# Unir los datasets
plot_data <- bind_rows(finalistas_data, todos_data)

# Calcular proporciones por grupo
plot_summary <- plot_data %>%
  group_by(grupo, isDropback) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n))

# Gráfico dropback por equipos
ggplot(plot_data, aes(x = grupo, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / 
                                  tapply(after_stat(count), after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                    labels = c("No Dropback", "Dropback"),
                    name = NULL) +
  labs(x = "Equipo", y = "Proporción") +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))


# grafico dropback por cuarto para todos los equipos
ggplot(plays, aes(x = quarter, fill = factor(isDropback)))+
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Cuarto", y = "Proporción") +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))


# grafico dropback por cuarto para KC
ggplot(plays[plays$possessionTeam == "KC",], aes(x = quarter, fill = factor(isDropback)))+
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Cuarto", y = "Proporción") +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))

# grafico dropback por cuarto para PHI
ggplot(plays[plays$possessionTeam == "PHI",], aes(x = quarter, fill = factor(isDropback)))+
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Cuarto", y = "Proporción") +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))

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

# grafico tiempo para todos los equipos
ggplot(plays, aes(x = timeCategory, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Intervalo de tiempo", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20,angle = 45, hjust = 1))


# grafico tiempo para KC
ggplot(plays[plays$possessionTeam == "KC",], aes(x = timeCategory, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Intervalo de tiempo", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20,angle = 45, hjust = 1))

# grafico tiempo para PHI
ggplot(plays[plays$possessionTeam == "PHI",], aes(x = timeCategory, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Intervalo de tiempo", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20,angle = 45, hjust = 1))

# barras para formaciones ofensivas totales
ggplot(plays, aes(x = reorder(offenseFormation, -table(offenseFormation)[offenseFormation]), fill = factor(isDropback))) +
  geom_bar(position = "stack") +
  #scale_y_continuous(labels = percent)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = "Dropback") +
  labs(x = "Formación ofensiva", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20,angle = 45, hjust = 1))

ggplot(plays, aes(x = reorder(offenseFormation, -table(offenseFormation)[offenseFormation]), fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = "Dropback") +
  labs(x = "Formación ofensiva", y = "Proporción") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# barras para formaciones ofensivas KC
ggplot(plays[plays$possessionTeam == "KC",], aes(x = reorder(offenseFormation, -table(offenseFormation)[offenseFormation]), fill = factor(isDropback))) +
  geom_bar(position = "stack") +
  #scale_y_continuous(labels = percent)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = "Dropback") +
  labs(x = "Formación ofensiva", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20,angle = 45, hjust = 1))

ggplot(plays[plays$possessionTeam == "KC",], aes(x = reorder(offenseFormation, -table(offenseFormation)[offenseFormation]), fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = "Dropback") +
  labs(x = "Formación ofensiva", y = "Proporción") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# barras para formaciones ofensivas PHI
ggplot(plays[plays$possessionTeam == "PHI",], aes(x = reorder(offenseFormation, -table(offenseFormation)[offenseFormation]), fill = factor(isDropback))) +
  geom_bar(position = "stack") +
  #scale_y_continuous(labels = percent)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = "Dropback") +
  labs(x = "Formación ofensiva", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20,angle = 45, hjust = 1))

ggplot(plays[plays$possessionTeam == "PHI",], aes(x = reorder(offenseFormation, -table(offenseFormation)[offenseFormation]), fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = "Dropback") +
  labs(x = "Formación ofensiva", y = "Proporción") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(g1, g3, g5, g2, g4,g6, ncol=3,
             top = textGrob("Dropback según Formación ofensiva", vjust = 0.5, hjust = 0.5))


# barras para alineaciones ofensivas totales
ggplot(plays, aes(x = reorder(receiverAlignment, -table(receiverAlignment)[receiverAlignment]), fill = factor(isDropback))) +
  geom_bar(position = "stack") +
  #scale_y_continuous(labels = percent)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = "Dropback") +
  labs(x = "Alineación de receptores", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20,angle = 75, hjust = -0.8, vjust = -0.4))

# barras para alineaciones ofensivas KC
ggplot(plays[plays$possessionTeam == "KC",], aes(x = reorder(receiverAlignment, -table(receiverAlignment)[receiverAlignment]), fill = factor(isDropback))) +
  geom_bar(position = "stack") +
  #scale_y_continuous(labels = percent)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = "Dropback") +
  labs(x = "Alineación de receptores", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20,angle = 75, hjust = -0.8, vjust = -0.4))

# barras para alineaciones ofensivas PHI
ggplot(plays[plays$possessionTeam == "PHI",], aes(x = reorder(receiverAlignment, -table(receiverAlignment)[receiverAlignment]), fill = factor(isDropback))) +
  geom_bar(position = "stack") +
  #scale_y_continuous(labels = percent)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = "Dropback") +
  labs(x = "Alineación de receptores", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20,angle = 75, hjust = -0.8, vjust = -0.4))


# grafico down para todos los equipos
ggplot(plays, aes(x = down, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Down", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))

ggplot(plays, aes(x = down)) +
  geom_bar() +
  labs(x = "Down", y = "Frecuencia", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))

# grafico tiempo para KC
ggplot(plays[plays$possessionTeam == "KC",], aes(x = down, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Down", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))

# grafico tiempo para PHI
ggplot(plays[plays$possessionTeam == "PHI",], aes(x = down, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Down", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))

# join de plays y games
plays <- plays %>%
  right_join(games, by = "gameId")

plays <- plays %>%
  mutate(
    estadio = case_when(
      homeTeamAbbr == possessionTeam ~ "local",
      homeTeamAbbr != possessionTeam  ~ "visitante"),
    estadio = factor(estadio, levels = c("local", "visitante")) # Orden lógico
  )


# grafico estadio para todos los equipos
ggplot(plays, aes(x = estadio, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Estadio", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))

# grafico tiempo para KC
ggplot(plays[plays$possessionTeam == "KC",], aes(x = estadio, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Estadio", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))

# grafico tiempo para PHI
ggplot(plays[plays$possessionTeam == "PHI",], aes(x = estadio, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Estadio", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))


plays <- plays %>%
  mutate(
    diferencia = case_when(
      estadio == "local"  ~ preSnapHomeScore - preSnapVisitorScore,
      estadio == "visitante" ~ preSnapVisitorScore - preSnapHomeScore,
    ))


plays <- plays %>%
  mutate(
    ganando = case_when(
      diferencia > 0 ~ "Ganando",
      diferencia < 0 ~ "Perdiendo",
      diferencia == 0 ~ "Empate"
    ),
    ganando = factor(ganando, levels = c("Ganando", "Perdiendo", "Empate")) # Orden lógico
    )

# grafico estadio para todos los equipos
ggplot(plays, aes(x = ganando, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Resultado parcial", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))

# grafico tiempo para KC
ggplot(plays[plays$possessionTeam == "KC",], aes(x = ganando, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Resultado parcial", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))

# grafico tiempo para PHI
ggplot(plays[plays$possessionTeam == "PHI",], aes(x = ganando, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Resultado parcial", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))


plays <- plays %>%
  mutate(
    posicion = case_when(
      yardlineSide == possessionTeam ~ "Atacante",
      yardlineSide != possessionTeam ~ "Defensivo",
      is.na(yardlineSide) ~ "Centro"),
    estadio = factor(posicion, levels = c("Atacante", "Defensivo", "Centro")) # Orden lógico
  )

# grafico estadio para todos los equipos
ggplot(plays, aes(x = posicion, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Ubicacion en el campo", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))

# grafico tiempo para KC
ggplot(plays[plays$possessionTeam == "KC",], aes(x = posicion, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Ubicacion en el campo", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))

# grafico tiempo para PHI
ggplot(plays[plays$possessionTeam == "PHI",], aes(x = posicion, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Ubicacion en el campo", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20))

