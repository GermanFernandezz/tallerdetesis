library(keras)
library(dplyr)
library(caret)
#install.packages("tensorflow")
library(tensorflow)
#install.packages("torch")
library(torch)
library(yardstick) 
library(pROC)

# Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025")

nombre <- paste0("phily_","resultados_red_modelo4_grid", ".csv")
datos <- read.csv("plays_team_red_PHI.csv")
colnames(datos)

#datos <- datos[,c(2:14,30:41,66:ncol(datos))] # Modelo 1 kansas
#datos <- datos[,c(2:14,30:ncol(datos))] # Modelo 2 kansas
#datos <- datos[,c(2:ncol(datos))] # Modelo 3 kansas
#datos <- datos[,c(2:41,66:ncol(datos))] # Modelo 4 kansas

#datos <- datos[,c(2:14,27:39,64:ncol(datos))] # Modelo 1 phily
#datos <- datos[,c(2:14,27:ncol(datos))] # Modelo 2 phily
#datos <- datos[,c(2:ncol(datos))] # Modelo 3 phily
datos <- datos[,c(2:39,64:ncol(datos))] # Modelo 4 phily

epochs <- 20  # Cantidad de épocas
gotita1 <- 0.2
aprendizaje <- 0.05
n_seeds <- 10

exactitudes <- c()
precisiones <- c()
precisiones_sec <- c()
f1_scores <- c()
roc_curves <- c()
set.seed(57522978)
seeds <- sample(1:50000, n_seeds)
preds_list <- list()

corte <- as.numeric(table(datos$isDropback)[2]/(nrow(datos)))

str(datos)
X <- datos[,c(1:(ncol(datos)-1))]
y <- datos$isDropback


# Normalizar las variables (muy importante para redes neuronales)
#X <- scale(X)

# Separar en entrenamiento y test
set.seed(28749658)
idx <- sample(seq_len(nrow(datos)), size = 0.8 * nrow(datos))
idx <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[idx, ]
y_train <- y[idx]
X_test <- X[-idx, ]
y_test <- y[-idx]


X_train <- torch_tensor(as.matrix(X_train), dtype = torch_float())
y_train <- torch_tensor(y_train, dtype = torch_float())

X_test <- torch_tensor(as.matrix(X_test), dtype = torch_float())
y_test <- torch_tensor(y_test, dtype = torch_float())


#set.seed(496)
for (i in seq_along(seeds)) {
  torch_manual_seed(seeds[i])
  model <- nn_module(
    initialize = function(input_dim, 
                          hidden1 = input_dim*2 ,
                          hidden2 = input_dim*1.5,
                          hidden3 = input_dim*1
    ) {
      self$fc1 <- nn_linear(input_dim, hidden1)  # Capa oculta 1 (8 -> 16)
      self$drop1 <- nn_dropout(p = gotita1) 
      self$fc2 <- nn_linear(hidden1, hidden3)  # Capa oculta 2 (16 -> 8)
      self$drop2 <- nn_dropout(p = gotita1) 
      #self$fc3 <- nn_linear(hidden2, hidden3)
      #self$drop3 <- nn_dropout(p = gotita1)
      self$fc4 <- nn_linear(hidden3, 1)   # Capa de salida (8 -> 1)
    },
    
    forward = function(x) {
      x %>%
        self$fc1() %>%
        nnf_relu() %>%  
        self$drop1() %>%
        self$fc2() %>%
        nnf_relu() %>% 
        self$drop2() %>%
        #self$fc3() %>%
        #self$drop3() %>%
        self$fc4() %>%
        nnf_sigmoid()  # Activación sigmoide para salida binaria
    }
  )
  
  net <- model(input_dim = ncol(X_train))
  loss_fn <- nnf_binary_cross_entropy  # Pérdida para clasificación binaria
  optimizer <- optim_adam(net$parameters, lr = aprendizaje)
  for (epoch in 1:epochs) {
    optimizer$zero_grad()  # Resetear gradientes
    y_pred <- net(X_train)  # Forward pass
    loss <- loss_fn(y_pred, y_train$unsqueeze(2))  # Calcular pérdida
    loss$backward()  # Backpropagation
    optimizer$step()  # Actualizar pesos
    train_loss <- loss$item()
    
  }
  
  with_no_grad({
    preds <- as.numeric(net(X_test))})
  #preds <- (preds > corte)$to(dtype = torch_int())})
  preds_list[[i]] <- preds
}

# Promediar las predicciones
preds_matrix <- do.call(cbind, preds_list)
final_preds <- rowMeans(preds_matrix)

# Paso 2: Ordenar y asignar clase 1 al 70% con mayor probabilidad
orden <- order(final_preds, decreasing = TRUE)
n_positivos <- round(corte * length(final_preds))

# Inicializar predicción binaria
clases_predichas <- rep(0, length(final_preds))
clases_predichas[orden[1:n_positivos]] <- 1

label_test <- as.numeric(y_test)

accuracy <- mean(clases_predichas == label_test)
print(accuracy)

data <- tibble(
  truth = factor(label_test),
  estimate = factor(clases_predichas, levels = levels(factor(label_test)))
)

# Calcular la precisión
precision_value <- yardstick::precision(data, truth = truth, estimate = estimate, event_level = "second")
print(precision_value$.estimate)

precision_value_secundario <- yardstick::precision(data, truth = truth, estimate = estimate)

# Calcular el F1-Score
f1_score <- f_meas(data, truth = truth, estimate = estimate, event_level = "second")
print(f1_score$.estimate)

recall_score <- yardstick::recall(data, truth = truth, estimate = estimate, event_level = "second")
print(recall_score$.estimate)

# Calcular la matriz de confusión
matriz_confusion <- table(Predicho = clases_predichas, Real = label_test)
print(matriz_confusion)

# Aquí puedes usar AUC o alguna otra métrica que prefieras
roc_curve <- roc(label_test, clases_predichas, treshholds = 1.0)
curva <- auc(roc_curve)  # AUC
roc_curve_custom <- roc(label_test, clases_predichas, thresholds = seq(0.9, 1, by = 0.01))
plot(roc_curve_custom)
roc_curve_custom

# Matriz de confusión
conf_mat <- confusionMatrix(factor(clases_predichas), factor(label_test), positive = "1")

# Ver el resumen completo
print(conf_mat)

resultados <- data.frame(
  epocas <- epochs,  # Cantidad de épocas
  dropout_1 <- gotita1,
  #dropout_2 <- gotita2,
  lr <- aprendizaje,
  exactitud = accuracy,
  precision = precision_value$.estimate,
  precision_secundario = precision_value_secundario$.estimate,
  f1 = f1_score$.estimate,
  auc = auc(roc_curve))

write.csv(resultados, nombre)
print(matriz_confusion)
