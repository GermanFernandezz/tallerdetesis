library(keras)
library(dplyr)
library(caret)
#install.packages("tensorflow")
library(tensorflow)
#install.packages("torch")
library(torch)
library(yardstick) 
library(pROC)
#install.packages("rsample")
library(rsample)

# Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025")

datos <- read.csv("plays_team_red_PHI.csv")

corte <- as.numeric(table(datos$isDropback)[2]/(nrow(datos)))

colnames(datos)
#datos <- datos[,-c(39)]

#datos <- datos[,c(2:14,30:41,66:ncol(datos))] # Modelo 1 kansas
#datos <- datos[,c(2:14,30:ncol(datos))] # Modelo 2 kansas
#datos <- datos[,c(2:ncol(datos))] # Modelo 3 kansas
#datos <- datos[,c(2:41,66:ncol(datos))] # Modelo 4 kansas

datos <- datos[,c(2:14,27:39,64:ncol(datos))] # Modelo 1 phily
#datos <- datos[,c(2:14,27:ncol(datos))] # Modelo 2 phily
#datos <- datos[,c(2:ncol(datos))] # Modelo 3 phily
#datos <- datos[,c(2:39,64:ncol(datos))] # Modelo 4 phily

#Objetos modificables
kansas <- "KC_"
phily <- "PHI_"
modelo1 <- "uno"
modelo2 <- "dos"
modelo3 <- "tres"
modelo4 <- "cuatro"

# nombre del archivo donde guardo los resultados
nombre <- paste0("log_opt_cv_red_profunda_", phily, modelo1, ".csv")

str(datos)
X <- datos[,c(1:(ncol(datos)-1))]
y <- datos$isDropback


# Separo los datos que voy a utilizar, 80%
set.seed(57522978)
idx <- sample(seq_len(nrow(datos)), size = 0.8 * nrow(datos))
idx <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[idx, ]
y_train <- y[idx]
X_test <- X[-idx, ]
y_test <- y[-idx]


# Crear folds
set.seed(123)
cv_folds <- vfold_cv(data = data.frame(X_train, y_train), v = 3)
epochs_vec <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)  # Cantidad de épocas
gotas <- c(0.2,0.3, 0.4,0.5, 0.6)
lerners <- c(0.01, 0.02,0.03,0.05,0.06, 0.1)

#Data frame para guardar resultados
resultados <- data.frame(
  gota1 = numeric(), 
  gota2 = numeric(),
  lr = numeric(), 
  epocas = integer(), 
  exactitud = numeric(), 
  precision = numeric(),
  precision_secundaria = numeric(),
  promedio = numeric()
)

# Loop para ir probando todas las combinaciones de la búsqueda
for(gota in gotas) {
  #configuracion de la red
  model <- nn_module(
    initialize = function(input_dim, 
                          hidden1 = input_dim*2,
                          hidden2 = input_dim*1.5,
                          hidden3 = input_dim*1
    ) {
      self$fc1 <- nn_linear(input_dim, hidden1)
      self$drop1 <- nn_dropout(p = gota) 
      self$fc2 <- nn_linear(hidden1, hidden2)
      self$drop2 <- nn_dropout(p = gota) 
      self$fc3 <- nn_linear(hidden2, hidden3)
      self$drop3 <- nn_dropout(p = gota)
      self$fc4 <- nn_linear(hidden3, 1)
    },
    
    forward = function(x) {
      x %>%
        self$fc1() %>%
        nnf_relu() %>%  
        self$drop1() %>%
        self$fc2() %>%
        nnf_relu() %>% 
        self$drop2() %>%
        self$fc3() %>%
        self$drop3() %>%
        self$fc4() %>%
        nnf_sigmoid()
    }
  )
  
  for(lerner in lerners) {
    for(epocas in epochs_vec) {
      precisiones <- c()
      precisiones_sec <- c()
      exactitudes <- c()
      #Entrenamiento y validacion con 3 cross fold validation
      for (fold in cv_folds$splits) {
        train_data <- analysis(fold)
        val_data <- assessment(fold)
        
        X_tr <- as.matrix(train_data[, 1:ncol(X_train)])
        y_tr <- train_data$y_train
        X_val <- as.matrix(val_data[, 1:ncol(X_train)])
        y_val <- val_data$y_train
        
        # Convertir a tensores
        X_tr <- torch_tensor(X_tr, dtype = torch_float())
        y_tr <- torch_tensor(y_tr, dtype = torch_float())
        X_val <- torch_tensor(X_val, dtype = torch_float())
        y_val <- torch_tensor(y_val, dtype = torch_float())
        
        # Modelo y entrenamiento
        torch_manual_seed(123)
        net <- model(input_dim = ncol(X_tr))
        optimizer <- optim_adam(net$parameters, lr = lerner)
        loss_fn <- nnf_binary_cross_entropy
      
        # Evaluación en fold
        with_no_grad({
          preds <- net(X_val)
          preds <- (preds > corte)$to(dtype = torch_int())
          
          truth <- factor(as_array(y_val), levels = c(0, 1))
          estimate <- factor(as_array(preds), levels = c(0, 1))
          result_df <- data.frame(truth = truth, estimate = estimate)
          
          precision_value <- yardstick::precision(result_df, truth = truth, estimate = estimate, event_level = "second")$.estimate
          precision_value_sec <-  yardstick::precision(result_df, truth = truth, estimate = estimate)$.estimate
          exactitud <- mean(as.numeric(preds) == as.numeric(y_val))
          
          precisiones <- c(precisiones, precision_value)
          precisiones_sec <- c(precisiones_sec, precision_value_sec)
          exactitudes <- c(exactitudes, exactitud)
          
        })
      }  # fin cross-validation
      
      # Promediar resultados de los 3 folds
      resultados <- rbind(resultados, data.frame(
        gota = gota,
        lr = lerner,
        epocas = epocas,
        exactitud = mean(exactitudes),
        precision = mean(precisiones),
        precision_secundaria = mean(precisiones_sec),
        promedio = (mean(exactitudes)+mean(precisiones)+mean(precisiones_sec))/3
        
      ))
      write.csv(resultados, nombre)
    }
  }
  }




