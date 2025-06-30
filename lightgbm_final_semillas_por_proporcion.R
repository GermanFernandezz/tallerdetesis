#install.packages("lightgbm")
library(lightgbm)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("yardstick")  # Instalar el paquete (si no lo tienes)
library(yardstick)  # Cargar el paquete
#install.packages("caret")
library(caret)
#install.packages("pROC")
library(pROC)



corte <- as.numeric(table(etiquetas)[2]/length(etiquetas))

# Proporción de entrenamiento (80%)
set.seed(28749658) # Para reproducibilidad
train_id <- sample(seq_len(nrow(datos)), size = 0.8 * nrow(datos))
train_id <- createDataPartition(etiquetas, p = 0.8, list = FALSE)

# Crear conjuntos
data_train <- datos[train_id, ]
data_test <- datos[-train_id, ]

label_train <- as.numeric(etiquetas[train_id])
label_test <- as.numeric(etiquetas[-train_id])

# Convertir a dataset de LightGBM
dtrain <- lgb.Dataset(data = as.matrix(data_train), label = label_train)
vtrain <- lgb.Dataset(data = as.matrix(data_test), label = label_test)

# Definir los parámetros del modelo
#params <- c(params_sinopt, params_opt)

# Supongamos que tenés: dtrain, dtest (de tipo lgb.Dataset) y test_matrix (data.frame o matrix)
n_seeds <- 5
seeds <- sample(1:9999, n_seeds)
preds_list <- list()

for (i in seq_along(seeds)) {
  param <- list(
    params,
    seed = seeds[i]
  )
  
  model <- lgb.train(
    params = param,
    data = dtrain,
    nrounds = 100,
    #valids = list(test = vtrain),
    #early_stopping_rounds = 10,
    verbose = 0
  )
  
  preds <- predict(model, as.matrix(data_test))
  preds_list[[i]] <- preds
}

# Promediar las predicciones
preds_matrix <- do.call(cbind, preds_list)
predicciones <- rowMeans(preds_matrix)


# Paso 2: Ordenar y asignar clase 1 al 70% con mayor probabilidad
orden <- order(predicciones, decreasing = TRUE)
n_positivos <- round(corte * length(predicciones))

# Inicializar predicción binaria
clases_predichas <- rep(0, length(predicciones))
clases_predichas[orden[1:n_positivos]] <- 1

accuracy <- mean(clases_predichas == label_test)
print(accuracy)

data <- tibble(
  truth = factor(label_test),
  estimate = factor(clases_predichas, levels = levels(factor(label_test))),
  probabilidad = predicciones
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

resultados <- data.frame(#feat_frac = params_opt$feature_fraction,
                         #lr = params_opt$learning_rate,
                         #num_leave = params_opt$num_leaves,
                         #min_data_in_leaf = params_opt$min_data_in_leaf,
                         #max_depth = params_opt$max_depth,
                         exactitud = accuracy,
                         precision = precision_value$.estimate,
                         precision_secundario = precision_value_secundario$.estimate,
                         f1 = f1_score$.estimate,
                         auc = auc(roc_curve))

write.csv(resultados, nombre)
