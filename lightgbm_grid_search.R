num_iteraciones <- 200
resultados <- data.frame()

lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "auc",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = 2,
  #max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO
  num_iterations = 9999, # un numero muy grande, lo limita early_stopping_rounds
  
  is_unbalance = TRUE, #
  #scale_pos_weight = 0.43, # scale_pos_weight > 0.0
  
  #drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0,
  
  bagging_fraction =0.8,
  pos_bagging_fraction = 0.7,
  neg_bagging_fraction = 0.9,
  
  extra_trees = TRUE, # Magic Sauce
  
  seed = 28749658
)

for (i in 1:num_iteraciones) {
  param <- list(
    learning_rate = runif(1, 0.01, 0.1),
    num_leaves = sample(20:50, 1),
    feature_fraction = runif(1, 0.5, 1.0),
    min_data_in_leaf = sample(10:50, 1),
    max_depth = sample(1:15, 1)
  )
  
  param <- c(lgb_basicos, param)
  
  modelo <- lgb.train(
    params = param,
    data = dtrain,
    nrounds = 100,
    valids = list(valid = dvalid),
    verbose = -1,
    early_stopping_rounds = 20
  )
  
  # Predecir sobre datos de validación
  predicciones <- predict(modelo, as.matrix(data_test))
  
  # Crear data frame con predicciones y verdad
  df_eval <- data.frame(
    truth = as.factor(label_test),
    estimate = predicciones
  )
  
  # Calcular AUC
  auc <- roc_auc(df_eval, truth, estimate)$.estimate
  
  # Guardar resultados
  resultados <- rbind(resultados, cbind(data.frame(t(param)), auc = auc))
}

# Ver la mejor combinación
mejor <- resultados[which.max(resultados$auc), ]
print(mejor)


# Definir los parámetros del modelo
params <- list(
  objective = "binary",   # Para clasificación binaria
  metric = "auc",         # Usar AUC como métrica
  learning_rate = 0.01,   # Tasa de aprendizaje
  num_leaves = 24,        # Número de hojas en cada árbol
  feature_fraction = 0.95, # Fracción de características a usar
  bagging_fraction = 0.99, # Fracción de muestras a usar
  bagging_freq = 8        # Frecuencia de muestreo para el bagging
)

# Entrenar el modelo
modelo <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = 100   # Número de iteraciones (árboles)
)

# Hacer predicciones con el modelo entrenado
predicciones <- predict(modelo, as.matrix(test_data))

# Evaluar el modelo
clases_predichas <- ifelse(predicciones > 0.7, TRUE, FALSE)
accuracy <- mean(clases_predichas == test_label)
print(accuracy)
