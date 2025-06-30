#install.packages("mlrMBO")
#install.packages("DiceKriging")
library(pROC)  # para calcular AUC si no usás evaluación interna
library(lightgbm)
library(mlrMBO)
library(tidyverse)

# Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025")

# Carga de datos
datos <- read.csv("plays_team_KC.csv")
sum(is.na(datos))
datos <- na.omit(datos)
etiquetas <- datos$isDropback
colnames(datos)

#datos <- datos[,c(2:7, 10:13, 38:39)] # modelo 1 kansas
#datos <- datos[,c(2:7, 10:39)] # modelo 2 kansas
#datos <- datos[,c(2:39)] # modelo 3 kansas
datos <- datos[,c(2:13, 38:39)] # modelo 4 kansas

#datos <- datos[,c(2:7, 10:13, 38:39)] # modelo 1 phily
#datos <- datos[,c(2:7, 10:39)] # modelo 2 phily
#datos <- datos[,c(2:39)] # modelo 3 phily

# Objetos modificables
kansas <- "KC_"
phily <- "PHI_"
modelo1 <- "uno"
modelo2 <- "dos"
modelo3 <- "tres"
modelo4 <- "cuatro"

nombre <- paste0("log_bo_lightgbm_", kansas, modelo4, ".csv")
bo_iteraciones <- 150 # iteraciones de la Optimizacion Bayesiana


# Aqui se cargan los hiperparametros que se optimizan
#  en la Bayesian Optimization
bo_lgb <- makeParamSet(
  makeNumericParam("learning_rate", lower = 0.01, upper = 0.2),
  makeNumericParam("feature_fraction", lower = 0.4, upper = 1.0),
  makeIntegerParam("num_leaves", lower = 8L, upper = 50L),
  makeIntegerParam("min_data_in_leaf", lower = 10L, upper = 70L),
  makeIntegerParam("max_depth", lower = 3L, upper = 10L)
  #makeNumericParam("bagging_fraction", lower = 0.5, upper = 1.0),
  #makeNumericParam("pos_bagging_fraction", lower = 0.5, upper = 1.0),
  #makeNumericParam("neg_bagging_fraction", lower = 0.5, upper = 1.0)
)


# Hiperparametros FIJOS de  lightgbm
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
  
  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0,

  bagging_fraction =0.8,
  pos_bagging_fraction = 0.7,
  neg_bagging_fraction = 0.9,
  
  extra_trees = TRUE, # Magic Sauce
  
  seed = 28749658
)


set.seed(57522978)  # Para reproducibilidad

# Proporción de entrenamiento (80%)
train_id <- sample(seq_len(nrow(datos)), size = 0.8 * nrow(datos))

# Crear conjuntos
data_train <- datos[train_id, ]
data_test <- datos[-train_id, ]

label_train <- as.numeric(etiquetas[train_id])
label_test <- as.numeric(etiquetas[-train_id])

# Convertir a dataset de LightGBM
dtrain <- lgb.Dataset(data = as.matrix(data_train), label = label_train)
vtrain <- lgb.Dataset(data = as.matrix(data_test), label = label_test)


# esta función luego se utiliza en la optimización
funcion_optimizar <- function(x) {
  # Combinar parámetros básicos con los optimizables
  param <- c(lgb_basicos, x)
  param$verbosity <- -1
  param$objective <- "binary"
  param$metric <- "auc"
  
  # Ejecutar validación cruzada con early stopping
  cv <- lgb.cv(
    params = param,
    data = dtrain,
    nrounds = 1000,
    nfold = 5,
    early_stopping_rounds = 50,
    verbose = -1,
    stratified = TRUE
  )
  
  # Obtener el mejor AUC (promedio en folds) de la validación cruzada
  best_auc <- max(unlist(cv$record_evals$valid$auc$eval))
  #best_logloss <- min(unlist(cv$record_evals$valid$binary_logloss$eval))
  
  # Guardar en log
  fila_log <- data.frame(
    auc = best_auc,
    learning_rate = x$learning_rate,
    feature_fraction = x$feature_fraction,
    num_leaves = x$num_leaves,
    min_data_in_leaf = x$min_data_in_leaf,
    max_depth = x$max_depth
  )
  
  write.table(
    fila_log,
    file = nombre,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(nombre),
    append = TRUE
  )
  
  # Retornar el score para la optimización (maximizar AUC)
  return(best_auc)
}




# libero espacio
#rm(dataset)
#gc()

# Aqui comienza la configuracion de la Bayesian Optimization

configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar, # la funcion que voy a maximizar
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = FALSE,  # para que no se impaciente Joaquin Tschopp
  par.set = bo_lgb, # definido al comienzo del programa
  has.simple.signature = FALSE # paso los parametros en una lista
)

kbayesiana <- paste0("resultados_bo",nombre, ".RDATA")
# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
  save.on.disk.at.time = 600, # se graba cada 600 segundos
  save.file.path = kbayesiana
) # se graba cada 600 segundos

# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
  ctrl,
  iters = bo_iteraciones
) # cantidad de iteraciones

# defino el método estandar para la creacion de los puntos iniciales,
# los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())


# establezco la funcion que busca el maximo
surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)

# inicio la optimizacion bayesiana
if (!file.exists(kbayesiana)) {
  run <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  run <- mboContinue(kbayesiana) # retomo en caso que ya exista
}


cat("\n\nLa optimizacion Bayesiana ha terminado\n")




