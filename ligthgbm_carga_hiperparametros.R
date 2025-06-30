params_sinopt <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "auc",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  #max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO
  num_iterations = 9999, # un numero muy grande, lo limita early_stopping_rounds
  
  is_unbalance = TRUE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0
  
  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
  
  bagging_fraction =0.8,
  pos_bagging_fraction = 0.7,
  neg_bagging_fraction = 0.9,
  
  extra_trees = TRUE, # Magic Sauce
  
  seed = 28749658
)
#parametros optimizables
params_opt <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "auc",
  max_bin = 20,
  learning_rate = 0.01,
  num_leaves = 15,
  feature_fraction = 0.8,
  min_data_in_leaf = 5,
  max_depth = 10
)

params <- c(params_sinopt, 
            params_opt)

#params$early_param$early_stopping_rounds <- 10
