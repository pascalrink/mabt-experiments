
rm(list = ls())
graphics.off()

library(dplyr)
library(doParallel)

# Specify simulation parameters here
n_cores <- 47
n_runs <- 5000
n_groundtruth <- 20000
n_feats <- 1000
n_models <- 100
cl_thresh <- 0.5
n_active_feats <- 10
n_folds <- 10

GenTrainEstPerfSelect <- function(
  filename, 
  n_cores, 
  n_runs, 
  feats_type, 
  n_train, 
  n_valid, 
  n_eval, 
  n_groundtruth, 
  n_feats, 
  n_models, 
  cv, 
  measure, 
  ..., 
  n_active_feats, 
  cl_thresh, 
  n_folds) {
  
  par_clust <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(par_clust)
  
  foreach::foreach(seed = seq(n_runs), .packages = c("dplyr")) %dopar% {
    
    source("2-models/data-generation.R")
    source("2-models/model-fitting.R")
    source("2-models/prediction.R")
    source("2-models/performance-estimation.R")
    source("2-models/selection.R")
    
    t0 <- Sys.time()
    set.seed(seed)
    dat <- GenerateData(feats_type, n_train, n_valid, n_eval, n_groundtruth, 
                        n_feats, n_active_feats = n_active_feats)
    
    models <- if (cv) {
      FitModelsCV(dat, n_models, measure, n_folds)
    } else {
      FitModels3(dat, n_models)
    }
    preds <- Predict(dat, models)
    performs <- EstPerforms(
      dat, preds, measure, cl_thresh = cl_thresh, models = models)
    select <- if (cv) {
      SelectCV(performs, models$refit$cvsd, n_folds)
    } else {
      Select3(performs)
    }
    t1 <- Sys.time()
    
    fn <- formatC(seed, width = 5, flag = "0") %>% 
      paste0(filename, "-", ., ".RDS")
    params <- list(
      filename = fn, n_cores = n_cores, n_runs = n_runs, 
      feats_type = feats_type, n_train = n_train, n_valid = n_valid, 
      n_eval = n_eval, n_groundtruth = n_groundtruth, n_feats = n_feats, 
      n_models = n_models, cv = cv, measure = measure, 
      n_active_feats = n_active_feats, cl_thresh = cl_thresh, 
      n_folds = n_folds, seed = seed)
    this <- list(labs = dat$labs$eval, preds = preds$eval, performs = performs, 
                 select = select, params = params)
    saveRDS(this, fn)
  }
  
  parallel::stopCluster(par_clust)
}

####################################################
#################### DO NOT RUN ####################
####################################################

# # 1 ----
# GenTrainEstPerfSelect(
#   filename = "1-savefiles/data/class/normal/n200/nocv/lasso-normal-200-nocv-class",
#   n_cores = n_cores,
#   n_runs = n_runs,
#   feats_type = "normal",
#   n_train = 100,
#   n_valid = 50,
#   n_eval = 50,
#   n_groundtruth = n_groundtruth,
#   n_feats = n_feats,
#   n_models = n_models,
#   cv = FALSE,
#   measure = "class",
#   n_active_feats = 10,
#   cl_thresh = 0.5,
#   n_folds = NA)
# 
# # 2 ----
# GenTrainEstPerfSelect(
#   filename = "1-savefiles/data/class/normal/n200/cv/lasso-normal-200-cv-class",
#   n_cores = n_cores,
#   n_runs = n_runs,
#   feats_type = "normal",
#   n_train = 100,
#   n_valid = 50,
#   n_eval = 50,
#   n_groundtruth = n_groundtruth,
#   n_feats = n_feats,
#   n_models = n_models,
#   cv = TRUE,
#   measure = "class",
#   n_active_feats = 10,
#   cl_thresh = 0.5,
#   n_folds = 10)
# 
# # 3 ----
# GenTrainEstPerfSelect(
#   filename = "1-savefiles/data/class/normal/n400/nocv/lasso-normal-400-nocv-class",
#   n_cores = n_cores,
#   n_runs = n_runs,
#   feats_type = "normal",
#   n_train = 200,
#   n_valid = 100,
#   n_eval = 100,
#   n_groundtruth = n_groundtruth,
#   n_feats = n_feats,
#   n_models = n_models,
#   cv = FALSE,
#   measure = "class",
#   n_active_feats = 10,
#   cl_thresh = 0.5,
#   n_folds = NA)
# 
# # 4 ----
# GenTrainEstPerfSelect(
#   filename = "1-savefiles/data/class/normal/n400/cv/lasso-normal-400-cv-class",
#   n_cores = n_cores,
#   n_runs = n_runs,
#   feats_type = "normal",
#   n_train = 200,
#   n_valid = 100,
#   n_eval = 100,
#   n_groundtruth = n_groundtruth,
#   n_feats = n_feats,
#   n_models = n_models,
#   cv = TRUE,
#   measure = "class",
#   n_active_feats = 10,
#   cl_thresh = 0.5,
#   n_folds = 10)
# 
# # 5 ----
# GenTrainEstPerfSelect(
#   filename = "1-savefiles/data/auc/normal/n400/cv/lasso-normal-400-cv-auc",
#   n_cores = n_cores,
#   n_runs = n_runs,
#   feats_type = "normal",
#   n_train = 200,
#   n_valid = 100,
#   n_eval = 100,
#   n_groundtruth = n_groundtruth,
#   n_feats = n_feats,
#   n_models = n_models,
#   cv = TRUE,
#   measure = "auc",
#   n_active_feats = 10,
#   cl_thresh = NA,
#   n_folds = 10)
# 
# # 6 ----
# GenTrainEstPerfSelect(
#   filename = "1-savefiles/data/class/caret/n200/nocv/lasso-caret-200-nocv-class",
#   n_cores = n_cores,
#   n_runs = n_runs,
#   feats_type = "caret",
#   n_train = 100,
#   n_valid = 50,
#   n_eval = 50,
#   n_groundtruth = n_groundtruth,
#   n_feats = n_feats,
#   n_models = n_models,
#   cv = FALSE,
#   measure = "class",
#   n_active_feats = 10,
#   cl_thresh = 0.5,
#   n_folds = NA)
# 
# # 7 ----
# GenTrainEstPerfSelect(
#   filename = "1-savefiles/data/class/caret/n200/cv/lasso-caret-200-cv-class",
#   n_cores = n_cores,
#   n_runs = n_runs,
#   feats_type = "caret",
#   n_train = 100,
#   n_valid = 50,
#   n_eval = 50,
#   n_groundtruth = n_groundtruth,
#   n_feats = n_feats,
#   n_models = n_models,
#   cv = TRUE,
#   measure = "class",
#   n_active_feats = 10,
#   cl_thresh = 0.5,
#   n_folds = 10)
# 
# # 8 ----
# GenTrainEstPerfSelect(
#   filename = "1-savefiles/data/class/caret/n400/nocv/lasso-caret-400-nocv-class",
#   n_cores = n_cores,
#   n_runs = n_runs,
#   feats_type = "caret",
#   n_train = 200,
#   n_valid = 100,
#   n_eval = 100,
#   n_groundtruth = n_groundtruth,
#   n_feats = n_feats,
#   n_models = n_models,
#   cv = FALSE,
#   measure = "class",
#   n_active_feats = 10,
#   cl_thresh = 0.5,
#   n_folds = NA)
# 
# # 9 ----
# GenTrainEstPerfSelect(
#   filename = "1-savefiles/data/class/caret/n400/cv/lasso-caret-400-cv-class",
#   n_cores = n_cores,
#   n_runs = n_runs,
#   feats_type = "caret",
#   n_train = 200,
#   n_valid = 100,
#   n_eval = 100,
#   n_groundtruth = n_groundtruth,
#   n_feats = n_feats,
#   n_models = n_models,
#   cv = TRUE,
#   measure = "class",
#   n_active_feats = 10,
#   cl_thresh = 0.5,
#   n_folds = 10)
# 
# # 10 ----
# GenTrainEstPerfSelect(
#   filename = "1-savefiles/data/auc/caret/n400/cv/lasso-caret-400-cv-auc",
#   n_cores = n_cores,
#   n_runs = n_runs,
#   feats_type = "caret",
#   n_train = 200,
#   n_valid = 100,
#   n_eval = 100,
#   n_groundtruth = n_groundtruth,
#   n_feats = n_feats,
#   n_models = n_models,
#   cv = TRUE,
#   measure = "auc",
#   n_active_feats = 10,
#   cl_thresh = NA,
#   n_folds = 10)
# 
# # 11 ----
# GenTrainEstPerfSelect(
#   filename = "1-savefiles/data/auc/normal/n600/cv/lasso-normal-600-cv-auc", 
#   n_cores = n_cores, 
#   n_runs = n_runs, 
#   feats_type = "normal", 
#   n_train = 300, 
#   n_valid = 150, 
#   n_eval = 150, 
#   n_groundtruth = n_groundtruth, 
#   n_feats = n_feats, 
#   n_models = n_models, 
#   cv = TRUE, 
#   measure = "auc", 
#   n_active_feats = 10, 
#   cl_thresh = NA, 
#   n_folds = 10)
# 
# # 12 ----
# GenTrainEstPerfSelect(
#   filename = "1-savefiles/data/auc/caret/n600/cv/lasso-caret-600-cv-auc", 
#   n_cores = n_cores, 
#   n_runs = n_runs, 
#   feats_type = "caret", 
#   n_train = 300, 
#   n_valid = 150, 
#   n_eval = 150, 
#   n_groundtruth = n_groundtruth, 
#   n_feats = n_feats, 
#   n_models = n_models, 
#   cv = TRUE, 
#   measure = "auc", 
#   n_active_feats = 10, 
#   cl_thresh = NA, 
#   n_folds = 10)




