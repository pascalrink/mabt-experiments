
FitModels3 <- function(dat, n_models) {
  
  train_models <- glmnet::glmnet(
    dat$feats$train, dat$labs$train, family = "binomial", nlambda = n_models)
  
  refit_models <- glmnet::glmnet(
    rbind(dat$feats$train, dat$feats$valid), c(dat$labs$train, dat$labs$valid), 
    family = "binomial", lambda = train_models$lambda)
  
  return(list(train = train_models, refit = refit_models))
}


FitModelsCV <- function(dat, n_models, measure, n_folds) {
  
  refit_models <- rbind(dat$feats$train, dat$feats$valid) %>% 
    as.matrix %>% glmnet::cv.glmnet(
      c(dat$labs$train, dat$labs$valid), family = "binomial", 
      nlambda = n_models, type.measure = measure, nfolds = n_folds)
  
  return(list(refit = refit_models))
}
