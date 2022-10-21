
GenerateData <- function(
    feats_type, 
    n_train, 
    n_valid, 
    n_eval, 
    n_groundtruth, 
    n_feats, 
    ..., 
    n_active_feats) {
  
  n_obs <- c("train" = n_train, "valid" = n_valid, "eval" = n_eval, 
             "groundtruth" = n_groundtruth)
  
  if (feats_type == "normal") {
    feats <- lapply(n_obs, function(.n_obs) {
      rnorm(.n_obs * n_feats) %>% matrix(ncol = n_feats)})
    coeffs <- c(rep(1, n_active_feats), rep(0, n_feats - n_active_feats))
    labs <- lapply(feats, function(.feats) {
      (boot::inv.logit(.feats %*% coeffs) > runif(nrow(.feats))) * 1.0})
  }
  
  if (feats_type == "caret") {
    feats <- lapply(n_obs, function(.n_obs) {
      caret::twoClassSim(.n_obs, noiseVars = 500, corrVars = 485, 
                         corrType = "exch", corrValue = 0.8, mislabel = 0.01)})
    labs <- lapply(feats, function(.feats) {(.feats$Class == "Class1") * 1.0})
    feats <- lapply(feats, function(.feats) {subset(.feats, select = -Class)})
  }
  
  return(list(feats = feats, labs = labs))
}