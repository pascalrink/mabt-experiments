EstPerforms <- function(dat, preds, measure, ..., cl_thresh, models) {
  
  sets <- names(preds)
  
  if (measure == "class") {
    .Accuracy <- function(.preds, .labs) {
      ((.preds > cl_thresh) * 1.0 == .labs) %>% mean
    }
    performs <- sets %>% lapply(function(.set) {
      apply(preds[[.set]], 2, .Accuracy, dat$labs[[.set]]) %>% unname})
  }
  
  if (measure == "auc") {
    .AUC <- function(.preds, .labs) {
      pROC::auc(c(.labs), .preds)
    }
    performs <- sets %>% lapply(function(.set) {
      apply(preds[[.set]], 2, .AUC, dat$labs[[.set]]) %>% unname})
  }
  
  names(performs) <- sets
  if (!"valid" %in% sets) {
    valid <- models$refit$cvm
    if (measure == "class") valid <- 1-valid
    performs <- c(list(valid = valid), performs)
  }
  return(performs)
}
