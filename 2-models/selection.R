
SelectHelper <- function(performs, rule, ..., cvsd, n_folds) {
  best_idx <- which.max(performs)
  best_cvsd <- cvsd[best_idx]
  models <- switch(
    rule, 
    "best" = best_idx, 
    "ten" = union(best_idx, which(performs >= quantile(performs, 0.9))), 
    "se" = which(
      performs >= performs[best_idx] - best_cvsd / sqrt(n_folds)))
  return(list(idx = models, performs = performs[models]))
}

SelectCV <- function(performs, cvsd, n_folds) {
  rules <- c("best", "ten", "se")
  valid_select <- lapply(
    rules, function(.rule) {
      SelectHelper(performs$valid, .rule, cvsd = cvsd, n_folds = n_folds)})
  names(valid_select) <- rules
  eval_select <- lapply(valid_select, function(.preselect) {
    .performs <- performs$eval
    .idx <- .preselect$idx[which.max(.performs[.preselect$idx])]
    return(list(idx = .idx, performs = .performs[.idx]))})
  groundtruth_select <- list(
    best = SelectHelper(performs$groundtruth, "best", cvsd = NA, n_folds = NA))
  return(list(
    valid = valid_select, eval = eval_select, groundtruth = groundtruth_select))
}


Select3 <- function(performs) {
  rules <- c("best", "ten")
  valid_select <- lapply(
    rules, function(.rule) {
      SelectHelper(performs$valid, .rule, cvsd = NA, n_folds = NA)})
  names(valid_select) <- rules
  eval_select <- lapply(valid_select, function(.preselect) {
    .performs <- performs$eval
    .idx <- .preselect$idx[which.max(.performs[.preselect$idx])]
    return(list(idx = .idx, performs = .performs[.idx]))})
  groundtruth_select <- list(
    best = SelectHelper(performs$groundtruth, "best", cvsd = NA, n_folds = NA))
  return(list(
    valid = valid_select, eval = eval_select, groundtruth = groundtruth_select))
}
