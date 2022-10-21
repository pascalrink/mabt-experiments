Predict <- function(dat, models) {
  arg_pairs <- c("eval",        "refit",
                 "groundtruth", "refit") %>% matrix(ncol = 2, byrow = TRUE)
  
  if ("train" %in% names(models)) {
    arg_pairs <- rbind(c("valid", "train"), arg_pairs)
  } 
  preds <- apply(arg_pairs, 1, function(.ap) {
    .dset <- dat$feats[[.ap[1]]]
    .models <- models[[.ap[2]]]
    as.matrix(.dset) %>% 
      predict(.models, ., s = .models$lambda, type = "response") %>% unname})
  names(preds) <- arg_pairs[, 1]
  return(preds)
}
