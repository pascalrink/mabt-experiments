
Bootstrap <- function(inst, n_boots) {
  resample_dat <- inst$preds
  measure <- inst$params$measure
  labs <- c(inst$labs)
  
  if (measure == "class") {
    resample_dat <- ((resample_dat > inst$params$cl_thresh) == labs) * 1.0
    .ClassBootStat <- function(d, i) as.matrix(d[i, ]) %>% colMeans
    boot <- boot::boot(resample_dat, .ClassBootStat, n_boots)
  }
  if (measure == "auc") {
    .AucBootStat <- function(d, i) {
      as.matrix(d[i, ]) %>% 
        apply(2, function(.preds) pROC::auc(labs[i], .preds))
    }
    boot <- boot::boot(resample_dat, .AucBootStat, n_boots, strata = labs)
  }
  return(boot)
}
