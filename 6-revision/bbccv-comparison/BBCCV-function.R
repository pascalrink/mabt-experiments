BBCCV <- function(sample_size, predictions, num_bootstraps_BBC_CV) {
  out_samples_performances = rep(NA, num_bootstraps_BBC_CV)
  boot_out = rep(NA, num_bootstraps_BBC_CV)
  
  for (current_bootstrap in 1:num_bootstraps_BBC_CV) {
    in_samples_ids <- sample(sample_size, sample_size, replace = TRUE)
    in_samples <- predictions[in_samples_ids, ]
    out_samples <- predictions[-in_samples_ids, ]
    
    best_index <- colMeans(in_samples) %>% which.max
    out_samples_perf <- mean(out_samples[, best_index])
    
    out_samples_performances[current_bootstrap] <- out_samples_perf
    boot_out[current_bootstrap] <- max(colMeans(out_samples)) - out_samples_perf
  }
  
  corrected_performance <- mean(out_samples_performances)
  test_boot <- mean(boot_out)
  
  return(list(corrected_performance = corrected_performance, 
              out_samples_performances = out_samples_performances, 
              test_boot = test_boot))
}