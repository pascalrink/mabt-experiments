
rm(list = ls())

library(dplyr)
library(doParallel)

n_cores <- 60
par_clust <- parallel::makeCluster(n_cores, outfile = "")
doParallel::registerDoParallel(par_clust)

n_obs_range <- c(200, 400, 600)
sapply(n_obs_range, function(n_obs) {
  t0 <- Sys.time()
  tuneranger_sim_results <- foreach::foreach(
    seed = 1:2, 
    .packages = c("dplyr", "mlr", "tuneRanger", "ranger")) %dopar% {
      source("5-example/MabtCi-function.R")
      source("6-revision/tuneranger-sim/TuneRangerSim-function.R")
      tuneranger_sim_result <- TuneRangerSim(seed, n_obs)
    }
  saveRDS(tuneranger_sim_results, paste0("6-revision/tunranger-sim/tr-n", n_obs, ".RDS"))
  t1 <- Sys.time()
  t1-t0
})
parallel::stopCluster(par_clust)

### 1000 runs in 4.243 hours
# n_runs <- 120  #500-2000 ~ 6.12h // 959 runs ~ 4.05h
