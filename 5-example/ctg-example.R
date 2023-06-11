
rm(list=ls())

library(dplyr)
library(doParallel)

source("5-example/GenCompTable-function.R")

# n_cores <- 40
# n_runs <- 100

# Read and prepare in data:
# Note: I initially did a manual export via MS Excel from the original file CTG.xls
#       (3rd sheet 'RAW data') to the 'CTG_raw.csv' file, both files are provided.
data <- readr::read_csv2("5-example/ctg-raw.csv")
data <- data[- which(rowSums(is.na(data))>0), ]
# -> remove 4 incomplete observations, everything else is complete
data <- data %>%
  mutate(Tendency = factor(Tendency),
         CLASS = factor(CLASS),
         NSP = factor(NSP)) %>%
  mutate(Date = (as.Date(Date, format="%d.%m.%Y"))) %>%
  select(-c(FileName, SegFile, b, e)) %>%
  select(-c(A, B, C, D, E, AD, DE, LD, FS, SUSP)) %>%
  arrange(Date) %>%
  select(-Date)

# EDA on endpoints
table(data$CLASS) # -> Class code (1 to 10) for classes A to SUSP
table(data$NSP)   # -> Normal=1; Suspect=2; Pathologic=3
table(data$CLASS, data$NSP)
colSums(is.na(data))

# Goal: prediction of NSP != 1,
# i.e. suspect or pathologic (without CLASS AS PREDICTOR)
dat <- data %>%
  mutate(Y = as.integer(NSP != 1)) %>%
  select(-c(CLASS, NSP))
dat %>% data.table::setcolorder(c("Y", names(dat)[-ncol(dat)]))

# split data: first 75% learning, last 25% evaluation:
indices <- 1:round(nrow(dat)*0.75)
learn <- dat[indices, ]
test  <- dat[-indices, ]

# use dummy coding for factor variables:
dummyMap <- caret::dummyVars(~ . , data=dat, fullRank=TRUE)
learn <- predict(dummyMap, learn)
test <- predict(dummyMap, test)

n_lambda <- 20
n_alpha_tune <- 5
alpha_tune <- seq(0,1, length.out = n_alpha_tune)
n_folds <- 10
eps <- ifelse(nrow(learn)>ncol(learn), 0.0001, 0.01)

CtgExampleAccuracy <- function(seed) {
  
  model.list <- list()
  val.perform <- list()
  val.perform.sd <- list()
  set.seed(seed)
  for (i in 1:n_alpha_tune) {
    
    max.lambda <- glmnet::glmnet(learn[,-1], learn[,1],
                                 family="binomial",
                                 alpha=alpha_tune[i], # alpha=1 <=> LASSO, alpha=0 <=> RIDGE
                                 nlambda=n_lambda)$lambda[1]
    lambda.seq <- seq(max.lambda, eps*max.lambda, length.out = n_lambda)
    
    model.list[[i]] <- glmnet::cv.glmnet(
      learn[, -1], learn[, 1], family = "binomial", alpha = alpha_tune[i],
      lambda = lambda.seq, type.measure = "class", nfolds = n_folds)
    model.list[[i]]$alpha <- alpha_tune[[i]]
    val.perform[[i]] <- 1 - model.list[[i]]$cvm
    val.perform.sd[[i]] <- model.list[[i]]$cvsd
  }
  
  val.perform <- unlist(val.perform)
  val.perform.sd <- unlist(val.perform.sd)
  best.val.mod <- which.max(val.perform)
  within1SE_selection <- which(
    val.perform >= val.perform[best.val.mod] - val.perform.sd[best.val.mod])
  
  preds <- list()
  for (i in 1:length(model.list)) {
    these_models <- model.list[[i]]
    preds[[i]] <- predict(
      these_models, test[, -1], s = these_models$lambda, type = "class") %>%
      unname
  }
  
  preds <- do.call(cbind, preds)
  
  similar_mat <- (preds == test[, 1]) * 1.0
  eval_perform <- colMeans(similar_mat) %>% unname
  
  best.eval.mod <- within1SE_selection[
    which.max(eval_perform[within1SE_selection])]
  best.eval.perform <- eval_perform[best.eval.mod]
  
  val.results <- val.perform[within1SE_selection]
  eval.results <- eval_perform[within1SE_selection]
  
  val.ranks <- rank(1-val.results, ties.method = "first")
  eval.ranks <- rank(1-eval.results, ties.method = "first")
  
  rank_mat <- data.frame(
    model = within1SE_selection, val.rank = val.ranks, eval.rank = eval.ranks)
  rank_mat$delta <- rank_mat$val.rank - rank_mat$eval.rank
  
  results_mat <- cbind(val=val.results, eval=eval.results, rank_mat )
  
  results_mat[, 1] <- round(results_mat[, 1], 4)
  results_mat[, 2] <- round(results_mat[, 2], 4)
  results_mat <- select(results_mat, -c(model, delta))
  
  source("5-example/MabtCi-function.R")
  
  # bootstrap
  within1SE_preds <- (preds[, within1SE_selection] == 1) * 1.0
  within1SE_similar_mat <- similar_mat[, within1SE_selection]
  eval_boot <- boot::boot(
    within1SE_similar_mat, function(x, i) colMeans(x[i, ]), R = 10000)
  alpha <- 0.05
  measure <- "class"
  select_id <- which(best.eval.mod == within1SE_selection)
  cls_thresh <- 0.5
  labs <- test[, 1]
  
  mabt_ci <- MabtCi(
    eval_boot, alpha, measure, select_id, within1SE_preds, cls_thresh, labs)
  
  # standard cis
  n_test <- nrow(preds)
  n_success <- eval_perform[best.val.mod] * n_test
  ci_methods <- c("wilson", "clopper-pearson", "wald")
  default_cis <- DescTools::BinomCI(
    n_success, n_test, conf.level = 1-alpha, sides = "left",
    method = ci_methods)
  
  # Sidak CIs
  n_success_sidak <- eval_perform[best.eval.mod] * n_test
  sidak_alpha <- 1 - (1-alpha)^(1/length(within1SE_selection))
  sidak_cis <- DescTools::BinomCI(
    n_success_sidak, n_test, conf.level = 1-sidak_alpha, sides = "left",
    method = ci_methods)
  rownames(sidak_cis) <- paste0(ci_methods, "-sidak")
  default_cis <- rbind(default_cis, sidak_cis)
  
  rt_obj <- list(mat = results_mat, default = default_cis, mabt = mabt_ci)
  return(rt_obj)
}

# par_clust <- parallel::makeCluster(n_cores)
# doParallel::registerDoParallel(par_clust)
# seeds <- seq(n_runs)
# t0 <- Sys.time()
# result_list <- foreach::foreach(seed = seeds, .packages = c("dplyr")) %dopar% {
#   return(CtgExampleAccuracy(seed))
# }
# t1 <- Sys.time()
# t1-t0
# parallel::stopCluster(par_clust)
# saveRDS(result_list, "5-example/ctg-example-acc.RDS")

## runtime = 2.8 mins on 40 cores

# Comparison of results ----
result_list <- readRDS("5-example/ctg-example-acc.RDS")
comp_results <- GenCompTable(result_list)
comp_results$table
do.call(rbind, comp_results$ci) %>% aggregate(ci ~ method, ., summary)


CtgExampleAccuracy(38) # 38 is THE (new) example  

