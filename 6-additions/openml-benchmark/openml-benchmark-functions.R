BenchmarkMabt <- function(n_cores, tasks) {
  
  CIsForTask <- function(id) {
    library(tuneRanger)
    source("5-example/MabtCi-function.R")
    
    task1 <- tasks[[which(id == lapply(tasks, function(.task) .task$id))]]$task
    task1 <- task1$mlr.task
    task1.data <- mlr::getTaskData(task1)
    task1.target <- task1$task.desc$target
    
    mixed <- (NROW(task1.data[task1.target]) %>% rnorm) > qnorm(0.9)
    task1.data[mixed, task1.target] <- sample(
      task1.data[mixed, task1.target], replace = FALSE)
    
    task1.n <- nrow(task1.data)
    learn.sample.ids <- sample(task1.n, 0.75 * task1.n)
    task1.test.data <- task1.data[-learn.sample.ids, ]
    test.n <- nrow(task1.test.data)
    task1 <- mlr::changeData(task1, task1.data[learn.sample.ids, ])
    task1.learn.data <- mlr::getTaskData(task1)
    
    learn.models <- tuneRanger::tuneRanger(
      task1, measure = list(acc), build.final.model = FALSE)
    learn.performs <- learn.models$results$acc
    
    preselect.ids <- which(learn.performs > quantile(learn.performs, 0.9))
    if (length(preselect.ids) == 0) preselect.ids <- which(
      learn.performs >= quantile(learn.performs, 0.9))
    learn.models.results <- learn.models$results[
      order(learn.models$results$exec.time, decreasing = FALSE), ]
    
    preselect.models.list <- apply(
      learn.models.results[preselect.ids, ], 1, function(tune.params) {
        rf.model <- ranger::ranger(
          data = task1.learn.data, num.trees = 2000, mtry = tune.params["mtry"],
          min.node.size = tune.params["min.node.size"], 
          sample.fraction = tune.params["sample.fraction"],
          dependent.variable.name = task1.target)})
    
    test.preds <- lapply(preselect.models.list, function(rf.model) {
      preds <- predict(rf.model, task1.test.data)$predictions
      return(preds)}) %>% simplify2array()
    
    test.labels <- task1.test.data[task1.target]
    test.similar.mat <- apply(test.preds, 2, function(test.preds.col) {
      return((test.preds.col == test.labels) * 1.0)})
    
    test.performs <- colMeans(test.similar.mat) %>% as.numeric
    test.best.id <- as.numeric(test.performs) %>% which.max
    
    test.boot <- boot::boot(
      test.similar.mat, 
      function(x, i) as.matrix(x[i, ]) %>% colMeans, R = 10000)
    mabt.ci.result <- MabtCi(
      test.boot, 0.05, "class", test.best.id, test.preds, 0.5, test.labels)
    
    learn.best.id <- which(preselect.ids == which.max(learn.performs))
    default.methods <- c("clopper-pearson", "wald", "wilson")
    default.ci.result <- DescTools::BinomCI(
      test.performs[learn.best.id] * test.n, test.n, conf.level = 0.95,
      sides = "left", method = default.methods)
    
    df <- data.frame(task.id = id, 
                     method = rownames(default.ci.result), 
                     bound = unname(default.ci.result[, 2]))
    df <- rbind(df, c(id, "mabt", mabt.ci.result))
    df$n.classes <- task1.data[task1.target] %>% table %>% length
    return(list(df = df, 
                selects = list(pre = preselect.ids, 
                               learn = learn.best.id, 
                               test = test.best.id), 
                performs = list(learn = learn.performs, 
                                test = test.performs)))
  }
  
  par_clust <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(par_clust)
  benchmark.result <- foreach::foreach(
    id = task.ids, 
    .packages = c("dplyr", "mlr", "tuneRanger", "ranger")) %dopar% {
      t0 <- Sys.time()
      task.result <- CIsForTask(id)
      t1 <- Sys.time()
      task.result <- c(task.result, list(runtime = t1-t0))
      return(task.result)
    }
  parallel::stopCluster(par_clust)
  return(benchmark.result)
}
