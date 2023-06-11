
rm(list = ls())

library(dplyr)
library(doParallel)
library(mlr)

set.seed(1)

# task.ids <- OpenML::listOMLTasks(
#   number.of.missing.values = 0, tag = "OpenML100",
#   estimation.procedure = "10-fold Crossvalidation")$task.id %>%
#   setdiff(readRDS("6-revision/openml-benchmark-task-ids.RDS"))
# saveRDS(task.ids, "6-revision/openml-benchmark-categ-task-ids.RDS")
task.ids <- readRDS("6-revision/openml-benchmark-categ-task-ids.RDS")

# time.estimate = list()
# for(i in seq_along(task.ids)) {
#   print(i)
#   task <- OpenML::getOMLTask(task.ids[i])
#   task <- OpenML::convertOMLTaskToMlr(task)$mlr.task
#   time.estimate[[i]] <- tuneRanger::estimateTimeTuneRanger(
#     task, num.threads <- 10, num.trees = 2000)
#   print(time.estimate[[i]])
#   saveRDS(time.estimate,
#           file = "6-revision/openml-benchmark-task-time-estimates-categ.RDS")
# }
# time.estimate <- readRDS("0-work/time.estimate.categ.RDS")

source("6-revision/BenchmarkMabt-function.R")

# task.ids <- task.ids[which((unlist(time.estimate)-100)<60)]
t0 <- Sys.time()
benchmark.results <- BenchmarkMabt(1)
t1 <- Sys.time()

saveRDS(benchmark.results, "6-revision/openml-benchmark.RDS")
