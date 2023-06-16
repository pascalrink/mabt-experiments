
rm(list = ls())

library(dplyr)
library(doParallel)
library(mlr)

source("6-revision/openml-benchmark/openml-benchmark-functions.R")
n.reps <- 10

multi.2cls.switch <- "2cls" # set "2cls" for two class classification problems 
                            # and "multi" for multiclass classification problems

# task.ids <- OpenML::listOMLTasks(
#   number.of.classes = 2, number.of.missing.values = 0, tag = "OpenML100",
#   estimation.procedure = "10-fold Crossvalidation")$task.id
# saveRDS(task.ids, "6-revision/openml-benchmark-task-ids.RDS")
# if (multi.2cls.switch == "multi") {
#   task.ids <- OpenML::listOMLTasks(
#     number.of.missing.values = 0, tag = "OpenML100",
#     estimation.procedure = "10-fold Crossvalidation")$task.id %>% 
#     setdiff(task.ids)
#   saveRDS(task.ids, "6-revision/openml-benchmark-categ-task-ids.RDS")
# }
task.ids <- switch(
  multi.2cls.switch,
  "multi" = readRDS("6-revision/openml-benchmark/openml-benchmark-categ-task-ids.RDS"),
  "2cls"  = readRDS("6-revision/openml-benchmark/openml-benchmark-task-ids.RDS"))
tasks <- switch(
  multi.2cls.switch, 
  "multi" = readRDS("6-revision/openml-benchmark/openml-benchmark-multi-cls-tasks.RDS"), 
  "2cls"  = readRDS("6-revision/openml-benchmark/openml-benchmark-2-cls-tasks.RDS"))

# time.estimate = list()
# for(i in seq_along(task.ids)) {
#   print(i)
#   task <- OpenML::getOMLTask(task.ids[i])
#   task <- OpenML::convertOMLTaskToMlr(task)$mlr.task
#   time.estimate[[i]] <- tuneRanger::estimateTimeTuneRanger(
#     task, num.threads <- 10, num.trees = 2000)
#   print(time.estimate[[i]])
#   switch(multi.2cls.switch, 
#          "multi" = "6-revision/openml-benchmark-time-ests-categ.RDS", 
#          "2cls" = "6-revision/openml-benchmark-time-ests.RDS") %>% 
#     saveRDS(time.estimate, .)
# }
time.estimate <- switch(
  multi.2cls.switch, 
  "multi" = readRDS("6-revision/openml-benchmark/openml-benchmark-time-ests-categ.RDS"), 
  "2cls"  = readRDS("6-revision/openml-benchmark/openml-benchmark-time-ests.RDS"))

task.ids <- task.ids[time.estimate < 10000]

n.cores <- length(task.ids) %>% min(parallel::detectCores()-4)
benchmark.results.list <- list()
runtimes.list <- list()
set.seed(1)
for (run in 1:n.reps) {
  t0 <- Sys.time()
  paste0("Started run # ", run, " of ", n.reps, " at ", Sys.time()) %>% print
  benchmark.results <- BenchmarkMabt(n.cores, tasks)
  benchmark.results.list <- c(benchmark.results.list, list(benchmark.results))
  switch(
    multi.2cls.switch, 
    "multi" = "6-revision/openml-benchmark/openml-benchmark-multi-cls-repeated.RDS", 
    "2cls"  = "6-revision/openml-benchmark/openml-benchmark-2-cls-repeated.RDS") %>% 
    saveRDS(benchmark.results.list, .)
  t1 <- Sys.time()
  runtimes.list <- c(runtimes.list, list(t1-t0))
  Sys.sleep(60)
}

