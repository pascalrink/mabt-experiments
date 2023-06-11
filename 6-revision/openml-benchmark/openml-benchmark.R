
rm(list = ls())

library(dplyr)
library(doParallel)
library(mlr)

source("6-revision/openml-benchmark-functions.R")
n_cores <- 40

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
  "multi" = readRDS("6-revision/openml-benchmark-categ-task-ids.RDS"), 
  "2cls"  = readRDS("6-revision/openml-benchmark-task-ids.RDS"))

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
  "multi" = readRDS("6-revision/openml-benchmark-time-ests-categ.RDS"), 
  "2cls"  = readRDS("6-revision/openml-benchmark-time-ests.RDS"))

t0 <- Sys.time()
benchmark.results <- BenchmarkMabt(n_cores)
switch(
  multi.2cls.switch, 
  "multi" = "6-revision/openml-benchmark-multi-cls.RDS", 
  "2cls"  = "6-revision/openml-benchmark-2-cls.RDS") %>% 
  saveRDS(benchmark.results, .)
t1 <- Sys.time()
t1-t0

# 2cls benchmark runtime:  49.4 mins / 40 cores
# multi benchmark runtime: 

