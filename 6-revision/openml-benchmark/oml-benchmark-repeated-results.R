
rm(list = ls())
graphics.off()

library(dplyr)
library(ggplot2)

DisplayResults <- function(multi.2cls.switch, repeated) {
  
  oml <- switch(
    multi.2cls.switch, 
    "multi" = "6-revision/openml-benchmark/openml-benchmark-multi-cls-repeated.RDS", 
    "2cls"  = "6-revision/openml-benchmark/openml-benchmark-2-cls-repeated.RDS") %>% 
    readRDS()
  if (!repeated) oml <- oml[1]
  
  n.1 <- length(oml[[1]])
  n.2 <- length(oml)
  
  mean.results <- list()
  for (i in 1:n.1) {
    lst <- list()
    for (j in 1:n.2) {
      lst <- c(lst, list(oml[[j]][[i]]$df))
    }
    lst.df <- do.call(rbind, lst)
    mean.df <- aggregate(as.numeric(bound) ~ task.id + method + n.classes, lst.df, mean) %>% 
      rename(bound = "as.numeric(bound)")
    mabt <- mean.df$bound[mean.df$method == "mabt"] %>% as.numeric
    bound <- mean.df$bound
    mean.df$abs.diff <- mabt - bound
    mean.df$rel.gain <- mabt / bound - 1
    mean.df$mabt.less.wrong.preds <- 1 - (1-mabt)/(1-bound)
    mean.df$max.abs.diff <- mabt - max(bound[mean.df$method != "mabt"])
    mean.df$max.rel.gain <- max(mean.df$rel.gain[mean.df$method != "mabt"])
    mean.df$max.mabt.less.wrong.preds <- max(mean.df$mabt.less.wrong.preds[mean.df$method != "mabt"])
    mean.results <- c(mean.results, list(mean.df))
  }
  
  mean.results.df <- do.call(rbind, mean.results)
  mean.results.df$method <- recode(mean.results.df$method, 
                                   "clopper-pearson" = "CP", 
                                   "wald" = "Wald", 
                                   "wilson" = "Wilson")
  
  mean.results.count <- lapply(mean.results, function(mr) {
    mr$rank <- rank(1-mr$bound, ties.method = "max")
    y <- expand.grid(mr$rank, mr$rank)
    methods <- mr$method
    n.methods <- length(methods)
    y$method1 <- rep(methods, each = n.methods)
    y$method2 <- rep(methods, times = n.methods)
    y <- y[y$method1 != y$method2, ]
    y$lbtr <- (y$Var1 > y$Var2) * 1.0
    return(y)
  }) %>% do.call(rbind, .)
  
  mean.results.count$comp <- paste0(
    mean.results.count$method1, " > ", mean.results.count$method2)
  subset(mean.results.count, method1 == "mabt") %>% 
    aggregate(lbtr ~ comp, ., sum) %>% print
  
  ### ---
  
  mean.results.df <- subset(mean.results.df, method != "mabt")
  mean.results.df <- mean.results.df[order(mean.results.df$rel.gain, decreasing = TRUE), ]
  
  abs.plot <- ggplot(mean.results.df, aes(x = method, y = abs.diff)) + 
    geom_boxplot() + ylab("absolute difference") + xlab("") + 
    theme(axis.text = element_text(size = 10, color = "black"), 
          text = element_text(size = 10, color = "black"), 
          axis.text.x = element_text(angle = 0))
  rel.plot <- ggplot(mean.results.df, aes(x = method, y = rel.gain)) + 
    geom_boxplot() + ylab("relative gain") + xlab("") + 
    theme(axis.text = element_text(size = 10, color = "black"), 
          text = element_text(size = 10, color = "black"), 
          axis.text.x = element_text(angle = 0))
  wp.plot <- ggplot(mean.results.df, aes(x = method, y = mabt.less.wrong.preds)) + 
    geom_boxplot() + ylab("less wrong predictions") + xlab("") + 
    theme(axis.text = element_text(size = 10, color = "black"), 
          text = element_text(size = 10, color = "black"), 
          axis.text.x = element_text(angle = 0))
  
  if (multi.2cls.switch == "multi" & repeated == FALSE) {
    # rel.gain
    maxval.rel <- 0.2
    far.outs.rel <- mean.results.df %>% filter(rel.gain > maxval.rel) %>%
      group_by(method) %>%
      summarize(far.outs.txt = paste0(round(rel.gain, 2), collapse = "\n"))
    rel.plot <- rel.plot +
      coord_cartesian(ylim = c(-0.03, maxval.rel)) +
      geom_text(data = far.outs.rel,
                aes(x = method, y = maxval.rel, label = far.outs.txt),
                size = 2.8, vjust = 0.7, hjust = -0.2) +
      geom_segment(
        data = far.outs.rel,
        aes(y = maxval.rel * 0.95, yend = maxval.rel, xend = factor(method)),
        arrow = arrow(length = unit(0.2, "cm")))
    
    # abs.diff
    maxval.abs <- 0.2
    far.outs.abs <- mean.results.df %>% filter(abs.diff > maxval.abs) %>%
      group_by(method) %>%
      summarize(far.outs.txt = paste0(round(abs.diff, 2), collapse = "\n"))
    abs.plot <- abs.plot +
      coord_cartesian(ylim = c(-0.01, maxval.abs)) +
      geom_text(data = far.outs.abs,
                aes(x = method, y = maxval.abs, label = far.outs.txt),
                size = 2.8, vjust = 0.7, hjust = -0.2) +
      geom_segment(
        data = far.outs.abs,
        aes(y = maxval.abs * 0.95, yend = maxval.abs, xend = factor(method)),
        arrow = arrow(length = unit(0.2, "cm")))
  }
  
  all.plots <- cowplot::plot_grid(abs.plot, rel.plot, wp.plot, nrow = 1)
  ifelse(repeated, "repeated", "single") %>%
    paste0("6-revision/openml-benchmark/oml-acc-", multi.2cls.switch, "-", ., ".eps") %>%
    cowplot::save_plot(all.plots)
  
  aggregate(abs.diff ~ method, mean.results.df, median) %>% print
  aggregate(rel.gain ~ method, mean.results.df, median) %>% print
  aggregate(mabt.less.wrong.preds ~ method, mean.results.df, median) %>% print
}

DisplayResults(multi.2cls.switch = "2cls", repeated = FALSE)
DisplayResults(multi.2cls.switch = "2cls", repeated = TRUE)
DisplayResults(multi.2cls.switch = "multi", repeated = FALSE)
DisplayResults(multi.2cls.switch = "multi", repeated = TRUE)


