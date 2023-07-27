
# This script generates the plots shown in the Supplement Document

rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)
library(ggpattern)

# specify the directory of the confidence interval results here
cis_dir <- "1-savefiles/cis/"

# specify the directory for the results plots here
plot_dir <- "4-plots/figures/supplement/"

source("4-plots/myggsave-function.R")

# returns all the result plots to a specific simulation parameter configuration
# Plots <- function(fpath, plot_path, plot_name) {
Plots <- function(fpath, plot_path, run_count) {
  flist <- list.files(fpath)
  df <- lapply(
    flist, function(.file) paste0(fpath, .file) %>% readRDS) %>% 
    do.call(rbind, .)
  
  n_colors <- table(df$rule) %>% length
  my_colors <- RColorBrewer::brewer.pal(3, "Pastel1")
  my_colors <- my_colors[1:n_colors]
  
  # Formatting ----
  df$method <- factor(df$method, c("bt", "clopper-pearson", "delong", 
                                   "hanley-mcneal", "mabt", "wald", "wilson"))
  df$method <- recode(
    df$method, "bt" = "BT", "clopper-pearson" = "CP", "delong" = "DeLong", 
    "hanley-mcneal" = "HM", "mabt" = "MABT", "wald" = "Wald", 
    "wilson" = "Wilson")
  df$rule <- recode(
    df$rule, "best" = "single best", "ten" = "top 10%", "se" = "within 1 SE")
  df$feats_type <- recode(
    df$feats_type, "caret" = "feature case B", "normal" = "feature case A")
  df$cv <- factor(df$cv) %>% recode("TRUE" = "CV validation performance", 
                                    "FALSE" = "non-CV validation performance")
  df$measure <- recode(df$measure, "auc" = "AUC", "class" = "Accuracy")
  df$covers <- df$covers * 1.0
  df$tightness <- df$groundtruth - df$bound
  df$n_eval <- recode(df$n_eval, 
                      "50" = "50 evaluation samples", 
                      "100" = "100 evaluation samples", 
                      "150" = "150 evaluation samples")
  
  # add 'undercovers y/n' 
  coverage_table <- aggregate(covers ~ rule + method, df, mean)
  coverage_table <- rename(coverage_table, coverage = covers)
  df <- merge(df, coverage_table)
  df$acceptable_coverage <- (1-df$full_alpha) - 
    sqrt((1-df$full_alpha) * df$full_alpha / df$n_runs)
  df$undercovers <- ifelse(
    df$coverage < df$acceptable_coverage, "liberal", "valid")
  
  # helper functions to plot figures for coverage, lower confidence bound, 
  # groundtruth performance, and tightness
  CoveragePlot <- function(df) {
    coverages <- aggregate(covers ~ method + rule, df, mean)
    ggplot(coverages, aes(x = method, y = covers, fill = rule, pattern = rule)) +
      geom_bar_pattern(stat = "identity", position = "dodge", 
                       color = "black", pattern_fill = "black", 
                       pattern_angle = 45, pattern_density = 0.1, 
                       pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
      geom_text(
        aes(label = substr(round(covers, 3), 2, 5)), vjust = -1, 
        color = "black", size = 5, position = position_dodge(width = 0.9)) + 
      coord_cartesian(ylim = c(0.9, 1)) +
      labs(pattern = "") +
      scale_fill_manual(values = my_colors) + 
      scale_pattern_manual(values = c("single best" = "none", 
                                      "top 10%" = "circle", 
                                      "within 1 SE" = "stripe")) + 
      scale_y_continuous(minor_breaks = seq(0, 1, 0.005)) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.key.size = unit(24, "points"), 
            legend.text = element_text(size = 16), 
            axis.text = element_text(size = 16, color = "black"), 
            axis.title = element_blank(), 
            text = element_text(size = 16, color = "black")) +
      guides(pattern = guide_legend(override.aes = list(fill = my_colors)),
             fill = "none")
  }
  
  BoundPlot <- function(df) {
    subset(df, covers == 1) %>% 
      ggplot(aes(x = method, y = bound, fill = rule, color = undercovers, pattern = rule)) + 
      geom_boxplot_pattern(
        pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
        pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
      labs(pattern = "", color = "") + 
      scale_fill_manual(values = my_colors) + 
      scale_pattern_manual(values = c("single best" = "none", 
                                      "top 10%" = "circle", 
                                      "within 1 SE" = "stripe")) + 
      scale_y_continuous(minor_breaks = seq(0, 1, 0.05)) + 
      scale_color_manual(values = c("grey71", "black")) + 
      theme_minimal() + 
      theme(legend.position = "bottom", legend.key.size = unit(24, "points"), 
            legend.text = element_text(size = 16), 
            axis.text = element_text(size = 16, color = "black"), 
            axis.title = element_blank(), 
            text = element_text(size = 16, color = "black")) +
      guides(pattern = guide_legend(override.aes = list(fill = my_colors)),
             fill = "none", 
             color = guide_legend(override.aes = list(pattern = "none")))
  }
  
  GroundtruthPlot <- function(df) {
    subset(df, method == "BT") %>% 
      ggplot(aes(fill = rule, y = groundtruth, pattern = rule)) + 
      geom_boxplot_pattern(color = "black", 
        pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
        pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
      labs(pattern = "") + 
      scale_fill_manual(values = my_colors) + 
      scale_pattern_manual(values = c("single best" = "none", 
                                      "top 10%" = "circle", 
                                      "within 1 SE" = "stripe")) + 
      scale_y_continuous(minor_breaks = seq(0, 1, 0.05)) + 
      theme_minimal() + 
      theme(legend.position = "bottom", legend.key.size = unit(24, "points"), 
            legend.text = element_text(size = 16), 
            axis.text.x = element_blank(), 
            axis.title = element_blank(), 
            text = element_text(size = 16, color = "black"), 
            ) +
      guides(pattern = guide_legend(override.aes = list(fill = my_colors)),
             fill = "none", 
             color = guide_legend(override.aes = list(pattern = "none")))
  }
  
  TightnessPlot <- function(df) {
    subset(df, covers == 1) %>% 
      ggplot(aes(x = method, y = tightness, fill = rule, color = undercovers, pattern = rule)) + 
      geom_boxplot_pattern(
        pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
        pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
      labs(pattern = "", color = "") + 
      scale_fill_manual(values = my_colors) + 
      scale_pattern_manual(values = c("single best" = "none", 
                                      "top 10%" = "circle", 
                                      "within 1 SE" = "stripe")) + 
      scale_y_continuous(minor_breaks = seq(0, 1, 0.05)) + 
      scale_color_manual(values = c("grey71", "black")) + 
      theme_minimal() + 
      theme(legend.position = "bottom", legend.key.size = unit(24, "points"), 
            legend.text = element_text(size = 16), 
            axis.text = element_text(size = 16, color = "black"), 
            axis.title = element_blank(), 
            text = element_text(size = 16, color = "black")) +
      guides(pattern = guide_legend(override.aes = list(fill = my_colors)),
             fill = "none", 
             color = guide_legend(override.aes = list(pattern = "none")))
  }
  
  coverage_plot <- CoveragePlot(df)
  paste0(plot_path, "coverage-", run_count, ".eps") %>% .myggsave(coverage_plot)
  
  bound_plot <- BoundPlot(df)
  paste0(plot_path, "bound-", run_count, ".eps") %>% .myggsave(bound_plot)
  
  groundtruth_plot <- GroundtruthPlot(df)
  paste0(plot_path, "groundtruth-", run_count, ".eps") %>% .myggsave(groundtruth_plot)
  
  tightness_plot <- TightnessPlot(df)
  paste0(plot_path, "tightness-", run_count, ".eps") %>% .myggsave(tightness_plot)
}

fns <- paste0(cis_dir, c(
  # Accuracy, case A features
  "class/normal/n200/cv/",
  "class/normal/n200/nocv/",
  "class/normal/n400/cv/",  
  "class/normal/n400/nocv/", 
  # Accuracy, case B features
  "class/caret/n200/cv/", 
  "class/caret/n200/nocv/", 
  "class/caret/n400/cv/", 
  "class/caret/n400/nocv/", 
  # AUC, case A features
  "auc/normal/n400/cv/",
  "auc/normal/n600/cv/",
  # AUC, case B features
  "auc/caret/n400/cv/", 
  "auc/caret/n600/cv/"))

length(fns) %>% seq %>% sapply(function(i) {Plots(fns[i], plot_dir, i)})
write.csv(fns, paste0(plot_dir, "fig-param-configs-list.csv"))

