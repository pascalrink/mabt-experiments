
# This script generates the plots shown in the Supplement Document

rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)

# specify the directory of the confidence interval results here
cis_dir <- "1-savefiles/cis/"

# specify the directory for the results plots here
plot_dir <- "4-plots/figures/supplement/"

# custom ggsave function to save in A4 format
.myggsave <- function(filename) {
  ggsave(filename=filename, height = 297, width = 210, units = "mm")
}

# returns all the result plots to a specific simulation parameter configuration
Plots <- function(fpath, plot_path, plot_name) {
  flist <- list.files(fpath)
  df <- lapply(
    flist, function(.file) paste0(fpath, .file) %>% readRDS) %>% 
    do.call(rbind, .)
  
  # Formatting ----
  df$method <- factor(df$method, c("bt", "clopper-pearson", "delong", 
                                   "hanley-mcneal", "mabt", "wald", "wilson"))
  df$method <- recode(
    df$method, "bt" = "BT", "clopper-pearson" = "CP", "delong" = "DeLong", 
    "hanley-mcneal" = "HMcN", "mabt" = "MABT", "wald" = "Wald", 
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
    df$coverage < df$acceptable_coverage, "undercovers", "valid")
  
  # helper functions to plot figures for coverage, lower confidence bound, 
  # groundtruth performance, and tightness
  CoveragePlot <- function(df) {
    coverages <- aggregate(covers ~ method + rule, df, mean)
    ggplot(coverages, aes(x = method, y = covers, fill = rule)) +
      geom_bar(stat="identity", position = "dodge") + 
      geom_text(
        aes(label = substr(round(covers, 3), 2, 5)), vjust = 1.6, 
        color = "white", size = 3, position = position_dodge(width = .9)) + 
      coord_cartesian(ylim = c(0.9, 1)) +
      labs(subtitle = "Coverage probability",
           x        = "",
           y        = "",
           fill     = "") +
      scale_fill_brewer(palette = "Set1") +
      scale_y_continuous(minor_breaks = seq(0, 1, 0.025)) +
      theme_minimal() +
      theme(legend.position = "none", legend.key.size = unit(10, "points"))
  }
  
  BoundPlot <- function(df) {
    subset(df, covers == 1) %>% 
      ggplot(aes(x = method, y = bound, fill = rule, color = undercovers)) + 
      geom_boxplot() + 
      labs(subtitle = "Lower confidence bound", 
           x        = "", 
           y        = "", 
           fill     = "", 
           color    = "") + 
      scale_fill_brewer(palette = "Set1") + 
      scale_y_continuous(minor_breaks = seq(0, 1, 0.025)) + 
      scale_color_manual(values = c("grey51", "black")) + 
      theme_minimal() + 
      theme(legend.position = "none", legend.key.size = unit(10, "points"))
  }
  
  GroundtruthPlot <- function(df) {
    ggplot(
      df, aes(fill = rule, y = groundtruth)) + 
      geom_boxplot() +
      labs(subtitle = "Final model true performance", 
           x        = "", 
           y        = "", 
           fill     = "") + 
      scale_fill_brewer(palette = "Set1") + 
      scale_y_continuous(minor_breaks = seq(0, 1, 0.025)) + 
      theme_minimal() + 
      theme(legend.position = "none", legend.key.size = unit(10, "points"), 
            axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  TightnessPlot <- function(df) {
    subset(df, covers == 1) %>% 
      ggplot(aes(x = method, y = tightness, fill = rule, color = undercovers)) + 
      geom_boxplot() + 
      labs(subtitle = "Tightness", 
           x        = "", 
           y        = "", 
           fill     = "", 
           color    = "") + 
      scale_fill_brewer(palette = "Set1") + 
      scale_y_continuous(minor_breaks = seq(0, 1, 0.025)) + 
      scale_color_manual(values = c("grey51", "black")) + 
      theme_minimal() + 
      theme(legend.position = "bottom", legend.key.size = unit(10, "points"))
  }
  
  # this function calls all the helper functions and returns 
  # the plots in a 2x2 grid
  Plot <- function(df) {
    plot_1 <- CoveragePlot(df)
    plot_2 <- BoundPlot(df)
    plot_3 <- GroundtruthPlot(df)
    plot_4 <- TightnessPlot(df)
    
    plots <- cowplot::plot_grid(
      plot_1, plot_2, plot_3, plot_4,
      ncol = 1)
    return(plots)
  }
  
  Plot(df)
  paste0(plot_path, plot_name) %>% .myggsave
}

# Accuracy, case A features
paste0(cis_dir, "class/normal/n200/cv/")   %>% Plots(plot_dir, "figure-A01.eps")
paste0(cis_dir, "class/normal/n200/nocv/") %>% Plots(plot_dir, "figure-A02.eps")
paste0(cis_dir, "class/normal/n400/cv/")   %>% Plots(plot_dir, "figure-A03.eps")
paste0(cis_dir, "class/normal/n400/nocv/") %>% Plots(plot_dir, "figure-A04.eps")

# Accuracy, case B features
paste0(cis_dir, "class/caret/n200/cv/")    %>% Plots(plot_dir, "figure-A05.eps")
paste0(cis_dir, "class/caret/n200/nocv/")  %>% Plots(plot_dir, "figure-A06.eps")
paste0(cis_dir, "class/caret/n400/cv/")    %>% Plots(plot_dir, "figure-A07.eps")
paste0(cis_dir, "class/caret/n400/nocv/")  %>% Plots(plot_dir, "figure-A08.eps")

# AUC, case A features
paste0(cis_dir, "auc/normal/n400/cv/")     %>% Plots(plot_dir, "figure-A09.eps")
paste0(cis_dir, "auc/normal/n600/cv/")     %>% Plots(plot_dir, "figure-A10.eps")

# AUC, case B features
paste0(cis_dir, "auc/caret/n400/cv/")      %>% Plots(plot_dir, "figure-A11.eps")
paste0(cis_dir, "auc/caret/n600/cv/")      %>% Plots(plot_dir, "figure-A12.eps")



