
# This script generates Figure 5 in the main document (coverage comparison)

rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)
library(ggpattern)

source("4-plots/myggsave-function.R")

cis_path <- "1-savefiles/cis/"
cis_files <- list.files(cis_path, recursive = TRUE)

df_lasso <- lapply(cis_files, function(.fn) {paste0(cis_path, .fn) %>% 
    readRDS}) %>% do.call(rbind, .)

rf_200_list <- readRDS("6-revision/tuneranger-sim/tuneranger-sim-n200.RDS")
rf_400_list <- readRDS("6-revision/tuneranger-sim/tuneranger-sim-n400.RDS")
rf_600_list <- readRDS("6-revision/tuneranger-sim/tuneranger-sim-n600.RDS")

FormatResultList <- function(rl, args) {
  df <- lapply(rl, function(lelem) {
    lapply(lelem, data.frame) %>% do.call(rbind, .)
  }) %>% do.call(rbind, .)
  df$n <- args[[1]]
  return(df)
}

df_rf <- rbind(FormatResultList(rf_200_list, "200"), 
               FormatResultList(rf_400_list, "400"), 
               FormatResultList(rf_600_list, "600"))
df_rf$covers <- df_rf$ci.lwr < df_rf$gt
df_rf$n_eval <- as.numeric(df_rf$n) / 4
df_rf$rule <- substr(df_rf$ci.method, nchar(df_rf$ci.method)-4, nchar(df_rf$ci.method))
df_rf$ci.method[df_rf$rule == "sidak"] <- substr(df_rf$ci.method[df_rf$rule == "sidak"], 1, nchar(df_rf$ci.method[df_rf$rule == "sidak"])-6)
df_rf$rule[df_rf$rule != "sidak"] <- "best"
df_rf$rule[df_rf$rule == "sidak" | df_rf$ci.method == "mabt"] <- "ten"

names(df_rf) <- c("id", "est", "method", "bound", "groundtruth", "sample size", "covers", "n_eval", "rule")
df_rf <- subset(df_rf, select = c(est, method, bound, groundtruth, covers, n_eval, rule))
df_rf$feats_type <- "caret"
df_rf$cv <- TRUE
df_rf$measure <- "class"
df_rf$classifier <- "random forest"

df_lasso$classifier <- "lasso"
df_lasso <- subset(df_lasso, select = c(est, method, bound, groundtruth, covers, n_eval, rule, feats_type, cv, measure, classifier))
df <- rbind(df_lasso, df_rf)

df$pipeline <- NA
df$pipeline[df$rule == "best"] <- "default"
df$pipeline[df$rule != "best"] <- "proposed"

df$method <- recode(
  df$method, "bt" = "BT", "clopper-pearson" = "CP", "delong" = "DeLong", 
  "hanley-mcneal" = "HM", "mabt" = "MABT", "wald" = "Wald", 
  "wilson" = "Wilson")
df$fill_color <- df$pipeline

my_colors <- RColorBrewer::brewer.pal(3, "Pastel1")

coverage_df <- aggregate(
  covers ~ method + rule + feats_type + n_eval + cv + measure + classifier + pipeline + fill_color, df, mean)

ggplot(coverage_df, aes(x = method, y = covers, fill = pipeline, pattern = pipeline)) + 
  geom_boxplot_pattern(
    pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
    pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  labs(pattern = "pipeline") + 
  scale_fill_manual(values = my_colors) + 
  scale_pattern_manual(values = c("proposed" = "circle", 
                                  "default" = "none")) + 
  scale_y_continuous(minor_breaks = seq(0, 1, 0.05)) + 
  theme_minimal() + 
  theme(legend.position = "bottom", legend.key.size = unit(24, "points"), 
        legend.text = element_text(size = 16), 
        axis.text = element_text(size = 16, color = "black"), 
        axis.title.y = element_blank(), 
        text = element_text(size = 16, color = "black"), 
        strip.text.x = element_text(size = 16), 
        axis.title.x = element_blank()) +
  guides(pattern = guide_legend(override.aes = list(fill = my_colors[1:2])), 
         fill = "none")
