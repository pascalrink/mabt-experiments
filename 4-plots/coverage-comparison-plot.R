
# This script generates Figure 5 in the main document

rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)

cis_path <- "1-savefiles/cis/"
cis_files <- list.files(cis_path, recursive = TRUE)

df <- lapply(cis_files, function(.fn) {paste0(cis_path, .fn) %>% 
    readRDS}) %>% do.call(rbind, .)
df$method <- recode(
  df$method, "bt" = "BT", "clopper-pearson" = "CP", "delong" = "DeLong", 
  "hanley-mcneal" = "HMcN", "mabt" = "MABT", "wald" = "Wald", 
  "wilson" = "Wilson")
df$fill_color <- df$method
df$fill_color <- recode(df$fill_color, "BT" = "1", "CP" = "2", "DeLong" = "2", "HMcN" = "2", "MABT" = "3", "Wald" = "2", "Wilson" = "2")

coverage_df <- aggregate(
  covers ~ method + rule + feats_type + n_eval + cv + measure + fill_color, df, mean)

ggplot(coverage_df, aes(x = method, y = covers, fill = fill_color)) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(0.9, 1)) + 
  theme_minimal() + 
  labs(#title = "Comparison of coverage probabilities from all simulation experiment configurations",
       x        = "",
       y        = "") + 
  scale_y_continuous(minor_breaks = seq(0, 1, 0.025)) + 
  theme(legend.position = "none")
ggsave("4-plots/figures/paper/figure-5.eps")


