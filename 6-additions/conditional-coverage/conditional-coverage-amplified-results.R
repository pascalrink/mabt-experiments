
rm(list = ls())
graphics.off()

library(dplyr)
library(ggplot2)
library(ggpattern)

source("4-plots/myggsave-function.R")

n_threshold <- 1000

GetNPerLambda <- function(df) {
  n_df <- aggregate(covers ~ lambda + method, df, NROW)
  n_df <- rename(n_df, n = covers)
  df <- merge(df, n_df) %>% subset(n >= n_threshold)
  return(df)
}

GetFirst1000 <- function(df) {
  by_lambda_list <- dplyr::group_split(df, lambda)
  by_lambda_list <- lapply(by_lambda_list, function(bll) bll[1:n_threshold, ])
  by_lambda_df <- do.call(rbind, by_lambda_list)
  by_lambda_df$n <- NULL
  return(by_lambda_df)
}

MyFunction <- function(d, m) {
  d <- subset(d, method == m)
  d$lambda %>% unique %>% length %>% paste0(m, ": # of models is ", .) %>% print
  d <- GetNPerLambda(d) %>% GetFirst1000
  d$lambda %>% unique %>% length %>% 
    paste0(m, ": for ", ., " models there have at least", 
           n_threshold, "replications") %>% 
    print
  return(d)
}

results_list <- readRDS(
  "6-additions/conditional-coverage/ccs-amplified-results.RDS")

df_list <- lapply(results_list, function(lelem) lelem$df)
df <- do.call(rbind, df_list)

mabt_df <- MyFunction(df, "MABT")
bbccv_df <- MyFunction(df, "BBC-CV")
ncv_df <- MyFunction(df, "NCV")

df <- rbind(mabt_df, bbccv_df, ncv_df)
per_lambda_df <- aggregate(covers ~ method + lambda, df, mean) %>% 
  rename(coverage = covers)

# analyze the results for models that have at least 1000 replications ----

aggregate(covers ~ method, df, mean)
aggregate(coverage ~ method, per_lambda_df, summary)

# ggplot for paper ----

my_colors <- RColorBrewer::brewer.pal(3, "Pastel1")

boxpl <- ggplot(
  per_lambda_df, 
  aes(x = factor(method), y = coverage, fill = method, pattern = method)) +
  coord_cartesian(ylim = c(0.83, 0.97)) +
  geom_boxplot_pattern(
    pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1,
    pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  labs(pattern = "", color = "", x = "") +
  scale_fill_manual(values = my_colors) +
  scale_pattern_manual(values = c("NCV" = "none",
                                  "MABT" = "circle",
                                  "BBC-CV" = "stripe")) +
  scale_y_continuous(
    breaks = c(0.85, 0.90, 0.95), minor_breaks = seq(0, 1, 0.01)) +
  scale_color_manual(values = c("black", "grey71")) +
  theme_minimal() +
  theme(legend.position = "none", legend.key.size = unit(24, "points"),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16, color = "black"),
        axis.title.y = element_blank(),
        text = element_text(size = 16, color = "black"),
        strip.text.x = element_text(size = 16), 
        panel.grid.major.x = element_blank()) +
  guides(pattern = guide_legend(override.aes = list(fill = my_colors)),
         fill = "none",
         color = guide_legend(override.aes = list(pattern = "none")))
.myggsave(
  "6-additions/conditional-coverage/ccs-amplified-results-boxplot.png", boxpl)

