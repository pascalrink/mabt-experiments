
# This scripts generates the AUC simulation experiment 
# results plots shown in the main document

rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)
library(ggpattern)

fpaths <- paste0(
  "1-savefiles/cis/auc/", 
  c("normal/n400/cv/", "caret/n400/cv/", "normal/n600/cv/", "caret/n600/cv/"))

df <- lapply(fpaths, function(.fp) {
  .flist <- list.files(.fp) 
  lapply(.flist, function(.fn) paste0(.fp, .fn) %>% readRDS) %>% 
    do.call(rbind, .)}) %>% do.call(rbind, .)

df <- subset(df, cv & ((method == "bt" & rule == "best") | 
                         (method == "mabt" & rule == "ten") | 
                         (method == "delong" & rule == "best")))
# there should be 5000 runs for each combination of these parameters
aggregate(seed ~ method + rule + n_eval + feats_type, df, NROW)

# Formatting ----
df$method <- factor(df$method, c("bt", "clopper-pearson", "hanley-mcneal", 
                                 "mabt", "delong", "wald", "wilson"))
df$method <- recode(df$method, 
                    "bt" = "BT", "clopper-pearson" = "CP", 
                    "hanley-mcneal" = "HM", "mabt" = "MABT", 
                    "delong" = "DeLong", "wald" = "Wald", 
                    "wilson" = "Wilson")
df$rule <- recode(
  df$rule, "best" = "single best", "ten" = "top 10%", "se" = "within 1 SE")
df$feats_type <- recode(
  df$feats_type, "caret" = "feature case B", "normal" = "feature case A")
df$cv <- factor(df$cv) %>% recode("TRUE" = "CV", "FALSE" = "max")
df$measure <- recode(df$measure, "auc" = "AUC", "class" = "Accuracy")
df$covers <- df$covers * 1.0
df$tightness <- df$groundtruth - df$bound

# add 'undercovers y/n'
coverage_table <- aggregate(covers ~ rule + method, df, mean)
coverage_table <- rename(coverage_table, coverage = covers)
df <- merge(df, coverage_table)
# acceptable coverage is nominal coverage minus one 
# standard error due to finite number of simulation runs
df$acceptable_coverage <- (1-df$full_alpha) - 
  sqrt((1-df$full_alpha) * df$full_alpha / df$n_runs)
df$undercovers <- ifelse(
  df$coverage < df$acceptable_coverage, "liberal", "valid") %>% factor

my_colors <- RColorBrewer::brewer.pal(3, "Pastel1")


# Figure 7 ----
coverage_df <- aggregate(covers ~ method + rule + n_eval + feats_type, df, mean)
ggplot(coverage_df, aes(x = factor(n_eval), y = covers, fill = method, pattern = method)) +
  geom_bar_pattern(stat = "identity", position = "dodge", 
                   color = "black", pattern_fill = "black", 
                   pattern_angle = 45, pattern_density = 0.1, 
                   pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  geom_text(
    aes(label = substr(round(covers, 3), 2, 5)), vjust = -1, 
    color = "black", size = 6, position = position_dodge(width = 0.9)) + 
  coord_cartesian(ylim = c(0.9, 0.97)) +
  facet_wrap(~ feats_type) + 
  labs(pattern = "", x = "evaluation sample size") +
  scale_fill_manual(values = my_colors) + 
  scale_pattern_manual(values = c("DeLong" = "none", 
                                  "MABT" = "circle", 
                                  "BT" = "stripe")) + 
  scale_y_continuous(minor_breaks = seq(0, 1, 0.005)) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.size = unit(16, "points"), 
        legend.text = element_text(size = 16), 
        axis.text = element_text(size = 16, color = "black"), 
        axis.title.y = element_blank(), 
        text = element_text(size = 16, color = "black"), 
        strip.text.x = element_text(size = 16)) +
  guides(pattern = guide_legend(override.aes = list(fill = my_colors)),
         fill = "none")
.myggsave("4-plots/figures/paper/fig-07.eps", last_plot())


# Figure 9 ----
subset(df, covers == 1) %>% 
  ggplot(aes(x = factor(n_eval), y = bound, fill = method, color = undercovers, pattern = method)) + 
  geom_boxplot_pattern(
    pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
    pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  facet_wrap(~ feats_type) +
  labs(pattern = "", color = "", x = "evaluation sample size") + 
  scale_fill_manual(values = my_colors) + 
  scale_pattern_manual(values = c("DeLong" = "none", 
                                  "MABT" = "circle", 
                                  "BT" = "stripe")) + 
  scale_y_continuous(minor_breaks = seq(0, 1, 0.05)) + 
  scale_color_manual(values = c("grey71", "black")) + 
  theme_minimal() + 
  theme(legend.position = "bottom", legend.key.size = unit(24, "points"), 
        legend.text = element_text(size = 16), 
        axis.text = element_text(size = 16, color = "black"), 
        axis.title.y = element_blank(), 
        text = element_text(size = 16, color = "black"), 
        strip.text.x = element_text(size = 16)) +
  guides(pattern = guide_legend(override.aes = list(fill = my_colors)),
         fill = "none", 
         color = guide_legend(override.aes = list(pattern = "none")))
.myggsave("4-plots/figures/paper/fig-09.eps", last_plot())


# Figure 11 ----
subset(df, covers == 1) %>% 
  ggplot(aes(x = factor(n_eval), y = tightness, fill = method, color = undercovers, pattern = method)) + 
  geom_boxplot_pattern(
    pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
    pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  facet_wrap(~ feats_type) + 
  labs(pattern = "", color = "", x = "evaluation sample size") + 
  scale_fill_manual(values = my_colors) + 
  scale_pattern_manual(values = c("DeLong" = "none", 
                                  "MABT" = "circle", 
                                  "BT" = "stripe")) + 
  scale_y_continuous(minor_breaks = seq(0, 1, 0.05)) + 
  scale_color_manual(values = c("grey71", "black")) + 
  theme_minimal() + 
  theme(legend.position = "bottom", legend.key.size = unit(24, "points"), 
        legend.text = element_text(size = 16), 
        axis.text = element_text(size = 16, color = "black"), 
        axis.title.y = element_blank(), 
        text = element_text(size = 16, color = "black"), 
        strip.text.x = element_text(size = 16)) +
  guides(pattern = guide_legend(override.aes = list(fill = my_colors)),
         fill = "none", 
         color = guide_legend(override.aes = list(pattern = "none")))
.myggsave("4-plots/figures/paper/fig-11.eps", last_plot())


# Figure 13 ----
# true performance of final model does not depend on the confidence interval 
# estimation method but on the selection rule; BT provides final model 
# performance from 'single best' selection rule, MABT provides final model 
# performance from 'top 10%' selection rule
subset(df, method %in% c("BT", "MABT")) %>% 
  ggplot(aes(x = factor(n_eval), y = groundtruth, fill = rule, pattern = rule)) + 
  geom_boxplot_pattern(color = "black", 
                       pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
                       pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  facet_wrap(~ feats_type) +
  labs(pattern = "", x = "evaluation sample size") +
  scale_fill_manual(values = my_colors[c(3, 2)]) + 
  scale_pattern_manual(values = c("single best" = "none", 
                                  "top 10%" = "circle")) + 
  scale_y_continuous(minor_breaks = seq(0, 1, 0.05)) + 
  theme_minimal() + 
  theme(legend.position = "bottom", legend.key.size = unit(24, "points"), 
        legend.text = element_text(size = 16), 
        axis.text = element_text(size = 16, color = "black"), 
        axis.title.y = element_blank(), 
        text = element_text(size = 16, color = "black"), 
        strip.text.x = element_text(size = 16)) +
  guides(pattern = guide_legend(override.aes = list(fill = my_colors[c(3, 2)])),
         fill = "none")
.myggsave("4-plots/figures/paper/fig-13.eps", last_plot())

