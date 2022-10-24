
# This scripts generates the AUC simulation experiment results plots 
# shown in the main document

rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)

fpaths <- paste0(
  "1-savefiles/cis/class/", 
  c("normal/n200/cv/", "caret/n200/cv/", "normal/n400/cv/", "caret/n400/cv/"))

df <- lapply(fpaths, function(.fp) {
  .flist <- list.files(.fp) 
  lapply(.flist, function(.fn) paste0(.fp, .fn) %>% readRDS) %>% 
    do.call(rbind, .)}) %>% do.call(rbind, .)

df <- subset(df, cv & ((method == "bt" & rule == "best") | 
                         (method == "mabt" & rule == "ten") | 
                         (method == "wilson" & rule == "best")))
# there should be 5000 runs for each combination of these parameters
aggregate(seed ~ method + rule + n_eval + feats_type, df, NROW)

# Formatting ----
df$method <- factor(df$method, c("bt", "clopper-pearson", "hanley-mcneal", 
                                 "mabt", "delong", "wald", "wilson"))
df$method <- recode(df$method, 
                    "bt" = "BT", "clopper-pearson" = "CP", 
                    "hanley-mcneal" = "HMcN", "mabt" = "MABT", 
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
  df$coverage < df$acceptable_coverage, "undercovers", "valid") %>% factor


# Figure 6a ----
coverage_df <- aggregate(covers ~ method + rule + n_eval + feats_type, df, mean)
ggplot(coverage_df, aes(x = factor(n_eval), y = covers, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(
    aes(label = substr(round(covers, 3), 2, 5)), vjust = 1.6, 
    color = "white", size = 3, position = position_dodge(width = .9)) + 
  coord_cartesian(ylim = c(0.9, 0.96)) +
  facet_wrap(~ feats_type) + 
  labs(subtitle = "Prediction accuracy: coverage probability",
       x        = "evaluation sample size",
       y        = "", 
       fill     = "") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(minor_breaks = seq(0, 1, 0.01)) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.size = unit(10, "points"))
ggsave("4-plots/figures/paper/figure-6a.eps")


# Figure 7a ----
subset(df, covers == 1) %>%
  ggplot(
    aes(x = factor(n_eval), y = bound, fill = method, color = undercovers)) + 
  geom_boxplot() + 
  facet_wrap(~ feats_type) + 
  scale_fill_brewer(palette = "Set1") + 
  scale_y_continuous(minor_breaks = seq(0, 1, 0.025)) + 
  scale_color_manual(values = c("black", "grey51")) + 
  theme_minimal() + 
  theme(legend.position = "bottom", legend.key.size = unit(10, "points")) + 
  labs(title    = "Predication accuracy: lower confidence bound", 
       x        = "evaluation sample size", 
       y        = "", 
       fill     = "", 
       color    = "")
ggsave("4-plots/figures/paper/figure-7a.eps")


# Figure 8a ----
subset(df, covers == 1) %>%
  ggplot(
    aes(x = factor(n_eval), y = groundtruth - bound, 
        fill = method, color = undercovers)) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(0, 0.35)) +
  facet_wrap(~ feats_type) + 
  scale_fill_brewer(palette = "Set1") + 
  scale_y_continuous(minor_breaks = seq(0, 1, 0.025)) + 
  scale_color_manual(values = c("black", "grey51")) + 
  theme_minimal() + 
  theme(legend.position = "bottom", legend.key.size = unit(10, "points")) + 
  labs(title    = "Predication accuracy: tightness", 
       x        = "evaluation sample size", 
       y        = "", 
       fill     = "", 
       color    = "")
ggsave("4-plots/figures/paper/figure-8a.eps")


# Figure 9a ----
# true performance of final model does not depend on the confidence interval 
# estimation method but on the selection rule; BT provides final model 
# performance from 'single best' selection rule, MABT provides final model 
# performance from 'top 10%' selection rule
subset(df, method %in% c("BT", "MABT")) %>% 
  ggplot(aes(x = factor(n_eval), y = groundtruth, fill = method)) +
  geom_boxplot() +
  facet_wrap(~ feats_type) +
  scale_fill_brewer(palette = "Set1", ) +
  scale_y_continuous(minor_breaks = seq(0, 1, 0.025)) +
  theme_minimal() + 
  theme(legend.position = "bottom", legend.key.size = unit(10, "points")) +
  labs(title    = "Predication accuracy: true performance of final model",
       x        = "evaluation sample size",
       y        = "",
       fill     = "")
ggsave("4-plots/figures/paper/figure-9a.eps")


