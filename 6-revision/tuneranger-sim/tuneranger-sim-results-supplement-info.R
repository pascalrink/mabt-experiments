
rm(list = ls())
graphics.off()

library(gtable)
library(cowplot)
library(grid)

shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}


# ----
library(dplyr)
library(ggplot2)
library(ggpattern)

source("4-plots/myggsave-function.R")

rl_200 <- readRDS("6-revision/tuneranger-sim/tuneranger-sim-n200.RDS")
rl_400 <- readRDS("6-revision/tuneranger-sim/tuneranger-sim-n400.RDS")
rl_600 <- readRDS("6-revision/tuneranger-sim/tuneranger-sim-n600.RDS")

# ----

GenCompTable <- function(result_list) {
  
  .FormatResults <- function(.lelem) {
    .lelem$default <- data.frame(.lelem$default)
    .lelem$proposed <- data.frame(.lelem$proposed)
    .lelem$sidak <- data.frame(.lelem$sidak)
    df <- do.call(rbind, .lelem)
    df$covers <- df$ci.lwr < df$gt
    df <- df[which(df$covers), ]
    if (nrow(df) == 0) {return(NULL)}
    df <- select(df, c(ci.method, ci.lwr))
    return(df)
  }
  
  .GenCompTable <- function(.lelem) {
    .lelem$rank <- rank(1-.lelem$ci.lwr, ties.method = "max")
    Y <- expand.grid(.lelem$rank, .lelem$rank)
    methods <- .lelem$ci.method
    n_methods <- length(methods)
    Y$method1 <- rep(methods, each = n_methods)
    Y$method2 <- rep(methods, times = n_methods)
    Y <- Y[Y$method1 != Y$method2, ]
    Y$lbtr <- (Y$Var1 > Y$Var2) * 1.0  # kein Unterschied ob > oder >=
    return(Y)
  }
  
  df_list <- lapply(result_list, function(rl) {
    df <- .FormatResults(rl)
    if (is.null(df)) return(NULL)
    df$diff <- NA
    if ("mabt" %in% df$ci.method) df$diff <- df$ci.lwr[df$ci.method == "mabt"] - df$ci.lwr
    return(df)
  })
  table_list <- lapply(df_list, .GenCompTable)
  table_df <- do.call(rbind, table_list)
  table_df$comparison <- paste0(table_df$method1, " > ", table_df$method2)
  
  return(list(table_sum  = aggregate(lbtr ~ comparison, table_df, sum), 
              table_mean = aggregate(lbtr ~ comparison, table_df, mean),
              cis = df_list))
}

comp_tab_200 <- GenCompTable(rl_200)
comp_tab_400 <- GenCompTable(rl_400)
comp_tab_600 <- GenCompTable(rl_600)

comp_tab_200$table_sum[substr(comp_tab_200$table_sum$comparison, 1, 4) == "mabt", ]
comp_tab_400$table_sum[substr(comp_tab_400$table_sum$comparison, 1, 4) == "mabt", ]
comp_tab_600$table_sum[substr(comp_tab_600$table_sum$comparison, 1, 4) == "mabt", ]

comp_tab_200$table_mean[substr(comp_tab_200$table_mean$comparison, 1, 4) == "mabt", ]
comp_tab_400$table_mean[substr(comp_tab_400$table_mean$comparison, 1, 4) == "mabt", ]
comp_tab_600$table_mean[substr(comp_tab_600$table_mean$comparison, 1, 4) == "mabt", ]

# ----


FormatResultList <- function(rl, args) {
  df <- lapply(rl, function(lelem) {
    lapply(lelem, data.frame) %>% do.call(rbind, .)
  }) %>% do.call(rbind, .)
  df$n <- args[[1]]
  return(df)
}

df <- rbind(FormatResultList(rl_200, "200"), 
            FormatResultList(rl_400, "400"), 
            FormatResultList(rl_600, "600"))
df$covers <- df$ci.lwr < df$gt
df$n_eval <- as.numeric(df$n) / 4

df$n_eval[df$n_eval ==  50] <- "Evaluation sample size 50"
df$n_eval[df$n_eval == 100] <- "Evaluation sample size 100"
df$n_eval[df$n_eval == 150] <- "Evaluation sample size 150"

df$n_eval <- factor(df$n_eval, 
                    levels = c("Evaluation sample size 50", "Evaluation sample size 100", "Evaluation sample size 150"))

df$rule <- "single best"
df$rule[df$ci.method == "mabt" | 
          substr(df$ci.method, nchar(df$ci.method)-4, nchar(df$ci.method)) == "sidak"] <- "top 10%"

df$ci.method <- recode(df$ci.method, 
                       "clopper-pearson_sidak" = "CP", "clopper-pearson" = "CP", 
                       "wald_sidak" = "Wald", "mabt" = "MABT", 
                       "wilson_sidak" = "Wilson", "wald" = "Wald", 
                       "wilson" = "Wilson")


# df <- subset(df, ci.method %in% c("CP", "MABT", "Wilson"))
df_covering <- subset(df, covers)

n_colors <- table(df$rule) %>% length
my_colors <- RColorBrewer::brewer.pal(3, "Pastel1")
my_colors <- my_colors[1:n_colors]

# add 'undercovers y/n'
coverage_table <- aggregate(covers ~ ci.method + n_eval + rule, df, mean)
coverage_table <- rename(coverage_table, coverage = covers)
df <- merge(df, coverage_table)
# acceptable coverage is nominal coverage minus one 
# standard error due to finite number of simulation runs
full_alpha <- 0.05
df$acceptable_coverage <- (1-full_alpha) - 
  sqrt((1-full_alpha) * full_alpha / length(rl_200))
df$undercovers <- ifelse(
  df$coverage < df$acceptable_coverage, "liberal", "valid") %>% factor


p1 <- ggplot(coverage_table, aes(x = ci.method, y = coverage, fill = rule, pattern = rule)) +
  geom_bar_pattern(stat = "identity", position = "dodge", 
                   color = "black", pattern_fill = "black", 
                   pattern_angle = 45, pattern_density = 0.1, 
                   pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  geom_text(
    aes(label = substr(round(coverage, 3), 2, 5)), vjust = -1, 
    color = "black", size = 5, position = position_dodge(width = 0.9)) + 
  coord_cartesian(ylim = c(0.9, 1)) +
  labs(pattern = "selection rule") +
  scale_fill_manual(values = my_colors) + 
  scale_pattern_manual(values = c("single best" = "none", 
                                  "top 10%" = "circle")) + 
  scale_y_continuous(minor_breaks = seq(0, 1, 0.005)) +
  theme_minimal() +
  theme(legend.position = "right", legend.key.size = unit(24, "points"), 
        legend.text = element_text(size = 16), 
        axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_blank(), 
        text = element_text(size = 16, color = "black")) +
  guides(pattern = guide_legend(override.aes = list(fill = my_colors)),
         fill = "none") + 
  facet_wrap(~ n_eval, nrow = 2)
graphics.off()
shift_legend(p1) %>% grid.draw 
# si-tr-sim-coverage.eps, 960x600


p2 <- subset(df, covers == 1) %>% 
  ggplot(aes(x = ci.method, y = ci.lwr, fill = rule, color = undercovers, pattern = rule)) + 
  geom_boxplot_pattern(
    pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
    pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  labs(pattern = "selection rule", color = "") + 
  scale_fill_manual(values = my_colors) + 
  scale_pattern_manual(values = c("single best" = "none", 
                                  "top 10%" = "circle")) + 
  scale_y_continuous(minor_breaks = seq(0, 1, 0.05)) + 
  scale_color_manual(values = c("grey71", "black")) + 
  theme_minimal() + 
  theme(legend.position = "right", legend.key.size = unit(24, "points"), 
        legend.text = element_text(size = 16), 
        axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_blank(), 
        text = element_text(size = 16, color = "black")) +
  guides(pattern = guide_legend(override.aes = list(fill = my_colors)),
         fill = "none", 
         color = guide_legend(override.aes = list(pattern = "none"))) + 
  facet_wrap(~ n_eval, nrow = 2)
graphics.off()
shift_legend(p2) %>% grid.draw 
# si-tr-sim-bound.eps, 960x600


p3 <- subset(df, ci.method == "CP") %>% 
  ggplot(aes(fill = rule, y = gt, pattern = rule)) + 
  geom_boxplot_pattern(color = "black", 
                       pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
                       pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  labs(pattern = "selection rule") + 
  scale_fill_manual(values = my_colors) + 
  scale_pattern_manual(values = c("single best" = "none", 
                                  "top 10%" = "circle")) + 
  scale_y_continuous(minor_breaks = seq(0, 1, 0.05)) + 
  theme_minimal() + 
  theme(legend.position = "right", legend.key.size = unit(24, "points"), 
        legend.text = element_text(size = 16), 
        axis.text.x = element_blank(), 
        axis.title = element_blank(), 
        text = element_text(size = 16, color = "black"), 
  ) +
  guides(pattern = guide_legend(override.aes = list(fill = my_colors)),
         fill = "none", 
         color = guide_legend(override.aes = list(pattern = "none"))) + 
  facet_wrap(~ n_eval, nrow = 2)
graphics.off()
shift_legend(p3) %>% grid.draw 
# si-tr-sim-gt.eps, 960x600


p4 <- subset(df, covers == 1) %>% 
  ggplot(aes(x = ci.method, y = gt-ci.lwr, fill = rule, color = undercovers, pattern = rule)) + 
  geom_boxplot_pattern(
    pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
    pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  labs(pattern = "selection rule", color = "") + 
  scale_fill_manual(values = my_colors) + 
  scale_pattern_manual(values = c("single best" = "none", 
                                  "top 10%" = "circle")) + 
  scale_y_continuous(minor_breaks = seq(0, 1, 0.05)) + 
  scale_color_manual(values = c("grey71", "black")) + 
  theme_minimal() + 
  theme(legend.position = "right", legend.key.size = unit(24, "points"), 
        legend.text = element_text(size = 16), 
        axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_blank(), 
        text = element_text(size = 16, color = "black")) +
  guides(pattern = guide_legend(override.aes = list(fill = my_colors)),
         fill = "none", 
         color = guide_legend(override.aes = list(pattern = "none"))) +
  facet_wrap(~ n_eval, nrow = 2)
graphics.off()
shift_legend(p4) %>% grid.draw 
# si-tr-sim-tightness.eps, 960x600


# ----

#### -----

ggplot(df_covering, aes(x = factor(n_eval), y = ci.lwr, fill = ci.method, pattern = ci.method)) + 
  geom_boxplot_pattern(color = "black", pattern_fill = "black", 
                       pattern_angle = 45, pattern_density = 0.1, 
                       pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  labs(pattern = "", x = "evaluation sample size") +
  scale_fill_manual(values = my_colors) + 
  scale_pattern_manual(values = c("Wilson" = "none", 
                                  "MABT" = "circle", 
                                  "CP" = "stripe")) + 
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
# .myggsave("6-revision/tuneranger-sim/figures/tr-sim-lwr-bound.eps", last_plot())

##### -----

ggplot(df_covering, aes(x = factor(n_eval), y = gt-ci.lwr, fill = ci.method, pattern = ci.method)) + 
  geom_boxplot_pattern(color = "black", pattern_fill = "black", 
                       pattern_angle = 45, pattern_density = 0.1, 
                       pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
  labs(pattern = "", x = "evaluation sample size") +
  scale_fill_manual(values = my_colors) + 
  scale_pattern_manual(values = c("Wilson" = "none", 
                                  "MABT" = "circle", 
                                  "CP" = "stripe")) + 
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
# .myggsave("6-revision/tuneranger-sim/figures/tr-sim-tightness.eps", last_plot())

##### ----

df$rule <- NA
df$rule[df$ci.method == "MABT"] <- "top 10%"
df$rule[df$ci.method != "MABT"] <- "single best"
subset(df, ci.method %in% c("Wilson", "MABT")) %>% 
  ggplot(aes(x = factor(n_eval), y = gt, fill = rule, pattern = rule)) + 
  geom_boxplot_pattern(color = "black", 
                       pattern_fill = "black", pattern_angle = 45, pattern_density = 0.1, 
                       pattern_spacing = 0.02, pattern_key_scale_factor = 0.5) + 
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
# .myggsave("6-revision/tuneranger-sim/figures/tr-sim-true-perform.eps", last_plot())

##### ----
