
rm(list = ls())
graphics.off()

library(dplyr)
library(ggplot2)

rl_200 <- readRDS("6-revision/tuneranger-sim/tuneranger-sim-n200.RDS")
rl_400 <- readRDS("6-revision/tuneranger-sim/tuneranger-sim-n400.RDS")
rl_600 <- readRDS("6-revision/tuneranger-sim/tuneranger-sim-n600.RDS")

FormatResultList <- function(rl, args) {
  df <- lapply(rl, function(lelem) {
    lapply(lelem, data.frame) %>% do.call(rbind, .)
  }) %>% do.call(rbind, .)
  df$n <- args[[1]]
  return(df)
}

df_200 <- FormatResultList(rl_200, "200")
df_400 <- FormatResultList(rl_400, "400")
df_600 <- FormatResultList(rl_600, "600")

df <- rbind(df_200, df_400, df_600)
df$covers <- df$ci.lwr < df$gt
df_covering <- subset(df, covers)

aggregate(covers ~ ci.method + n, df, mean) %>% ggplot(aes(x = n, y = covers, fill = ci.method)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_cartesian(ylim = c(0.9, 1.0)) + 
  geom_hline(yintercept = 0.95)

ggplot(df_covering, aes(x = n, y = ci.lwr, fill = ci.method)) + 
  geom_boxplot()

subset(df, ci.method %in% c("wilson", "mabt")) %>% 
  ggplot(aes(x = n, y = gt, fill = ci.method)) + 
  geom_boxplot()

ggplot(df_covering, aes(x = n, y = gt - ci.lwr, fill = ci.method)) + 
  geom_boxplot()
