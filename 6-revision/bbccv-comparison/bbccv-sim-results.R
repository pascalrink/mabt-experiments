
rm(list = ls())
graphics.off()

bbccv_200 <- readRDS("6-revision/bbccv-comparison/bbccv-sim-n200.RDS")
bbccv_400 <- readRDS("6-revision/bbccv-comparison/bbccv-sim-n400.RDS")

FormatResultList <- function(rl, args) {
  df <- lapply(rl, data.frame) %>% do.call(rbind, .)
  df$n <- args[[1]]
  return(df)
}

df_200 <- FormatResultList(bbccv_200, 200)
df_400 <- FormatResultList(bbccv_400, 400)
df <- rbind(df_200, df_400)
df$covers <- df$lwr_bound < df$true_performance

aggregate(covers ~ n, df, mean)