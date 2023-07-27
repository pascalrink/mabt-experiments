
rm(list = ls())
graphics.off()

library(ggplot2)

source("4-plots/myggsave-function.R")

set.seed(5)


x <- rbinom(100, 1, 0.85)
selected_performance <- mean(x)

bootstrap <- boot::boot(x, statistic = function(x, i) {mean(x[i])}, R = 1000)



.Tilt <- function(tau, x) {
  empirical_influence_values <- x
  exponential_tilting_weights <- boot::exp.tilt(
    empirical_influence_values, lambda = tau * length(empirical_influence_values))$p
  importance_weights <- boot::imp.weights(bootstrap, q = exponential_tilting_weights)
  # tilted_ecdf_at_observed_performance <- spatstat.geom::ewcdf(
  #   bootstrap$t[, relative_selected_config], importance_weights)(selected_performance)
  p_value <- (sum(importance_weights * (bootstrap$t > selected_performance)) + 
                sum(importance_weights * (bootstrap$t == selected_performance)) / 2) / 
    bootstrap$R
  return(list(
    tau = tau, p_value = p_value, exponential_tilting_weights = exponential_tilting_weights))
}

tau <- -0.41
tilt <- .Tilt(tau, x)

bootstrap_2 <- boot::boot(x, statistic = function(x, i) {mean(x[i])}, R = 1000,
                          weights = tilt$exponential_tilting_weights, stype = "i")




t1 <- bootstrap$t
t2 <- bootstrap_2$t

df1 <- data.frame(t1)
df2 <- data.frame(t2)
names(df2) <- "t1"
# 
# ggplot(df1, aes(x = t1)) + 
#   geom_histogram(aes(y = ..density..),
#                  colour = 1, fill = "white") +
#   geom_density() +
#   
#   ggplot(df2, aes(x = t2)) +
#   geom_histogram(aes(y = ..density..),
#                  colour = 1, fill = "white") +
#   geom_density()

####

etapa1 <- df1#data.frame(AverageTemperature = rnorm(100000, 16.9, 2))
etapa2 <- df2#data.frame(AverageTemperature = rnorm(100000, 17.4, 2))

#Now, combine your two dataframes into one.  First make a new column in each.
etapa1$e <- 'etapa1'
etapa2$e <- 'etapa2'

# combine the two data frames etapa1 and etapa2
combo <- rbind(etapa1, etapa2)

ggplot(combo, aes(t1, fill = e, linetype = e)) + geom_density(alpha = 0.2) + 
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal() + 
  theme(legend.position = "none") + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid = element_blank(), title = element_blank()) + 
  geom_segment(x = mean(t1), xend = mean(t1), y = 0, yend = 10, linetype = 3) + 
  geom_segment(x = mean(t2)+0.006, xend = mean(t2)+0.006, y = 0, yend =  8.45, linetype = 3) +
  annotate("segment", x = mean(t1), y = 8, xend = mean(t2)+0.006, yend = 8,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))
.myggsave("4-plots/figures/paper/fig-2.png", last_plot())
