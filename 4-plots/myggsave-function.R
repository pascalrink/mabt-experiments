# custom ggsave function to save in 1:sqrt(2) aspect ratio
.myggsave <- function(filename, plot) {
  ggsave(filename=filename, plot = plot, height = 147, width = 208, units = "mm")
}