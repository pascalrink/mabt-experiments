
GetHeartData <- function() {
  dat <- read.csv("5-example/processed.cleveland.data", header = FALSE)
  col_names <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", 
                 "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
  names(dat) <- col_names
  dat$num <- (dat$num > 0) * 1.0
  factor_cols <- which(
    col_names %in% c("sex", "cp", "fbs", "restecg", 
                     "exang", "slope", "thal", "num"))
  dat[factor_cols] <- lapply(dat[factor_cols], factor)
  return(dat)
}

