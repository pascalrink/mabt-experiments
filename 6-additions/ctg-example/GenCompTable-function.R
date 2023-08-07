
GenCompTable <- function(result_list) {
  
  .FormatResults <- function(.lelem) {
    dflt <- .lelem$default
    prpsd <- .lelem$mabt
    
    methods <- c(rownames(dflt), "mabt")
    cis <- unname(dflt[, 2]) %>% c(., prpsd)
    
    df <- data.frame(method = methods, ci = cis)
    return(df)
  }
  
  .GenCompTable <- function(.lelem) {
    .lelem$rank <- rank(1-.lelem$ci, ties.method = "max")
    Y <- expand.grid(.lelem$rank, .lelem$rank)
    methods <- .lelem$method
    n_methods <- length(methods)
    Y$method1 <- rep(methods, each = n_methods)
    Y$method2 <- rep(methods, times = n_methods)
    Y <- Y[Y$method1 != Y$method2, ]
    Y$lbtr <- (Y$Var1 > Y$Var2) * 1.0
    return(Y)
  }
  
  df_list <- lapply(result_list, function(rl) {
    df <- .FormatResults(rl)
    df$diff <- df$ci[df$method == "mabt"] - df$ci
    df$maxdiff <- max(df$diff)
    return(df)
  })
  table_list <- lapply(df_list, .GenCompTable)
  table_df <- do.call(rbind, table_list)
  table_df$comparison <- paste0(table_df$method1, " > ", table_df$method2)
  
  return(list(table = aggregate(lbtr ~ comparison, table_df, sum), 
              cis = df_list))
}