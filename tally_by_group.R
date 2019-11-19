# Tally by group
addframenumbers <- function(df){
  df$frame <- 1:nrow(df)
  return(df)
}

tally_by_group <- function(df, group_col = "trial"){
  library(data.table)
  groups <- split(df, df[, group_col])
  groups2 <- lapply(groups, addframenumbers)
  df_new <- rbindlist(groups2) %>% as.data.frame()
  return(df_new)
}

