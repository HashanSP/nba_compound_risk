zero_impute_by_players <- function(df, players, vars) {
  idx <- df$PLAYER %in% players
  if (!any(idx)) return(df)
  df[idx, vars] <- lapply(df[idx, vars], function(col) { col[is.na(col)] <- 0; col })
  df
}
