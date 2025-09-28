#pick the right columns present in your pred_df_out -----------------
first_existing <- function(df, candidates) {
  nm <- candidates[candidates %in% names(df)]
  if (length(nm) == 0) NA_character_ else nm[1]
}