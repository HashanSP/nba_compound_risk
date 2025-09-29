# helper to build a faceted boxplot for a given set of columns ---------------- 
make_severity_plot <- function(df, models, ncol = 2,
                               pos_levels = c("PG","SG","SF","PF","C")) {
  # keep only requested models, preserving input order
  cols <- models[models %in% names(df)]
  if (length(cols) == 0) stop("None of the requested models found: ", paste(models, collapse=", "))
  
  # find position column
  pos_col <- intersect(c("Position", "player_position"), names(df))[1]
  if (is.na(pos_col)) stop("No position column found (looked for: Position, player_position).")
  
  plot_df <- df %>%
    dplyr::select(all_of(pos_col), dplyr::all_of(cols)) %>%
    dplyr::rename(Position = !!pos_col) %>%
    tidyr::pivot_longer(dplyr::all_of(cols),
                        names_to = "severity_model", values_to = "Sj") %>%
    dplyr::filter(!is.na(Position), is.finite(Sj)) %>%
    dplyr::mutate(
      Position = factor(Position, levels = pos_levels),
      severity_model = factor(severity_model, levels = cols)  # <— enforce facet order
    )
  
  ggplot(plot_df, aes(x = Position, y = Sj)) +
    geom_boxplot(outlier.alpha = 0.35, width = 0.6) +
    scale_y_log10(labels = comma) +
    labs(
      title = "Distribution of Expected Injuries and Missed Games by Player Position",
      x = "Player position",
      y = "Expected Values"
    ) +
    facet_wrap(~ severity_model, ncol = ncol) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}
