# ------------------------------------------------------------
# Data analysis scripts to produce results used in the abstract:
# ------------------------------------------------------------
# "Modeling Player Injuries in the National Basketball Association: 
#             An Actuarial Approach"
# ------------------------------------------------------------
# LAUTIER, PEIRIS, JEONG
# 2025
# ------------------------------------------------------------
# R version 4.4.3 (2025-02-28 ucrt) -- "Trophy Case"
# Copyright (C) 2025 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64
# ------------------------------------------------------------
# ------------------------------------------------------------
# INSTRUCTIONS
# ------------------------------------------------------------
# supporting files:
# master-data.csv                  player-summary-injury.csv
# missed-game-summary-full.csv     injury-specific-summary.csv
#
# ------------------------------------------------------------
# packages and helpers
# ------------------------------------------------------------

require(dplyr)      #analysis
require(MASS)       #analysis
require(tidyr)      #analysis
require(ggplot2)    #figures
require(scales)     #figures
require(knitr)      #latex table
require(kableExtra) #latex table
require(extrafont)  #font_import() first time for 'Times New Roman'

# functions used throughout -------------------------
source('./code/make_severity_plot.R')
source('./code/zero_impute_by_player.R')
source('./code/first_existing.R')

# ------------------------------------------------------------
# load data for training
# ------------------------------------------------------------

# master data ---------------------------------------------
master_df<- read.csv('./clean_data/master_data.csv')

# player summary for number of injuries ---------------------------------------------
injury_travel_summary <- read.csv('./clean_data/player_summary_injury.csv')

# player summary for financial loss per missed game ---------------------------------------------
missed_game_summary_full <- read.csv('./clean_data/missed_game_summary_full.csv')

# player summary for missed games per injury ---------------------------------------------
injury_specific_sum <- read.csv('./clean_data/injury_specific_summary.csv')

# ------------------------------------------------------------
# data pre processing for modeling
# ------------------------------------------------------------

# List of all the box‐score columns you care about ---------------------------------------------
box_vars <- c(
  "FGM",   "FGA",   "FG3M",  "FG3A",  "FTM",   "FTA",
  "PF",    "PTS",   "STL",   "BLK",   "TO",    "OREB",
  "DREB",  "AST",   "BLKA",  "PFD",   "POSS",  "SCREEN_ASSISTS",
  "DEFLECTIONS", "CHARGES_DRAWN",    "CONTESTED_SHOTS_2PT", 
  "CONTESTED_SHOTS_3PT", "OFF_BOXOUTS",  "DEF_BOXOUTS",
  "OFF_LOOSE_BALLS_RECOVERED", "DEF_LOOSE_BALLS_RECOVERED",
  "D_FGM", "D_FGA",  "DRIVES", "DIST_MILES_OFF", "DIST_MILES_DEF",
  "TOUCHES", "PASSES_MADE", "PASSES_RECEIVED", "SECONDARY_AST",
  "POTENTIAL_AST", "FT_AST",  "OREB_CONTEST", "OREB_CHANCES",
  "DREB_CONTEST", "DREB_CHANCES"
)

# Compute per‐player season **averages** of each stat ---------------------------------------------
player_box_avg <- master_df %>%
  filter(GP == 1) %>%
  group_by(PLAYER) %>%
  summarize(
    across(
      all_of(box_vars),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_avg"
    ),
    .groups = "drop"
  )

# Compute per‐player season **totals** of each stat ---------------------------------------------
player_box_sum <- master_df %>%
  group_by(PLAYER) %>%
  summarize(
    across(
      all_of(box_vars),
      ~ sum(.x, na.rm = TRUE),
      .names = "{.col}_sum"
    ),
    .groups = "drop"
  )


# We want both in one table, so we can join: ---------------------------------------------
model_data_sum <- injury_travel_summary %>%
  left_join(player_box_sum, by = "PLAYER")
model_data_sum <- model_data_sum[!is.na(model_data_sum$win_pct),]

model_data_avg <- injury_travel_summary %>%
  left_join(player_box_avg, by = "PLAYER")
model_data_avg <- model_data_avg[!is.na(model_data_avg$win_pct),] 

# Identify predictors ---------------------------------------------
base_vars <- c(
  "travel_games", "total_travel", "played_games_48h", "total_min", "avg_perc_min",
  "played_games_72h", "played_games_7d",  "player_position",
  "games_played", "height", "weight", "age_season"
)

# fit all models by excluding players with less presence on court and the season ---------------------------------------------
# excluding plyaers with a salary less than 100,000 and games played < 5 and 5 players with highest average perc mins
(PAYERS_1e5<-injury_travel_summary$PLAYER[injury_travel_summary$salary<100000 & 
                                            injury_travel_summary$games_played<5])

# Sort players by avg_perc_min (ties broken by first occurrence) with average less than 2 mins per game---------------------------------------------
bottom5_players_avg_perc_min <- injury_travel_summary$PLAYER[injury_travel_summary$avg_perc_min<=0.0625]

excluding_players <- union(PAYERS_1e5, bottom5_players_avg_perc_min)

# ------------------------------------------------------------
# model Ij with poisson - equation 1
# ------------------------------------------------------------

# training subset ---------------------------------------------
model_data_sum_1e5 <- model_data_sum %>%
  # Filter out excluded reasons or NA
  filter(!PLAYER %in% excluding_players)

# Poisson GLM ---------------------------------------------
mod_inj_poisson <- glm(injuries ~ total_travel + avg_perc_min + player_position + 
                         played_games_72h + height + weight + age_season + 
                         FGA_sum + FG3A_sum + FTA_sum + PF_sum + PTS_sum + STL_sum + 
                         BLK_sum + TO_sum + OREB_sum + DREB_sum + AST_sum + PFD_sum + 
                         POSS_sum + SCREEN_ASSISTS_sum + DEFLECTIONS_sum + CHARGES_DRAWN_sum + 
                         CONTESTED_SHOTS_2PT_sum + CONTESTED_SHOTS_3PT_sum + OFF_BOXOUTS_sum + 
                         DEF_BOXOUTS_sum + OFF_LOOSE_BALLS_RECOVERED_sum + DEF_LOOSE_BALLS_RECOVERED_sum + 
                         D_FGM_sum + D_FGA_sum + DRIVES_sum + DIST_MILES_OFF_sum + 
                         DIST_MILES_DEF_sum + PASSES_MADE_sum + PASSES_RECEIVED_sum + 
                         SECONDARY_AST_sum + POTENTIAL_AST_sum + FT_AST_sum + OREB_CONTEST_sum + 
                         OREB_CHANCES_sum + DREB_CONTEST_sum + DREB_CHANCES_sum,
                       data = model_data_sum_1e5,
                       family = poisson(link = "log"),
                       offset = log(games_count)   # offset ensures rate per game
)

# summary of fitted model---------------------------------------------
summary(mod_inj_poisson)

# Choose model for prediction---------------------------------------------
mod_I_final <- mod_inj_poisson

# prediction data---------------------------------------------
pred_I_df <- injury_travel_summary %>%
  left_join(player_box_sum, by = "PLAYER")

# Predict E[I_j | x_j] (frequency) for each player-season---------------------------------------------
pred_I_df$E_I <- predict(mod_I_final, newdata = pred_I_df, type = "response")

# ------------------------------------------------------------
# model Mjk with poisson - equation 2
# ------------------------------------------------------------

# training subset---------------------------------------------
injury_specific_sum_1e5 <- injury_specific_sum %>%
  # Filter out excluded reasons or NA
  filter(!PLAYER %in% excluding_players)

length(unique(injury_specific_sum_1e5$PLAYER))

# Poisson GLM---------------------------------------------
mod_mjk_poisson <- glm(
  missed_games ~  injuries_prior + PERCENTAGE_MIN_prior+AVG_TRAVEL_MILES_prior + total_miles_prior 
  + FGA_prior_sum + FG3A_prior_sum + FTA_prior_sum + PF_prior_sum + PTS_prior_sum 
  + STL_prior_sum + BLK_prior_sum + TO_prior_sum + OREB_prior_sum + DREB_prior_sum 
  + AST_prior_sum + PFD_prior_sum + POSS_prior_sum + SCREEN_ASSISTS_prior_sum 
  + DEFLECTIONS_prior_sum + CHARGES_DRAWN_prior_sum + CONTESTED_SHOTS_2PT_prior_sum 
  + CONTESTED_SHOTS_3PT_prior_sum + OFF_BOXOUTS_prior_sum + DEF_BOXOUTS_prior_sum 
  + OFF_LOOSE_BALLS_RECOVERED_prior_sum + DEF_LOOSE_BALLS_RECOVERED_prior_sum 
  + DRIVES_prior_sum + DIST_MILES_OFF_prior_sum 
  + DIST_MILES_DEF_prior_sum + PASSES_MADE_prior_sum + PASSES_RECEIVED_prior_sum 
  + SECONDARY_AST_prior_sum + POTENTIAL_AST_prior_sum + FT_AST_prior_sum + OREB_CONTEST_prior_sum 
  + OREB_CHANCES_prior_sum + DREB_CONTEST_prior_sum + DREB_CHANCES_prior_sum,
  data   = injury_specific_sum_1e5,
  family = poisson(link = "log")
)

# summary of fitted model---------------------------------------------
summary(mod_mjk_poisson)

# Choose model for prediction---------------------------------------------
mod_M_final <- mod_mjk_poisson

# for prediction data take the whole data again---------------------------------------------
pred_Mjk_df <- read.csv('./clean_data/injury_specific_summary.csv')

# Predict E[M | x_j] for each player (severity).---------------------------------------------
# injury_specific_summary: injury-level data used to fit severity model (missed_games >= 1)
# Predict per injury,
pred_Mjk_df$pred_M <- predict(mod_M_final, newdata = pred_Mjk_df, type = "response")

# then aggregate to player---------------------------------------------
player_severity <- pred_Mjk_df %>%
  group_by(PLAYER) %>%
  summarise(
    n_injuries   = n(),
    E_M_total    = sum(pred_M, na.rm = TRUE), # expected missed games per player
    E_M_mean     = mean(pred_M, na.rm = TRUE),   # expected missed games per injury
    E_M_median   = median(pred_M, na.rm = TRUE),
    # optional: weighted by a relevance weight, e.g., minutes or time to injury
    E_M_weighted = weighted.mean(pred_M, w = pmax(played_games_prior, 1), na.rm = TRUE)
  ) 

# Use E_M_mean as the player’s severity per injury.---------------------------------------------
pred_I_df$E_M <- player_severity$E_M_mean

# sanity check---------------------------------------------
sum(is.na(pred_I_df$E_M))

# if E_M has NA run below
# rows to fix---------------------------------------------
rows_na <- which(is.na(pred_I_df$E_M))
players_na <- pred_I_df$PLAYER[rows_na]

# make a copy of the scoring frame---------------------------------------------
newd_fix <- pred_Mjk_df

# numeric predictors used by the model---------------------------------------------
tm <- terms(mod_mjk_poisson)                 # our fitted model
vars <- all.vars(tm)                         # predictors in the formula
num_vars <- intersect(vars, names(newd_fix))
num_vars <- num_vars[sapply(newd_fix[num_vars], is.numeric)]

# Zero-impute ONLY for those players (leave everyone else untouched)---------------------------------------------
newd_fix <- zero_impute_by_players(pred_Mjk_df, players_na, num_vars)

# Re-predict for just those players’ rows---------------------------------------------
rows_players <- which(newd_fix$PLAYER %in% players_na)

pred_fix <- predict(mod_mjk_poisson,
  newdata = newd_fix[rows_players, ],
  type    = "response")

# Write predictions back only where E_M was NA for those players---------------------------------------------
rows_to_update <- which(pred_Mjk_df$PLAYER %in% players_na & is.na(pred_Mjk_df$pred_M)) #or take rows_players
pred_Mjk_df$pred_M[rows_to_update] <- pred_fix[match(rows_to_update, rows_players)]

# re predict---------------------------------------------
player_severity <- pred_Mjk_df %>%
  group_by(PLAYER) %>%
  summarise(
    n_injuries   = n(),
    E_M_total    = sum(pred_M, na.rm = TRUE), # expected missed games per player
    E_M_mean     = mean(pred_M, na.rm = TRUE),   # expected missed games per injury
    E_M_median   = median(pred_M, na.rm = TRUE),
    # optional: weighted by a relevance weight, e.g., minutes or time to injury
    E_M_weighted = weighted.mean(pred_M, w = pmax(played_games_prior, 1), na.rm = TRUE)
  ) 

# Use E_M_mean as the player’s severity per injury.---------------------------------------------
pred_I_df$E_M <- player_severity$E_M_mean

# sanity check - if 0 we are good---------------------------------------------
sum(is.na(pred_I_df$E_M))

# now move to use equation 4  E[S_j | x_j] = E[C_j | x_j] * E[M_j | x_j] * E[I_j | x_j]
# Compound expectation with with E[C_j | x_j] = 1---------------------------------------------
pred_I_df$E_S <- pred_I_df$E_I * pred_I_df$E_M 

# ------------------------------------------------------------
# model Cjl with gamma - equation 3
# ------------------------------------------------------------

# training subset---------------------------------------------
missed_game_summary_1e5 <- missed_game_summary_full %>%
  # Filter out excluded reasons or NA
  filter(!PLAYER %in% excluding_players)

# subset players with positive lost game salary---------------------------------------------
missed_game_summary_1e5 <- missed_game_summary_1e5 %>% filter(GAME_SALARY!=0)

# since one game doesn't have attendance data, remove it---------------------------------------------
missed_game_summary_complete <- missed_game_summary_1e5 %>% filter(!is.na(attendance))

# Scale variables (mean 0, sd 1) so PCA isn't dominated by dollar scale---------------------------------------------
df_scaled <- scale(missed_game_summary_complete[, c("attendance","SGV")]) 

# Run PCA---------------------------------------------
pca_res <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

# View proportion of variance explained---------------------------------------------
summary(pca_res)

# Get loadings (weights for each variable in the principal components)---------------------------------------------
pca_res$rotation[, 1]

# Create the Loss Index (first principal component scores)---------------------------------------------
df<-missed_game_summary_complete
df$loss_index <- pca_res$x[, 1]

# Normalize loss_index to a positive scale---------------------------------------------
loss_index_scaled <- exp(df$loss_index)

# Adjusted loss salary as the response for financial loss per missed game---------------------------------------------
df$adj_salary_claimed <- df$GAME_SALARY * loss_index_scaled

# take the box score sum data with other variables ---------------------------------------------
data_for_model <- cbind(df[,"adj_salary_claimed"],missed_game_summary_complete[,-c(2:6)])
names(data_for_model)[1] <-"adj_salary_claimed"

# colnames(data_for_model)

# build the formula -----------------------------------------------------
prior_vars <- c(
  "PERCENTAGE_MIN_prior","PLAYED_RECENT_GAMES_72h_prior","AVG_TRAVEL_MILES_prior",
  "FGA_prior_sum","FG3A_prior_sum","FTA_prior_sum","PF_prior_sum","PTS_prior_sum",
  "STL_prior_sum","BLK_prior_sum","TO_prior_sum","OREB_prior_sum","DREB_prior_sum",
  "AST_prior_sum","PFD_prior_sum","POSS_prior_sum","SCREEN_ASSISTS_prior_sum",
  "DEFLECTIONS_prior_sum","CHARGES_DRAWN_prior_sum","CONTESTED_SHOTS_2PT_prior_sum",
  "CONTESTED_SHOTS_3PT_prior_sum","OFF_BOXOUTS_prior_sum","DEF_BOXOUTS_prior_sum",
  "OFF_LOOSE_BALLS_RECOVERED_prior_sum","DEF_LOOSE_BALLS_RECOVERED_prior_sum",
  "DRIVES_prior_sum","DIST_MILES_OFF_prior_sum","DIST_MILES_DEF_prior_sum",
  "PASSES_MADE_prior_sum","PASSES_RECEIVED_prior_sum","SECONDARY_AST_prior_sum",
  "POTENTIAL_AST_prior_sum","FT_AST_prior_sum","OREB_CONTEST_prior_sum",
  "OREB_CHANCES_prior_sum","DREB_CONTEST_prior_sum","DREB_CHANCES_prior_sum"
)

form_gamma <- as.formula(
  paste("adj_salary_claimed ~", paste(prior_vars, collapse = " + "))
)

# fit Gamma GLM (log link) ---------------------------------------------
mod_gamma <- glm(form_gamma,
  data   = data_for_model,
  family = Gamma(link = "log"))

# summary of fitted model---------------------------------------------
summary(mod_gamma) 

# use full model for prediction---------------------------------------------
mod_C_final_gam<-mod_gamma

# get the full list of players for prediction---------------------------------------------
pred_Cjl_df <- read.csv('./clean_data/missed_game_summary_full.csv')

# Predict per injury,---------------------------------------------
pred_Cjl_df$pred_C <- predict(mod_C_final_gam, newdata = pred_Cjl_df, type = "response")

# then aggregate to player---------------------------------------------
player_severity_gam <- pred_Cjl_df %>%
  group_by(PLAYER) %>%
  summarise(
    n_injuries   = n(),
    E_C_total    = sum(pred_C, na.rm = TRUE), # expected missed games per player
    E_C_mean     = mean(pred_C, na.rm = TRUE),   # expected missed games per injury
    E_C_median   = median(pred_C, na.rm = TRUE),
    # optional: weighted by a relevance weight, e.g., minutes or time to injury
    E_C_weighted = weighted.mean(pred_C, w = pmax(games_prior, 1), na.rm = TRUE)
  )

# Use E_M_mean as the player’s severity per injury.---------------------------------------------
pred_I_df$E_C_gam <- player_severity_gam$E_C_mean

# check for NAs, if not we are good---------------------------------------------
sum(is.na(pred_I_df$E_C_gam))

# apply equation 4: ---------------------------------------------
# E[S_j | x_j] = E[C_j | x_j] * E[M_j | x_j] * E[I_j | x_j]
pred_I_df$E_S_gam_glm_glm <- pred_I_df$E_C_gam * pred_I_df$E_S

# ------------------------------------------------------------
# Results
# ------------------------------------------------------------
# organize the output with relevant metrics and variables
pred_df_out <- pred_I_df %>%
  dplyr::select(any_of(c(
    "PLAYER", "E_I", "E_M", "E_M_glmm", "E_S", "E_C_gam", "E_S_gam_glm_glm","games_count", "age_season",
    "player_position", "injuries", "missed_games", "games_played", "avg_perc_min", "salary_claimed", "salary" )))

head(pred_df_out) #optional

# Safe average missed games (avoid divide-by-zero) --------------------------------
pred_df_out <- pred_df_out %>%
  mutate( avg_missed_games = if_else(injuries > 0,
                               missed_games / injuries, 0))

# tables with 10 players in Sj metric
# pick the right columns present in your pred_df_out -----------------
first_existing <- function(df, candidates) {
  nm <- candidates[candidates %in% names(df)]
  if (length(nm) == 0) NA_character_ else nm[1]
}

# prepare the output table ----------------------------
metrics_map <- c(
  Ij = first_existing(pred_df_out, c("E_I")),                         # expected injuries
  Mj = first_existing(pred_df_out, c("E_M")),                         # expected missed games
  Cj = first_existing(pred_df_out, c("E_C_gam")),                     # expected cost/claims
  S2j = first_existing(pred_df_out, c("E_S")),                        # expected missed game severity
  S3j = first_existing(pred_df_out, c("E_S_gam_glm_glm"))             # compound expected severity
)

# stop if any metric is missing ----------------------------
if (any(is.na(metrics_map))) {
  stop("Missing required metric columns: ", paste(names(metrics_map)[is.na(metrics_map)], collapse = ", "))
}

# build a compact [PLAYER × metric] table ----------------------------
metrics_df <- pred_df_out %>%
  transmute(
    PLAYER,
    Ij = .data[[metrics_map["Ij"]]],
    Mj = .data[[metrics_map["Mj"]]],
    Cj = .data[[metrics_map["Cj"]]],
    S2j = .data[[metrics_map["S2j"]]],
    S3j = .data[[metrics_map["S3j"]]]
  ) %>%
  pivot_longer(-PLAYER, names_to = "metric", values_to = "expected") %>%
  filter(!is.na(expected))

# join some context columns to display with the tables ----------------------------
context_cols <- c("salary","salary_claimed","age_season","player_position","avg_perc_min")
ctx <- pred_df_out %>%
  dplyr::select(tidyselect::any_of(c("PLAYER", context_cols)))

# updated table ----------------------------
metrics_df <- metrics_df %>% left_join(ctx, by = "PLAYER")

# get the output table for the abstract
# Pick the 10 players with highest expected compound financial loss ----------------------------
(players_10 <- with(subset(metrics_df, metric == "S3j"), PLAYER[order(-expected)][1:10]))

# Wider table: one row per player, metrics as columns + context ----------------------------
worst10_table <- metrics_df %>%
  dplyr::filter(PLAYER %in% players_10) %>%
  dplyr::select(PLAYER, metric, expected,
                salary, salary_claimed, age_season, player_position, avg_perc_min) %>%
  tidyr::pivot_wider(names_from = metric, values_from = expected) %>%
  dplyr::select(
    PLAYER, salary, salary_claimed, age_season, player_position, avg_perc_min,
    tidyselect::any_of(c("Ij","Mj","S2j","Cj","S3j"))
  )

# View
colnames(worst10_table)<-c("Player", "Salary", "Salary claimed", "Age", "Position", "Avg. % MINs",
                           "Injuries", "Avg. missed games", "Compound missed games", "Avg. cost", "Compound financial loss")
print(worst10_table, n = Inf) # optional

# table 1  --------------------------------------------------------
# digits per column (same length as ncol(tbl_out)); NA = let kable decide
digits_vec <- rep(NA, ncol(worst10_table))
names(digits_vec) <- names(worst10_table)
digits_vec[c("Injuries",
             "Avg. missed games",
             "Compound missed games",
             "Avg. cost",
             "Compound financial loss")] <- c(3,3,2,0,0)
digits_vec[c("Salary","Salary claimed")] <- 0
digits_vec["Age"] <- 1
digits_vec["Avg. % MINs"] <- 3

latex_code <-
  kbl(
    worst10_table,
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    caption  = "Players with highest 10 expected compound financial loss.",
    label    = "tab:highest_premium_glm_glm_gam",
    align    = "lrrrlrrrrrr",
    digits   = digits_vec
  ) |>
  # remove "striped" (and any row_spec/column_spec with background)
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  add_header_above(c(" " = 1, "Context" = 5,"Expected metrics" = 5))

# paste this code in latex to get the table 1. (note: edit Avg. % MINs as Avg. \% MINs)
latex_code


# figure 1 --------------------------------------------------------
# update column names
colnames(pred_df_out) <- c("Player", "Number of Injuries", "Missed Games Per Injury", "Aggregate Loss (Games)", "Value Per Game ($)", "Aggregate Loss ($)",
                           "Games count", "Age", "Position", "Act. Injuries", "Act. missed games", "Games played",
                           "Avg. % MINs", "Salary claimed", "Salary", "Avg. missed games")

# which models go in each frame ---------------- 
group1 <- c("Number of Injuries","Missed Games Per Injury","Aggregate Loss (Games)")
group2 <- c("Value Per Game ($)", "Aggregate Loss ($)")

# build the two frames using helper function---------------- 
p1 <- make_severity_plot(pred_df_out, group1, ncol = 3)  # E_I, E_M, E_S in one frame
p2 <- make_severity_plot(pred_df_out, group2, ncol = 2)  # E_S_gam_glm_glm, E_C_gam in another

# show them---------------- 
print(p1)
print(p2)

# Adjustments ---------------- 
# Put p2’s y-axis in millions (override the helper’s y-scale)
p2 <- p2 +
  scale_y_log10(
    breaks = c(1e5, 1e6, 1e7, 1e8),
    labels = scales::label_number(scale = 1e-6, suffix = "M", accuracy = 0.1)
  ) +
  labs(y = "Expected Values (Millions)")

# replace the title p2---------------- 
p2 <- p2 + labs(title = "Distribution of Expected Financial Losses by Player Position")

# Use Times New Roman for both plots---------------- 
tnr_theme <- theme(
  text              = element_text(family = "Times New Roman"),
  axis.title.x      = element_text(size = 9, family = "Times New Roman"),
  axis.title.y      = element_text(size = 9, family = "Times New Roman"),
  axis.text.x       = element_text(size = 9, family = "Times New Roman"),
  axis.text.y       = element_text(size = 9, family = "Times New Roman"),
  legend.text       = element_text(size = 9, family = "Times New Roman"),
  strip.text        = element_text(size = 9, family = "Times New Roman")
)

p1 <- p1 + theme_bw() + tnr_theme
p2 <- p2 + theme_bw() + tnr_theme

# show them---------------- 
print(p1)
print(p2)


