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
require(purr)       #for CV settings
require(scales)     #for scaling figure axis
require(ggimage)    #Enables adding images to ggplot

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

# per team summary of master data ---------------------------------------------
team_injury_summary <- read.csv('./clean_data/team_injury_summary.csv')

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

# ------------------------------------------------------------
# figure 1
# ------------------------------------------------------------

ggplot(team_injury_summary %>% 
    mutate(TEAM_ABBREVIATION = reorder(TEAM_ABBREVIATION, total_travel)),
  aes(x = TEAM_ABBREVIATION, y = total_travel)
) +
  geom_point(size = 2) +
  labs(x = "Team", y = "Total Miles") +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

# ------------------------------------------------------------
# figure 2
# ------------------------------------------------------------

ggplot(injury_travel_summary, 
       aes(y = played_games_72h, x = total_travel, size=injuries,color =  avg_perc_min)) +
  geom_point() +
  scale_color_gradient(low = "green", high = "red") +
  labs(
    title = "Injuries and Played Minutes by Average Games Played within 72h vs. Total Travel",
    x = "Total Travel",
    y = "Average Games Played within 72h"
  ) +
  theme_minimal()

# figure 3
ggplot(team_injury_summary, 
       aes(x = player_games_lost, y = salary_dollars_lost)) +
  # A square "background" with color driven by the number_of_injuries
  geom_point(aes(color = injuries),
             size = 25,       # overall square size
             shape = 22,      # square with an outline (stroke)
             fill = "white",  # fill color for the square
             stroke = 2       # thickness of the outline
  ) +
  # The team logos plotted on top
  geom_image(aes(image = team_logo),
             size = 0.1     # adjust for desired logo size
  ) +
  scale_color_gradient(low = "green", high = "red") +
  scale_y_continuous(
    labels = function(x) paste0(x / 1e6, "M")
  )+
  labs(
    title = "Teams by Player Games Lost vs. Salary Dollars Lost",
    x = "Player Games Lost",
    y = "Salary Dollars Lost",
    color = "Injuries"  # legend title
  ) +
  theme_minimal()

# figure 4
ggplot(team_injury_summary, 
       aes(x = player_games_lost, y = w_percentage)) +  # Remove color
  geom_point(aes(color = injuries),
             size = 25,       # overall square size
             shape = 22,      # square with an outline (stroke)
             fill = "white",  # fill color for the square
             stroke = 2       # thickness of the outline
  ) +
  geom_image(aes(image = team_logo), size = 0.1) +  # Adjust size of logos
  scale_color_gradient(low = "green", high = "red") +
  labs(
    title = "Teams by player games lost vs. win percentage",
    y = "Win Percentage",
    x = "Player Games Lost",
    color = "Injuries"
  ) + theme_minimal()


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

# table 10 : summary of fitted model---------------------------------------------
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

# table 11 :summary of fitted model---------------------------------------------
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

# table 12 :summary of fitted model---------------------------------------------
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

# ------------------------------------------------------------
# table 4  
# ------------------------------------------------------------
# digits per column (same length as ncol(tbl_out)); NA = let kable decide
digits_vec <- rep(NA, ncol(worst10_table))
names(digits_vec) <- names(worst10_table)
digits_vec[c("Injuries",
             "Avg. missed games",
             "Compound missed games",
             "Avg. cost",
             "Compound financial loss")] <- c(3, 3, 2, 0, 0)
digits_vec[c("Salary","Salary claimed")] <- 0
digits_vec["Age"] <- 1
digits_vec["Avg. % MINs"] <- 3

latex_code <-
  worst10_table |>
  dplyr::arrange(dplyr::desc(`Compound financial loss`)) |>  # <-- sort descending
  kbl(
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
  add_header_above(c(" " = 1, "Context" = 5, "Expected metrics" = 5))

# paste this code in latex to get the table 1. (note: edit Avg. % MINs as Avg. \% MINs)
latex_code


# figure 5 --------------------------------------------------------
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

########################################################
#OOF setting
########################################################

#use the seed
set.seed(2022)

# ---------------------------------------
# Exclusions (same rules we used in the above section)
# ---------------------------------------
PAYERS_1e5 <- injury_travel_summary$PLAYER[
  injury_travel_summary$salary < 100000 &
    injury_travel_summary$games_played < 5
]
bottom5_players_avg_perc_min <- injury_travel_summary$PLAYER[
  injury_travel_summary$avg_perc_min <= 0.0625
]
excluding_players <- union(PAYERS_1e5, bottom5_players_avg_perc_min)

# ---------------------------------------
# Prepare model datasets (filtered)
# ---------------------------------------
# Ij (player-season, with offset log(games_count))
model_I <- model_data_sum %>%
  filter(!PLAYER %in% excluding_players) %>%
  droplevels()

# Mjk (injury-level)
model_M <- injury_specific_sum %>%
  filter(!PLAYER %in% excluding_players) %>%
  droplevels()

# Cjl base (missed-game-level) + PCA → adj_salary_claimed
missed_game_summary <- missed_game_summary_full %>% filter(GAME_SALARY!=0)
model_C_base <- missed_game_summary %>%
  filter(!PLAYER %in% excluding_players, !is.na(attendance)) %>%
  droplevels()

df_scaled <- scale(model_C_base[, c("attendance","SGV")])
pca_res   <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

model_C <- model_C_base %>%
  mutate(loss_index = pca_res$x[, 1],
         adj_salary_claimed = GAME_SALARY * exp(loss_index))

# ---------------------------------------
# Final model formulas (your finalized settings)
# ---------------------------------------
form_I <- injuries ~ total_travel + avg_perc_min + player_position + 
  played_games_72h + height + weight + age_season + 
  FGA_sum + FG3A_sum + FTA_sum + PF_sum + PTS_sum + STL_sum + 
  BLK_sum + TO_sum + OREB_sum + DREB_sum + AST_sum + PFD_sum + 
  POSS_sum + SCREEN_ASSISTS_sum + DEFLECTIONS_sum + CHARGES_DRAWN_sum + 
  CONTESTED_SHOTS_2PT_sum + CONTESTED_SHOTS_3PT_sum + OFF_BOXOUTS_sum + 
  DEF_BOXOUTS_sum + OFF_LOOSE_BALLS_RECOVERED_sum + DEF_LOOSE_BALLS_RECOVERED_sum + 
  D_FGM_sum + D_FGA_sum + DRIVES_sum + DIST_MILES_OFF_sum + 
  DIST_MILES_DEF_sum + PASSES_MADE_sum + PASSES_RECEIVED_sum + 
  SECONDARY_AST_sum + POTENTIAL_AST_sum + FT_AST_sum + OREB_CONTEST_sum + 
  OREB_CHANCES_sum + DREB_CONTEST_sum + DREB_CHANCES_sum


form_M <- missed_games ~ injuries_prior + PERCENTAGE_MIN_prior + AVG_TRAVEL_MILES_prior + total_miles_prior + 
  FGA_prior_sum + FG3A_prior_sum + FTA_prior_sum + PF_prior_sum + PTS_prior_sum + 
  STL_prior_sum + BLK_prior_sum + TO_prior_sum + OREB_prior_sum + DREB_prior_sum + 
  AST_prior_sum + PFD_prior_sum + POSS_prior_sum + SCREEN_ASSISTS_prior_sum + 
  DEFLECTIONS_prior_sum + CHARGES_DRAWN_prior_sum + CONTESTED_SHOTS_2PT_prior_sum + 
  CONTESTED_SHOTS_3PT_prior_sum + OFF_BOXOUTS_prior_sum + DEF_BOXOUTS_prior_sum + 
  OFF_LOOSE_BALLS_RECOVERED_prior_sum + DEF_LOOSE_BALLS_RECOVERED_prior_sum + 
  DRIVES_prior_sum + DIST_MILES_OFF_prior_sum + DIST_MILES_DEF_prior_sum + 
  PASSES_MADE_prior_sum + PASSES_RECEIVED_prior_sum + SECONDARY_AST_prior_sum + 
  POTENTIAL_AST_prior_sum + FT_AST_prior_sum + OREB_CONTEST_prior_sum + 
  OREB_CHANCES_prior_sum + DREB_CONTEST_prior_sum + DREB_CHANCES_prior_sum

prior_vars_C <- c(
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
form_C <- as.formula(paste("adj_salary_claimed ~", paste(prior_vars_C, collapse = " + ")))

# ---------------------------------------
# Folds at PLAYER level (intersection)
# ---------------------------------------
players_I <- unique(model_I$PLAYER)
players_M <- unique(model_M$PLAYER)
players_C <- unique(model_C$PLAYER)
eligible_players <- Reduce(intersect, list(players_I, players_M, players_C))

#control number of folds
K <- 33
fold_map <- tibble(
  PLAYER = sort(eligible_players),
  fold   = sample(rep(1:K, length.out = length(eligible_players)))
)

# ---------------------------------------
# Containers for OUT-OF-FOLD predictions
# ---------------------------------------
oof_I <- tibble(PLAYER = eligible_players, fold = NA_integer_, E_I_oof = NA_real_)
oof_M <- tibble(PLAYER = eligible_players, fold = NA_integer_, E_M_oof = NA_real_)  # mean per-injury
oof_C <- tibble(PLAYER = eligible_players, fold = NA_integer_, E_C_oof = NA_real_)  # mean per-missed-game

# ---------------------------------------
# CV loop: fit on train players, predict on held-out players
# ---------------------------------------
for (k in 1:K) {
  test_players  <- fold_map$PLAYER[fold_map$fold == k]
  train_players <- fold_map$PLAYER[fold_map$fold != k]
  
  # ----- Ij: fit on train, predict on TEST players -----
  train_I <- model_I %>% filter(PLAYER %in% train_players)
  test_I  <- model_I %>% filter(PLAYER %in% test_players)
  
  if (nrow(train_I) > 0 && nrow(test_I) > 0) {
    fit_I <- glm(form_I,
                 data   = train_I,
                 family = poisson(link = "log"),
                 offset = log(games_count))
    # ensure valid games_count in test
    test_I <- test_I %>%
      mutate(games_count = ifelse(is.na(games_count) | games_count <= 0, 1, games_count))
    mu_I <- predict(fit_I, newdata = test_I, type = "response")
    # we want ONE OOF number per player-season row; aggregate to player mean if multiple rows
    I_player <- test_I %>%
      mutate(mu = mu_I) %>%
      group_by(PLAYER) %>%
      summarise(E_I_oof = mean(mu, na.rm = TRUE), .groups = "drop") %>%
      mutate(fold = k)
    oof_I <- oof_I %>%
      rows_update(I_player, by = "PLAYER")
  }
  
  # ----- Mjk: fit on train injuries, predict per-injury on TEST players, then player-level mean -----
  train_M <- model_M %>% filter(PLAYER %in% train_players)
  test_M  <- model_M %>% filter(PLAYER %in% test_players)
  
  if (nrow(train_M) > 0 && nrow(test_M) > 0) {
    fit_M <- glm(form_M, data = train_M, family = poisson(link = "log"))
    mu_M  <- predict(fit_M, newdata = test_M, type = "response")
    M_player <- test_M %>%
      mutate(mu = mu_M) %>%
      group_by(PLAYER) %>%
      summarise(E_M_oof = mean(mu, na.rm = TRUE), .groups = "drop") %>%
      mutate(fold = k)
    oof_M <- oof_M %>%
      rows_update(M_player, by = "PLAYER")
  }
  
  # ----- Cjl: fit on train missed-games, predict per-missed-game on TEST players, then player-level mean -----
  train_C <- model_C %>% filter(PLAYER %in% train_players, adj_salary_claimed > 0)
  test_C  <- model_C %>% filter(PLAYER %in% test_players)#,  adj_salary_claimed > 0)
  
  if (nrow(train_C) > 0 && nrow(test_C) > 0) {
    # align factor levels in test to train (if any factors in form_C)
    train_mf <- model.frame(form_C, data = train_C)
    fct_vars <- names(Filter(is.factor, train_mf))
    if (length(fct_vars)) {
      for (v in fct_vars) {
        if (v %in% names(test_C)) {
          test_C[[v]] <- factor(test_C[[v]], levels = levels(train_mf[[v]]))
        }
      }
    }
    fit_C <- glm(form_C, data = train_C, family = Gamma(link = "log"))
    mu_C  <- predict(fit_C, newdata = test_C, type = "response")
    C_player <- test_C %>%
      mutate(mu = mu_C) %>%
      group_by(PLAYER) %>%
      summarise(E_C_oof = mean(mu, na.rm = TRUE), .groups = "drop") %>%
      mutate(fold = k)
    oof_C <- oof_C %>%
      rows_update(C_player, by = "PLAYER")
  }
}

# ---------------------------------------
# Combine OOF per-player predictions
# ---------------------------------------
oof_pred <- oof_I %>%
  full_join(oof_M %>% dplyr::select(PLAYER, E_M_oof), by = "PLAYER") %>%
  full_join(oof_C %>% dplyr::select(PLAYER, E_C_oof), by = "PLAYER") %>%
  mutate(
    # compound severity from OOF freq × OOF severity
    E_S_oof = E_I_oof * E_M_oof,
    # optional cost-weighted compound
    E_S_gam_glm_glm_oof = E_S_oof * E_C_oof
  )

# Join context (salary, etc.) for display
context_cols <- c("salary","salary_claimed","age_season","player_position","avg_perc_min","injuries")
ctx <- injury_travel_summary %>%
  dplyr::select(any_of(c("PLAYER", context_cols))) %>%
  distinct()

oof_pred <- oof_pred %>% left_join(ctx, by = "PLAYER")

str(oof_pred)

# ---------------------------------------
# Build top 10 table from OOF predictions (i.e players with higher predicted premiums)
# ---------------------------------------
## ---- pick the right columns present in your pred_df_out -----------------
first_existing <- function(df, candidates) {
  nm <- candidates[candidates %in% names(df)]
  if (length(nm) == 0) NA_character_ else nm[1]
}

metrics_map <- c(
  Ij = first_existing(oof_pred, c("E_I_oof")),                                    # expected injuries
  Mj = first_existing(oof_pred, c("E_M_oof")),                         # ,"E_M" expected missed games
  Cj = first_existing(oof_pred, c("E_C_oof")),                                #"E_S_twd_glm_glm" expected cost/claims
  S2j = first_existing(oof_pred, c("E_S_oof")),
  S3j = first_existing(oof_pred, c("E_S_gam_glm_glm_oof"))                          #,"E_S" expected severity
)

## ---- build a compact [PLAYER × metric] table from OOF predictions --------
metrics_df <- oof_pred %>%
  transmute(
    PLAYER,
    Ij  = .data[[metrics_map["Ij"]]],
    Mj  = .data[[metrics_map["Mj"]]],
    Cj  = .data[[metrics_map["Cj"]]],
    S2j = .data[[metrics_map["S2j"]]],
    S3j = .data[[metrics_map["S3j"]]]
  ) %>%
  pivot_longer(-PLAYER, names_to = "metric", values_to = "expected") %>%
  filter(is.finite(expected))

## ---- join context columns (from oof_pred if present; else fallback) ------
context_cols <- c("salary","salary_claimed","age_season","player_position","avg_perc_min","injuries")

if (all(context_cols %in% names(oof_pred))) {
  ctx <- oof_pred %>% dplyr::select(any_of(c("PLAYER", context_cols))) %>% distinct()
} else {
  ctx <- injury_travel_summary %>% dplyr::select(any_of(c("PLAYER", context_cols))) %>% distinct()
}

metrics_df <- metrics_df %>% left_join(ctx, by = "PLAYER")

# ------------------------------------------------------------
# Top 10 players by OOF compound financial loss (S3j = E_S_gam_glm_glm_oof)
# ------------------------------------------------------------

# Pick the 10 players with highest OOF S3j
players_10_oof <- metrics_df %>%
  filter(metric == "S3j") %>%
  arrange(desc(expected)) %>%
  slice(1:10) %>%
  pull(PLAYER)

# Wider table: one row per player, metrics as columns + context
worst10_table_oof <- metrics_df %>%
  filter(PLAYER %in% players_10_oof) %>%
  dplyr::select(
    PLAYER, metric, expected,
    salary, salary_claimed, age_season, player_position, avg_perc_min, injuries
  ) %>%
  tidyr::pivot_wider(names_from = metric, values_from = expected) %>%
  dplyr::select(
    PLAYER, salary, salary_claimed, age_season, player_position, avg_perc_min,
    tidyselect::any_of(c("Ij","Mj","S2j","Cj","S3j"))
  ) %>%
  arrange(desc(S3j))

# Nice column names (same style as your earlier table)
colnames(worst10_table_oof) <- c(
  "Player", "Salary", "Salary claimed", "Age", "Position", "Avg. % MINs", 
  "Expected injuries", "Avg. missed games", "Compound missed games",
  "Avg. cost", "Compound financial loss"
)

#table with predicted claim values
worst10_table_oof

# ------------------------------------------------------------
# table 13  
# ------------------------------------------------------------
# digits per column (same as before)
digits_vec <- rep(NA, ncol(worst10_table_oof))
names(digits_vec) <- names(worst10_table_oof)

digits_vec[c("Expected injuries",
             "Avg. missed games",
             "Compound missed games",
             "Avg. cost",
             "Compound financial loss")] <- c(3, 3, 2, 0, 0)
digits_vec[c("Salary","Salary claimed")] <- 0
digits_vec["Age"]         <- 1
digits_vec["Avg. % MINs"] <- 3
align_str <- "lrrrlrrrrrr" # length 11

latex_code_oof <-
  kbl(
    worst10_table_oof,
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    caption  = "Players with the 10 highest out-of-fold expected compound financial loss.",
    label    = "tab:highest_premium_oof",
    align    = align_str,
    digits   = digits_vec
  ) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  add_header_above(c(" " = 1, "Context" = 5, "OOF expected metrics" = 5))

latex_code_oof

# ------------------------------------------------------------
# figure 6 
# ------------------------------------------------------------

# update column names
# ------------------------------------------------------------
# Rename OOF prediction columns to nice display names
# ------------------------------------------------------------

oof_pred_named <- oof_pred %>%
  dplyr::rename(
    `Number of Injuries`    = E_I_oof,
    `Missed Games Per Injury` = E_M_oof,
    `Aggregate Loss (Games)`  = E_S_oof,
    `Value per game ($)`      = E_C_oof,
    `Aggregate Loss ($)`      = E_S_gam_glm_glm_oof
  )

group1 <- c("Number of Injuries", "Missed Games Per Injury", "Aggregate Loss (Games)")
group2 <- c("Value per game ($)", "Aggregate Loss ($)")


p1 <- make_severity_plot(oof_pred_named, group1, ncol = 3)
p2 <- make_severity_plot(oof_pred_named, group2, ncol = 2)

# Put p2’s y-axis in millions (override helper’s y-scale)
p2 <- p2 +
  scale_y_log10(
    breaks = c(1e5, 1e6, 1e7, 1e8),
    labels = scales::label_number(scale = 1e-6, suffix = "M", accuracy = 0.1)
  ) +
  labs(y = "Expected Values (Millions)")

# Replace the title
p2 <- p2 + labs(title = "Distribution of Expected Financial Losses by Player Position")

# (Optional) Times New Roman + theme_bw like before
tnr_theme <- theme(
  text         = element_text(family = "Times New Roman"),
  axis.title.x = element_text(size = 9),
  axis.title.y = element_text(size = 9),
  axis.text.x  = element_text(size = 9),
  axis.text.y  = element_text(size = 9),
  strip.text   = element_text(size = 9),
  legend.text  = element_text(size = 8)
)

p1 <- p1 + theme_bw() + tnr_theme
p2 <- p2 + theme_bw() + tnr_theme

print(p1)
print(p2)

# ------------------------------------------------------------
# Sensitivity: 10% reduction in minutes-related covariates
# for top-10 players (players_10)
# ------------------------------------------------------------

# ------------------------------------------------------------
# Baseline Ij, Mj, Cj, Sj for worst-10 players
# ------------------------------------------------------------

# Ij (frequency) from pred_I_df
base_I <- pred_I_df %>%
  dplyr::filter(PLAYER %in% players_10) %>%
  dplyr::select(Player = PLAYER, I_base = E_I)

# Mj (severity) from player_severity (E_M_mean)
base_M <- player_severity %>%
  dplyr::filter(PLAYER %in% players_10) %>%
  dplyr::select(Player = PLAYER, M_base = E_M_mean)

# Cj (cost per game) from player_severity_gam (E_C_mean)
base_C <- player_severity_gam %>%
  dplyr::filter(PLAYER %in% players_10) %>%
  dplyr::select(Player = PLAYER, C_base = E_C_mean)

# Combine and baseline Sj
sens_base <- base_I %>%
  dplyr::left_join(base_M, by = "Player") %>%
  dplyr::left_join(base_C, by = "Player") %>%
  dplyr::mutate(S_base = I_base * M_base * C_base)

sens_base

# ------------------------------------------------------------
# Scenario A: Minutes -10%
# ------------------------------------------------------------

## Ij
pred_I_df_min <- pred_I_df %>%
  dplyr::mutate(
    avg_perc_min = dplyr::if_else(
      PLAYER %in% players_10,
      avg_perc_min * 0.9,
      avg_perc_min
    )
  )

pred_I_df_min$E_I_min <- predict(
  mod_I_final,
  newdata = pred_I_df_min,
  type   = "response"
)

sens_I_min <- pred_I_df_min %>%
  dplyr::filter(PLAYER %in% players_10) %>%
  dplyr::select(Player = PLAYER, I_min = E_I_min)

## Mj
pred_Mjk_df_min <- pred_Mjk_df %>%
  dplyr::mutate(
    PERCENTAGE_MIN_prior = dplyr::if_else(
      PLAYER %in% players_10,
      PERCENTAGE_MIN_prior * 0.9,
      PERCENTAGE_MIN_prior
    )
  )

pred_Mjk_df_min$pred_M_min <- predict(
  mod_M_final,
  newdata = pred_Mjk_df_min,
  type   = "response"
)

player_severity_min <- pred_Mjk_df_min %>%
  dplyr::group_by(PLAYER) %>%
  dplyr::summarise(
    M_min = mean(pred_M_min, na.rm = TRUE),
    .groups = "drop"
  )

sens_M_min <- player_severity_min %>%
  dplyr::filter(PLAYER %in% players_10) %>%
  dplyr::select(Player = PLAYER, M_min)

## Cj
pred_Cjl_df_min <- pred_Cjl_df %>%
  dplyr::mutate(
    PERCENTAGE_MIN_prior = dplyr::if_else(
      PLAYER %in% players_10,
      PERCENTAGE_MIN_prior * 0.9,
      PERCENTAGE_MIN_prior
    )
  )

pred_Cjl_df_min$pred_C_min <- predict(
  mod_C_final_gam,
  newdata = pred_Cjl_df_min,
  type   = "response"
)

player_severity_gam_min <- pred_Cjl_df_min %>%
  dplyr::group_by(PLAYER) %>%
  dplyr::summarise(
    C_min = mean(pred_C_min, na.rm = TRUE),
    .groups = "drop"
  )

sens_C_min <- player_severity_gam_min %>%
  dplyr::filter(PLAYER %in% players_10) %>%
  dplyr::select(Player = PLAYER, C_min)

## Combine scenario A
sens_A <- sens_base %>%
  dplyr::left_join(sens_I_min, by = "Player") %>%
  dplyr::left_join(sens_M_min, by = "Player") %>%
  dplyr::left_join(sens_C_min, by = "Player") %>%
  dplyr::mutate(S_A = I_min * M_min * C_min) %>%
  dplyr::select(Player, S_A)


# ------------------------------------------------------------
# Scenario B: Travel -10%
# ------------------------------------------------------------

## Ij – total_travel
pred_I_df_tr <- pred_I_df %>%
  dplyr::mutate(
    total_travel = dplyr::if_else(
      PLAYER %in% players_10,
      total_travel * 0.9,
      total_travel
    )
  )

pred_I_df_tr$E_I_tr <- predict(
  mod_I_final,
  newdata = pred_I_df_tr,
  type   = "response"
)

sens_I_tr <- pred_I_df_tr %>%
  dplyr::filter(PLAYER %in% players_10) %>%
  dplyr::select(Player = PLAYER, I_tr = E_I_tr)

## Mj – AVG_TRAVEL_MILES_prior and total_miles_prior
pred_Mjk_df_tr <- pred_Mjk_df %>%
  dplyr::mutate(
    AVG_TRAVEL_MILES_prior = dplyr::if_else(
      PLAYER %in% players_10,
      AVG_TRAVEL_MILES_prior * 0.9,
      AVG_TRAVEL_MILES_prior
    ),
    total_miles_prior = dplyr::if_else(
      PLAYER %in% players_10,
      total_miles_prior * 0.9,
      total_miles_prior
    )
  )

pred_Mjk_df_tr$pred_M_tr <- predict(
  mod_M_final,
  newdata = pred_Mjk_df_tr,
  type   = "response"
)

player_severity_tr <- pred_Mjk_df_tr %>%
  dplyr::group_by(PLAYER) %>%
  dplyr::summarise(
    M_tr = mean(pred_M_tr, na.rm = TRUE),
    .groups = "drop"
  )

sens_M_tr <- player_severity_tr %>%
  dplyr::filter(PLAYER %in% players_10) %>%
  dplyr::select(Player = PLAYER, M_tr)

## Cj – AVG_TRAVEL_MILES_prior
pred_Cjl_df_tr <- pred_Cjl_df %>%
  dplyr::mutate(
    AVG_TRAVEL_MILES_prior = dplyr::if_else(
      PLAYER %in% players_10,
      AVG_TRAVEL_MILES_prior * 0.9,
      AVG_TRAVEL_MILES_prior
    )
  )

pred_Cjl_df_tr$pred_C_tr <- predict(
  mod_C_final_gam,
  newdata = pred_Cjl_df_tr,
  type   = "response"
)

player_severity_gam_tr <- pred_Cjl_df_tr %>%
  dplyr::group_by(PLAYER) %>%
  dplyr::summarise(
    C_tr = mean(pred_C_tr, na.rm = TRUE),
    .groups = "drop"
  )

sens_C_tr <- player_severity_gam_tr %>%
  dplyr::filter(PLAYER %in% players_10) %>%
  dplyr::select(Player = PLAYER, C_tr)

## Combine scenario B
sens_B <- sens_base %>%
  dplyr::left_join(sens_I_tr, by = "Player") %>%
  dplyr::left_join(sens_M_tr, by = "Player") %>%
  dplyr::left_join(sens_C_tr, by = "Player") %>%
  dplyr::mutate(S_B = I_tr * M_tr * C_tr) %>%
  dplyr::select(Player, S_B)

# ------------------------------------------------------------
# Scenario C: 72h games -10%
# ------------------------------------------------------------

## Ij – played_games_72h
pred_I_df_72h <- pred_I_df %>%
  dplyr::mutate(
    played_games_72h = dplyr::if_else(
      PLAYER %in% players_10,
      played_games_72h * 0.9,
      played_games_72h
    )
  )

pred_I_df_72h$E_I_72h <- predict(
  mod_I_final,
  newdata = pred_I_df_72h,
  type   = "response"
)

sens_I_72h <- pred_I_df_72h %>%
  dplyr::filter(PLAYER %in% players_10) %>%
  dplyr::select(Player = PLAYER, I_72h = E_I_72h)

## Mj – unchanged, reuse baseline
sens_M_72h <- sens_base %>%
  dplyr::select(Player, M_72h = M_base)

## Cj – PLAYED_RECENT_GAMES_72h_prior
pred_Cjl_df_72h <- pred_Cjl_df %>%
  dplyr::mutate(
    PLAYED_RECENT_GAMES_72h_prior = dplyr::if_else(
      PLAYER %in% players_10,
      PLAYED_RECENT_GAMES_72h_prior * 0.9,
      PLAYED_RECENT_GAMES_72h_prior
    )
  )

pred_Cjl_df_72h$pred_C_72h <- predict(
  mod_C_final_gam,
  newdata = pred_Cjl_df_72h,
  type   = "response"
)

player_severity_gam_72h <- pred_Cjl_df_72h %>%
  dplyr::group_by(PLAYER) %>%
  dplyr::summarise(
    C_72h = mean(pred_C_72h, na.rm = TRUE),
    .groups = "drop"
  )

sens_C_72h <- player_severity_gam_72h %>%
  dplyr::filter(PLAYER %in% players_10) %>%
  dplyr::select(Player = PLAYER, C_72h)

## Combine scenario C
sens_C <- sens_base %>%
  dplyr::left_join(sens_I_72h,  by = "Player") %>%
  dplyr::left_join(sens_M_72h,  by = "Player") %>%
  dplyr::left_join(sens_C_72h,  by = "Player") %>%
  dplyr::mutate(S_C = I_72h * M_72h * C_72h) %>%
  dplyr::select(Player, S_C)

# ------------------------------------------------------------
# Combine all scenarios + base and compute % change in Sj
# ------------------------------------------------------------

sens_S_all <- sens_base %>%
  dplyr::select(Player, S_base) %>%
  dplyr::left_join(sens_A, by = "Player") %>%
  dplyr::left_join(sens_B, by = "Player") %>%
  dplyr::left_join(sens_C, by = "Player") %>%
  tidyr::pivot_longer(
    cols      = c(S_base, S_A, S_B, S_C),
    names_to  = "scenario",
    values_to = "S"
  ) %>%
  dplyr::mutate(
    scenario = dplyr::recode(
      scenario,
      "S_base" = "Base",
      "S_A"    = "Minutes -10%",
      "S_B"    = "Travel -10%",
      "S_C"    = "72h games -10%"
    )
  ) %>%
  dplyr::group_by(Player) %>%
  dplyr::mutate(
    S_base_player = S[scenario == "Base"][1],
    dS_pct        = 100 * (S - S_base_player) / S_base_player
  ) %>%
  dplyr::ungroup()

sens_S_all %>%
  dplyr::arrange(Player, scenario) %>%
  dplyr::select(Player, scenario, S, dS_pct)


# ------------------------------------------------------------
# table 5- Sensitivity table for top-10 players
# ------------------------------------------------------------


# Convert S to millions and reshape wide
sens_table <- sens_S_all %>%
  mutate(
    S_M = S / 1e6,   # Sj in millions
    scenario_code = dplyr::recode(
      scenario,
      "Base"            = "base",
      "Minutes -10%"    = "min",
      "Travel -10%"     = "travel",
      "72h games -10%"  = "g72h"
    )
  ) %>%
  dplyr::select(Player, scenario_code, S_M, dS_pct) %>%
  pivot_wider(
    names_from  = scenario_code,
    values_from = c(S_M, dS_pct),
    names_sep   = "_"
  ) %>%
  rename(
    Base              = S_M_base,
    MPG               = S_M_min,
    Travel_miles      = S_M_travel,
    Games_in_72h      = S_M_g72h,
    MPG_pct           = dS_pct_min,
    Travel_miles_pct  = dS_pct_travel,
    Games_in_72h_pct  = dS_pct_g72h
  ) %>%
  # ensure no NA % changes sneak into the table
  mutate(
    MPG_pct    = ifelse(is.na(MPG_pct),    0, MPG_pct),
    Travel_miles_pct = ifelse(is.na(Travel_miles_pct), 0, Travel_miles_pct),
    Games_in_72h_pct    = ifelse(is.na(Games_in_72h_pct),    0, Games_in_72h_pct)
  ) %>%
  arrange(desc(Base))

(sens_table<-sens_table[,-6])


digits_vec <- c(
  Base             = 2,
  MPG              = 2,
  Travel_miles     = 2,
  Games_in_72h     = 2,
  MPG_pct          = 1,
  Travel_miles_pct = 1,
  Games_in_72h_pct = 1
)

sens_latex <-
  kbl(
    sens_table,
    format   = "latex",
    booktabs = TRUE,
    caption  = "Sensitivity of expected compound financial loss for top 10 players under alternative covariate scenarios.",
    label    = "tab:top10_sensitivity_Sj",
    align    = "lrrrrrrr",          # 1 (Player) + 7 numeric columns
    digits   = c(NA, digits_vec),   # first is Player
    na       = ""
  ) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  add_header_above(
    c(
      " " = 1,
      "Expected compound loss - Sj (Millions)" = 4,
      "% change from base Sj" = 3
    )
  )

sens_latex

# ------------------------------------------------------------
# Plot for table 5 : Sj under Base and each scenario for worst-10 players
# ------------------------------------------------------------

sens_S_all$scenario <- factor(
  sens_S_all$scenario,
  levels = c("Base", "Minutes -10%", "Travel -10%", "72h games -10%")
)

gg_sens_S <- ggplot(
  sens_S_all,
  aes(x = scenario, y = S, group = Player, colour = Player)
) +
  geom_line(alpha = 0.7) +
  geom_point(size = 1.8) +
  scale_y_log10(
    labels = scales::label_number(scale = 1e-6, suffix = "M", accuracy = 0.1)
  ) +
  labs(
    x = "",
    y = "Expected Compound Financial Loss (Millions, log scale)",
    title = "Sensitivity of Expected Compound Financial Loss (Sj) for Top 10 Players",
    colour = "Player"
  ) +
  theme_bw() + tnr_theme

print(gg_sens_S)

