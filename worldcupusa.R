library(hockeyR)
library(fastRhockey)
library(dplyr)
library(tidyverse)
library(gt)
library(gtExtras)


nhl_stats <- get_skater_stats_hr(2024)

centers <- nhl_stats %>%
  filter(position == "C") 

stats_c <- centers %>%
  select(goals, assists, shots_on_goal, plus_minus, shooting_percent, faceoff_win_percent)

stats_c$shooting_percent[which(is.na(stats_c$shooting_percent))] <- 0
stats_c$faceoff_win_percent[which(is.na(stats_c$faceoff_win_percent))] <- 0

pca_c <- prcomp(stats_c, scale = TRUE)
weights_c <- pca_c$rotation[,1]
weighted_avg_c <- rowSums(stats_c * weights_c)
centers$metric_c <- weighted_avg_c

left_wingers <- nhl_stats %>%
  filter(position == "LW") 

stats_lw <- left_wingers %>%
  select(goals, assists, shots_on_goal, plus_minus, shooting_percent)

stats_lw$shooting_percent[which(is.na(stats_lw$shooting_percent))] <- 0

pca_lw <- prcomp(stats_lw, scale = TRUE)
weights_lw <- pca_lw$rotation[,1]
weighted_avg_lw <- rowSums(stats_lw * weights_lw)
left_wingers$metric_lw <- weighted_avg_lw

right_wingers <- nhl_stats %>%
  filter(position == "RW") 

stats_rw <- right_wingers %>%
  select(goals, assists, shots_on_goal, plus_minus, shooting_percent)

stats_rw$shooting_percent[which(is.na(stats_rw$shooting_percent))] <- 0

pca_rw <- prcomp(stats_rw, scale = TRUE)
weights_rw <- pca_rw$rotation[,1]
weighted_avg_rw <- rowSums(stats_rw * weights_rw)
right_wingers$metric_rw <- weighted_avg_rw

att_defense <- nhl_stats %>%
  filter(position == "D")

stats_ad <- att_defense %>%
  select(goals, assists, shots_on_goal, plus_minus, shooting_percent)

stats_ad$shooting_percent[which(is.na(stats_ad$shooting_percent))] <- 0

pca_ad <- prcomp(stats_ad, scale = TRUE)
weights_ad <- pca_ad$rotation[,1]
weighted_avg_ad <- rowSums(stats_ad * weights_ad)
att_defense$metric_ad <- weighted_avg_ad

def_defense <- nhl_stats %>%
  filter(position == "D")

stats_dd <- def_defense %>%
  select(blocks, hits, time_on_ice)

pca_dd <- prcomp(stats_dd, scale = TRUE)
weights_dd <- pca_dd$rotation[,1]
weighted_avg_dd <- rowSums(stats_dd * weights_dd)
def_defense$metric_dd <- weighted_avg_dd

centers[,31] <- as.data.frame(apply(centers[,31], 2, function(x) rank(x) / length(x) * 100))
left_wingers[,31] <- as.data.frame(apply(left_wingers[,31], 2, function(x) rank(x) / length(x) * 100))
right_wingers[,31] <- as.data.frame(apply(right_wingers[,31], 2, function(x) rank(x) / length(x) * 100))
att_defense[,31] <- as.data.frame(apply(att_defense[,31], 2, function(x) rank(x) / length(x) * 100))
def_defense[,31] <- as.data.frame(apply(def_defense[,31], 2, function(x) rank(x) / length(x) * 100))

goalies <- get_goalie_stats_hr(2024) 

stats_g <- goalies %>%
  select(saves, goals_against_average, quality_starts)

pca_g <- prcomp(stats_g, scale = TRUE)
weights_g <- pca_g$rotation[,1]
weighted_avg_g <- rowSums(stats_g * weights_g)
goalies$metric_g <- weighted_avg_g

goalies[,29] <- as.data.frame(apply(goalies[,29], 2, function(x) rank(x) / length(x) * 100))

centers <- centers %>%
  select(player, team_abbr, metric_c)

left_wingers <- left_wingers %>%
  select(player, team_abbr, metric_lw)

right_wingers <- right_wingers %>%
  select(player, team_abbr, metric_rw)

att_defense <- att_defense %>%
  select(player, team_abbr, metric_ad)

def_defense <- def_defense %>%
  select(player, team_abbr, metric_dd)

goalies <- goalies %>%
  select(player, team_abbr, metric_g)

# had to do manual search for american due to nhl databases not responding

centers <- centers %>%
  arrange(-metric_c) %>%
  filter(row_number() %in% c(2,5,11,20,30))

left_wingers <- left_wingers %>%
  arrange(-metric_lw) %>%
  filter(row_number() %in% c(2,4,6,8,14))

right_wingers <- right_wingers %>%
  arrange(-metric_rw) %>%
  filter(row_number() %in% c(3,4,7,14,15))

right_wingers <- right_wingers %>% head(4) # only 9 wingers, percentile of last right winger worse than percentile of last left winger

att_defense <- att_defense %>%
  arrange(-metric_ad) %>%
  filter(row_number() %in% c(2,7,10,17))

def_defense <- def_defense %>%
  arrange(-metric_dd) %>%
  filter(row_number() %in% c(6,9,16,17))

goalies <- goalies %>%
  arrange(-metric_g) %>%
  filter(row_number() %in% c(1,2,4))

logos <- team_logos_colors %>%
  select(team_abbr, team_logo_espn)

centers <- inner_join(centers, logos, by = "team_abbr") %>% select(-team_abbr)

ct <- centers %>% gt() %>% 
  gt_img_rows(columns = team_logo_espn) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(player, team_logo_espn, metric_c)
  ) %>%
  data_color(
    columns = metric_c,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::purple_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team_logo_espn = md("**Team**"),
    metric_c = md("**Percentile**")
  ) %>%
  tab_header(
    title = md("**US National Team Projected Centers**"),
    subtitle = "2025 Hockey World Cup - Based on 23-24 NHL Stats"
  ) 

gtsave(ct, "centers.png")

left_wingers <- inner_join(left_wingers, logos, by = "team_abbr") %>% select(-team_abbr)

lwt <- left_wingers %>% gt() %>% 
  gt_img_rows(columns = team_logo_espn) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(player, team_logo_espn, metric_lw)
  ) %>%
  data_color(
    columns = metric_lw,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::purple_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team_logo_espn = md("**Team**"),
    metric_lw = md("**Percentile**")
  ) %>%
  tab_header(
    title = md("**US National Team Projected Left Wingers**"),
    subtitle = "2025 Hockey World Cup - Based on 23-24 NHL Stats"
  ) 

gtsave(lwt, "leftwingers.png")

right_wingers <- inner_join(right_wingers, logos, by = "team_abbr") %>% select(-team_abbr)

rwt <- right_wingers %>% gt() %>% 
  gt_img_rows(columns = team_logo_espn) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(player, team_logo_espn, metric_rw)
  ) %>%
  data_color(
    columns = metric_rw,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::purple_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team_logo_espn = md("**Team**"),
    metric_rw = md("**Percentile**")
  ) %>%
  tab_header(
    title = md("**US National Team Projected Right Wingers**"),
    subtitle = "2025 Hockey World Cup - Based on 23-24 NHL Stats"
  ) 

gtsave(rwt, "rightwingers.png")

att_defense <- inner_join(att_defense, logos, by = "team_abbr") %>% select(-team_abbr)

adt <- att_defense %>% gt() %>% 
  gt_img_rows(columns = team_logo_espn) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(player, team_logo_espn, metric_ad)
  ) %>%
  data_color(
    columns = metric_ad,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::purple_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team_logo_espn = md("**Team**"),
    metric_ad = md("**Percentile**")
  ) %>%
  tab_header(
    title = md("**US National Team Projected Attacking Defensemen**"),
    subtitle = "2025 Hockey World Cup - Based on 23-24 NHL Stats"
  ) 

gtsave(adt, "attackingdefense.png")

def_defense <- inner_join(def_defense, logos, by = "team_abbr") %>% select(-team_abbr)

ddt <- def_defense %>% gt() %>% 
  gt_img_rows(columns = team_logo_espn) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(player, team_logo_espn, metric_dd)
  ) %>%
  data_color(
    columns = metric_dd,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::purple_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team_logo_espn = md("**Team**"),
    metric_dd = md("**Percentile**")
  ) %>%
  tab_header(
    title = md("**US National Team Projected Defending Defensemen**"),
    subtitle = "2025 Hockey World Cup - Based on 23-24 NHL Stats"
  ) 

gtsave(ddt, "defendingdefense.png")

goalies <- inner_join(goalies, logos, by = "team_abbr") %>% select(-team_abbr)

got <- goalies %>% gt() %>% 
  gt_img_rows(columns = team_logo_espn) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(player, team_logo_espn, metric_g)
  ) %>%
  data_color(
    columns = metric_g,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::purple_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team_logo_espn = md("**Team**"),
    metric_g = md("**Percentile**")
  ) %>%
  tab_header(
    title = md("**US National Team Projected Goalies**"),
    subtitle = "2025 Hockey World Cup - Based on 23-24 NHL Stats"
  ) 

gtsave(got, "goalies.png")
