

library(nflfastR)
library(dplyr)
library(slider)


#get weekly team stats for a season 


season <- 2025

team_stats_2025 <- calculate_stats(seasons = season,summary_level = "week",stat_type = "team",season_type = "REG")


games_2025  <- nflreadr::load_schedules(seasons = season)




setdiff(names(table(games_2024$away_team)) , names(table(team_stats_2024$team)))



testd <- team_stats_2024 |> filter(team == 'ARI') |> select(week,passing_yards)

r <- testd |>  arrange(week) %>%
  mutate(ave_passing_yards = slider::slide_dbl(passing_yards, mean, .before = 5, .after = 0)) %>%
  ungroup()


k = 3

ave_team_stats_2024 <- team_stats_2024 |> filter(season_type == "REG") |>
                     group_by(team) |> mutate( week = as.character(week)) |> 
                     mutate(across(where(is.numeric),~slide_dbl(.x,mean,.before=k , .after = 0)))


mutate(across(where(is.factor), as.character))
                    
