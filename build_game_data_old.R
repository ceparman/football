
library(nflfastR)
library(dplyr)
library(slider)
library(tools)




calculate_stat_values <- function(stats ){
  
  
  new_stats <- stats |> 
    mutate(  pass_yard_per_attempt = passing_yards/attempts,
             rushing_yard_per_attempt = rushing_yards/carries,
             rush_pass_ratio = carries/attempts,
             yac = passing_yards_after_catch / attempts,
             fg_perc = fg_made/fg_att,
             turnovers = rushing_fumbles_lost + passing_interceptions
    )
  
  
  return(new_stats)  
  
  
  
}




stats_to_use <- c( "passing_yards", "rushing_yards","penalty_yards",
                   "sacks_suffered","def_sacks","fg_perc","turnovers",
                   "receiving_epa","passing_cpoe","rushing_epa","pass_yard_per_attempt","rushing_yard_per_attempt","rush_pass_ratio","yac")  
k=5

for(s in 2010:2024) {

#get games 

games <- readRDS(file.path("games",paste0("games_",s,".rds") )) |> filter(game_type== "REG")  |>
  mutate(away_team = case_when( away_team == "LV" ~ "OAK",
                                away_team == "STL" ~ "LA",
                                away_team == "SD"  ~ "LAC",
                                TRUE ~ away_team )) |>
  mutate(home_team = case_when( home_team == "LV" ~ "OAK",
                                home_team == "STL" ~ "LA",
                                home_team == "SD"  ~ "LAC",
                                TRUE ~ home_team )) 

#build points scored and allowed 

home_point <- games |> 
  rename(team = home_team, points_scored = home_score, points_allowed = away_score) |>
  select(week, team, points_allowed, points_scored)
away_point <- games |> 
  rename(team = away_team, points_scored = away_score, points_allowed = home_score) |>
  select(week, team, points_allowed, points_scored)


points <- rbind(home_point,away_point) |> arrange (week,team) |>
  group_by(team) |> mutate( week = as.character(week)) |> 
  mutate(across(where(is.numeric),~slide_dbl(.x,mean,.before=k , .after = 0))) |> 
  mutate(week = as.numeric(week) +1) 

#|>
 # mutate(team = case_when( team == "LV" ~ "OAK",
#                           TRUE ~ team ))


#load team stats

stats <-  readRDS(file.path("ave_season_stats",paste0("ave_teams_stats_",s,".rds") )) |>
  mutate(week = as.numeric(week)) |>
  mutate(team = case_when( team == "LV" ~ "OAK",
                           TRUE ~ team )) |>
    calculate_stat_values() 
  

#for each game get home and away stat

#Cant do week one because we must lag data
#select_stats <- stats |> select(all_of(c("week","team",stats_to_use) )) 

select_stats <- stats |> select(-c("season","season_type","opponent_team" ) )

home_game <- games |> filter(week > 1) |> 
       mutate(stats_week = case_when(  home_rest > 11 ~week -2,
                                       TRUE ~ week -1)) |> 
       mutate(game_result = home_score - away_score) |>
         group_by(week) |>
          left_join(select_stats, by = join_by( stats_week == week, home_team == team )) |>
           select(c(game_id , home_team ,week,stats_to_use,game_result,spread_line,season,surface,wind,
                    temp,location,roof,away_rest,home_rest,div_game)) |>
            mutate(home_beat_spread = as.integer((game_result - spread_line) > 0) ) |> 
            rename_at(vars(stats_to_use), ~ paste0("home_",stats_to_use)) |>
            left_join(points, by = join_by( week == week, home_team == team )) |>
            rename( home_points_allowed = points_allowed, home_points_scored = points_scored) |>
            ungroup()
         
away_game <- games |> filter(week > 1) |>
             mutate(stats_week = case_when(  away_rest > 11 ~ week -2,
                                             TRUE ~ week -1)) |> 
               group_by(week) |>
            left_join(select_stats, by = join_by( stats_week == week, away_team == team )) |>
            select(c(game_id , away_team, week,stats_to_use)) |>
           left_join(points, by = join_by( week == week, away_team == team )) |>
             rename( away_points_allowed = points_allowed, away_points_scored = points_scored) |>
            rename_at(vars(stats_to_use), ~ paste0("away_",stats_to_use)) |>
               ungroup()
          

  
game_data <- home_game |> left_join(away_game,by=join_by(game_id,week)) |>
  mutate( temp = case_when(is.na(temp) ~70,
                                 TRUE ~temp) ) |>
  mutate( wind = case_when(is.na(wind) ~0,
                           TRUE ~wind) ) |>
  mutate( surface = case_when(surface == "grass" ~"grass",
                           TRUE ~ "turf") )

           
filename  <- paste0("model_data_",s,".rds")

saveRDS(game_data ,file.path("model_data",filename))


}
      








