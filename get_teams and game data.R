
library(nflfastR)
library(nflreadr)
library(dplyr)
library(slider)


seasons <- 2010:2017

for(s in seasons) {
  
stats <- calculate_stats(seasons = s,summary_level = "week",stat_type = "team",season_type = "REG")  
  
filename <- paste0("teams_stats_",s,".rds")

saveRDS(stats,file.path("season_stats",filename))
  
game <- nflreadr::load_schedules(seasons = s)

filename <- paste0("games_",s,".rds")

saveRDS(game,file.path("games",filename))

}

