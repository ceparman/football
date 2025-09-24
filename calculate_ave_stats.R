
library(nflfastR)
library(dplyr)
library(slider)
library(tools)


k = 3 #How many weeks to average over


stat_files <- list.files("season_stats/")


for(file in stat_files){
  

season_data <- readRDS(file.path("season_stats/",file))  
  
ave <- season_data |> filter(season_type == "REG") |>
  group_by(team) |> mutate( week = as.character(week)) |> 
  mutate(across(where(is.numeric),~slide_dbl(.x,mean,.before=k , .after = 0)))

  filename <- paste0("ave_",file_path_sans_ext(file),".rds")
  
  saveRDS(ave,file.path("ave_season_stats",filename))
  
  
  
  
}