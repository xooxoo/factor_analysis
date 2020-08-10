library(readr)
library(tidyr)
library(tibble)
library(dplyr)
library(spacetime)
library(sp)
library(lubridate)

model_data <- read_csv("data/means_by_stations_glo_93-18.csv") %>% 
  filter(year > 1993)
hist_data <- read_csv("data/means_by_stations_historical_50-93.csv") %>% select(-sigma)

full_df <- bind_rows(model_data %>% select(names(hist_data)), hist_data) %>% 
  arrange(year, station, Layer)

get_pcs_and_eofs <- function(df, var_names, layers, out_name){
  for(layer in layers){
    for(variable in var_names){
      message(paste("processing",layer, variable, sep = " "))
      
      # creating temp_df with each layer and variable and nodes which contains it
      # transform it to wide matrix format
      
      temp_df <- df %>% 
        filter(Layer == layer) %>% 
        select(year, station, all_of(variable)) %>% 
        arrange(year, station) %>% 
        pivot_wider(id_cols = year,
                    names_from = station,
                    values_from = all_of(variable)) %>% 
        select_if(~sum(is.na(.)) < 10) %>% 
        mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>% 
        mutate(year = ymd(paste0(year,"01","01")))
      
      stations <- names(temp_df)[-1]  
      
      node_df <- df %>% 
        filter(Layer == layer) %>% 
        select(station, Lat, Lon) %>% 
        group_by(station) %>% 
        summarize_all(first) %>% 
        arrange(station)
      
      node_df <- as.data.frame(node_df)
      rownames(node_df) <- node_df$station
      
      coordinates(node_df) <- ~Lon+Lat
      proj4string(node_df) = '+proj=longlat +datum=WGS84'
      
      node_df <- node_df[match(stations, node_df$station),]
      space <- list(values = stations)
      eof_f_st <-  stConstruct(temp_df[stations], 
                               space, 
                               temp_df$year, 
                               SpatialObj = node_df)
      
      eof_f_1_PCs <- eof(eof_f_st, 'temporal', returnEOFs = FALSE)
      eof_f_2_EOF <- eof(eof_f_st)
      PCs_contrib <- (rbind(eof_f_1_PCs$rotation[,1:5], 
                            summary(eof_f_1_PCs)$importance[,1:5]))
      
      rownames(PCs_contrib) <- gsub('X','', rownames(PCs_contrib))
      PCs_contrib <- rownames_to_column(as.data.frame(PCs_contrib), var = "year") %>% 
        write_csv(paste0("data/PCs/", "PCs_", out_name,
                         "_", layer, "_", variable, ".csv"))
      
      eof_tibble <- rownames_to_column(as.data.frame(eof_f_2_EOF[1:5]), var = "station") %>% 
        mutate(station = gsub("X", "", station)) %>% 
        arrange(station) %>% 
        write_csv(paste0("data/EOFs/", "EOFs_", out_name,
                         "_", layer, "_", variable, ".csv"))
      
    }
  }
}

get_pcs_and_eofs(full_df, c("so", "temp", "Th"), c("Upper", "Atl", "Bottom"), "hist_glo_50-18")

