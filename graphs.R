library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(RColorBrewer)

dir <- "data/PCs/"

filelist <- list.files(dir, pattern = ".csv", recursive = TRUE)

for(file in filelist){
  df <- read_csv(paste0(dir,file))
  
  pcs <- df %>%
    slice(-((nrow(df)-2):nrow(df))) %>% 
    mutate(year = ymd(year))
  
  cumulitive <- df %>% 
    slice_tail(n=3)
  
  cumulitive %>% 
    filter(year == "Proportion of Variance") %>% 
    pivot_longer(cols = -year, names_to="PCs") %>% 
    ggplot(aes(PCs, value))+
    geom_col(fill = "#636e72")+
    geom_line(aes(group = 1))+
    geom_point()+
    theme(panel.background = element_rect(fill = "#dfe6e9"))+
    ggtitle(gsub(".csv","",file))+
    ggsave(paste0(dir, gsub('.csv', '_hist.png',file)), width = 16, height = 10)
  
  pcs %>% 
    select(-PC5) %>% 
    pivot_longer(-year, 
                 names_to = "PCs",
                 names_transform = list(PCs = as.factor)) %>% 
    ggplot()+
    geom_path(aes(year, value, col =PCs), size = 1.5)+
    geom_point(aes(year, value, col = PCs), size = 2, shape = 21, stroke = 1, fill = "white")+
    facet_wrap(PCs~.)+
    theme(legend.position = "null")+
    scale_color_brewer(palette = "Set1")+
    ggtitle(gsub(".csv","",file))+
    theme(panel.background = element_rect(fill = "#2d3436"))+
    ggsave(paste0(dir, gsub('.csv', '.png',file)), width = 16, height = 10)
}

