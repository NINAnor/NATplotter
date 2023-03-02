library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(grid)

naturtyper_long <- readRDS("shinyData/naturtyper_long.rds")
naturtyper <- readRDS("shinyData/naturtyper.rds")


areal <- "#FF9933"
ant <- "#FFCC99"
myBase_size <- 9
nt <- unique(sort(naturtyper$naturtypekode_short))[-102]

for(i in 1:length(nt)){
  print(i)
  
 
  temp <- naturtyper %>% filter(naturtypekode_short==nt[i])
  dat2 <- naturtyper_long %>% 
    filter(naturtype == temp$naturtype[1])
  
  
  temp1 <- temp %>% group_by(kartleggingsår) %>%
    summarise(Antall_lokaliteter = n(),
              Areal_km2 = round(sum(km2), 0))
  
  temp2 <- temp %>% group_by(tilstand) %>%
    summarise(Antall_lokaliteter = n(),
              Areal_km2 = round(sum(km2), 0))
  
  temp3 <- temp %>% group_by(naturmangfold) %>%
    summarise(Antall_lokaliteter = n(),
              Areal_km2 = round(sum(km2), 0))
  
  temp4 <- temp %>% group_by(lokalitetskvalitet) %>%
    summarise(Antall_lokaliteter = n(),
              Areal_km2 = round(sum(km2), 0))
  
  
   g1 <- ggplot(temp1, aes(x = kartleggingsår, y = Antall_lokaliteter))+
    geom_bar(stat="identity",
             fill = ant,
             colour = "grey20",
             linewidth=1.5)+
    theme_bw(base_size = myBase_size)+
     ggtitle(nt[i])
  
 
  g2 <- ggplot(temp1, aes(x = kartleggingsår, y = Areal_km2))+
    geom_bar(stat="identity",
             fill = areal,
             colour = "grey20",
             linewidth=1.5)+
    theme_bw(base_size = myBase_size)
   
  
  g3 <- ggplot(temp2, aes(x = tilstand, y = Antall_lokaliteter))+
    geom_bar(stat="identity",
             fill = ant,
             colour = "grey20",
             linewidth=1.5)+
    theme_bw(base_size = myBase_size)
   
  
  g4 <- ggplot(temp2, aes(x = tilstand, y = Areal_km2))+
    geom_bar(stat="identity",
             fill = areal,
             colour = "grey20",
             linewidth=1.5)+
    theme_bw(base_size = myBase_size)

  g5 <- ggplot(temp3, aes(x = naturmangfold, y = Antall_lokaliteter))+
    geom_bar(stat="identity",
             fill = ant,
             colour = "grey20",
             linewidth=1.5)+
    theme_bw(base_size = myBase_size)
  
  g6 <- ggplot(temp3, aes(x = naturmangfold, y = Areal_km2))+
    geom_bar(stat="identity",
             fill = areal,
             colour = "grey20",
             linewidth=1.5)+
    theme_bw(base_size = myBase_size)
  
  g7 <- ggplot(temp4, aes(x = lokalitetskvalitet, y = Antall_lokaliteter))+
    geom_bar(stat="identity",
             fill = ant,
             colour = "grey20",
             linewidth=1.5)+
    theme_bw(base_size = myBase_size)
  
  g8 <- ggplot(temp4, aes(x = lokalitetskvalitet, y = Areal_km2))+
    geom_bar(stat="identity",
             fill = areal,
             colour = "grey20",
             linewidth=1.5)+
    theme_bw(base_size = myBase_size)
  
  
  if(nrow(dat2)>0){
    facet <- dat2 %>%
      group_by(NiN_variable_code, NiN_variable_value) %>%
      summarise(Antall_lokaliteter = n(),
                Areal_km2 = round(sum(km2), 0)) %>%
      ggplot(aes(x = NiN_variable_value, y = Antall_lokaliteter))+
      geom_bar(stat="identity",
               fill = "#FFCC99",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = myBase_size)+
      facet_wrap(.~NiN_variable_code,
                 scales = "free",
                 ncol = 4)
    } else {facet <- textGrob("No data")}
    
  
  gg_out <- ggdraw() +
    draw_plot(g1, x = 0, y = .75, width = .25, height = .25) +
    draw_plot(g2, x = .25, y = .75, width = .25, height = .25) +
    draw_plot(g3, x = 0.5, y = .75, width = .25, height = .25) +
    draw_plot(g4, x = .75, y = .75, width = .25, height = .25) +
    draw_plot(g5, x = 0, y = .5, width = .25, height = .25) +
    draw_plot(g6, x = .25, y = .5, width = .25, height = .25) +
    draw_plot(g7, x = 0.5, y = .5, width = .25, height = .25) +
    draw_plot(g8, x = .75, y = .5, width = .25, height = .25) +
    draw_plot(facet, x = 0, y = 0, width = 1, height = 0.5)
  

  ggsave(paste0("pdf/", nt[i], "_plots.pdf"),
         plot = gg_out,
         width = 15, height = 20)

}








