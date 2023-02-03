library(sf)
library(dplyr)
library(units)
library(stringr)
# This script take the file Natur_Naturtyper_NiN_norge_med_svalbard_25833.gdb, stored in 
# "P:/41001581_egenutvikling_anders_kolstad/data/" and processes it for the shiny app.
# Replace the file and rerun the script when neccessary.

dir <- substr(getwd(), 1,2)

path <- ifelse(dir == "C:", 
                "P:/41001581_egenutvikling_anders_kolstad/data/",
                "/data/Egenutvikling/41001581_egenutvikling_anders_kolstad/data/")

dat <- sf::read_sf(paste0(path, "Natur_Naturtyper_NiN_norge_med_svalbard_25833.gdb"))
dat$km2 <- drop_units(st_area(dat))/1000
names(dat) <- textclean::replace_non_ascii(names(dat))
dat <- dat %>%
  as.data.frame() %>%
  select(-SHAPE) %>%
  mutate(uk_ingenstatus = str_replace_na(uk_ingenstatus, "Nei")) %>%
  mutate(uk_ingenstatus = str_replace(uk_ingenstatus, "1", "Ja")) %>%
  
  mutate(uk_naertruet = str_replace_na(uk_naertruet, "Nei")) %>%
  mutate(uk_naertruet = str_replace(uk_naertruet, "1", "Ja")) %>%
  
  mutate(uk_sentralokosystemfunksjon = str_replace_na(uk_sentralokosystemfunksjon, "Nei")) %>%
  mutate(uk_sentralokosystemfunksjon = str_replace(uk_sentralokosystemfunksjon, "1", "Ja")) %>%
  
  mutate(uk_spesieltdarligkartlagt = str_replace_na(uk_spesieltdarligkartlagt, "Nei")) %>%
  mutate(uk_spesieltdarligkartlagt = str_replace(uk_spesieltdarligkartlagt, "1", "Ja")) %>%
  
  mutate(uk_truet = str_replace_na(uk_truet, "Nei")) %>%
  mutate(uk_truet = str_replace(uk_truet, "1", "Ja"))
  
  
  
saveRDS(dat, paste0(path, "naturtyper.rds"))

is.na(dat$uk_ingenstatus)
