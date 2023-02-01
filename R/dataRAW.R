library(sf)
library(dplyr)
library(units)
# This script take the file Natur_Naturtyper_NiN_norge_med_svalbard_25833.gdb, stored in 
# "P:/41001581_egenutvikling_anders_kolstad/data/" and processes it for the shiny app.
# Replace the file and rerun the script when neccessary.

dir <- substr(getwd(), 1,2)

path <- ifelse(dir == "C:", 
                "P:/41001581_egenutvikling_anders_kolstad/data/",
                "/data/Egenutvikling/41001581_egenutvikling_anders_kolstad/data/")

dat <- sf::read_sf(paste0(path, "Natur_Naturtyper_NiN_norge_med_svalbard_25833.gdb"))
dat$km2 <- drop_units(st_area(dat))/1000
dat <- dat %>%
  as.data.frame() %>%
  select(-SHAPE)

names(dat) <- textclean::replace_non_ascii(names(dat))
  
saveRDS(dat, paste0(path, "naturtyper.rds"))
