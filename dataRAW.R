library(sf)
library(dplyr)
library(units)
library(stringr)
library(units)
# This script take the file Natur_Naturtyper_NiN_norge_med_svalbard_25833.gdb, stored in 
# "P:/41001581_egenutvikling_anders_kolstad/data/" and processes it for the shiny app.
# Replace the file and rerun the script when neccessary.
varList <- c("kartleggingsår", 
                       "tilstand", 
                       "naturmangfold", 
                       "lokalitetskvalitet", 
                       "mosaikk", 
                       "usikkerhet", 
                       "hovedøkosystem", 
                       "oppdragstaker", 
                       "uk_nærtruet",
                       "uk_sentraløkosystemfunksjon",
                       "uk_spesieltdårligkartlagt",
                       "uk_truet")


dir <- substr(getwd(), 1,2)

path <- ifelse(dir == "C:", 
                "P:/41001581_egenutvikling_anders_kolstad/data/",
                "/data/Egenutvikling/41001581_egenutvikling_anders_kolstad/data/")

dat <- sf::read_sf(paste0(path, "Natur_Naturtyper_NiN_norge_med_svalbard_25833.gdb"))



# Get county delimination
path_county <- ifelse(dir == "C:", 
                      "R:/GeoSpatialData/AdministrativeUnits/Norway_AdministrativeUnits/Original/Norway_County/versjon2022/Basisdata_0000_Norge_25833_Fylker_FGDB.gdb",
                      "/data/R/GeoSpatialData/AdministrativeUnits/Norway_AdministrativeUnits/Original/Norway_County/versjon2022/Basisdata_0000_Norge_25833_Fylker_FGDB.gdb")
st_layers(path_county)

counties <- sf::read_sf(path_county, layer = "fylke")
counties_names <- sf::read_sf(path_county, layer = "administrativenhetnavn")
counties$fylke <- counties_names$navn[match(counties$objid, counties_names$fylke_fk)]

#library(tmap)
#tm_shape(counties)+
#  tm_polygons(col = "fylke")

counties <- counties %>%
  mutate(fylke =  recode(fylke,
                       "MÃ¸re og Romsdal" = "Møre og Romsdal",
                       "TrÃ¸ndelag" = "Trøndelag"))


dat_counties <- st_intersection(dat, counties)
unique(st_geometry_type(dat_counties)) # no lines introduced
nrow(dat)-nrow(dat_counties)  # 38 new polygons mean 76 polygins were split between counties

dups <- dat_counties$identifikasjon_lokalid[duplicated(dat_counties$identifikasjon_lokalid)]
dups2 <- dat_counties[dat_counties$identifikasjon_lokalid %in% dups,]

# Assign these to the county where they are biggest
options("scipen"=100, "digits"=4)
dups3 <- dups2 %>%
  mutate(area = units::drop_units(st_area(dups2))/1000, .after =  identifikasjon_lokalid)

dups4 <- dups3 %>%
  group_by(identifikasjon_lokalid) %>%
  arrange(desc(area), .by_group=T) %>%
  slice_head(n = 1)


'%!in%' <- Negate('%in%')
dat_counties_filtered <- dat_counties %>%
  filter(identifikasjon_lokalid %!in% dups4$identifikasjon_lokalid )

to_add <- dat %>%
  filter(identifikasjon_lokalid %in% dups4$identifikasjon_lokalid)
to_add$fylke <- dups4$fylke[match(to_add$identifikasjon_lokalid, dups4$identifikasjon_lokalid)]


dat_counties_filtered_added <- bind_rows(dat_counties_filtered,
                                         to_add)
  

#View(dat_counties_filtered_added[dat_counties_filtered_added$identifikasjon_lokalid %in% dups4$identifikasjon_lokalid, ])   #OK

dat <- dat_counties_filtered_added

dat$km2 <- drop_units(st_area(dat))/1000
#unique(dat$tilstand)
#names(dat) <- textclean::replace_non_ascii(names(dat))

dat2 <- dat %>%
  as.data.frame() %>%
  select(-SHAPE) %>%
  mutate(uk_ingenstatus = str_replace_na(uk_ingenstatus, "Nei")) %>%
  mutate(uk_ingenstatus = str_replace(uk_ingenstatus, "1", "Ja")) %>%
  
  mutate(uk_nærtruet = str_replace_na(uk_nærtruet, "Nei")) %>%
  mutate(uk_nærtruet = str_replace(uk_nærtruet, "1", "Ja")) %>%
  
  mutate(uk_sentraløkosystemfunksjon = str_replace_na(uk_sentraløkosystemfunksjon, "Nei")) %>%
  mutate(uk_sentraløkosystemfunksjon = str_replace(uk_sentraløkosystemfunksjon, "1", "Ja")) %>%
  
  mutate(uk_spesieltdårligkartlagt = str_replace_na(uk_spesieltdårligkartlagt, "Nei")) %>%
  mutate(uk_spesieltdårligkartlagt = str_replace(uk_spesieltdårligkartlagt, "1", "Ja")) %>%
  
  mutate(uk_truet = str_replace_na(uk_truet, "Nei")) %>%
  mutate(uk_truet = str_replace(uk_truet, "1", "Ja")) %>%

  mutate(across(all_of(varList), ~na_if(., ''))) %>%
  
  mutate(tilstand =  recode(tilstand, 
                            "Svært redusert" = "1 - Svært redusert",
                            "Dårlig" = "2 - Dårlig",
                            "Moderat" = "3 - Moderat",
                            "God" = "4 - God")) %>%
  mutate(naturmangfold =  recode(naturmangfold, 
                            "Lite" = "1 - Lite",
                            'Moderat' = "2 - Moderat",
                            "Stort" = "3 - Stort")) %>%
  mutate(lokalitetskvalitet =  recode(lokalitetskvalitet, 
                                 "Svært lav kvalitet" = "1 - Svært lav kvalitet",
                                 'Lav kvalitet' = "2 - Lav kvalitet",
                                 "Moderat kvalitet" = "3 - Moderat kvalitet",
                                 "Høy kvalitet" = "4 - Høy kvalitet",
                                 "Svært høy kvalitet" = "5 - Svært høy kvalitet"))
  

unique(dat2$tilstand)

# These one did not work for some reason, so doing it seperately
dat2$tilstand[dat2$tilstand == "Dårlig"] <- "2 - Dårlig"
dat2$tilstand[dat2$tilstand == "Svært redusert"] <- "1 - Svært redusert"


saveRDS(dat2, paste0(path, "naturtyper.rds"))
saveRDS(naturtyper, "shinyData/naturtyper.rds")




