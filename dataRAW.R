library(sf)
library(dplyr)
library(units)
library(stringr)
library(units)

# This script take the file downloaded from geoNorge and processes it for the shiny app.
# Replace the file and rerun the script when neccessary.

# UPDATE 15.02.23: Updated the script to include year 2022. Several of the columns had new names!


varList <- c("kartleggingsår", 
                       "tilstand", 
                       "naturmangfold", 
                       "lokalitetskvalitet", 
                       "mosaikk", 
                       "usikkerhet", 
                       "hovedøkosystem", 
                       "oppdragstaker", 
                       "kriterium_nærTruet",
                       "kriterium_sentralØkosystemFunksjon",
                       "kriterium_spesieltDårligKartlagt",
                       "kriterium_truet")



# path to data set
dir <- substr(getwd(), 1,2)

#path_old <- ifelse(dir == "C:", 
#                "P:/41001581_egenutvikling_anders_kolstad/data/",
#                "/data/Egenutvikling/41001581_egenutvikling_anders_kolstad/data/")

path <- ifelse(dir == "C:", 
               "R:/GeoSpatialData/Habitats_biotopes/Norway_Miljodirektoratet_Naturtyper_nin/Original/Naturtyper_nin_0000_norge_25833_FILEGDB/Naturtyper_nin_0000_norge_25833_FILEGDB.gdb",
               "/data/R/GeoSpatialData/Habitats_biotopes/Norway_Miljodirektoratet_Naturtyper_nin/Original/Naturtyper_nin_0000_norge_25833_FILEGDB/Naturtyper_nin_0000_norge_25833_FILEGDB.gdb")


# Read data ----------------------
#dat <- sf::read_sf(paste0(path, "Natur_Naturtyper_NiN_norge_med_svalbard_25833.gdb"))
dat <- sf::read_sf(path)

# Get county delimination
path_county <- ifelse(dir == "C:", 
                      "R:/GeoSpatialData/AdministrativeUnits/Norway_AdministrativeUnits/Original/Norway_County/versjon2022/Basisdata_0000_Norge_25833_Fylker_FGDB.gdb",
                      "/data/R/GeoSpatialData/AdministrativeUnits/Norway_AdministrativeUnits/Original/Norway_County/versjon2022/Basisdata_0000_Norge_25833_Fylker_FGDB.gdb")
# View layer names
st_layers(path_county)

# read two layers
counties <- sf::read_sf(path_county, layer = "fylke")
counties_names <- sf::read_sf(path_county, layer = "administrativenhetnavn")

# ... and combine them
counties$fylke <- counties_names$navn[match(counties$objid, counties_names$fylke_fk)]

# View counties
#library(tmap)
#tm_shape(counties)+
#  tm_polygons(col = "fylke")

# Fix some naming errors
unique(counties$fylke)
counties <- counties %>%
  mutate(fylke =  recode(fylke,
                       "MÃ¸re og Romsdal" = "Møre og Romsdal",
                       "TrÃ¸ndelag" = "Trøndelag"))


# Finding and removing 'old' nature types that were not mapped in 2022 ---------------
# list all types
keepers_pre <- unique(dat$naturtype)
#Extract the year when these were mapped
years <- NULL
for(i in 1:length(keepers_pre)){
  years[i] <- paste(sort(unique(dat$kartleggingsår[dat$naturtype == keepers_pre[i]])), collapse = ", ")
}
#Combine into one data frame
keepers_df <- data.frame(
  "Nature_type" = keepers_pre,
  "Year"        = years
)
# find those mapped in 2021
keepers <- keepers_df$Nature_type[grepl("2022" , keepers_df$Year)]
# and cut the rest
dat <- dat[dat$naturtype %in% keepers,]


# Get fylke and region for each polygon -----------------
dat_counties <- st_intersection(dat, counties)
# Check for boundary issues
unique(st_geometry_type(dat_counties)) # no lines introduced
nrow(dat)-nrow(dat_counties)  # 47 new polygons means 94 polygons were split between counties
# find those that are duplicated
dups <- dat_counties$identifikasjon_lokalId[duplicated(dat_counties$identifikasjon_lokalId)]
dups2 <- dat_counties[dat_counties$identifikasjon_lokalId %in% dups,]
# Assign these to the county where they are biggest
# - force non-scientific nmbers
options("scipen"=100, "digits"=4)
# calculate area
dups3 <- dups2 %>%
  mutate(area = units::drop_units(st_area(dups2))/1000, .after =  identifikasjon_lokalId)
# Arrange by area and chose the one (the row) with the biggest area. Thta row will contain the 'fylke' that we want to assign this locality to.
dups4 <- dups3 %>%
  group_by(identifikasjon_lokalId) %>%
  arrange(desc(area), .by_group=T) %>%
  slice_head(n = 1)
# Remove all duplicates from the data set
#'%!in%' <- Negate('%in%')
dat_counties_filtered <- dat_counties %>%
  filter(!identifikasjon_lokalId %in% dups4$identifikasjon_lokalId )
# Find the duplicatd localities in the data set from before the intersection 
to_add <- dat %>%
  filter(identifikasjon_lokalId %in% dups4$identifikasjon_lokalId)
# Attach fylke name to those
to_add$fylke <- dups4$fylke[match(to_add$identifikasjon_lokalId, dups4$identifikasjon_lokalId)]
# and bind with the rest of the data
dat_counties_filtered_added <- bind_rows(dat_counties_filtered,
                                         to_add)
  
#View(dat_counties_filtered_added[dat_counties_filtered_added$identifikasjon_lokalId %in% dups4$identifikasjon_lokalId, ])   #OK


# rename and clean
dat <- dat_counties_filtered_added
rm(counties, counties_names, dat_counties, dat_counties_filtered, dat_counties_filtered_added,
   dups, dups2, dups3, dups4, keepers_df, to_add)

# Combine fylke into regions --------------------------------------
dat <- dat %>%
  mutate(region =
         case_when(fylke %in% c("Agder", "Vestfold og Telemark") ~ "Sørlandet",
                   fylke %in% c("Oslo", "Innlandet", "Viken") ~ "Østlandet",
                   fylke %in% c("Troms og Finnmark", "Nordland") ~ "NordNorge",
                   fylke %in% c("Rogaland", "Vestland") ~ "Vestlandet",
                   fylke %in% c("Møre og Romsdal", "Trøndelag") ~ "Midt-Norge"))

#table(dat$region)

# Calculate area
dat$km2 <- drop_units(st_area(dat))/1000



# Some recoding --------------------------------------------
names(dat)

dat2 <- dat %>%
  as.data.frame() %>%
  select(-SHAPE) %>%
  mutate(kriterium_ingenStatus = str_replace_na(kriterium_ingenStatus, "Nei")) %>%
  mutate(kriterium_ingenStatus = str_replace(kriterium_ingenStatus, "1", "Ja")) %>%
  
  mutate(kriterium_nærTruet = str_replace_na(kriterium_nærTruet, "Nei")) %>%
  mutate(kriterium_nærTruet = str_replace(kriterium_nærTruet, "1", "Ja")) %>%
  
  mutate(kriterium_sentralØkosystemFunksjon = str_replace_na(kriterium_sentralØkosystemFunksjon, "Nei")) %>%
  mutate(kriterium_sentralØkosystemFunksjon = str_replace(kriterium_sentralØkosystemFunksjon, "1", "Ja")) %>%
  
  mutate(kriterium_spesieltDårligKartlagt = str_replace_na(kriterium_spesieltDårligKartlagt, "Nei")) %>%
  mutate(kriterium_spesieltDårligKartlagt = str_replace(kriterium_spesieltDårligKartlagt, "1", "Ja")) %>%
  
  mutate(kriterium_truet = str_replace_na(kriterium_truet, "Nei")) %>%
  mutate(kriterium_truet = str_replace(kriterium_truet, "1", "Ja")) %>%

  mutate(across(all_of(varList), ~na_if(., ''))) %>%
  
  mutate(tilstand =  recode(tilstand, 
                            "sværtRedusert" = "1 - Svært redusert",
                            "dårlig" = "2 - Dårlig",
                            "moderat" = "3 - Moderat",
                            "god" = "4 - God")) %>%
  mutate(naturmangfold =  recode(naturmangfold, 
                            "lite" = "1 - Lite",
                            'moderat' = "2 - Moderat",
                            "stort" = "3 - Stort")) %>%
  mutate(lokalitetskvalitet =  recode(lokalitetskvalitet, 
                                 "sværtLavKvalitet" = "1 - Svært lav kvalitet",
                                 'lavKvalitet' = "2 - Lav kvalitet",
                                 "moderatKvalitet" = "3 - Moderat kvalitet",
                                 "høyKvalitet" = "4 - Høy kvalitet",
                                 "sværtHøyKvalitet" = "5 - Svært høy kvalitet"))
  


# Melted data set  -----------------------------------------
# (one row for each nin-variable within each locality)
dat2_long <- tidyr::separate_rows(dat2, ninBeskrivelsesvariable, sep=",") %>%
  separate(col = ninBeskrivelsesvariable,
           into = c("NiN_variable_code", "NiN_variable_value"),
           sep = "_",
           remove=F) %>%
  mutate(NiN_variable_value = as.numeric(NiN_variable_value)) %>%
  filter(!str_detect(NiN_variable_code, "LKM")) %>%
  group_by(naturtype, NiN_variable_code, NiN_variable_value) %>%
  filter(n() > 2) %>% ungroup()
# The 'expected two pieces' warning is fine to ignore


# STOPPED HERE 15.02  --------------------


# Remove nin-variables not recorded in 2021 ---------------
# list all types
var_keepers_pre <- unique(dat2_long$NiN_variable_code)
#Extract the year when these were mapped
years <- NULL
for(i in 1:length(var_keepers_pre)){
  years[i] <- paste(sort(unique(dat2_long$kartleggingsår[dat2_long$NiN_variable_code == var_keepers_pre[i]])), collapse = ", ")
}
#Combine into one data frame
var_keepers_df <- data.frame(
  "variable" = var_keepers_pre,
  "Year"        = years
)
# find those mapped in 2021
keepers_var <- var_keepers_df$variable[grepl("2021" , var_keepers_df$Year)]
# and cut the rest
dat2_long <- dat2_long[dat2_long$NiN_variable_code %in% keepers_var,]


# remove those that end in '-' and that consist of only one symbol --------------
dat2_long_2 <- dat2_long %>%
  filter(!str_detect(NiN_variable_code, "-$"))
nrow(dat2_long_2) # 1084750
dat2_long_2 <- dat2_long_2 %>%
  filter(nchar(NiN_variable_code)>1)
nrow(dat2_long_2) # 1082675

# Remove those with only zeros
temp <- dat2_long_2 %>%
  count(NiN_variable_code, NiN_variable_value) %>%
  pivot_wider(id_cols = NiN_variable_code,
                names_from = NiN_variable_value,
              values_from = n) %>%
  mutate(rowsums =  rowSums(select(., -NiN_variable_code, -'NA') != 0, na.rm=T)) %>%
  filter(rowsums >0)

dat2_long_3 <- dat2_long_2 %>%
  filter(NiN_variable_code %in% temp$NiN_variable_code)

length(unique(dat2_long_2$NiN_variable_code))-length(unique(dat2_long_3$NiN_variable_code))
# This operation cut 55 variables


# Check for other weird NiN variable names ----------------------------
# There are a lot of errors in the column ninBeskrivelsesvariable
View(table(dat2_long_3$NiN_variable_code))

# This looks better.


#saveRDS(dat2, "shinyData/naturtyper.rds")
#saveRDS(dat2_long_3, "shinyData/naturtyper_long.rds")



