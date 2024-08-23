
# Create an object defining the geographic extent of the model domain

#### Set up ####

rm(list=ls())                                                   

Packages <- c("tidyverse", "sf", "stars", "rnaturalearth", "raster")                  # List handy packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

source("./R scripts/@_Region file.R")                                       # Define project region 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

#### Update elevations in domain polygon for extracting from NEMO-MEDUSA ####

## We need the volume calculations to be correct for exchanging water masses

Domains <- readRDS("./Objects/Domains.rds")  

GEBCO2 <- raster("../Shared data/GEBCO_2020.nc")

Elevation <- crop(GEBCO2, st_transform(Domains, crs = st_crs(GEBCO2)))

Elevation_Off <- Elevation ; Elevation_Off[Elevation < -DDepth] <- -DDepth

Offshore_elevation <- exactextractr::exact_extract(Elevation_Off, st_transform(filter(Domains, Shore == "Offshore"), st_crs(Elevation)), "mean")

Domains <- mutate(Domains, Elevation = case_when(Shore == "Offshore" ~ Offshore_elevation,
                                                 T ~ Elevation)) %>% 
  arrange(Shore)

saveRDS(Domains, "./Objects/Domains.rds")

#### Format to domains object ####

offshore <- readRDS("./Objects/Domains.rds") %>%  filter(Shore == "Offshore") %>% 
  st_transform(4326)

EEZ <- st_bbox(offshore) %>% 
  st_as_sfc()

GEBCO <- read_stars("../Shared data/GEBCO_2020.nc")
st_crs(GEBCO) <- st_crs(EEZ)

overhang <- transmute(offshore, 
                      Shore = "Offshore",
                      area = as.numeric(st_area(offshore)),
                      Elevation = -(DDepth-SDepth)) %>%                         # I don't think Elevation matters as the overhang is a plane not a volume
  st_transform(crs = crs)

saveRDS(overhang, "./Objects/Overhang.rds")

