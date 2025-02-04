
# Create an object defining the geographic extent of the model domain

#### Set up ####

rm(list=ls())                                                   

Packages <- c("tidyverse", "sf", "stars", "rnaturalearth", "raster")                  # List handy packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

source("./R scripts/@_Region file.R")                                       # Define project region 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

mask <- readRDS("./Objects/Domains.rds") %>%  filter(Shore == "Offshore") %>% 
  st_transform(4326)

EEZ <- st_bbox(mask) %>% 
  st_as_sfc()

# GEBCO <- read_stars("../Shared data/GEBCO_2020.nc")
# st_crs(GEBCO) <- st_crs(EEZ)

#### Introduce some offshore habitat ####
GEBCO <- raster("../Shared data/GEBCO_2020.nc")

crop <- as(extent(-7, 16, 60, 74), "SpatialPolygons")
crs(crop) <- crs(GEBCO)

GEBCO <- crop(GEBCO, crop)

#### Polygons based on depth ####

Depths <- GEBCO
Depths[GEBCO >= -DDepth | GEBCO < -ODepth] <- NA

Depths[is.finite(Depths)] <- -ODepth

#Depths <- GEBCO[EEZ] %>% 
#  st_as_stars()
# 
# Depths[[1]][Depths[[1]] > units::set_units(-DDepth, "m") | Depths[[1]] < units::set_units(-DDepth, "m")] <- NA
#
# Depths[[1]][is.finite(Depths[[1]])] <- units::set_units(-ODepth, "m")

Bottom <- st_as_stars(Depths) %>%
  st_as_sf(merge = TRUE) %>%
  st_make_valid() %>%
  group_by(Elevation.relative.to.sea.level) %>%
  summarise(Depth = abs(mean(Elevation.relative.to.sea.level))) %>%
  st_make_valid() %>% 
  st_transform(crs = 4326)

ggplot(Bottom) +
  geom_sf(aes(fill = Depth), alpha = 0.2) +
  theme_minimal()

#### Update elevations in domain polygon for extracting from NEMO-MEDUSA ####

## We need the volume calculations to be correct for exchanging water masses

Domains <- readRDS("./Objects/Domains.rds")  

GEBCO2 <- raster("../Shared data/GEBCO_2020.nc")

Elevation <- crop(GEBCO2, st_transform(Domains, crs = st_crs(GEBCO2)))

Elevation_Off <- Elevation ; Elevation_Off[Elevation < -ODepth] <- -ODepth

Offshore_elevation <- exactextractr::exact_extract(Elevation_Off, st_transform(filter(Domains, Shore == "Offshore"), st_crs(Elevation)), "mean")

Domains <- mutate(Domains, Elevation = case_when(Shore == "Offshore" ~ Offshore_elevation,
                                                 T ~ Elevation)) %>% 
  arrange(Shore)

saveRDS(Domains, "./Objects/Domains.rds")

#### Format to domains object ####

sf_use_s2(FALSE)

offshore <- readRDS("./Objects/Domains.rds") %>%  
  filter(Shore == "Offshore") %>% 
  st_transform(4326) %>% 
  st_make_valid() %>% 
  st_difference(Bottom) %>% 
  st_cast("POLYGON") %>% 
  mutate(area = as.numeric(st_area(.))) %>% 
  filter(area == max(area))

ggplot() +
  geom_sf(data = offshore) +
  geom_sf(data = Bottom, fill = NA, colour = "red")

# EEZ <- st_bbox(offshore) %>% 
#   st_as_sfc()
# 
# GEBCO <- read_stars("../Shared data/GEBCO_2020.nc")
# st_crs(GEBCO) <- st_crs(EEZ)

overhang <- transmute(offshore, 
                      Shore = "Offshore",
                      area = as.numeric(st_area(offshore)),
                      Elevation = -(DDepth-SDepth)) %>%                         # I don't think Elevation matters as the overhang is a plane not a volume
  st_transform(crs = crs)

saveRDS(overhang, "./Objects/Overhang.rds")

#### extra offshore habitat ####

D2 <- readRDS("./Objects/Domains.rds") %>%  
  filter(Shore == "Offshore") %>% 
  st_transform(4326) %>% 
  st_make_valid() %>% 
  st_intersection(Bottom) %>% 
  mutate(area = as.numeric(st_area(.)))

proportion <- D2$area/sum(Domains$area)

saveRDS(D2, "./Objects/D2.rds")
