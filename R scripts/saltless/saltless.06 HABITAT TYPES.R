
# Convert NGU classess to 8 StrathE2E habitat types

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "stars")                           # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/Domains.rds") %>%                  # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                       # Transform to Lat/Lon to match other objects

sediment <- readRDS("./Data/modelled_sediment.rds") # Import full sediment grid

translate <- read.csv("./Data/Sediment habitats.csv") %>%        # Import sediment aggregations
  mutate(Sed_class = as.factor(SEDKORNSTR)) %>% 
  select(Sed_class, Habitat)                                     # Drop excess columns

overhang <- readRDS("./Objects/Overhang.rds") %>%                # Import overhang to scale proportions
  st_transform(crs = 4326) %>% 
  mutate(Habitat = "Overhang") %>% 
  st_make_valid()

#### Fill in gaps following addition of offshore sand ####

shape <- function(matrix) {
  
  shape <-  matrix %>% 
    list() %>% 
    st_polygon() %>% 
    st_sfc() %>% 
    st_sf(Region = implementation, geometry = .)
  st_crs(shape) <- st_crs(4326)                                        
  shape <- st_transform(shape, crs = crs)
  return(shape)
  
}                      # Convert a matrix of lat-lons to an sf polygon

habitat_mask <- matrix(c(16.23, 63.5,
                        20.25, 63.5,
                        10, 60,
                        0, 58.5,
                        0, 59,
                        0, 60.5,
                        0, 63,
                        10, 63.5,
                        16.23, 63.5),
                      ncol = 2, byrow = T) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = implementation,.)
st_crs(habitat_mask) <- st_crs(4326)                                        

sf_use_s2(FALSE)

NGU <- Sediment <- st_read("../Norwegian Shelf/Data/NGU oversikt", layer = "KornstorrelseFlate_oversikt") %>%                # Import NGU shapefile
  st_as_sf(crs = 4326) %>%                                                    # Assign CRS                                   
  dplyr::select(Sed_class = SEDKORNSTR) %>%                                   # Sample polygons by bathymetric grid
  st_intersection(habitat_mask)

 D2 <- readRDS("./Objects/D2.rds") %>% 
   transmute(Habitat = "Silt",
             Shore = "Offshore")
# 
# look <- st_cast(D2, "POLYGON") %>% 
#   mutate(area = as.numeric(st_area(.))) %>% 
#   filter(area > 1e8)
# 
# ggplot(look) +
#   geom_sf(aes(fill = area)) # limit is 1 degree east

#### Define geographic extent of each habitat type ####

sf_use_s2(F)

habitats <- left_join(sediment, translate) %>%                   # Attach habitat labels to predicted NGU classes
  mutate(Sed_class = as.factor(Sed_class),                       # Convert to factors
         Habitat = as.factor(Habitat)) %>% 
  sfc_as_cols()                                                  # Get coordinates from sf formatting to define a raster

numeric_habitats <- mutate(habitats, Habitat = as.numeric(Habitat)) # Convert factor to numeric as st_rasterize expects numbers

polygons <- st_rasterize(numeric_habitats["Habitat"],            # Rasterize habiat labels   
                         nx = length(unique(habitats$x)),        # At the resolution of the original data
                         ny = length(unique(habitats$y))) %>% 
  st_as_sf(aspoints = FALSE, merge = TRUE) %>%                   # Merge pixels into contiguous polygons
  mutate(Habitat = factor(Habitat, labels = levels(habitats$Habitat))) %>% # Reinstate labels for factor
  group_by(Habitat) %>%  
  summarise(Habitat = Habitat[1])                                # Combine polygons into a single row per habitat

plot(polygons)

polygons <- st_intersection(st_make_valid(st_transform(polygons, crs = crs)), # Split sediment polygons along model zones
                            st_transform(domains, crs = crs)) %>% 
  select(-c(Elevation, area)) %>%                                # Drop excess data
  st_transform(crs = 4326)                                       # Switch back to mercator

#saveRDS(polygons, "./Objects/Habitats.rds")

#### Accommodate overhang ####

ggplot() +                                                       # Check for overhang overlap with sediment data
  geom_sf(data = polygons, aes(fill = Habitat)) +
  geom_sf(data = overhang, aes(fill = Habitat), colour = "red", alpha = 0.5) +
  theme_minimal()

polygons <- st_difference(polygons, overhang) %>%                # Cut overhang out of sediment
  st_cast("POLYGON") %>%                                         # Expose each shape to allow removing whispy bits
  mutate(clean = as.numeric(st_area(.))) %>%                     # Get the size of each shape, assuming whisps are small
  filter(clean > 150) %>%                                        # Adjust to get rid of ghosts around the edge of the overhang
  group_by(Habitat, Shore, area) %>%                             # Cheat way to return to "MULTIPOLYGON", and drop redundant columns
  summarise(Elevation = mean(Elevation)) %>%                     # Clean
  bind_rows(overhang) %>%                                        # Add overhang polygon
  mutate(Habitat = str_remove(Habitat, "_strath")) %>%           # Clean Habitat labels
  bind_rows(D2)
#sf_use_s2(T)

mask <- st_union(polygons)

look <- st_intersection(domains, NGU) %>% 
  mutate(Sed_class = as.factor(Sed_class)) %>% 
  left_join(translate) %>%                  # Attach habitat labels to predicted NGU classes
  st_difference(mask)

G_Y2 <- c(`Inshore Rock` = "#40333C", `Inshore Silt` = "#284481", `Inshore Sand` = "#9097CC", `Inshore Gravel` = "#4A8FA1",
          `Offshore Rock` = "#d7c288", `Offshore Silt` = "#ffb700", `Offshore Sand` = "#FFD25F", `Offshore Gravel` = "#ffedbd", `Offshore Overhang` = '#b01313')

ggplot() +                                                       # Check for overhang overlap with sediment data
  geom_sf(data = polygons, aes(fill = paste(Shore, Habitat))) +
  scale_fill_manual(values = (G_Y2)) +
  geom_sf(data = look, aes(fill = paste(Shore, Habitat))) +
  theme_minimal()

#### Finish filling gaps ####

final <- bind_rows (polygons, look) %>% 
  group_by(Shore, Habitat) %>% 
  summarise(area = mean(area)) %>% 
  st_make_valid()

s2 <- filter(domains, Shore == "Inshore") %>% 
  st_difference(st_union(final)) %>% 
  st_cast("POLYGON") %>% 
  mutate(area = as.numeric(st_area(.))) %>% 
  filter(area == max(area)) %>%
  transmute(Shore = "Inshore",
            Habitat = "Sand")

final <- bind_rows(final, s2) %>% 
  group_by(Shore, Habitat) %>% 
  summarise(area = mean(area)) %>% 
  ungroup() %>% 
  st_make_valid() %>% 
  mutate(area = as.numeric(st_area(.)))
  
ggsave("./Figures/saltless/habitats dominant.png", bg = "white")

saveRDS(final, "./Objects/Habitats.rds")

#### Calculate proportion of model zones in each habitat ####

#polygons <- readRDS("./Objects/Habitats.rds")

proportions <- final %>% 
  mutate(Cover = as.numeric(st_area(.))) %>%                     # Measure the area of each habitat type
  st_drop_geometry() %>%                                         # Drop SF formatting
  mutate(Cover = Cover/sum(Cover)) %>%                           # Calculate the proportion of the model zone in each sediment polygon 
  rename(Bottom = Habitat)

saveRDS(proportions, "./Objects/Sediment area proportions.rds")

ggplot(proportions) +
  geom_col(aes(x = Shore, y = Cover*100, fill = Bottom), position = "Dodge") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top") +
  viridis::scale_fill_viridis(discrete = T, name = "Sediment class:") +
  labs(y = "Cover (%)", x = NULL, caption = "Percentage of model domain in each habitat class")

ggsave("./Figures/saltless/Habitat types.png", width = 16, height = 8, units = "cm")
