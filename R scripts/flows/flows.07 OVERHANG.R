
# Summarise the data extracted from NEMO-ERSEM, dealing with deep convection issues
# readRDS("./Objects/vertical boundary/.")  # Marker so network script can see where the data is being pulled from

#!!# Line 81 needs to be revisited once Yuri rebuilds the data set. Does fixing the NE model output make the number of cells consistent?

#### Setup ####

rm(list=ls())                                                                   # Wipe the brain

Packages <- c("tidyverse", "data.table", "furrr", "sf")                         # List packages
lapply(Packages, library, character.only = TRUE)                                # Load packages

plan(multisession)

overhang <- readRDS("./Objects/Overhang.rds")

deep_convection_is <- 0.14                                                      # Threshold above which vertical diffusivity = deep convection

data <- list.files("./Objects/overhang/", full.names = T) %>% # Import data
  future_map(~{
    
    packet <- readRDS(.x)

    days <- ifelse(nrow(packet)%%6 == 0, 6, 5)                                  # If the number of rows is not divisible into 6 then it is a 5 day february
    
    packet <- mutate(packet, Day = rep(1:days, each = nrow(packet)/days)*5)      # Insert a column of days
    
  }, .progress = TRUE) %>% 
  rbindlist()

# messages <- map(data, pluck, "error")
# 
# messages<- messages[!is.null(messages)]

#### Quantify the amount of deep convection ####

## For more discussion see the appropriate entry in ./Notes

total_mixing <- group_by(data, Month) %>%                                                                   
  summarise(Deep_convection_proportion = mean(Vertical_diffusivity > deep_convection_is)) # What proportion of values are deep convection?

ggplot(total_mixing) +
  geom_line(aes(x = Month, y = Deep_convection_proportion)) +
  theme_minimal() +
  ylim(0,1) +
  labs(y = "Proportion of model domain as deep convection")

#### Mean vertical diffusivity ignoring deep convection ####

normal_mixing <- select(data, Vertical_diffusivity, Year, Month) %>%            # Discard excess variables
  filter(Vertical_diffusivity < deep_convection_is) %>%                         # Remove deep convection
  group_by(Year, Month) %>%                                                     # Create a monthly time series
  summarise(Vertical_diffusivity = mean(Vertical_diffusivity, na.rm = T)) %>% 
  ungroup()

saveRDS(normal_mixing, "./Objects/overhang diffusivity.rds")

#### Mean velocities and concentrations ####

sf_use_s2(F)

samples <- filter(data, Year == 2015, Month == 1, Day == 5) %>% 
  st_as_sf(coords = c("longitude", "latitude"), remove = F)

area <- st_union(samples) %>%                                               # Combine              
  st_voronoi() %>%                                                          # And create a voronoi tesselation
  st_collection_extract(type = "POLYGON") %>%                               # Expose the polygons
  sf::st_sf() %>%                                                           # Reinstate sf formatting
  st_join(samples) %>%                                                       # Rejoin meta-data from points
  arrange(x, y) %>%                                                         # Order the polygons to match the points
  st_set_crs(4326) %>% 
#  st_transform(crs = st_crs(overhang)) %>% 
#  st_make_valid() %>% 
  st_intersection(st_make_valid(st_transform(overhang, crs = 4326))) %>% 
  mutate(area_m2 = as.numeric(st_area(.))) %>% 
  select(x, y, area_m2)
    
ggplot(area) +                                                              # Check the polygons match correctly with points
  geom_sf(aes(fill = area_m2), size = 0.05, colour = "white") +
  theme_minimal() +
  NULL

exchanges <- select(data, Vertical_velocity, NH4, NO3, Detritus, Year, Month, Day, x, y) %>%        # Discard excess variables
  mutate(Direction = ifelse(Vertical_velocity > 0, "Upwelling", "Downwelling")) %>% # Identify upwelling and downwelling
  left_join(st_drop_geometry(area)) %>% 
  drop_na(area_m2) %>% 
  mutate(Vertical_velocity = abs(Vertical_velocity)*area_m2) %>%                    # Scale up flow rate to volume of water
  group_by(Year, Month, Day, Direction) %>%                                         # Create a time series for the whole area
  summarise(NH4 = weighted.mean(NH4,  w = Vertical_velocity, na.rm = T),            # Flow-weighted mean concentrations of nutrient and detritus
            NO3 = weighted.mean(NO3,  w = Vertical_velocity, na.rm = T),            
            Detritus = weighted.mean(Detritus, w = Vertical_velocity, na.rm = T),
            Vertical_velocity = sum(Vertical_velocity, na.rm = T)) %>% 
  group_by(Year, Month, Direction) %>%                                          # Average for across the month, now we have taken into account direction
  summarise(NH4 = mean(NH4, na.rm = T),                                         
            NO3 = mean(NO3, na.rm = T),            
            Detritus = mean(Detritus, na.rm = T),
            Vertical_velocity = mean(Vertical_velocity, na.rm = T)) %>% 
    ungroup()

saveRDS(exchanges, "./Objects/overhang exchanges.rds")
