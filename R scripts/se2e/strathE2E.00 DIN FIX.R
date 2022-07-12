
library(MiMeMo.tools)

#domain <- readRDS("./Objects/Domains.rds")

all_data <- read.csv("./Data/ICES Nutrients/f414f1b3-53a0-43b6-9e80-32452f5ff6c2.txt", sep = "\t") %>% 
  select(Cruise, Station, Day, Month, Year, Latitude..degrees_north., Longitude..degrees_east., Depth..m.,
         Nitrate.Nitrogen..NO3.N...umol.l., Ammonium.Nitrogen..NH4.N...umol.l.) %>% 
  drop_na(Nitrate.Nitrogen..NO3.N...umol.l., Ammonium.Nitrogen..NH4.N...umol.l.) %>% 
  st_as_sf(coords = c("Longitude..degrees_east.","Latitude..degrees_north."), crs = 4326) %>% # Convert to sf for spatial filter later
  st_transform(crs = st_crs(domain)) %>%  
  group_by(Cruise, Station, Day, Month, Year) %>%                           # Per cast
  arrange(Depth..m., .by_group = TRUE)                                      # Order depths ascending

#### Calculate proportion ####

shallow_proportion <- all_data %>% 
  mutate(weights = calculate_depth_share(Depth..m., min_depth = 0, max_depth = 40), # Calculate share of the depth column per sample
         Depth_layer = "Shallow")                                           # Label depth layer 

deep_proportion <- all_data %>% 
  mutate(weights = calculate_depth_share(Depth..m., min_depth = 40, max_depth = 400), # Calculate share of the depth column per sample
         Depth_layer = "Deep")                                              # Label depth layer 

final <- bind_rows(shallow_proportion, deep_proportion) %>%                 # Combine estimates
  filter(weights > 0) %>%                                                   # Drop samples outside the depth window
  group_by(Cruise, Station, Day, Month, Year, Depth_layer) %>% 
  summarise(Ammonium = weighted.mean(Ammonium.Nitrogen..NH4.N...umol.l., weights), # Weighted averages
            DIN =  weighted.mean(Nitrate.Nitrogen..NO3.N...umol.l. + Ammonium.Nitrogen..NH4.N...umol.l., weights),
            Samples = n()) %>%                                              # Number of samples contributing to each estimate
  ungroup() %>% 
  mutate(Proportion = Ammonium/DIN) %>%                                     # Get a proportion of ammonium to total DIN
  st_join(domain) %>%                                                       # Check which are in the model domain
  st_drop_geometry() %>%                                                    # Simplify the output
  drop_na() %>% 
  group_by(Depth_layer, Month) %>%                                          # Decided not to group by shore because there were few inshore samples   
  summarise(Proportion = weighted.mean(Proportion, Samples),                # Calculate average, weighting by the number of samples
            Casts = n()) %>%                                                # Number of CTD casts contributing to each estimate
  ungroup() 

saveRDS(final, "./Objects/Ammonia to DIN.rds")


ggplot(final) +
  geom_line(aes(x = Month, y = Casts, colour = Depth_layer)) +
  theme_minimal()

ggplot(final) +
  geom_line(aes(x = Month, y = Proportion, colour = Depth_layer)) +
  theme_minimal()

ggplot() +
  geom_sf(data = domain) +
  geom_sf(data = all_data, aes(fill = Station)) +
  theme_minimal()
