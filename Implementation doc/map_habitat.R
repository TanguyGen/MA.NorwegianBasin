
packages <- c("tidyverse", "data.table", "ncdf4", "stars", "rayshader", "tictoc","ggmap","ggOceanMaps","sf")   # List packages
lapply(packages, library, character.only = TRUE)    

habitat<-readRDS("./Implementation doc/Habitats.rds")

Inshore_colors <- c(
  "Gravel" = "#f2f0f7",
  "Rock" = "#ffedbd",  
  "Sand" = "#ffd25f", 
  "Silt" = "#ffb700"
)
Offshore_colors<-c("Overhang" = "#225ea8")

inshore_data <- subset(habitat, Shore == "Inshore")
offshore_data <- subset(habitat, Shore == "Offshore")

dt <- data.frame(lon = c(-5, -5, 15, 15), lat = c(60, 75, 60, 75))

basemap(data = dt,land.col="black")+
  geom_sf(data = inshore_data, aes(fill = Habitat), color = "NA", size = 0.2) +
  scale_fill_manual(values = Inshore_colors, name = "Inshore Habitats", guide = guide_legend(order = 1)) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = offshore_data, aes(fill = Habitat), color = "NA", size = 0.2) +
  scale_fill_manual(values = Offshore_colors, name = "Offshore Habitats", guide = guide_legend(order = 2))+
  theme_minimal() +
  coord_sf()+
  labs(fill = "Zone and Habitat")
