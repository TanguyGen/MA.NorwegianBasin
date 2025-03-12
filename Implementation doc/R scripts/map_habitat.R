packages <- c("tidyverse", "data.table","ggOceanMaps","sf")   # List packages
lapply(packages, library, character.only = TRUE)    

habitat<-readRDS("./Implementation doc/R scripts/Data/Habitats.rds") 

Inshore_colors <- c(
  "Gravel" = "#f2f0f7ff",
  "Rock" = "#bebebeff",  
  "Sand" = "#ffd25f", 
  "Silt" = "#ffb700"
)
Offshore_colors<-c("Overhang" = "#33425f",
                   "Gravel"= "#5A72A0",
                   "Silt" = "#83B4FF",
                   "Sand" = "#FDFFE2")

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


dt2<- data.frame(lon = c(-30, -30, 20, 20), lat = c(44, 85, 44, 85))

bbox <- st_bbox(c(xmin = -5, xmax = 15, ymin = 60, ymax = 75), crs = st_crs(4326))
coords_3995 <- st_bbox(st_transform(st_as_sfc(bbox), crs = 3995))
xmin <- coords_3995["xmin"]
xmax <- coords_3995["xmax"]
ymin <- coords_3995["ymin"]
ymax <- coords_3995["ymax"]

# Create the second plot and overlay the rectangle
basemap(data = dt2, land.col = "black") +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, # Prevent conflicts with other layers
    color = "red",       # Box color
    fill = NA,           # Transparent fill
    linetype = "dashed", # Dashed line for the box
    size = 0.8           # Line thickness
  ) +
  coord_sf(crs = 3995) +
  theme_minimal()

