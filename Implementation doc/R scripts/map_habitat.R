
packages <- c("ggOceanMaps","sf","dplyr")   # List packages
lapply(packages, library, character.only = TRUE)    

habitat<-readRDS("./Implementation doc/R scripts/Data/Habitats.rds") 

Inshore_colors <- c(
  "Gravel" = "#f2f0f7",
  "Rock" = "grey",  
  "Sand" = "#ffd25f", 
  "Silt" = "#ffb700"
)
Offshore_colors<-c("Overhang" = "#225ea8")

inshore_data <- subset(habitat, Shore == "Inshore")
offshore_data <- subset(habitat, Shore == "Offshore")

dt <- data.frame(lon = c(-5, -5, 15, 15), lat = c(60, 75, 60, 75))
ICES<-ices_areas%>%
  filter(Area_Full=="27.2.a.1"|Area_Full=="27.2.a.2")%>%
  st_transform(crs=st_crs(inshore_data))


basemap(data = dt,land.col="black")+
  geom_sf(data = inshore_data, aes(fill = Habitat), color = "NA", size = 0.2) +
  scale_fill_manual(values = Inshore_colors, name = "Inshore Habitats", guide = guide_legend(order = 1)) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = offshore_data, aes(fill = Habitat), color = "NA", size = 0.2) +
  scale_fill_manual(values = Offshore_colors, name = "Offshore Habitats", guide = guide_legend(order = 2))+
  ggnewscale::new_scale_fill() +
  geom_sf(data = ICES, aes(color = "red"),linewidth = 0.6,alpha=0) +
  scale_color_manual(values="red",name = "ICES areas 27.2", labels="",guide = guide_legend(order = 3))+
  theme_minimal() +
  coord_sf()+
  labs(fill = NULL)

ggsave("./Implementation doc/img/habitats.png", dpi = 1500)


domain <- readRDS("./Objects/Domains.rds") 


basemap(data = dt,land.col="black",bathymetry =TRUE) +
  geom_sf(data = domain, alpha=0,linewidth = 0.6, color = "red", size = 0.2) +
  theme_minimal() +
  coord_sf()+
  labs(fill = NULL)



