
#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf", "raster", "stars")                       # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages

source("./R scripts/@_Region file.R")

base <- raster("./Data/gebco_2024_n75.0_s60.0_w-5.0_e15.0.nc")                            # Import bathymetry

domain <- readRDS("./Implementation doc/Habitats.rds") %>% 
  st_transform(crs = crs)

buffer <- summarise(domain, area = mean(st_area(domain))) %>% 
  st_buffer(dist = 230000)

ggplot() +
  geom_sf(data = buffer) +
  geom_sf(data = domain, colour = "yellow")

window <- st_bbox(buffer)

clip <- c(-60, -35, -36, -20) %>% 
  extent() %>% 
  as("SpatialPolygons")
crs(clip) <- crs(base)                                                         # Match crs to bathymetry

base <- crop(base, clip)                                                       # Crop bathymetry

plot(base)

line <- rasterToContour(base, levels = c(-DDepth)) %>% 
  st_as_sf() %>% 
  st_transform(crs = crs)

ggplot() +
  geom_sf(data = line) +
  geom_sf(data = domain, fill = "red")

star <- base
star[star < 0] <- NA
star[is.finite(star)] <- 0

plot(star)

star2 <- st_as_stars(star) %>% 
  st_as_sf(as_points = FALSE, merge = TRUE) %>% 
  st_transform(crs = crs)

#### Plotting ####

overlay <- mutate(domain, class = case_when(Habitat == "Rock" ~ 0,
                                            Habitat == "Gravel" ~ 3,
                                            Habitat == "Sand" ~ 2,
                                            Habitat == "Silt" ~ 1)) %>% 
  mutate(class = case_when(Shore == "Inshore" ~ paste0("S", class),
                           T ~ paste0("D", class)))

G_Y2 <- c(S0 = "#40333C", S1 = "#284481", S2 = "#9097CC", S3 = "#4A8FA1",
          D0 = "#d7c288", D1 = "#ffb700", D2 = "#FFD25F", D3 = "#ffedbd")

ggplot() +
  geom_sf(data = overlay, aes(fill = class), size = 0.05, colour = "white") +
  geom_sf(data = star2, fill = "black", size = 0) +
  geom_sf(data = line, colour = "grey69") +
  scale_fill_manual(values = G_Y2) +
  coord_sf(ylim = c(window["ymin"], window["ymax"]), xlim = c(window["xmin"], window["xmax"]), expand = F) +
  theme_minimal() +
  theme(text = element_text(family = "Avenir", size = 10),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = c(1-0.87, 1-0.09),
        #  legend.key = element_rect(colour = NA),
        legend.key.size = unit(0.3, "cm")) +
  guides(fill = guide_legend(ncol = 2, title.hjust = 0.5, title.position = "left",
                             title.theme = element_text(angle = 90, size = 8, colour = "white"),
                             label.theme = element_text(size = 6, colour = "white"),
                             #                             override.aes = list(colour = NA))) +
                             override.aes = list(colour = "black"))) +
  labs(fill = "Habitats",
       #       subtitle = "The Barents Sea Model Domain",
       #caption = "'S' habitats (blues) are located in the shallow inshore zone.
       #'D' habitats (yellows) are located in the deep offshore zone.", 
       x = NULL, y = NULL) +
  annotate("text", label = paste0(DDepth, " m"), x = window["xmin"]+ 220000 , y = window["ymin"]+ 70000, 
           vjust = 0, hjust = 0, angle = 45, size = 3, colour = "grey69") +
  NULL

ggsave("./img/habitats.png", width = 11, height = 11, units = "cm", dpi = 500)
