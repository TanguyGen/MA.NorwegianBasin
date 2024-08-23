
# Create a spatial grid to bind extracted NE model outputs to, including distance from shore and bathymetry
# Remember to mount the idrive by typing midrive into the Konsole

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "ncdf4", "sf")                                # List handy data packages
lapply(packages, library, character.only = TRUE)                            # Load packages

Space <- list.files("../Shared data/NEMO_ERSEM/Yuri_test/", recursive = TRUE, full.names = TRUE) %>% 
  .[8] %>%                                                                  # Name an example NE file
  nemoRsem::get_spatial(depthvar = "depth")                                # And pull the spatial variables

#### Get NEMO-MEDUSA bathymetry data ####

NE_bath <- "../Shared data/NEMO_ERSEM/Yuri_test/STRATH_bathymetry.nc"

raw <- nc_open(NE_bath)
bath_lat <- ncvar_get(raw, varid = "nav_lat")
bath_lon <- ncvar_get(raw, varid = "nav_lon")
bath_bath <- ncvar_get(raw, varid = "bathy")
nc_close(raw)

#### Crop to the shared data files extent ####

tl <- which(bath_lat == Space$nc_lat[1,1] & bath_lon == Space$nc_lon[1,1], arr.ind = TRUE) # Cut out Bathymetry columns which match SA CROP
tr <- which(bath_lat == Space$nc_lat[1,ncol(Space$nc_lat)] & bath_lon == Space$nc_lon[1,ncol(Space$nc_lon)], arr.ind = TRUE)# Where in the big grid matches each corner of SA?
bl <- which(bath_lat == Space$nc_lat[nrow(Space$nc_lat),1] & bath_lon == Space$nc_lon[nrow(Space$nc_lon),1], arr.ind = TRUE)  
br <- which(bath_lat == Space$nc_lat[nrow(Space$nc_lat), ncol(Space$nc_lat)] & bath_lon == Space$nc_lon[nrow(Space$nc_lon),ncol(Space$nc_lon)], arr.ind = TRUE)

bath <- bath_bath[tl[,"row"]:(tl[,"row"]+nrow(Space$nc_lat)-1),
                  tl[,"col"]:(tl[,"col"]+ncol(Space$nc_lat)-1)]

lat <- bath_lat[tl[,"row"]:(tl[,"row"]+nrow(Space$nc_lat)-1),
                tl[,"col"]:(tl[,"col"]+ncol(Space$nc_lat)-1)]

lon <- bath_lon[tl[,"row"]:(tl[,"row"]+nrow(Space$nc_lat)-1),
                tl[,"col"]:(tl[,"col"]+ncol(Space$nc_lat)-1)]

#### Further region specific crop to speed up extraction ####

grid <- setNames(reshape2::melt(lat), c("x", "y", "Latitude")) %>% 
  left_join(setNames(reshape2::melt(lon), c("x", "y", "Longitude"))) %>% 
  left_join(setNames(reshape2::melt(bath), c("x", "y", "Bathymetry"))) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) #%>% # Set dataframe to SF format
#  st_transform(crs) 

ggplot(grid) +
  geom_raster(aes(x=x, y=y, fill = Bathymetry))

saveRDS(grid, file = "./Objects/NE_grid.rds")      # Save
