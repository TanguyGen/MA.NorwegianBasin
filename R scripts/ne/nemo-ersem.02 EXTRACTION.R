
# Pull the contents of netcdf files: Remember to mount the idrive by typing midrive into the Konsole
# saveRDS("./Objects/NE_Months/.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "nemoRsem", "furrr", "ncdf4")                                 # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multisession)                                                          # Choose the method to parallelise by with furrr

all_files <- rbind(categorise_files("I:/Science/MS/Shared/MA/CNRM_ssp370", recursive = FALSE),      # For historical and projection runs
                   categorise_files("I:/Science/MS/Shared/MA/CNRM_hist/", recursive = FALSE)) %>%   # Build metadata for each file
  drop_na() %>% 
  select(-Name)

domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  select(-c(Elevation, area))                                               # Drop unneeded data which would get included in new NM files

crop <- readRDS("./Objects/Domains.rds") %>%                                # Load SF polygons of the MiMeMo model domains
  st_buffer(dist = 50000) %>%                                               # It needs to be a bit bigger for sampling flows at the domain boundary
  summarise() %>%                                                           # Combine polygons to avoid double sampling
  mutate(Shore = "Buffer")

Bathymetry <- readRDS("./Objects/NE_grid.rds") %>%                          # Import NEMO-ERSEM bathymetry
  st_drop_geometry() %>%                                                    # Drop sf geometry column 
  select(-c("x", "y"), latitude = Latitude, longitude = Longitude)          # Clean column so the bathymetry is joined by lat/lon

#### Build summary scheme ####

scheme <- scheme_strathE2E(get_spatial(paste0(all_files$Path[1], all_files$File[1]), depthvar = "depth"),
                           Bathymetry, SDepth, DDepth, crop) %>% 
  select(x, y, layer, group, weight, slab_layer, longitude, latitude, Bathymetry) %>%   # Get a scheme to summarise for StrathE2E
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% # Convert to sf object
  st_join(st_transform(domains, crs = 4326)) %>%                            # Attach model zone information
  st_drop_geometry()                                                        # Drop sf formatting

#### !!!! Get Yuri's temporary mask fix. ####

raw <- nc_open("I:/Science/MS/Shared/MA/STRATH_GS1p0_reg025_mesh_mask.nc")
mask <- ncvar_get(raw, varid = "Tmask")                                          # 3D array where 1 is ocean
nav_lat <- ncvar_get(raw, varid = "lat")                                        # We need lat lon to join the grid to our different crop
nav_lon <- ncvar_get(raw, varid = "lon")                                        
nc_close(raw)

image(mask[,,1])
image(mask[,,53])

fix <- reshape2::melt(mask, varname = c("x", "y", "layer"), value.name = "ocean_mask") %>% 
  filter(ocean_mask == 1) 

scheme <- scheme %>% 
  left_join(fix) %>% 
  drop_na(ocean_mask) %>% 
  select(-ocean_mask) %>% 
  group_by(slab_layer, x, y) %>%                                                # Rebuild grouping in case any pixels are dropped, ruining the order
  mutate(group = cur_group_id()) %>%                                            
  ungroup()

#### end fix ####

start <- scheme_to_start()                                                  # Get netcdf vectors which define the minimum
count <- scheme_to_count()                                                  # amount of data to import
scheme <- scheme_reframe(scheme) 

scheme_result <- arrange(scheme, group) %>%                                 # Create a meta-data object to attach to the summaries
  select(x, y, slab_layer, longitude, latitude, Shore, Bathymetry) %>% 
  distinct() %>% 
  mutate(slab_layer = if_else(slab_layer == 1, "S", "D"),
         weights = case_when(slab_layer == "S" & Bathymetry >= SDepth ~ SDepth,     # Weights for zonal averages by thickness of water column
                             slab_layer == "S" & Bathymetry < SDepth ~ Bathymetry,
                             slab_layer == "D" & Bathymetry >= DDepth ~ (DDepth - SDepth),
                             slab_layer == "D" & Bathymetry < DDepth ~ (Bathymetry - SDepth)))

look <- filter(scheme_result, slab_layer == "S") 

ggplot(look) +
  geom_raster(aes(x = x, y = y, fill = Shore))

#### extract ####

##!!## could go through and remove even more unneccessary files from the process.

tictoc::tic()
all_files %>%
#  filter(!Type %in% thetao_conc( "difvho", "wo")) %>% 
  filter(Type == "thetao_con") %>%                                              # The only monthly volumes the compiler actually needs are temperatures. Others are boundary (5-daily) extractions
  split(., f = list(.$Month, .$Year)) %>%                                       # Specify the timestep to average files over.
  #.[1:12] %>%
  future_map(NEMO_ERSEM, analysis = "slabR", summary = scheme_result,
             scheme = scheme, start = start, count = count,
             out_dir = "./Objects/NE_Months", .progress = T)                    # Perform the extraction and save an object for each month (in parallel)
tictoc::toc() # 9 hours 40 to extract for all files.

#### check ####

# NE.01.2020 <- readRDS("./Objects/NE_Months/NE.01.2020.rds")
# 
# ggplot(NE.01.2020) +
# #  geom_raster(aes(x = x, y = y, fill = Temperature)) +
#   geom_raster(aes(x = x, y = y, fill = NO3)) +
#   facet_wrap(vars(slab_layer))
