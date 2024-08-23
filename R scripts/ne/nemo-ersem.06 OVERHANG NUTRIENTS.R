
# Interpolate the exchange at the vertical boundary from NEMO-MEDUSA model output:
# saveRDS("./Objects/vertical boundary/.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "nemoRsem", "furrr", "ncdf4", "tictoc")          # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multisession)

domain <- readRDS("./Objects/Overhang.rds") %>%                             # Get the horizontal area to extract over 
  select(Shore) 

example <- list.files("I:/Science/MS/Shared/MA/CNRM_ssp370",            # File to pull dimensions from 
                      recursive = T, full.names = TRUE, pattern = "N4_n")[5]

#### Create summary scheme to interpolate a depth layer over a given area #####

Bathymetry <- readRDS("./Objects/NE_grid.rds") %>%                          # Import NEMO-MEDUSA bathymetry
  st_drop_geometry() %>%                                                    # Drop sf geometry column 
  select(-c("x", "y"), latitude = Latitude, longitude = Longitude)          # Clean column so the bathymetry is joined by lat/lon

scheme <- scheme_interp_slice(get_spatial(example, depthvar = "depth"), DDepth, domain) # Get a scheme for linear interpolation between 2 depth layers

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
  group_by(y, x) %>%                                                        # Rebuild grouping in case any pixels are dropped, ruining the order
  mutate(group = cur_group_id()) %>%                                            
  ungroup()

#### end fix ####

start <- scheme_to_start()                                                  # Get netcdf vectors which define the minimum
count <- scheme_to_count()                                                  # amount of data to import

scheme <- scheme_reframe(scheme) %>%                                        # Adjust scheme indices so they match the array subset
  left_join(Bathymetry) %>%                                                 # Attach bathymetry to summary scheme
  filter(depth < Bathymetry & DDepth < Bathymetry) %>%                      # Drop points where the target depth or next deeper layer are below the sea floor
  group_by(y, x) %>%                                                        # Redefine the group column as removing points can disrupt this
  mutate(group = cur_group_id()) %>%                                        # Create a new grouping column for the summary scheme
  ungroup()

summary <- filter(scheme, layer == 1) %>%                                   # Create the metadata to attach to summaries
  arrange(group) %>%                                                        # Summaries will be returned in group order, so make sure these match
  mutate(depth = DDepth) %>%                                                # Lets return the depth we interpolated to
  select(x, y, longitude, latitude, depth)                                  # As well as horizontal information

#### Extract ####

files <- rbind(categorise_files("I:/Science/MS/Shared/MA/CNRM_ssp370", recursive = FALSE),      # For historical and projection runs
               categorise_files("I:/Science/MS/Shared/MA/CNRM_hist/", recursive = FALSE)) %>%   # Build metadata for each file
  drop_na() %>% 
  select(-Name) %>% 
  filter(Type %in% c("N4", "N3", "RP")) %>%                    # Nitrate, Ammonia, Detritus only
  split(., f = list(.$Month, .$Year))                                       # Get a DF of file names for each time step to summarise to

tic()
files %>% 
#  .[1:12] %>%
  future_map(NEMO_ERSEM, analysis = "slabR",                        # Interpolate files in paralell
           out_dir = "./Objects/overhang nutrient", scheme = scheme,
           start = start, count = count, summary = summary, 
           collapse_days = FALSE, .progress = T)
toc()

#### Check ####

# NE.01.2015 <- readRDS("./Objects/overhang nutrient/NE.01.2015.rds") %>%
#   mutate(day = rep(1:6, each = nrow(summary)))
# 
# ggplot(NE.01.2015) +
#   geom_raster(aes(x= x, y = y, fill = NH4)) +
#   facet_wrap(vars(day)) +
#   theme_minimal() +
#   NULL

#### Combine ####

W_files <- list.files("./Objects/overhang/", full.names = TRUE)
files <- list.files("./Objects/overhang nutrient/", full.names = TRUE)

future_map2(files, W_files, ~{
  
  slimmed <- readRDS(.x) %>%     # Import nutrient data
    select(NO3, NH4, Detritus)   # Select just the variables of interest
  
  readRDS(.y) %>%                # Import flow data
    cbind(slimmed) %>%           # add the nutrient variables
    saveRDS(.y)                  # overwrite the flow data

  }, .progress = T)

# NE.01.2015 <- readRDS("./Objects/overhang/NE.01.2015.rds") %>%                  # Check both sets of data are combined
#   mutate(day = rep(1:6, each = nrow(summary)))
