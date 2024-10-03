
# Processing irradiance data used to force the Mission Atlantic NEMO-ERSEM runs

#### Set up ####

rm(list=ls())                                                                   # Wipe the brain

packages <- c("tidyverse", "terra", "exactextractr", "tictoc", "ncdf4")         # List packages
lapply(packages, library, character.only = TRUE)                                # Load packages
source("./R scripts/@_Region file.R")                                           # Define project region 

# A weakness of terra is that you can't use furrr to parallelise operations, something about the C code fights with the threading. 

forecast <- list.files(str_glue("I:/Science/MS-Marine/MA/CNRM_{ssp}/irradiance/"), recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as.data.frame() %>%                                                           # Turn the vector into a dataframe/tibble
  rename(value = 1) %>% 
  separate(value, into = c(NA, "Year"), 
           remove = FALSE, sep = "_y") %>%                                      # Extract the year from the file name
  mutate(Year = str_sub(Year, end = -4)) %>%                                    # Drop file extension to get number
  rename(File = "value")

all_files <- list.files("I:/Science/MS-Marine/MA/CNRM_hist/irradiance/", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as.data.frame() %>%                                                           # Turn the vector into a dataframe/tibble
  rename(value = 1) %>% 
  separate(value, into = c(NA, "Year"), 
           remove = FALSE, sep = "_y") %>%                                      # Extract the year from the file name
  mutate(Year = str_sub(Year, end = -4)) %>%                                    # Drop file extension to get number
  rename(File = "value") %>% 
  filter(Year < 2015) %>% 
  rbind(forecast)

domains <- readRDS("./Objects/Domains.rds") %>%                                 # Load SF polygons of the model domain
  st_transform(crs = 4326) %>% 
  summarise(area = sum(area))                                                   # Quick and dirty polygon union

#### Extraction ####

tic()

extract <- map2(all_files$File, all_files$Year, ~{

Watts <- rast(.x) %>%                                                           # Load the data for a year
  rotate() %>%                                                                  # change 0:360 to -180:180
  exact_extract(domains, "mean", progress = TRUE) %>%                           # Get the average value across the model domain
  as.matrix() %>% 
  as.vector                                                                     # Convert from wide data frame to a vector

if(length(Watts)%%365 == 0) Days <- 365                                         # Catch leap years
if(length(Watts)%%366 == 0) Days <- 366
 
light <- data.frame(Watts = Watts, Day = rep(1:Days, each = 8), Year = .y) %>% # 3 hour time step means 8 steps per day
  group_by(Day, Year) %>% 
    summarise(Watts = mean(Watts, na.rm = TRUE)) %>%                            # Average power(Watts) per day 
    mutate(Light = nemoRsem::shortwave_to_einstein(Watts),                                # Convert to Einstens for Strath E2E
           Date = as.Date(Day-1, origin = str_glue("{Year}-01-01")),               
           Month = lubridate::month(Date)) %>% 
  group_by(Month) %>% 
  summarise(Light = mean(Light, na.rm = TRUE),                                  # average per month
            Date = mean(Date)) 

}) %>% 
  data.table::rbindlist()

toc()

#saveRDS(extract, "./Objects/light.rds")

#### Plot ####

ggplot(data = extract) +
  geom_line(aes(x = Date, y = Light), linewidth = 0.25) +
  theme_minimal() +
  labs(y = NULL, caption = "NEMO-ERSEM driving data") +
  theme(legend.position = "top") +
  ylab(expression("Light (Em"^{-2}*"d"^{-1}*" )")) +
  NULL

#ggsave("./Figures/saltless/Light.png", last_plot(), dpi = 500, width = 12, height = 10 , units = "cm")
