
## Overwrite example boundary data

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)
source("./R scripts/@_Region file.R")

Boundary_template <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Driving/chemistry_CELTIC_SEA_2003-2013.csv"))  # Read in example boundary drivers

## Iterate over different time periods ##

map2(seq(2010, 2060, by = 10),
     seq(2019, 2069, by = 10), ~{
       
#### Last minute data manipulation ####

My_boundary_data<- readRDS("./Objects/Boundary measurements.rds") %>%                        # Import data
  filter(between(Year, .x, .y)) %>%                                                      # Limit to reference period
  group_by(Month, Compartment, Variable) %>%                                                 # Average across years
  summarise(Measured = mean(Measured, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Month) %>%                                                                         # Order months ascending
  mutate(Compartment = factor(Compartment, levels = c("Inshore S", "Offshore S", "Offshore D"),
                              labels = c("Inshore S" = "SI", "Offshore S" = "SO", "Offshore D" = "D"))) %>%
  pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured) # Spread columns to match template

My_overhang <- readRDS("./Objects/overhang exchanges.rds") %>% 
  filter(between(Year, .x, .y), Direction == "Upwelling") %>%                            # Limit to reference period 
  group_by(Month) %>%                                                                        # Average across years
  summarise(NO3 = mean(NO3, na.rm = T),
            NH4 = mean(NH4, na.rm = T),
            Detritus = mean(Detritus, na.rm = T)) %>%  
  ungroup() %>% 
  arrange(Month)                                                                             # Order months ascending

My_river_N <- readRDS("./Objects/NE River input.rds") %>%   
  filter(between(as.numeric(Year), 2010, 2019)) %>%                                          # Limit to reference period 
  mutate(Month = lubridate::month(Date)) %>% 
  group_by(Month) %>%                                                                        # Average across years
  summarise(NO3 = mean(NO3, na.rm = T),
            NH4 = mean(NH4, na.rm = T)) %>%  
  ungroup() %>% 
  arrange(Month)                                                                             # Order months ascending

My_atmosphere <- readRDS(stringr::str_glue("./Objects/Atmospheric N deposition {ssp}.rds")) %>% 
  filter(between(Year, 2010, 2019)) %>%                                                      # Limit to reference period
  group_by(Month, Oxidation_state, Shore,  Year) %>%
  summarise(Measured = sum(Measured, na.rm = T)) %>%                                         # Sum across deposition states 
  summarise(Measured = mean(Measured, na.rm = T)) %>%                                        # Average over years
  ungroup() %>% 
  pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%            # Spread to match template
  arrange(Month)                                                                             # Order months ascending

#### Create new file ####

Boundary_new <- rename(Boundary_template, SO_ammonia = SO_ammona, D_nitrate = D_intrate) %>% # Fix Mike's typos
                mutate(SO_nitrate = My_boundary_data$SO_NO3,
                       SO_ammonia = My_boundary_data$SO_NH4,
                       SO_phyt = My_boundary_data$SO_Diatoms + My_boundary_data$SO_Other_phytoplankton,
                       SO_detritus = My_boundary_data$SO_Detritus,
                       D_nitrate = My_boundary_data$D_NO3, 
                       D_ammonia = My_boundary_data$D_NH4, 
                       D_phyt = My_boundary_data$D_Diatoms + My_boundary_data$D_Other_phytoplankton,
                       D_detritus = My_boundary_data$D_Detritus,
                       SI_nitrate = My_boundary_data$SI_NO3,
                       SI_ammonia = My_boundary_data$SI_NH4,
                       SI_phyt = My_boundary_data$SI_Diatoms + My_boundary_data$SI_Other_phytoplankton, 
                       SI_detritus = My_boundary_data$SI_Detritus,
                       ## Rivers
                       RIV_nitrate = My_river_N$NO3,     
                       RIV_ammonia = My_river_N$NH4,          
                       RIV_detritus = 0,
                       ## Atmosphere, daily deposition as monthly averages
                       SO_ATM_nitrate_flux = My_atmosphere$Offshore_O,
                       SO_ATM_ammonia_flux = My_atmosphere$Offshore_R,
                       SI_ATM_nitrate_flux = My_atmosphere$Inshore_O,
                       SI_ATM_ammonia_flux = My_atmosphere$Inshore_R, 
                       SI_other_nitrate_flux = 0,   # Can be used for scenarios
                       SI_other_ammonia_flux = 0,
                       SO_other_nitrate_flux = 0,   # Can be used for scenarios
                       SO_other_ammonia_flux = 0,
                       ## Overhang
                        DO_nitrate = My_overhang$NO3,
                        DO_ammonia	= My_overhang$NH4,
                        DO_detritus = My_overhang$Detritus
                       ) 
                       
write.csv(Boundary_new, file = stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Driving/chemistry_{toupper(implementation)}_{as.character(.x)}-{as.character(.y)}-{ssp}.csv"), row.names = F)

     })