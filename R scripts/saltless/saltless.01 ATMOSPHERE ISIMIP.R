
# Extract monthly significant wave height

#### Setup ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate")       # List packages
lapply(Packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")

domains <- readRDS("./Objects/Domains.rds") %>%                             # Import inshore polygon
  st_transform(crs = 4326)

files<-list.files(path = "./Shared data/ISIMIP Atmosphere", pattern = "*.nc", full.names = TRUE)

Reduced <- brick("./Shared data/ISIMIP Atmosphere/ndep-nhx_histsoc_monthly_1850_2014.nc") # import reduced nitrogen deposition
Oxidised <- brick("./Shared data/ISIMIP Atmosphere/ndep-noy_histsoc_monthly_1850_2014.nc")# import oxidised nitrogen deposition

ReducedSSP370<-brick("./Shared data/ISIMIP Atmosphere/ndep-nhx_ssp370soc_monthly_2015_2100.nc")
OxidisedSSP370<-brick("./Shared data/ISIMIP Atmosphere/ndep-noy_ssp370soc_monthly_2015_2100.nc")

ReducedSSP126<-brick("./Shared data/ISIMIP Atmosphere/ndep-nhx_ssp126soc_monthly_2015_2100.nc")
OxidisedSSP126<-brick("./Shared data/ISIMIP Atmosphere/ndep-noy_ssp126soc_monthly_2015_2100.nc")
  
#### Calculate mean disturbance per habitat area, weighting by cell coverage ####

Deposition <- map(list(Reduced, Oxidised), ~{
  
exact_extract(.x, domains, "mean") %>%                                      # Extract deposition    
  mutate(Shore = domains$Shore) %>%                                         # Add metadata
  pivot_longer(-Shore, names_to = "Date", values_to = "Measured")           # Move time steps into a single column
  
}) %>%
  map2_dfr(c("R", "O"), ~{ mutate(.x, Oxidation_state = .y)}) %>%           # Add label for each variable
  mutate(Date = str_remove(Date, "mean.X")) %>%                             # Fix date string
  mutate(Date = ymd("1850-01-01") %m+% months(as.numeric(Date))) %>%  # Format as date
  mutate(Month = month(Date), 
         Year = year(Date),
         Measured = full_to_milli((Measured/14)/days_in_month(Date)))       # Convert to daily rate, and from grams to mmol

Deposition370<- map(list(ReducedSSP370, OxidisedSSP370), ~{
  
  exact_extract(.x, domains, "mean") %>%                                      # Extract deposition    
    mutate(Shore = domains$Shore) %>%                                         # Add metadata
    pivot_longer(-Shore, names_to = "Date", values_to = "Measured")           # Move time steps into a single column
  
}) %>%
  map2_dfr(c("R", "O"), ~{ mutate(.x, Oxidation_state = .y)}) %>%           # Add label for each variable
  mutate(Date = str_remove(Date, "mean.X")) %>%                             # Fix date string
  mutate(Date = ymd("2015-01-01") %m+% months(as.numeric(Date)-4248)) %>%  # Format as date
  mutate(Month = month(Date), 
         Year = year(Date),
         Measured = full_to_milli((Measured/14)/days_in_month(Date)))%>%       # Convert to daily rate, and from grams to mmol
  rbind(Deposition)


Deposition126<- map(list(ReducedSSP126, OxidisedSSP126), ~{
  
  exact_extract(.x, domains, "mean") %>%                                      # Extract deposition    
    mutate(Shore = domains$Shore) %>%                                         # Add metadata
    pivot_longer(-Shore, names_to = "Date", values_to = "Measured")           # Move time steps into a single column
  
}) %>%
  map2_dfr(c("R", "O"), ~{ mutate(.x, Oxidation_state = .y)}) %>%           # Add label for each variable
  mutate(Date = str_remove(Date, "mean.X")) %>%                             # Fix date string
  mutate(Date = ymd("2015-01-01") %m+% months(as.numeric(Date)-4248)) %>%  # Format as date
  mutate(Month = month(Date), 
         Year = year(Date),
         Measured = full_to_milli((Measured/14)/days_in_month(Date)))%>%       # Convert to daily rate, and from grams to mmol
  rbind(Deposition)

#### Plot ####

Deposition_lab370 <- mutate(Deposition370, Oxidation_state = factor(Oxidation_state, levels = c("O", "R"),
                                                          labels = c(expression("Oxidised Nitrogen (NO"["y"]*")"), expression("Reduced Nitrogen (NH"["x"]*")"))))
Deposition_lab126 <- mutate(Deposition126, Oxidation_state = factor(Oxidation_state, levels = c("O", "R"),
                                                                 labels = c(expression("Oxidised Nitrogen (NO"["y"]*")"), expression("Reduced Nitrogen (NH"["x"]*")"))))
ggplot(data = filter(Deposition_lab370, Year > 2010)) + 
  geom_line(aes(x = Date, y = Measured, colour = Shore), size = 0.15) +
  theme_minimal() +
  facet_grid(rows = vars(Oxidation_state), scales = "free_y", labeller = label_parsed) +
  labs(y = expression("mmols N m"^{-2}*"Day"^{-1}), caption = "ISIMIP Atmospheric Nitrogen deposition") +
  theme(legend.position = "top") +
  NULL

ggsave("./Figures/saltless/Atmospheric N Deposition SSP370.png", last_plot(), dpi = 500, width = 18, height = 10 , units = "cm")

ggplot(data = filter(Deposition_lab126, Year > 2010)) + 
  geom_line(aes(x = Date, y = Measured, colour = Shore), size = 0.15) +
  theme_minimal() +
  facet_grid(rows = vars(Oxidation_state), scales = "free_y", labeller = label_parsed) +
  labs(y = expression("mmols N m"^{-2}*"Day"^{-1}), caption = "ISIMIP Atmospheric Nitrogen deposition") +
  theme(legend.position = "top") +
  NULL

ggsave("./Figures/saltless/Atmospheric N Deposition SSP126.png", last_plot(), dpi = 500, width = 18, height = 10 , units = "cm")

#### Save ####
Deposition370 %>% 
  dplyr::select(Month, Oxidation_state, Shore,  Year, Measured) %>%  
  saveRDS("./Objects/Atmospheric N deposition ssp370.rds")

Deposition126 %>% 
  dplyr::select(Month, Oxidation_state, Shore,  Year, Measured) %>%  
  saveRDS("./Objects/Atmospheric N deposition ssp126.rds")
