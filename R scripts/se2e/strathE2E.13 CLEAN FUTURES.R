
## Create model variants for new time periods

rm(list=ls())                                                                  # Wipe the brain
library(MiMeMo.tools)
library(StrathE2E2)
source("./R scripts/@_Region file.R")

#### Copy first variant ####

map2(seq(2020, 2060, by = 10),
     seq(2029, 2069, by = 10), ~{
      
R.utils::copyDirectory(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/"), # Copy 
                       stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}-{ssp}/")) # Into
})

#### Rinse decades to change the set up file to read one time period, delete others ####

map2(seq(2010, 2060, by = 10),
     seq(2019, 2069, by = 10), ~{
       
        # Change the set up file to point to new driving data
        
        Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}-{ssp}/MODEL_SETUP.csv"))
        
        Setup_file[2,1] <- stringr::str_glue("physics_{toupper(implementation)}_{as.character(.x)}-{as.character(.y)}-{ssp}.csv")
        Setup_file[3,1] <- stringr::str_glue("chemistry_{toupper(implementation)}_{as.character(.x)}-{as.character(.y)}-{ssp}.csv")

        write.csv(Setup_file,
                  file = stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}-{ssp}/MODEL_SETUP.csv"),
                  row.names = F)
        
        # Delete other files for this variant
        
        unwanted_files <- list.files(str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}-{ssp}/Driving/"),
                                     full.names = TRUE) %>% 
          .[-grep(str_glue("{as.character(.x)}-{as.character(.y)}-{ssp}.csv"), .)]                                         # Drop files which start this year
        
        unlink(unwanted_files)                                                  # Delete the rest
        
      })
