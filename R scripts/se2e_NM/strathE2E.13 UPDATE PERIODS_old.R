
## Create model variants for new time periods

rm(list=ls())                                                                  # Wipe the brain
library(MiMeMo.tools)
library(StrathE2E2)
source("./R scripts/@_Region file.R")

#### First ####

#e2e_copy("Ascension", "2010-2019", dest.path = "./StrathE2E") # Copy the parameterised model

#### Rinse decades for climate change updates ####

map2(seq(2020, 2090, by = 10),
     seq(2029, 2099, by = 10), ~{
       
       R.utils::copyDirectory(str_glue("./StrathE2E/Models/{implementation}/2010-2019"),          
                              str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}"))          

       # #### Turn off fishing effort in the future only ####
       # 
       # Effort <- read.csv(str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}/Param/fishing_activity_{toupper(implementation)}_2010-2019.csv")) %>% 
       #   mutate(`Activity_.s.m2.d.` = case_when(str_detect(Gear_name, "Artisanal") ~ `Activity_.s.m2.d.`,         # Keep fishing effort for artisinal gears only
       #                                          T ~ 0)) 
       # 
       # write.csv(Effort, row.names = F,
       #           file = str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}/Param/fishing_activity_{toupper(implementation)}_{as.character(.x)}-{as.character(.y)}.csv"))   # Read in example boundary drivers
       # 
       # unlink(stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}/Param/fishing_activity_{toupper(implementation)}_2010-2019.csv"))
       
     })

walk2(seq(2020, 2090, by = 10),
      seq(2029, 2099, by = 10), ~{
        
        update_boundary_period_MA(.x, .y, str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}"), island = TRUE)
        update_physics_period_MA(.x, .y, str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}"), ice = FALSE, island = TRUE)
        
        # Change the set up file to point to new driving data
        
        Setup_file <- read.csv(stringr::str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}/MODEL_SETUP.csv"))
        
        Setup_file[2,1] <- stringr::str_glue("physics_{toupper(implementation)}_{as.character(.x)}-{as.character(.y)}.csv")
        Setup_file[3,1] <- stringr::str_glue("chemistry_{toupper(implementation)}_{as.character(.x)}-{as.character(.y)}.csv")
   #     Setup_file[12,1] <- stringr::str_glue("fishing_activity_{toupper(implementation)}_{as.character(.x)}-{as.character(.y)}.csv")
        
        write.csv(Setup_file,
                  file = stringr::str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}/MODEL_SETUP.csv"),
                  row.names = F)
      })
