
## Using the initial conditions at steady state for the previous decade gets us the 2050s, but we still error in the 60s and onwards 

library(MiMeMo.tools)
library(StrathE2E2)
source("./R scripts/@_Region file.R")

walk2(seq(2020, 2060, by = 10),
      seq(2029, 2069, by = 10), safely(~{
        
        file.copy(stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x-10)}-{as.character(.y-10)}-{ssp}/Param/initial_values_{toupper(implementation)}_{as.character(.x-10)}-{as.character(.y-10)}-{ssp}.csv"),
                  stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}-{ssp}/Param")) 
        
        ## Update set up file
        
        Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}-{ssp}/MODEL_SETUP.csv"))
        
        Setup_file[4,1] <- stringr::str_glue("initial_values_{toupper(implementation)}_{as.character(.x-10)}-{as.character(.y-10)}-{ssp}.csv")
        
        write.csv(Setup_file,
                  file = stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}-{ssp}/MODEL_SETUP.csv"),
                  row.names = F)
        
        #### Run to steady state ####        
        
        model <- e2e_read(implementation, str_glue("{as.character(.x)}-{as.character(.y)}-{ssp}"),
                           models.path = "StrathE2E/", results.path = "StrathE2E/Results/")
        
        results <- e2e_run(model,nyears = 100)
        
        #### Update starting conditions ####
        
        e2e_extract_start(model, results, csv.output = TRUE)                # Update starting conditions to the end of a simulation
        
        file.rename(stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}-{ssp}/Param/initial_values-base.csv"),
                    stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}-{ssp}/Param/initial_values_{toupper(implementation)}_{as.character(.x)}-{as.character(.y)}-{ssp}.csv"))
        
        unlink(stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}-{ssp}/Param/initial_values_{toupper(implementation)}{as.character(.x-10)}-{as.character(.y-10)}-{ssp}.csv"))
        unlink(stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}-{ssp}/Param/initial_values-test.csv"))
        
        ## Update set up file
        
        Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}-{ssp}/MODEL_SETUP.csv"))
        
        Setup_file[4,1] <- stringr::str_glue("initial_values_{toupper(implementation)}_{as.character(.x)}-{as.character(.y)}-{ssp}.csv")
        
        write.csv(Setup_file,
                  file = stringr::str_glue("./StrathE2E/{implementation}/{as.character(.x)}-{as.character(.y)}-{ssp}/MODEL_SETUP.csv"),
                  row.names = F)
        
      }, quiet = FALSE))

#### Check things updated ####

model <- e2e_read(implementation,str_glue("2040-2049-{ssp}"), models.path = "StrathE2E/", results.path = "StrathE2E/Results/")

results <- e2e_run(model,nyears = 10)                                # Run the model

e2e_plot_ts(model, results)                                          # Should start from a steady state

