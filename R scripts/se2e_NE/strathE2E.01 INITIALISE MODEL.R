
## Initialise model

library(ggplot2) ; source("./R scripts/@_Region file.R") # ggplot2 is needed to source the Region file

R.utils::copyDirectory("./Data/Norwegian_Basin_10_24_TG/Norwegian_Basin/2010-2019-ssp370/",              # Copy example model 
                       stringr::str_glue("./StrathE2E/{implementation}/2010-2019/"))    # Into new implementation

dir.create("./StrathE2E/Results")                                                       # Create results folder for model runs
  