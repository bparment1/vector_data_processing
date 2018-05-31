############### SESYNC Research Support: Processing data ########## 
## Exploration of data for FishingandUrbanInequality pursuit gropu
## 
## DATE CREATED: 05/31/2018
## DATE MODIFIED: 06/07/2018
## AUTHORS: Benoit Parmentier 
## PROJECT: 
## ISSUE: 
## TO DO:
##
## COMMIT: testing tigris
##
## Links to investigate:
#https://journal.r-project.org/archive/2016/RJ-2016-043/index.html
#https://walkerke.github.io/2017/05/tigris-metros/

###################################################
#

###### Library used

library(gtools)                              # loading some useful tools 
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(raster)
library(gdata)                               # various tools with xls reading, cbindX
library(rasterVis)                           # Raster plotting functions
library(parallel)                            # Parallelization of processes with multiple cores
library(maptools)                            # Tools and functions for sp and other spatial objects e.g. spCbind
library(maps)                                # Tools and data for spatial/geographic objects
library(plyr)                                # Various tools including rbind.fill
library(spgwr)                               # GWR method
library(rgeos)                               # Geometric, topologic library of functions
library(gridExtra)                           # Combining lattice plots
library(colorRamps)                          # Palette/color ramps for symbology
library(ggplot2)
library(lubridate)
library(dplyr)
library(rowr)                                # Contains cbind.fill
library(car)
library(sf)
library(tigris)

library(tidyverse)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

###### Functions used in this script and sourced from other files

create_dir_fun <- function(outDir,out_suffix=NULL){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    outDir <- file.path(outDir,out_name)
  }
  #create if does not exists
  if(!file.exists(outDir)){
    dir.create(outDir)
  }
  return(outDir)
}

#Used to load RData object saved within the functions produced.
load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

metro_tracts <- function(metro_name) {
  
  # First, identify which states intersect the metro area using the
  # `states` function in tigris
  st <- states(cb = TRUE)
  cb <- core_based_statistical_areas(cb = TRUE)
  metro <- filter(cb, grepl(metro_name, NAME))
  
  stcodes <- st[metro,]$STATEFP
  
  # Then, fetch the tracts, using rbind_tigris if there is more
  # than one state
  if (length(stcodes) > 1) {
    tr <- rbind_tigris(
      map(stcodes, function(x) {
        tracts(x, cb = TRUE)
      })
    )
  } else {
    tr <- tracts(x, cb = TRUE)
  }
  
  # Now, find out which tracts are within the metro area
  within <- st_within(tr, metro)
  
  within_lgl <- map_lgl(within, function(x) {
    if (length(x) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  # Finally, subset and return the output
  output <- tr[within_lgl,]
  
  return(output)
  
}

### Other functions ####

#function_processing_data <- ".R" #PARAM 1
#script_path <- "/nfs/bparmentier-data/Data/projects/FishingandUrbanInequality-data/scripts" #path to script #PARAM 
#source(file.path(script_path,function_processing_data)) #source all functions used in this script 1.


############################################################################
#####  Parameters and argument set up ###########

out_suffix <- "data_exploration_05312018" #output suffix for the files and ouptut folder #param 12

in_dir <- "/nfs/bparmentier-data/Data/projects/FishingandUrbanInequality-data/data"
out_dir <- "/nfs/bparmentier-data/Data/projects/FishingandUrbanInequality-data/outputs"

file_format <- ".tif" #PARAM5
NA_flag_val <- -9999 #PARAM7
create_out_dir_param=TRUE #PARAM9

############## START SCRIPT ############################

######### PART 0: Set up the output dir ################

if(is.null(out_dir)){
  out_dir <- in_dir #output will be created in the input dir
}
#out_dir <- in_dir #output will be created in the input dir

out_suffix_s <- out_suffix #can modify name of output suffix
if(create_out_dir_param==TRUE){
  out_dir <- create_dir_fun(out_dir,out_suffix)
  setwd(out_dir)
}else{
  setwd(out_dir) #use previoulsy defined directory
}

###################### PART 1: ###########

orwa <- rbind_tigris(
  tracts("OR", cb = TRUE), 
  tracts("WA", cb = TRUE)
)

ggplot(orwa) + geom_sf()

cb <- core_based_statistical_areas(cb = TRUE)

pdx <- filter(cb, grepl("Portland-Vancouver", NAME))

ggplot(pdx) + geom_sf()

### Now get Florida

fl <- rbind_tigristracts("OR", cb = TRUE) 

cb <- core_based_statistical_areas(cb = TRUE)
fl <- tracts(state = 'FL', county = c('Hillsborough'))

pdx <- filter(cb, grepl("Tampa", NAME))

