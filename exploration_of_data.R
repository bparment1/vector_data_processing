############### SESYNC Research Support: Processing data ########## 
## Exploration of data for FishingandUrbanInequality pursuit gropu
## 
## DATE CREATED: 05/31/2018
## DATE MODIFIED: 06/13/2018
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
#https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html

#https://journal.r-project.org/archive/2016/RJ-2016-043/RJ-2016-043.pdf
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

library(acs) # census api
library(leaflet)

library(tidyverse)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

## Add key to .Renviron
#Sys.setenv(CENSUS_KEY=YOURKEYHERE)
## Reload .Renviron
#readRenviron("~/.Renviron")
## Check to see that the expected key is output in your R console
#Sys.getenv("CENSUS_KEY")

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
    tr <- tracts(stcodes, cb = TRUE)
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

# Write function to get ZCTAs for a given metro
get_zcta_zones <- function(metro_name) {
  ## This gets all zcta in sf format:
  zips <- zctas(cb = TRUE) #all zcta in US (4,269 polygons)
  
  metros <- core_based_statistical_areas(cb = TRUE) # get all metro areas
  # Subset for specific metro area
  # (be careful with duplicate cities like "Washington")
  my_metro <- metros[grepl(sprintf("^%s", metro_name),
                           metros$NAME, ignore.case = TRUE), ]
  
  # Find all ZCTAs that intersect the metro boundary
  
  class(my_metro)
  class(zips)
  #metro_zips <- over(my_metro, zips, returnList = TRUE)[[1]]
  #Error in (function (classes, fdef, mtable)  : 
  #            unable to find an inherited method for function ‘over’ for signature ‘"sf", "sf"’
  
  my_metro_sp <- as(my_metro,"Spatial")
  zips_sp <- as(zips,"Spatial")
  
  metro_zips <- over(my_metro_sp, zips_sp, returnList = TRUE)[[1]]
  
  my_zips <- zips[zips$ZCTA5CE10 %in% metro_zips$ZCTA5CE10, ]
  # Return those ZCTAs
  return(my_zips)
}



### Other functions ####

#function_processing_data <- ".R" #PARAM 1
#script_path <- "/nfs/bparmentier-data/Data/projects/FishingandUrbanInequality-data/scripts" #path to script #PARAM 
#source(file.path(script_path,function_processing_data)) #source all functions used in this script 1.


############################################################################
#####  Parameters and argument set up ###########

out_suffix <- "data_exploration_05312018" #output suffix for the files and ouptut folder #param 12

in_dir <- "/nfs/bparmentier-data/Data/projects/FishingandUrbanInequality-data/gis_data"
out_dir <- "/nfs/bparmentier-data/Data/projects/FishingandUrbanInequality-data/outputs"

file_format <- ".tif" #PARAM5
NA_flag_val <- -9999 #PARAM7
create_out_dir_param=TRUE #PARAM9

NOLA_MSA_counties_fname <- "NolaMSA_county2010.shp"
Tampa_MSA_counties_fname <- "TampaMSA_county2010.shp"
zcta_reprojected_fname <- "zcta2010_reprojected.shp"
zcta_sites_fname <- "zcta2010_reprojected_sites.shp"

### param
key_file <- "~/censuskey.txt"

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

key_val <- as.character(readLines(key_file))

###################### PART 1: ###########


## This gets all zcta in sf format:
zips <- zctas(cb = TRUE) #all zcta in US (4,269 polygons)
## This gets all the metropolitan and core based areas?
metros <- core_based_statistical_areas(cb = TRUE) # get core stat areas

### used for comparison:
NOLA_MSA_counties_sf <- st_read(file.path(in_dir,NOLA_MSA_counties_fname))
dim(NOLA_MSA_counties_sf)  
Tampa_MSA_counties_sf <- st_read(file.path(in_dir,Tampa_MSA_counties_fname))
dim(Tampa_MSA_counties_sf)  

selected_cities <- c("Tampa","New Orleans")
metro_tampa <- filter(metros, grepl(selected_cities[1], NAME)) ## Anything with Tampa in the name
plot(metro_tampa$geometry)
metro_NOLA <- filter(metros, grepl(selected_cities[2], NAME)) ## Anything with New Orleans in the name

#urban_NOLA <- urban_areas()
plot(metro_NOLA$geometry)
plot(NOLA_MSA_counties_sf$geometry,add=T,border="red")
plot(metro_tampa$geometry)
plot(Tampa_MSA_counties_sf$geometry,add=T,border="red")

#[847] "Tampa-St. Petersburg-Clearwater, FL"                                          
#[611] "New Orleans-Metairie, LA"                                         

tracts_tampa <- metro_tracts("Tampa")
plot(tracts_tampa)
tracts_NOLA <- metro_tracts("New Orleans")
plot(tracts_NOLA)
class(tracts_NOLA)

plot(NOLA_MSA_counties_sf$geometry)
plot(tracts_NOLA,add=T,border="red")

### Can also get counties:
Louisiana_counties_sf <- counties("Louisiana",resolution = "500k") # can change the resolution

plot(Louisiana_counties_sf)
st_crs(Louisiana_counties_sf)
st_crs(tracts_NOLA) #same projection


##### Example of getting data with ZIP code lined up to ZCTA:

# Read in the IRS data
zip_data <- "https://www.irs.gov/pub/irs-soi/13zpallnoagi.csv"
df <- read_csv(zip_data) %>%
  mutate(zip_str = str_pad(as.character(ZIPCODE), width = 5,
                           side = "left", pad = "0"),
         incpr = A02650 / N02650) %>%
  select(zip_str, incpr)

class(df)
names(df)
df$zip_str
dim(dfw)
dim(df)

df_NOLA <- get_zcta_zones("New Orleans")

df_NOLA_income <- geo_join(df_NOLA,df,"ZCTA5CE10","zip_str")
plot(df_NOLA_income['incpr'],
     main="Average income by zip")


############################ End of script #####################################################

#income_data <- acs.fetch(endyear = 2012, 
#                         geography = geo.make(state = "TX", 
#                                              county = c(113, 439), 
#                                              tract = "*"), 
#                         variable = "B19013_001",
#                         key = key_val)
#dfw_merged <- geo_join(dfw, income_df, "GEOID", "GEOID")

