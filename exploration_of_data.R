############### SESYNC Research Support: Processing data ########## 
##
## Exploration of data for FishingandUrbanInequality pursuit gropu
## 
## DATE CREATED: 05/31/2018
## DATE MODIFIED: 06/14/2018
## AUTHORS: Benoit Parmentier 
## PROJECT: 
## ISSUE: 
## TO DO:
##
## COMMIT: adding plot of outputs for Tampa
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
library(classInt)
library(acs) # census api
library(leaflet)

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

## This is a slightly modified function from the package and paper by
#Kyle Walker , The R Journal (2016) 8:2, pages 231-242.

### Other functions ####

function_processing_data <- "exploration_of_data_functions_06142018.R" #PARAM 1
script_path <- "/nfs/bparmentier-data/Data/projects/FishingandUrbanInequality-data/scripts" #path to script #PARAM 
source(file.path(script_path,function_processing_data)) #source all functions used in this script 1.


############################################################################
#####  Parameters and argument set up ###########

out_suffix <- "data_exploration_06142018" #output suffix for the files and ouptut folder #param 12

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
df_zips_income <- read_csv(zip_data) %>%
  mutate(zip_str = str_pad(as.character(ZIPCODE), width = 5,
                           side = "left", pad = "0"),
         incpr = A02650 / N02650) %>%
  select(zip_str, incpr)

class(df_zips_income)
names(df_zips_income)
df_zips_income$zip_str

NOLA_zcta_sf <- get_zcta_zones("New Orleans")

NOLA_income_zcta_sf <- geo_join(NOLA_zcta_sf,df_zips_income,"ZCTA5CE10","zip_str")
plot(NOLA_income_zcta_sf['incpr'],
     main="Average income by zip")

Tampa_zcta_sf <- get_zcta_zones("Tampa")
Tampa_income_zcta_sf <- geo_join(Tampa_zcta_sf,df_zips_income,"ZCTA5CE10","zip_str")

## Example of ploting
col_palette <- matlab.like(256)
range_val <- range(Tampa_income_zcta_sf$incpr,na.rm=T)
seq_val <- round((range_val[2] - range_val[1])/10)
break_seq <- seq(range_val[1],range_val[2],seq_val)
breaks.qt <- classIntervals(Tampa_income_zcta_sf$incpr, n=length(break_seq), 
                            style="fixed", fixedBreaks=break_seq, intervalClosure='right')

## Color for each class
#colcode = findColours(breaks.qt , c('darkblue', 'blue', 'lightblue', 'palegreen','yellow','lightpink', 'pink','brown3',"red","darkred"))
plot(Tampa_income_zcta_sf['incpr'],
     main="Average income by zip",
     at=breaks.qt$brks,
     col=col_palette)

plot(Tampa_income_zcta_sf['incpr'],
     main="Average income by zip",
     at=seq(25,275,25))


############################ End of script #####################################################

#income_data <- acs.fetch(endyear = 2012, 
#                         geography = geo.make(state = "TX", 
#                                              county = c(113, 439), 
#                                              tract = "*"), 
#                         variable = "B19013_001",
#                         key = key_val)
#dfw_merged <- geo_join(dfw, income_df, "GEOID", "GEOID")

