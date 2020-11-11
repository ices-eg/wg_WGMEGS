
# ----------------------------
#
#   load packages
#
# ----------------------------
# install packages if are required
# spatial stuff

if(!require(rgdal, quietly = TRUE)) install.packages("rgdal") 
if(!require(sp, quietly = TRUE)) install.packages("sp") 
if(!require(maps, quietly = TRUE)) install.packages("maps") 
if(!require(mapdata, quietly = TRUE)) install.packages("mapdata") 
if(!require(maptools, quietly = TRUE)) install.packages("maptools") 
if(!require(spdep, quietly = TRUE)) install.packages("spdep") 



# workflow
if(!require(plyr, quietly = TRUE)) install.packages("plyr") 
if(!require(tidyr, quietly = TRUE)) install.packages("tidyr") 
if(!require(dplyr, quietly = TRUE)) install.packages("dplyr") 


# for modelling
if(!require(gamm4, quietly = TRUE)) install.packages("gamm4") 
if(!require(lubridate, quietly = TRUE)) install.packages("lubridate") 


# for plotting
library(lattice, quietly = TRUE)
library(latticeExtra, quietly = TRUE)

# ----------------------------
#
#   create folders
#
# ----------------------------
#create  figures and output folder

if (!dir.exists("images")) dir.create("images")
if (!dir.exists("output")) dir.create("output")

# load local utilites -------------------

#source("scripts/utilities.R")
