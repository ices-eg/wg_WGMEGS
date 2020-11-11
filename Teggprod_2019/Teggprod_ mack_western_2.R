####Teggprod####Teggprod####Teggprod####Teggprod####Teggprod####Teggprod####Teggprod
####Teggprod####Teggprod####Teggprod####Teggprod####Teggprod####Teggprod####Teggprod
####Teggprod####Teggprod####Teggprod####Teggprod####Teggprod####Teggprod####Teggprod

######################
## SCRIPT: Total Annual egg Production: Traditional Method
## MACKEREL WESTERN COMPONENT
## version: August 2019
## Author: Gersom Costas 
## email : gersom.costas@ieo.es
######################
##
## SECTIONS
##
## 0. Preliminar
## 1. Download Survey grid
## 2. Import survey data
## 3. Convert survey data to spatial object.(Ploting hauls)
## 4. Spatial overlap: survey grid - station positions
## 5. Interpolation
## 6. Daily Egg production in rectangles-periods
## 7. Variance
## 8. ANNUAL EGG PRODUCTION:egg production and variance in periods




######################
#  0. Preliminar
######################

getwd() # print the current working directory -
rm(list = ls())           #Cleaning

##Installing and loading  packages and functions

source("scripts/00_install.R")



####################
#  1. Download Survey grid  
#####################

#importing grid data by region. for mackerel western component area (https://github.com/GersomCostas/Mack_western-component-grid)

load("data/AEPM_grid_mack_Western.RData")

# view survey grid for mackerel Western component

western_grid(RECT_p)

# if you want: Save survey grid for mackerel western component into figures folder

#western.grid_save(RECT_p)



####################
#  2. Import raw survey data.
#####################

##Data and dates files should be a plane files: csv columns separated by a comma. See survey_data_column_description.txt and dates_survey_description.txt files

# names of survey data file. Without extension!!.

file_name="data/add_portuguese data _24.04.2020"

#name of date file. Without extension.

file_date="data/dates_MEGS_2019_24.04.2020_New2"

##Arguments:

  #Choosing year

  year=2019

  #Choosing component. "W": western; "S": southern

  component="W"

##transform files to standardize format. And create survey data object (object name specify component (Southern o western ) and survey year)
##don't worry about warnings ("There were 50 or more warnings (use warnings() to see the first 50)"). Even if R throws a warning, it continues to execute the code regardless. You can usually ignore warnings unless column names are misspelled 

source("scripts/01_Import_data.R")


##NOTE: NEXT you MUST   rename the object name in this script. Name of new object is into consola ("NOTE!!! New file:      "). Be very careful  not change extensions of R objects  (.ls, .lspdf)


##Checking imported data

summary(survey_megs_W_2019)

#date summary by period

dateperiod(survey_megs_W_2019)

## estimate Daily egg production (DEP) by rectangle
# Daily egg production by rectangle (eggs per m2/ day) was calculated using the data on stage I eggs per m2. Temperature used to calculate the duration of stage I eggs  was taken at 20 m depth  it is not available the use temperature at 5 m.
# DEP for mackerel is estimated with the Mendiola egg development equation (Mendiola et al., 2006) and  the Lockwood egg development equation (Lockwood et al., 1981)

survey_megs_W_2019<-daily_eggprod(survey_megs_W_2019)



####################
#  3. Convert survey data to spatial object.
#####################

# transform survey data to  Spatial object (list) using function "ls_spat" .  Actually is a list (periods) of spatial objects.


survey_megs_W_2019.ls<-ls_spat(survey_megs_W_2019)


##Checking  period dates in spatial object (".id" means period, dates: start date, datee: end date )

ldply(survey_megs_W_2019.ls, function(x)data.frame(dates = min(x@data$Date), datee = max(x@data$Date)))



## Ploting hauls
###################################################

##Create a multi-paneled plotting window.  Where "A" refers to the number of rows and "B" to the number of columns (and where each cell will hold a period). ad-hoc: Depending total number of periods

A<-3    # number of rows 
B<-2    # number of columns


##View plot

plothaul_view_w(survey_megs_W_2019.ls)


##If you like save the plot into images folder. Named as "survey"

plothaul_save_w(survey_megs_W_2019.ls)



####################
#  4. Spatial overlap: survey grid - station positions 
#####################

#overlapping survey grid  and haul positions: assign each station to staistical rectangle. Using function "overlap.grid".
##don't worry about warnings .  it continues running the code  

survey_megs_W_2019.lspdf<-overlap.grid(survey_megs_W_2019.ls)



####################
#  5. Interpolation
#####################

#For unsampled rectangles within the designated survey areas. the convention for extrapolation used on all previous surveys has been used. 2 types of contiguity based relations:

### Rook Contiguity: contiguity does not include corners, only borders, thus comprising only polygons sharing more than one boundary point ####Qualifying the unsampled rectangle
### Queen Contiguity: defines a neighbour when at least one point on the boundary of one polygon is shared with at least one point of its neighbour (common border or corner). Once qualified the unsampled rectangle, the mean daily egg production values of all surrounding rectangles, both immediately adjacent and diagonally adjacent are used to calculate the interpolated value   ####interpolating


### Creating Rook Contiguity in grid
RECT_nb_rook <- poly2nb(RECT_p,queen=F)

### Creating Queen Contiguity    in grid

RECT_nb <- poly2nb(RECT_p)

#associate rectangle names with the neighbour list, 
r.id <- attr(RECT_nb, "region.id")

##ploting neighbourhood:

plot.neighbour_w (RECT_p)


#### NOW INTERPOLATION!!!!!!!. The interpolated value (unsampled rectangle) is the arithmetic mean of all those surrounding rectangles including zeros (at least two sampled rectangles).

survey_megs_W_2019.lspdf<-interpol(survey_megs_W_2019.lspdf)



####################
#  6. Daily Egg production in rectangles-periods
#####################

survey_megs_W_2019.lspdf<-eggprod(survey_megs_W_2019.lspdf)



####################
#  7. Variance
#####################

##coefficient of variation (variance in egg counts are distributed with a constant spatio-temporal coefficient of variation ). These coefficients of variation (CV) are used to calculate the variance of Annual Egg Production estimation. 
#Traditional methodology estimates CV by rectangle using  rectangles with replicate hauls. If no rectangles with replicate haul  (warning: "Error in `contrasts<-contrasts can be applied only to factors with 2 or more levels") in a period we use a general CV (all replicated rectangles in survey).
#An alternative methodology in progress is estimate CV by a GAM

cv_traditional<- coef_var_trad(survey_megs_W_2019.ls)
cv_gam<- cv_gam(survey_megs_W_2019.ls)

##VARIANCE in rectangles. the variance in egg densities for each sampling rectangle.

survey_megs_W_2019.lspdf<-var_mack(survey_megs_W_2019.lspdf)



####################
#  8. ANNUAL EGG PRODUCTION:egg production and variance in periods
#####################

 ##table. exported to output folder
library(tidyr)
tabDEP<-tableDEP(survey_megs_W_2019.lspdf)

#saveRDS()
tabDEP_midday<-tableDEP2(survey_megs_W_2019.lspdf)






##export egg production by rectangle in periods as plane file. output folder
  
MACKwest_2019_plane<-ldply(survey_megs_W_2019.lspdf,function(x)data.frame(x@data))
  
head(MACKwest_2019_plane)

write.csv(MACKwest_2019_plane,"output/west_df2019.csv", row.names=F)
  
  
APPENDIX
#estimation SSB
tableDEP_final<-read.csv("output/output_table.csv")
#ssbw <- t(t(taep.mack) / Frw * 2 * 1.08) * 1e-9
#Frw <- 1142 provisional fec
Frw <- 1147
varFrw<-NA
TAEP<-sum(tableDEP_final$AEP,na.rm=T)
varTAEP<-sum(tableDEP_final$A_var_trad,na.rm=T)


SSBW<-(TAEP/Frw * 2 * 1.08) * 1e-9


#Variance (biomass) = Biomass2 x (CV(E)2 + CV(F)2)
VarSSB<-(SSBW* 1e9)^2*((sqrt(varTAEP)/TAEP)^2+(sqrt(varFrw)/Frw)^2)

Final_table<-matrix(c(TAEP,varTAEP,Frw, varFrw,SSBW,  VarSSB), nrow=2) 
colnames(Final_table)<-c("TAEP","Frw","SSBW")
rownames(Final_table)<-c("estimate","variance")
Final_table<-as.data.frame(Final_table)

Final_table[3,]<-paste(round((c(sqrt(Final_table[2,1])/Final_table[1,1],sqrt(Final_table[2,2])/Final_table[1,2],sqrt(Final_table[2,3])/(Final_table[1,3]* 1e9))),4)*100,"%")
row.names(Final_table)[3]<-"CV"
Final_table[4,1]<-"??"
row.names(Final_table)[4]<-"CV-data"
Final_table

write.csv(Final_table,"output/Final_table_western_2019.csv",row.names = T,na="NA")
