#99.  ALL


##header
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


#########################
#
# import data files
#
#########################

#if (!dir.exists("db")) dir.create("db")

#file_name=""


path<-file.path(paste(file_name,"csv", sep="."))

data<-data.frame(Unique.ID=character(0),
                 Period=character(0),
                 Component=character(0), 
                 Country=character(0),
                 Vessel=character(0),
                 Gear=character(0),
                 Haul=character(0),
                 Aperture=character(0),
                 Ap.area=character(0) ,
                 Effic=character(0),
                 Day=character(0),
                 Month=character(0),
                 Year=character(0),
                 Hour=character(0),
                 Minutes=character(0),
                 LatDeg=character(0),
                 LatMins=character(0),
                 LongDeg=character(0),
                 LongMins=character(0),
                 "E.W"=character(0),
                 declon=character(0),
                 declat=character(0) , 
                 HALFST=character(0) ,
                 FlowRevs=character(0),
                 FlowCal=character(0),
                 FlowRevsB=character(0),
                 FlowCalB=character(0),
                 Sdepth=character(0),
                 Bdepth=character(0),
                 VolFilt=character(0),
                 TempSur.5m.=character(0),
                 Temp20m=character(0),
                 Temp50m=character(0),
                 Temp100m=character(0),
                 TempB=character(0),
                 Sal5m =character(0),
                 Sal20m=character(0),
                 Mac1=character(0),
                 Mac2=character(0),
                 Mac3=character(0),
                 Mac4=character(0),
                 Mac5=character(0),
                 MacFactor=character(0),
                 Raised_MacStage1=character(0),
                 Raised_MacStage2=character(0),
                 Raised_MacStage3=character(0),
                 Raised_MacStage4=character(0),
                 Raised_MacStage5=character(0),
                 Hom1=character(0),
                 Hom2=character(0),
                 Hom3=character(0),
                 Hom4=character(0),
                 HomFactor=character(0),
                 Raised_HomStage1=character(0),
                 Raised_HomStage2=character(0),
                 Raised_HomStage3=character(0),
                 Raised_HomStage4=character(0),
                 Others=character(0),
                 OthFactor=character(0),
                 RaisedOthers=character(0),
                 NoteLineRef_no=character(0))

megs_data_survey<-plyr::rbind.fill(data,dplyr::mutate_all(read.csv(path, stringsAsFactors=FALSE, strip.white=TRUE,sep=','),as.character))%>%
  
  dplyr::mutate_at(funs(as.factor(.)),.vars=c("Unique.ID","Country","Vessel", "Component", "Gear", "Haul" ,"E.W","HALFST","Bdepth", "NoteLineRef_no"))%>%
  
  dplyr::mutate_if(is.character,funs(as.numeric(.)))%>%
  
  dplyr::mutate(Lat=(declat+0.011), Lon=(declon-0.011))%>%
  
  dplyr::mutate(Date=lubridate::dmy(paste(Day,Month,Year,sep="/")))%>%
  
  dplyr::mutate(Mac1_m2=(Raised_MacStage1*Sdepth)/VolFilt)%>%
  
  dplyr::mutate(Hom1_m2=(Raised_HomStage1*Sdepth)/VolFilt)%>%
  
  dplyr::mutate(nhaul=1)%>%
  
  plyr::rename( c("TempSur.5m." = "Temp5m", "HALFST"= "RECT"))


#year<-  bind_rows

#component<-"

#subsetting component and year

subset(megs_data_survey, Year==year & Component==component)->subset_name
assign(paste("survey_megs",component,year, sep="_"),subset_name)%>%droplevels()

rm(subset_name, data)
#print(summary(subset_name))
print(paste("NOTE!!!! New file: survey_megs",component,year, sep="_"))

#DATES

path2<-file.path(paste(file_date,"csv", sep="."))

tdate <- read.csv(path2,  strip.white=TRUE,stringsAsFactors=FALSE,sep=',')%>%
  
  mutate(dates=dmy(dates),datee=dmy(datee), period=as.character(period) )%>%
  survper()


#10. DAILY EGG PRODUCTION ----

# Daily egg production by rectangle (eggs per m2/ day) was calculated using the 
#data on stage I eggs per m2  ( stage I Egg /m2)
# The temperature used to calculate the duration of stage I eggs for both 
#species (Mackerel and Horse Mackerel) was taken at 20 m depth  it is not 
#available the use temperature at 5 m.


# For Mackerel by Lockwood eq.(For stage I mackerel eggs)


Mac1_m2_day_Lockw <- function(data)
  
{
  
  for (i in 1:length(data$Mac1_m2))
    
  {
    
    if (is.na( data$Temp20m[i]))
      
      data$Mac_m2_day.Lockw[i]<-24*data$Mac1_m2[i]/(exp(-1.61*log(data$Temp5m[i])+7.76))
    
    else
      
      data$Mac_m2_day.Lockw[i]<-24*data$Mac1_m2[i]/(exp(-1.61*log(data$Temp20m[i])+7.76))
    
  }
  
  data
  
}


# For Mackerel by Mendiola eq.(For stage I mackerel eggs)


Mac1_m2_day_Mend <- function(data)
  
{
  
  for (i in 1:length(data$Mac1_m2))
    
  {
    
    if (is.na( data$Temp20m[i]))
      
      data$Mac_m2_day.Mend[i]<-24*data$Mac1_m2[i]/(exp(-1.61*log(data$Temp5m[i])+7.76))
    
    else
      
      data$Mac_m2_day.Mend[i]<-24*data$Mac1_m2[i]/(exp(-1.31*log(data$Temp20m[i])+6.90))
    
  }
  
  data
  
}

# For Horse Mackerel by Pipe et al.  equation. (For stage I  Horse mackerel eggs)


Hom1_m2_day_Pipe <- 
  function(data)
  {
    for (i in 1:length(data$Hom1_m2)){
      
      if (is.na( data$Temp20m[i]))
        
        data$Hom_m2_day.Pipe[i]<-24*data$Hom1_m2[i]/(exp(-1.608*log(data$Temp5m[i])+7.713))
      
      else
        data$Hom_m2_day.Pipe[i]<-24*data$Hom1_m2[i]/(exp(-1.608*log(data$Temp20m[i])+7.713))
    }
    data
  }


##combining all egg production equations###

daily_eggprod <- function(data)
  
{
  data1<-Mac1_m2_day_Lockw(data)
  
  data2<-Mac1_m2_day_Mend(data1)
  
  Hom1_m2_day_Pipe(data2)
  
}

#20. SPATIAL OBJECT ---

#Transforming data into  Spatial object. this spatial object will be a list of 
#spatial objects where each spatial object is a period


ls_spat <- function(data)
  
{
  
  data<- plyr::dlply(data, c("Period"), as.data.frame)
  
  data<-plyr::llply(data,function(x){sp::SpatialPointsDataFrame(x[ ,c("Lon","Lat")],
                                                                x,proj4string=CRS("+proj=longlat   +ellps=WGS84 +datum=WGS84"))})
  
  data
  
}

#30. OVERLAPPING: SPATIAL AGGREGATION ON RECTANGLE ---

#overlapping rectangles  and haul positions. Future

overlap2 <- function(data)
  
{
  
  over1<- plyr::llply(data, function(x) sp::over(RECT_p,x[ ,c("Period","Bdepth","VolFilt","Temp5m",
                                                              "Temp20m","Mac1_m2","Hom1_m2",
                                                              "Mac_m2_day.Lockw","Mac_m2_day.Mend",
                                                              "Hom_m2_day.Pipe")], returnList = F,
                                                 fn=mean,na.rm=T ))
  
  over2<- plyr::llply(data, function(x) sp::over(RECT_p,x[ ,c("Raised_MacStage1","Raised_MacStage2",
                                                              "Raised_MacStage3","Raised_MacStage4",
                                                              "Raised_MacStage5","Raised_HomStage1",
                                                              "Raised_HomStage2","Raised_HomStage3",
                                                              "Raised_HomStage4","nhaul")], 
                                                 returnList = F, fn=sum,na.rm=T))
  
  data <- mapply(cbind,over1,over2,SIMPLIFY=FALSE)
  
  data
  
}


##version to mapping into TAEP


overlap <- function(data)
  
{
  
  over1<- plyr::llply(data, function(x) sp::over(RECT_p,x[ ,c("Period","Bdepth","VolFilt","Temp5m",
                                                              "Temp20m","Mac1_m2","Hom1_m2",
                                                              "Mac_m2_day.Lockw","Mac_m2_day.Mend",
                                                              "Hom_m2_day.Pipe","Date","Lat","Lon")],
                                                 returnList = F, fn=mean,na.rm=T ))
  
  over2<- plyr::llply(data, function(x) sp::over(RECT_p,x[ ,c("Raised_MacStage1","Raised_MacStage2",
                                                              "Raised_MacStage3","Raised_MacStage4",
                                                              "Raised_MacStage5","Raised_HomStage1",
                                                              "Raised_HomStage2","Raised_HomStage3",
                                                              "Raised_HomStage4","nhaul")], 
                                                 returnList = F, fn=sum,na.rm=T))
  
  data <- mapply(cbind,over1,over2,SIMPLIFY=FALSE)
  
  data
  
}

#40. CHARACTERISTICS OF RECTANGLES ---

#adding  rectangle names and characteristics

add_sr <- function(data)
  
{
  
  for (i in 1:length(data))
    
  {
    data[[i]]$RECT<-as.factor(rownames(data[[i]]))
    #name of rectangle
    
  }
  
  data<-plyr::llply(data,function(x)join(x,RECT[ ,c("lon","lat","sea_ratio","RECT","Area")],"RECT" ))
  #joining charact of rectangles
  
  for (i in 1:length(data))
    
  {
    
    rownames(data[[i]])<-data[[i]]$RECT
    # cuadricula la pongo como nombre fila
    
  }
  
  data <-plyr::llply(data,function(x) {sp::SpatialPolygonsDataFrame(RECT_p, x)})
  #convert to en SpatialPolygonsDataFrame
  
  data
}


###Combining overlap and adding rectangle characteristics


overlap.grid <- function(data)
  
{    
  data1<-overlap(data)
  
  add_sr(data1)
  
}  


#40. CHARACTERISTICS OF RECTANGLES ---

#adding  rectangle names and characteristics

add_sr <- function(data)
  
{
  
  for (i in 1:length(data))
    
  {
    data[[i]]$RECT<-as.factor(rownames(data[[i]]))
    #name of rectangle
    
  }
  
  data<-plyr::llply(data,function(x)join(x,RECT[ ,c("lon","lat","sea_ratio","RECT","Area")],"RECT" ))
  #joining charact of rectangles
  
  for (i in 1:length(data))
    
  {
    
    rownames(data[[i]])<-data[[i]]$RECT
    # cuadricula la pongo como nombre fila
    
  }
  
  data <-plyr::llply(data,function(x) {sp::SpatialPolygonsDataFrame(RECT_p, x)})
  #convert to en SpatialPolygonsDataFrame
  
  data
}


#50. INTERPOLATION  ---

# Extrapolation based on arithmetic means is used in unsampled rectangles immediately
# adjacent to at least two sampled rectangles.
# creating 2 new variables:
# Mac_m2_day.Mend: egg density/m2 by Mendiola eq.  
# method: how is calculated the egg density/m2 in rectangle; 
#"S":  mean of Sampled egg densities in rectangle
#"I":  Interpolated egg density


# Previously: creating  contiguity neighbours for rectangles

#RECT_nb_rook <- poly2nb(RECT_p,queen=F)

#RECT_nb <- poly2nb(RECT_p)

# Associate rectangle names with the neighbour list.

#r.id <- attr(RECT_nb, "region.id")

# interpolation function

interpol<-  function(data)
  
{
  
  r.id <- attr(RECT_nb, "region.id")
  
  for (i in 1:length(data))
    
  {
    
    data[[i]]@data$Mac_m2_day.Mend2<-sqrt(-1)
    #Creating new variable
  }
  
  for (i in 1:length(data))
    
  {
    
    data[[i]]@data$sampled<-data[[i]]@data$Mac1_m2
    #Creating another new variable
  }
  
  for (j in 1:length(data))
    
  {
    
    for (i in 1:length(data[[j]]@data$Mac1_m2))
      
    {
      
      if (!is.na( data[[j]]@data$Mac1_m2[i]))
        
        data[[j]]@data$sampled[i]<-1    
      #damos valor de 1 para usar siguiente loop
    }
    
  }
  
  #interpolamos 1 manera
  for (j in 1:length(data))
  {
    
    for (i in 1:length(data[[j]]@data$Mac1_m2))
      
    {
      #1st step in interpolation
      if (is.na( data[[j]]@data$Mac_m2_day.Mend[i])&sum(!is.na(data[[j]]@data[ c(RECT_nb_rook[[match(data[[j]]@data$RECT[i], r.id)]]),"sampled"]))>1)
        
        data[[j]]@data$Mac_m2_day.Mend2[i]<-mean(data[[j]]@data[ c(RECT_nb[[match(data[[j]]@data$RECT[i], r.id)]]),"Mac_m2_day.Mend"],na.rm=T)
      
      else
        
        data[[j]]@data$Mac_m2_day.Mend2[i]<-data[[j]]@data$Mac_m2_day.Mend[i]
    }
  }
  
  # adding  sampled variable
  for (j in 1:length(data)) 
    
  {
    
    for (i in 1:length(data[[j]]@data$Mac1_m2))
      
    {
      
      data[[j]]@data$method[i]<-NA
      
    }
    
  }
  
  #if it was interpolated
  for (j in 1:length(data))
    
  {
    
    for (i in 1:length(data[[j]]@data$Mac_m2_day.Mend))
      
    {
      
      if (is.na( data[[j]]@data$Mac_m2_day.Mend[i])&(!is.na(data[[j]]@data$Mac_m2_day.Mend2[i])))
        
        data[[j]]$method[i]<-"I"## means interpolated rectangle
      
      if (!is.na( data[[j]]@data$Mac_m2_day.Mend[i]))
        
        data[[j]]$method[i]<-"S" ## means sampled rectangle
      
    }
    
    data[[j]]@data$method<-as.factor(as.character(data[[j]]@data$method))
    
  }
  
  
  # Colin way: asignado area ponderada
  
  for (i in 1:length(data))
    
  { 
    
    data[[i]]@data$A<-sqrt(-1)  
    
  }
  
  for (i in 1:length(data))
    
  {
    
    data[[i]]@data$add<-sqrt(-1)
    
  }
  
  for (j in 1:length(data))
    
  {
    
    for (i in 1:length(data[[j]]@data$Mac1_m2))
      
    {
      
      if ( !is.na(data[[j]]@data$method[i])&data[[j]]@data$method[i]=="I")
        
        data[[j]]@data$add[i]<-sum( data[[j]]@data[ c(RECT_nb[[match(data[[j]]@data$RECT[i], r.id)]]),"sampled"],na.rm=T)
      
      data[[j]]@data$A[i]<-(data[[j]]@data$Area[i]/data[[j]]@data$add[i])
      
    }
    
  }
  
  # variable: contributed area of unsampled rectangles
  for (i in 1:length(data))
    
  {
    
    data[[i]]@data$contr_Area<-sqrt(-1)
    
  }
  
  for (j in 1:length(data))
    
  {
    
    for (i in 1:length(data[[j]]@data$Mac1_m2))
      
    {
      
      if ( !is.na(data[[j]]@data$method[i])&data[[j]]@data$method[i]=="S")
        
        
        data[[j]]@data$contr_Area[i]<-sum( data[[j]]@data[ c(RECT_nb[[match(data[[j]]@data$RECT[i], r.id)]]),"A"], na.rm=T)
      
    }
    
  }
  
  data    
}



#60. EGG PRODUCTION IN RECTANGLES ---

#estimating egg production by rectangle    using Mendiola eq.

eggprod <- function(data)
  
{
  
  for (j in 1:length(data))
    
  {
    
    for (i in 1:length(data[[j]]@data$Mac_m2_day.Mend))
      
    {
      
      data[[j]]@data$egg_prod[i]<-sqrt(-1)#nueva variable nombre
      
    }
    
  }
  
  for (j in 1:length(data))
    
  { 
    
    for (i in 1:length(data[[j]]@data$Mac_m2_day.Mend))
      
    {
      
      data[[j]]@data$egg_prod2[i]<-sqrt(-1)#nueva variable nombre
      
    }
    
  }
  
  for (j in 1:length(data)) # Gersom way
    
  {
    
    for (i in 1:length(data[[j]]@data$Mac_m2_day.Mend2))
      
    {
      
      data[[j]]@data$egg_prod[i]<- data[[j]]@data$Mac_m2_day.Mend2[i]*data[[j]]@data$Area[i]*data[[j]]@data$sea_ratio[i]
    }
    
  }
  
  for (j in 1:length(data)) # Colin way
    
  {
    
    for (i in 1:length(data[[j]]@data$Mac_m2_day.Mend2))
      
    { 
      
      data[[j]]@data$egg_prod2[i]<- data[[j]]@data$Mac_m2_day.Mend[i]*(data[[j]]@data$Area[i]*data[[j]]@data$sea_ratio[i]+data[[j]]@data$contr_Area[i])
      
    }
    
  }
  
  data
  
}


#70. VARIANCE ---
# for mackerel
# When there's extrapolation the variance is adjusted appropiately ( including 
#contributed area of rectangles used for extrapolation)
## this function need some work. 


# cv estimation 
coef_var_trad   <- function(data)
  
{
  
  cv_trad <- 
    
  {
    
    data_11<-data
    
    data1<- llply(data, function(x) over(x[ ,c("Period","VolFilt","Temp5m","Temp20m","Mac1_m2","Hom1_m2")],RECT_p, returnList = F ))
    
    data01 <- lapply(1:length(data1), function(i)cbind(as.data.frame(data_11[[i]]),data1[[i]]))
    
    data01 <- llply(data01, function(x){subset(x, Mac1_m2 > 0)})
    
    tab<-llply(data01,function (x)subset(data.frame(table(x$RECT)), Freq > 1))
    
    data1 <- lapply(1:length(data01), function(i)
      
    {
      
      subset(data01[[i]], RECT %in% as.vector(unlist(tab[[i]][1])))
      
    }
    
    )
    
    
    out <- ldply(data1,failwith(NA, function(x)
      
    {
      
      (sqrt( exp(c( summary(lm(log(Mac1_m2) ~ RECT,x))$ sigma^2 )) - 1))
      
    }
    
    ) 
    
    )
    
    names(out)<-c("cv")
    
    out
    
  }
  
  
  # estimation cv. traditional method
  
  cv_trad_all <- 
    
  {
    
    data_12<-data
    
    data2<- llply(data, function(x) over(x[ ,c("Period","VolFilt","Temp5m","Temp20m","Mac1_m2","Hom1_m2")],RECT_p, returnList = F ))
    
    data02 <- lapply(1:length(data2), function(i)cbind(as.data.frame(data_12[[i]]),data2[[i]]))
    
    data02 <- llply(data02, function(x){subset(x, Mac1_m2 > 0)})
    
    tab<-llply(data02,function (x)subset(data.frame(table(x$RECT)), Freq > 1))
    
    data2 <- lapply(1:length(data02), function(i)
      
    {
      
      subset(data02[[i]], RECT %in% as.vector(unlist(tab[[i]][1])))
    }
    )
    
    data2<-ldply(data2,data.frame)
    
    data2$RECT<-paste(data2$RECT,data2$Period,sep="-")
    
    out <-  sqrt( exp(c( summary(lm(log(Mac1_m2) ~ RECT,data2))$ sigma^2 )) - 1)
    
    names(out)<-c("cv_tot")
    
    out
    
  }
  
  
  #####
  #cv_trad_all
  #In case some period without cv assign the cv all periods together
  
  for (i in 1:nrow(cv_trad))
  {
    if (is.na(cv_trad[i,]))
      cv_trad[i,]<-cv_trad_all
    else
      cv_trad[i,]<-cv_trad[i,]
  }
  
  cv_trad
  
  print(cv_trad)
}
####


# Estimation of cv using gam.

cv_gam <- function(data)
  
{
  
  data_1<-data
  
  data<- llply(data, function(x) over(x[ ,c("Period","VolFilt","Temp5m","Temp20m","Mac1_m2","Hom1_m2")],RECT_p, returnList = F ))
  
  data0 <- lapply(1:length(data), function(i)cbind(as.data.frame(data_1[[i]]),data[[i]]))
  
  data <- ldply(data0, function(x){subset(x, Mac1_m2 > 0)})
  
  out<-  sqrt( exp( c(gamm4(log(Mac1_m2) ~ s(Period, Lon, Lat), random = ~ (1|RECT), data = data) $ gam $ sig2)- 1))
  
  names(out)<-c("cv")
  
  out
  print(out)
}





##VARIANCE IN RECTANGLES.  variance in egg densities for each sampling square (rectangle)

var_mack <- function(data)
  
{
  
  for (j in 1:length(data))
    
  {
    
    for (i in 1:length(data[[j]]@data$Mac1_m2)) 
      
    {
      
      data[[j]]@data$variance[i]<-sqrt(-1)
      
      data[[j]]@data$var_gam[i]<-sqrt(-1)
      
    }
    
  }
  
  for (j in 1:length(data))#variance by traditional method
    
  {
    
    for (i in 1:length(data[[j]]@data$Mac_m2_day.Mend2))
      
    {
      if(!is.na(data[[j]]@data$method[i])&data[[j]]@data$method[i]=="S")
        
        data[[j]]@data$variance[i]<- data[[j]]@data$Mac_m2_day.Mend2[i]^2*(sum(data[[j]]@data$Area[i],data[[j]]@data$contr_Area[i], na.rm=T))^2*cv_traditional[j,]^2/data[[j]]@data$nhaul[i]
      
      else
        
        data[[j]]@data$variance[i]<- sqrt(-1)
      
    }
    
  }
  
  for(j in 1:length(data))#variance by gam
    
  {
    
    for (i in 1:length(data[[j]]@data$Mac_m2_day.Mend2))
      
    {
      
      if(!is.na(data[[j]]@data$method[i])&data[[j]]@data$method[i]=="S")
        
        data[[j]]@data$var_gam[i]<- data[[j]]@data$Mac_m2_day.Mend2[i]^2*(sum(data[[j]]@data$Area[i],data[[j]]@data$contr_Area[i], na.rm=T))^2*cv_gam^2/data[[j]]@data$nhaul[i]
      
      else
        
        data[[j]]@data$variance[i]<- sqrt(-1)
      
    }
    
  }
  
  data
  
}


#80. DATES  ---

#Dates of start (dates) and end of each period (datee)



dateperiod <- function(data)
  
{
  
  out<- data.frame(cbind(ddply(data[ ,c("Period","Date")], .(Period), summarise, Per_dates=min(Date)), Per_datee=ddply(data[ ,c("Period","Date")], .(Period), summarise,max(Date))[ ,2]))
  
  out$days<-as.numeric(as.character((out$Per_datee-out$Per_dates )+1))
  
  print(paste("NOTE!!!! period dates in imported file survey_megs",component,year, sep="_"))
  
  print(out, row.names = FALSE)
  
  print(paste("NOTE!!!! start and finish dates assumed in periods from date file. component:",component,"Year:",year,sep=" "))
  
  print("(period 0 means complete spawning season)")
  
  print(    select (tdate, Year, period,Component,dates,datee,days_per), row.names = FALSE)
  
}


#function: period length


survper<-
  function(data)
  {
    
    tdate<-subset(data, Year==year&Component==component)
    
    for (i  in 1:length(tdate$period))
    {
      tdate$days_per<-sqrt(-1)  #v
    }
    
    
    for (i  in 1:length(tdate$period))
    {
      tdate$days_per[i]<-as.numeric((tdate$datee[i]-tdate$dates[i] )+1)
    }
    
    for (i  in 1:length(tdate$period))
    {
      tdate$inter_period<-sqrt(-1)  #v
    }
    
    for (i  in 1:length(tdate$period))
    {
      tdate$inter_period[i]<-paste(tdate[i,2],tdate[i+1,2],sep="$")
    }
    
    for (i  in 1:length(tdate$period))
    {
      tdate$days_interper<-sqrt(-1)  #v
    }
    
    for (i in 2:length(tdate$period)-1)
      
    {
      tdate$days_interper[1]<-as.numeric(tdate[2,4]-tdate[1,4])
      tdate$days_interper[i]<-as.numeric(tdate[i+1,4]-tdate[i,5]-1)
      tdate$days_interper[length(tdate$period)]<-as.numeric(tdate[1,5]-tdate[i+1,5])
    }
    
    
    tdate
    
  }


#90. OUTPUT TABLE ---

tableDEP <- function(data)
  
{
  
  out<-  ldply(data,function(x)data.frame(DEP = sum(x@data$egg_prod, na.rm=T), var_trad = sum(x@data$variance, na.rm=T), var_gam = sum(x@data$var_gam, na.rm=T)))
  
  out<-dplyr::rename(out, period=.id )     
  
  out<-join((select(tdate,Year, period, Component, dates, datee, days_per) %>%   rename(days = days_per)),out)
  
  out<-rbind.fill(out,(select(tdate,Year, inter_period, Component, dates, datee, days_interper) %>%   rename(period = inter_period, days = days_interper)))
  
  out<-arrange(out,period)
  
  if (is.na( out$DEP[2]))
    
    out$DEP[2]<-(out$DEP[2+1]/2)*(out$days[2]/(out$days[2]+out$days[2+1]/2))     # daily production between start date and first sampled period
  
  for (i in 3:length(out$DEP)-1)
    
    if (is.na( out$DEP[i]))
      
    {
      
      if (out$DEP[i-1]<out$DEP[i+1])
        
      {
        
        out$DEP[i]<-out$DEP[i-1]+(((out$DEP[i+1]-out$DEP[i-1])*(out$days[i-1]/2+out$days[i]/2))/(out$days[i-1]/2+out$days[i]+out$days[i+1]/2))
        
      }
      
      else
        
      {
        
        out$DEP[i]<-out$DEP[i+1]+(((out$DEP[i-1]-out$DEP[i+1])*(out$days[i+1]/2+out$days[i]/2))/(out$days[i+1]/2+out$days[i]+out$days[i-1]/2))    # daily production between sampled periods
        
      }
      
    }
  
  
  if (is.na( out$DEP[length(out$DEP)]))
    
  {
    
    out$DEP[length(out$DEP)]<-(out$DEP[length(out$DEP)-1]/2)*(out$days[length(out$DEP)]/(out$days[length(out$DEP)]+out$days[length(out$DEP)-1]/2))   # daily production between last sampled period and # end date
    
  }
  
  out$AEP<-sqrt(-1)
  
  for (i in 2:length(out$DEP))
    
  {
    
    out$AEP[i]<-out$days[i]*out$DEP[i]
    
  }
  
  out$A_var_trad<-sqrt(-1)
  
  for (i in 2:length(out$DEP))
    
  {
    
    out$A_var_trad[i]<-out$days[i]^2*out$var_trad[i]
    
  }
  
  out$A_var_gam<-sqrt(-1)
  
  for (i in 2:length(out$DEP))
    
  {
    
    out$A_var_gam[i]<-out$days[i]^2*out$var_gam[i]
    
  }
  
  print(out)
  write.csv(out, "output/output_table.csv", na="", row.names = F)
}


#91. PLOTS ---

## WESTERN

## Maps of sampling rectange by period

###saving plot

plothaul_save_w<- function (data)
{
  
  png("images/survey.png",
      width = 7, height = 11, units = "in", pointsize = 12,
      bg = "white", res = 800,
      type = "cairo-png")
  
  par(mfrow=c(A,B), mar=c(1,1,1,1) + 0.2)
  for(j in 1:length(data))
    
  {
    map(database = "worldHires",  xlim = c(-23,0.5), ylim = c(43,68.5),fill=T, col="darkgreen", lwd=.5)
    plot(RECT_p, border="grey",  xlim = c(-23,0.5), ylim = c(43,68.5), add=T, lwd=.5)
    degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)
    degAxis(1, at = c(seq(-23,1, by=3)), cex.axis = 0.5, las=2)
    map(database = "worldHires",  xlim = c(-23,0.5), ylim = c(43,68.5),fill=T, col="darkgreen", add=T,  lwd=.5)
    points(data [[j]] @coords,col="red", pch=20,cex=.7)
    title(paste("Year= ", mean(data [[j]]@data$Year, na.rm=T),  "| Period= " ,  mean(data [[j]]@data$Period , na.rm=T)))
    box()
    
  }
  
  dev.off()
  
}


###View plot

plothaul_view_w<- function (data)
  
{
  
  X11(7,11)
  par(mar=c(1,1,1,1) + 0.1, oma=c(1,1,1,1),mfrow=c(A,B))
  
  
  for(j in 1:length(data))
    
  {
    
    map(database = "worldHires",  xlim = c(-23,0.5), ylim = c(43,68.5),fill=T, col="darkgreen", lwd=.5)
    plot(RECT_p, border="grey",  xlim = c(-23,0.5), ylim = c(43,68.5), add=T, lwd=.5)
    degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)
    degAxis(1, at = c(seq(-23,1, by=3)), cex.axis = 0.5, las=2)
    map(database = "worldHires",  xlim = c(-23,0.5), ylim = c(43,68.5),fill=T, col="darkgreen", add=T,  lwd=.5)
    points(data [[j]] @coords,col="red", pch=20,cex=.7)
    title(paste("Year= ", mean(data [[j]]@data$Year, na.rm=T),  "| Period= " ,  mean(data [[j]]@data$Period , na.rm=T)))
    box()
    
  }
  
  
}


## survey grid for mackerel western component

###showing western grid figure

western_grid<- function (data)
  
{
  
  X11(5,7)
  
  par(mar=c(2,2,2,2) + 0.1)
  
  map(database = "worldHires",  xlim = c(-23,0.5), ylim = c(43,68.5),fill=T, type="n")
  plot(data, border="grey",  xlim = c(-23,0.5), ylim = c(43,68.5))
  degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)
  degAxis(1, at = c(seq(-23,1, by=3)), cex.axis = 0.5, las=2)
  map(database = "worldHires",  xlim = c(-23,0.5), ylim = c(43,68.5),fill=T, col="darkgreen",add=T)
  title("mackerel western grid")
  box()
  
}


###saving western grid figure

western.grid_save<- function (data)
{
  
  png("western_survey_grid.png",
      width = 5, height = 7, units = "in", pointsize = 10,
      bg = "white", res = 800,
      type = "cairo-png")
  
  par(mar=c(2,2,2,2) + 0.1)
  map(database = "worldHires",  xlim = c(-23,0.5), ylim = c(43,68.5),fill=T, type="n")
  plot(data, border="grey",  xlim = c(-23,0.5), ylim = c(43,68.5))
  degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)
  degAxis(1, at = c(seq(-23,1, by=3)), cex.axis = 0.5, las=2)
  map(database = "worldHires",  xlim = c(-23,0.5), ylim = c(43,68.5),fill=T, col="darkgreen",add=T)
  title("Mackerel western area grid")
  box()
  dev.off()
}


#####


plot.neighbour_w<-      function (data)
{
  X11(11,7)
  par(mar=c(1,1,1,1) + 0.1, oma=c(1,1,1,1), mfrow=c(1,2))
  
  
  
  plot(RECT_p, border="grey",  xlim = c(-23,0.5), ylim = c(43,68.5),  lwd=.5)
  degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)
  degAxis(1, at = c(seq(-23,1, by=3)), cex.axis = 0.5, las=2)
  coords <- coordinates(RECT_p)
  plot(RECT_nb_rook, coordinates(RECT_p), pch=19, cex=0.6,lwd=0.3, col="blue",add=TRUE)
  title("Rook neighborhood")
  box()
  
  plot(RECT_p, border="grey",  xlim = c(-23,0.5), ylim = c(43,68.5),  lwd=.5)
  degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)
  degAxis(1, at = c(seq(-23,1, by=3)), cex.axis = 0.5, las=2)
  coords <- coordinates(RECT_p)
  plot(RECT_nb, coordinates(RECT_p), pch=19, cex=0.6,lwd=0.3, col="blue",add=TRUE)
  title("Queen neighborhood")
  box()
  
}

#SOUTHERN ---

## Maps of sampling rectange by period

###saving plot

plothaul_save_s<- function (data)
{
  
  png("images/survey_southern.png",
      width = 7, height = 11, units = "in", pointsize = 12,
      bg = "white", res = 800,
      type = "cairo-png")
  
  par(mfrow=c(A,B), mar=c(1,1,1,1) + 0.2)
  for(j in 1:length(data))
    
  {
    map(database = "worldHires",  xlim = c(-11,-1), ylim = c(36,45.5),fill=T, col="darkgreen", lwd=.5)
    plot(RECT_p, border="grey", xlim = c(-11,-1), ylim = c(36,45.5), add=T, lwd=.5)
    degAxis(2,las=1,at=seq(36,45,by=1),cex.axis = 0.5,las=2)
    degAxis(1,at=seq(-11,-1,by=1),cex.axis=.5, cex.axis = 0.5, las=2)
    map(database = "worldHires",  xlim = c(-11,-1), ylim = c(36,45.5),fill=T, col="darkgreen", add=T,  lwd=.5)
    points(data [[j]] @coords,col="red", pch=20,cex=.7)
    title(paste("Year= ", mean(data [[j]]@data$Year, na.rm=T),  "| Period= " ,  mean(data [[j]]@data$Period , na.rm=T)))
    box()
    
  }
  
  dev.off()
  
}


###View plot

plothaul_view_s<- function (data)
  
{
  
  X11(7,11)
  par(mar=c(1,1,1,1) + 0.1, oma=c(1,1,1,1),mfrow=c(A,B))
  
  
  for(j in 1:length(data))
    
  {
    
    map(database = "worldHires",  xlim = c(-11,-1), ylim = c(36,45.5),fill=T, col="darkgreen", lwd=.5)
    plot(RECT_p, border="grey", xlim = c(-11,-1), ylim = c(36,45.5), add=T, lwd=.5)
    degAxis(2,las=1,at=seq(36,45,by=1),cex.axis = 0.5,las=2)
    degAxis(1,at=seq(-11,-1,by=1),cex.axis=.5, cex.axis = 0.5, las=2)
    map(database = "worldHires",  xlim = c(-11,-1), ylim = c(36,45.5),fill=T, col="darkgreen", add=T,  lwd=.5)
    points(data [[j]] @coords,col="red", pch=20,cex=.7)
    title(paste("Year= ", mean(data [[j]]@data$Year, na.rm=T),  "| Period= " ,  mean(data [[j]]@data$Period , na.rm=T)))
    box()
    
  }
  
  
}


## survey grid for mackerel southern component

###showing southern grid figure

southern_grid<- function (data)
  
{
  
  X11(5,7)
  
  par(mar=c(2,2,2,2) + 0.1)
  
  map(database = "worldHires",  xlim = c(-11,-1), ylim = c(36,45.5),fill=T, type="n")
  plot(data, border="grey",  xlim = c(-11,-1), ylim = c(36,45.5))
  degAxis(2,las=1,at=seq(36,45,by=1),cex.axis = 0.5,las=2)
  degAxis(1,at=seq(-11,-1,by=1),cex.axis=.5, cex.axis = 0.5, las=2)
  map(database = "worldHires",   xlim = c(-11,-1), ylim = c(36,45.5),fill=T, col="darkgreen",add=T)
  title("mackerel southern grid")
  box()
  
}



#####
plot.neighbour_s<-      function (data)
{
  X11(11,7)
  par(mar=c(1,1,1,1) + 0.1, oma=c(1,1,1,1), mfrow=c(1,2))
  
  
  
  plot(RECT_p, border="grey",  xlim = c(-11,-1), ylim = c(36,45.5),  lwd=.5)
  degAxis(2,las=1,at=seq(36,45,by=1),cex.axis = 0.5,las=2)
  degAxis(1,at=seq(-11,-1,by=1),cex.axis=.5, cex.axis = 0.5, las=2)
  coords <- coordinates(RECT_p)
  plot(RECT_nb_rook, coordinates(RECT_p), pch=19, cex=0.6,lwd=0.3, col="blue",add=TRUE)
  title("Rook neighborhood")
  box()
  
  plot(RECT_p, border="grey",  xlim = c(-11,-1), ylim = c(36,45.5),  lwd=.5)
  degAxis(2,las=1,at=seq(36,45,by=1),cex.axis = 0.5,las=2)
  degAxis(1,at=seq(-11,-1,by=1),cex.axis=.5, cex.axis = 0.5, las=2)
  coords <- coordinates(RECT_p)
  plot(RECT_nb, coordinates(RECT_p), pch=19, cex=0.6,lwd=0.3, col="blue",add=TRUE)
  title("Queen neighborhood")
  box()
  
}





