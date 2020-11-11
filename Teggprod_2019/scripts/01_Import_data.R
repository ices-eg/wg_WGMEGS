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
 # tdate <-bind_rows((select(tdate,Year, inter_period, Component, dates, datee, days_interper) %>%   rename(period = inter_period, days = days_interper)), (select(tdate,Year, period, Component, dates, datee, days_per) %>%   rename(days = days_per))) %>% arrange(period)

