   ####################################################################
## SCRIPT : ANNUAL EGG PRODUCTION; TRADITIONAL METHOD; version MARCH 2014
##  FUNCTIONS
## Gersom Costas (Instituto Espa?ol de Oceanografia)
## gersom.costas@vi.ieo.es
##
## SECTIONS
## 1. CREATING GRID
## 2. IMPORT DATA AND CREATE VARIABLES
## 3. SPATIAL AGGREGATION ON RECTANGLE
## 4. INTERPOLATION IN UNSAMPLED RECTANGLES
## 5. EGG PRODUCTION IN RECTANGLES
## 6. WORLD OF VARIANCE 
## 7. EGG PRODUCTION IN PERIODS
## 8. VARIANCE IN PERIODS
## 9. ANNUAL EGG PRODUCTION
## 10. PLOTS

####################################################################
#LIBRARIES

 library(sp)
 library(plyr)
 library(maptools)
 library(spdep)
 library(gamm4)
 library(lubridate)
 library(rgdal)
#############################################################################################################################
#############################################################################################################################
#  2. IMPORTING DATA AND CREATING VARIABLES
 #############################################################################################################################
 ###########################################################################################################################
# Daily egg production by rectangle (eggs per m2/ day) was calculated using the data on stage I eggs per m2  ( stage I Egg /m2)
# The temperature used to calculate the duration of stage I eggs for both species (Mackerel and Horse Mackerel) was taken at 20 m depth  it is not available the use temperature at 5 m. 

# For Mackerel by Lockwood eq.(For stage I mackerel eggs)

Mac1_m2_day_Lockw <- 
  function(data)
  {
    for (i in 1:length(data$Mac1_m2)){
      
      if (is.na( data$Temp20m[i]))
        
        
        data$Mac_m2_day.Lockw[i]<-24*data$Mac1_m2[i]/(exp(-1.61*log(data$Temp5m[i])+7.76))
      
      else
        data$Mac_m2_day.Lockw[i]<-24*data$Mac1_m2[i]/(exp(-1.61*log(data$Temp20m[i])+7.76))  
      
    }
    data
  }


# For Mackerel by Mendiola eq.(For stage I mackerel eggs)

Mac1_m2_day_Mend <- 
  function(data)
  {
    for (i in 1:length(data$Mac1_m2)){
      
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


#transforming data into  Spatial object. this spatial object will be a list of spatial objects where each spatial object is a period




ls_spat <- 
  function(data)
  {
    data<- dlply(data, c("Period"), as.data.frame)
    data<-llply(data,function(x){SpatialPointsDataFrame(x[ ,c("Lon","Lat")],x,proj4string=CRS("+proj=longlat   +ellps=WGS84 +datum=WGS84"))})
    data
  }




#############################################################################################################################
#############################################################################################################################
#  3. SPATIAL AGGREGATION ON RECTANGLE
#############################################################################################################################
###########################################################################################################################

# 
###overlapping grid rectangles  and haul positions

overlap <- 
  
  function(data)
  {
    over1<- llply(data, function(x) over(RECT_p,x[ ,c("Period","Bdepth","VolFilt","Temp5m","Temp20m","Mac1_m2","Hom1_m2","Mac_m2_day.Lockw","Mac_m2_day.Mend","Hom_m2_day.Pipe","Date","Lat","Lon")], returnList = F, fn=mean,na.rm=T ))
    
    over2<- llply(data, function(x) over(RECT_p,x[ ,c("Raised_MacStage1","Raised_MacStage2","Raised_MacStage3","Raised_MacStage4","Raised_MacStage5","Raised_HomStage1","Raised_HomStage2","Raised_HomStage3","Raised_HomStage4","nhaul")], returnList = F, fn=sum,na.rm=T))
    
    data<-mapply(cbind,over1,over2,SIMPLIFY=FALSE)
    data
    
  }

#adding  rectangle names and characteristics

add_sr <- 
  function(data)
  {
    for (i in 1:length(data)){
      data[[i]]$RECT<-as.factor(rownames(data[[i]]))# 
    }
    data<-llply(data,function(x)join(x,RECT[ ,c("lon","lat","sea_ratio","RECT","Area")],"RECT" ))   #adding characteristics
    
    for (i in 1:length(data)){
      rownames(data[[i]])<-data[[i]]$RECT# row name is rectangle name 
    }
    
    data <-llply(data,function(x) {SpatialPolygonsDataFrame(RECT_p, x)})#convert to SpatialPolygonsDataFrame
    
    data
  }


####INTERPOLATION!!!!!!!. arithmetic mean.Extrapolation based on arithmetic means is used in unsampled rectangles immediately adjacent to at least two sampled rectangles.
# creating 2 new variables:Hom_m2_day.Pipe: egg density/m2 by Pipe eq. And method: how is calculated the egg density/m2 in rectangles; "S"  mean of   haul egg densities or"I":interpolated egg density

interpol<-
  function(data)
  {
    r.id <- attr(RECT_nb, "region.id")

    for (i in 1:length(data))       #
    {
      data[[i]]@data$Hom_m2_day.Pipe2<-sqrt(-1)  #daily egg prod, by rectangle using arithmetic mean extrapolation
    }

    for (i in 1:length(data))       #
    {
      data[[i]]@data$sampled<-data[[i]]@data$Hom1_m2#
    }

    for (j in 1:length(data))

    {
      for (i in 1:length(data[[j]]@data$Hom1_m2))
      {

        if (!is.na( data[[j]]@data$Hom1_m2[i]))

          data[[j]]@data$sampled[i]<-1    #dummy variable. 1 means sampled rectangle

      }
    }

    #interpolating by arithmetic mean

    for (j in 1:length(data))
    {
      for (i in 1:length(data[[j]]@data$Hom1_m2)){

        if (is.na( data[[j]]@data$Hom_m2_day.Pipe[i])&sum(!is.na(data[[j]]@data[ c(RECT_nb[[match(data[[j]]@data$RECT[i], r.id)]]),"sampled"]))>1)

          data[[j]]@data$Hom_m2_day.Pipe2[i]<-mean(data[[j]]@data[ c(RECT_nb[[match(data[[j]]@data$RECT[i], r.id)]]),"Hom_m2_day.Pipe"],na.rm=T)

        else
          data[[j]]@data$Hom_m2_day.Pipe2[i]<-data[[j]]@data$Hom_m2_day.Pipe[i]
      }
    }

    #m?s variables sampled

    for (j in 1:length(data))
    {
      for (i in 1:length(data[[j]]@data$Hom1_m2))
      {
        data[[j]]@data$method[i]<-NA# kind of rectangle

      }
    }

    for (j in 1:length(data))                            #
    {
      for (i in 1:length(data[[j]]@data$Hom_m2_day.Pipe))
      {

        if (is.na( data[[j]]@data$Hom_m2_day.Pipe[i])&(!is.na(data[[j]]@data$Hom_m2_day.Pipe2[i])))
          data[[j]]$method[i]<-"I"                  #interpolated

        if (!is.na( data[[j]]@data$Hom_m2_day.Pipe[i]))
          data[[j]]$method[i]<-"S"                   #sampled
      }
      data[[j]]@data$method<-as.factor(as.character(data[[j]]@data$method))
    }


    #raising sampled rect.(plus  additional  area due to the filling in of unsampled adjacent cells)

    for (i in 1:length(data))       #
    {
      data[[i]]@data$A<-sqrt(-1)  #

    }
    for (i in 1:length(data))       
    {
      #
      data[[i]]@data$add<-sqrt(-1)      #number sampled rectangles adjacent unsampled rectangle
    }


    for (j in 1:length(data))

    {
      for (i in 1:length(data[[j]]@data$Hom1_m2))
      {

        if ( !is.na(data[[j]]@data$method[i])&data[[j]]@data$method[i]=="I")

          data[[j]]@data$add[i]<-sum( data[[j]]@data[ c(RECT_nb[[match(data[[j]]@data$RECT[i], r.id)]]),"sampled"],na.rm=T)
        data[[j]]@data$A[i]<-(data[[j]]@data$Area[i]/data[[j]]@data$add[i])



      }
    }



    for (i in 1:length(data))       #
    {
      data[[i]]@data$contr_Area<-sqrt(-1)  #additional (contributory) area due to the filling in of unsampled adjacent rect
    }



    for (j in 1:length(data))
    {
      for (i in 1:length(data[[j]]@data$Hom1_m2))
      {
        if ( !is.na(data[[j]]@data$method[i])&data[[j]]@data$method[i]=="S")


          data[[j]]@data$contr_Area[i]<-sum( data[[j]]@data[ c(RECT_nb[[match(data[[j]]@data$RECT[i], r.id)]]),"A"], na.rm=T)


      }
    }



    data    }




#############################################################################################################################
#  5. EGG PRODUCTION IN RECTANGLES
#############################################################################################################################
###########################################################################################################################


 #estimating egg production by rectangle

eggprod<-
  function(data)
  {

    for (j in 1:length(data))
    {
      for (i in 1:length(data[[j]]@data$Hom_m2_day.Pipe))
      {
        data[[j]]@data$egg_prod[i]<-sqrt(-1)#new variable name

      }

    }
    for (j in 1:length(data))
    {
      for (i in 1:length(data[[j]]@data$Hom_m2_day.Pipe))
      {

        data[[j]]@data$egg_prod2[i]<-sqrt(-1)#new variable name
      }

    }

    for (j in 1:length(data)) #by arithmetic mean

    {
      for (i in 1:length(data[[j]]@data$Hom_m2_day.Pipe2))
      {
        data[[j]]@data$egg_prod[i]<- data[[j]]@data$Hom_m2_day.Pipe2[i]*data[[j]]@data$Area[i]*data[[j]]@data$sea_ratio[i]
      }

    }

    for (j in 1:length(data)) # by adding additional area due to the fill in

    {
      for (i in 1:length(data[[j]]@data$Hom_m2_day.Pipe2))
      {
        data[[j]]@data$egg_prod2[i]<- data[[j]]@data$Hom_m2_day.Pipe[i]*(data[[j]]@data$Area[i]*data[[j]]@data$sea_ratio[i]+data[[j]]@data$contr_Area[i])
      }

    }


    data
  }

#############################################################################################################################
#############################################################################################################################
#  6. WORLD OF VARIANCE 
#############################################################################################################################
###########################################################################################################################

# when there's extrapolation the variance is adjusted appropiately ( including contributed area of rectangles used for extrapolation)

# estimation traditional cv  by period

cv_lmok <- 
  function(data)
  {
    data_1<-data
    data<- llply(data, function(x) over(x[ ,c("Period","VolFilt","Temp5m","Temp20m","Mac1_m2","Hom1_m2")],RECT_p, returnList = F )) 
    
    data0 <- lapply(1:length(data), function(i)cbind(as.data.frame(data_1[[i]]),data[[i]]))
    data0 <- llply(data0, function(x){subset(x, Hom1_m2 > 0)})
    tab<-llply(data0,function (x)subset(data.frame(table(x$RECT)), Freq > 1))
    
    data <- lapply(1:length(data0), function(i)
    {
      
      subset(data0[[i]], RECT %in% as.vector(unlist(tab[[i]][1])))
    }
    )
    
    
    
    out <- ldply(data,failwith(NA, function(x) 
    {
      ( sqrt( exp(c( summary(lm(log(Hom1_m2) ~ RECT,x))$ sigma^2 )) - 1))
      
    }
    ) )
    
    
    names(out)<-c("cv")
    out
  }  


cv_lmok_bad <- 
  function(data)
  {
    data_1<-data
    data<- llply(data, function(x) over(x[ ,c("Period","VolFilt","Temp5m","Temp20m","Mac1_m2","Hom1_m2")],RECT_p, returnList = F )) 
    
    data<-mapply(cbind,data_1,data,SIMPLIFY=FALSE)
    
    data0 <- llply(data, function(x){subset(x, Hom1_m2 == 0)})
    
    tab <- llply(data0,function (x) with(x, table(RECT)))
    
    data <- llply(data, function(x){subset(x, Hom1_m2 > 0)})
    nam <- llply(tab,function (x)names(x[x==0]))
    data <- lapply(1:length(data_1), function(i)
    {
      subset(data[[i]], RECT %in% nam[[i]])
    }
    )
    
    
    tab <- llply(data,function (x) with(x, table(RECT)))
    nam <- llply(tab,function (x)names(x[x > 1]))
    
    
    data <- lapply(1:length(data_1), function(i)
    {
      subset(data[[i]], RECT %in% nam[[i]])
    }
    )
    
    out <- ldply(data,failwith(NA, function(x) 
    {
      ( sqrt( exp(c( summary(lm(log(Hom1_m2) ~ RECT,x))$ sigma^2 )) - 1))
      
    }
    ) )
    
    
    names(out)<-c("cv")
    out
  }  


#
#  estimation trad cv   all periods together
cv_lm_allok <- 
  function(data)
  {
    data_1<-data
    data<- llply(data, function(x) over(x[ ,c("Period","VolFilt","Temp5m","Temp20m","Mac1_m2","Hom1_m2")],RECT_p, returnList = F )) 
    
    data0 <- lapply(1:length(data), function(i)cbind(as.data.frame(data_1[[i]]),data[[i]]))
    data0 <- llply(data0, function(x){subset(x, Hom1_m2 > 0)})
    tab<-llply(data0,function (x)subset(data.frame(table(x$RECT)), Freq > 1))
    
    data <- lapply(1:length(data0), function(i)
    {
      
      subset(data0[[i]], RECT %in% as.vector(unlist(tab[[i]][1])))
    }
    )
    
    
    data<-ldply(data,data.frame)
    data$RECT<-paste(data$RECT,data$Period,sep="-")
    
    out <-  sqrt( exp(c( summary(lm(log(Hom1_m2) ~ RECT,data))$ sigma^2 )) - 1)
    
    names(out)<-c("cv_tot")
    out
  }



  cv_lm_allok_bad <- 
    function(data)
    {
      data_1<-data
      data<- llply(data, function(x) over(x[ ,c("Period","VolFilt","Temp5m","Temp20m","Mac1_m2","Hom1_m2")],RECT_p, returnList = F )) 
      
      data<-mapply(cbind,data_1,data,SIMPLIFY=FALSE)
      
      data0 <- llply(data, function(x){subset(x, Hom1_m2 == 0)})
      
      tab <- llply(data0,function (x) with(x, table(RECT)))
      
      data <- llply(data, function(x){subset(x, Hom1_m2 > 0)})
      nam <- llply(tab,function (x)names(x[x==0]))
      data <- lapply(1:length(data_1), function(i)
      {
        subset(data[[i]], RECT %in% nam[[i]])
      }
      )
      
      
      tab <- llply(data,function (x) with(x, table(RECT)))
      nam <- llply(tab,function (x)names(x[x > 1]))
      
      
      data <- lapply(1:length(data_1), function(i)
      {
        subset(data[[i]], RECT %in% nam[[i]])
      }
      )
      
      data<-ldply(data,data.frame)
      data$RECT<-paste(data$RECT,data$Period,sep="-")
    
    out <-  sqrt( exp(c( summary(lm(log(Hom1_m2) ~ RECT,data))$ sigma^2 )) - 1)
    
    names(out)<-c("cv_tot")
    out
  }


#these need some work



#In case some period without cv assign the cv all periods together


#for (i in 1:nrow(cv_trad))
#{
  
 # if (is.na(cv_trad[i,]))
  #  cv_trad[i,]<-cv_trad_all
  #else
  #  cv_trad[i,]<-cv_trad[i,]
#}

#cv_trad

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # estimation of cv by gam. Colin method
  cv_gam <- 
    function(data)
    {
      data_1<-data
      data<- llply(data, function(x) over(x[ ,c("Period","VolFilt","Temp5m","Temp20m","Mac1_m2","Hom1_m2")],RECT_p, returnList = F )) 
      data0 <- lapply(1:length(data), function(i)cbind(as.data.frame(data_1[[i]]),data[[i]]))
      data <- ldply(data0, function(x){subset(x, Hom1_m2 > 0)})
      
      out<-  sqrt( exp( c(gamm4(log(Hom1_m2) ~ s(Period, Lon, Lat), random = ~ (1|RECT), data = data) $ gam $ sig2)- 1))
      
      
      names(out)<-c("cv")
      out
    }
  
  
cv_gam_bad <- 
  function(data)
  {     
    data_1<-data
    
    data<- llply(data, function(x) over(x[ ,c("Period","VolFilt","Temp5m","Temp20m","Mac1_m2","Hom1_m2")],RECT_p, returnList = F )) 
    
    data<-mapply(cbind,data_1,data,SIMPLIFY=FALSE)
    
    data <- ldply(data, function(x){subset(x, Hom1_m2 > 0)})
    
    out<-  sqrt( exp( c(gamm4(log(Hom1_m2) ~ s(Period, Lon, Lat), random = ~ (1|RECT), data = data) $ gam $ sig2)- 1))
    
    
    names(out)<-c("cv")
    out
  }


################################################################################ 


##VARIANCE IN RECTANGLES. the variance in egg densities for each sampling square (rectangle)

 var<-
   function(data)
   {

     for (j in 1:length(data))
        {
          for (i in 1:length(data[[j]]@data$Hom1_m2))
            {
              data[[j]]@data$variance[i]<-sqrt(-1)#new variable name
              data[[j]]@data$var_gam[i]<-sqrt(-1)#new variable name
            }

        }


     for (j in 1:length(data))#variance by traditional method
      {
        for (i in 1:length(data[[j]]@data$Hom_m2_day.Pipe2))
            {
                if   (!is.na(data[[j]]@data$method[i])&data[[j]]@data$method[i]=="S")
                data[[j]]@data$variance[i]<- data[[j]]@data$Hom_m2_day.Pipe2[i]^2*(sum(data[[j]]@data$Area[i],data[[j]]@data$contr_Area[i], na.rm=T))^2*cv_trad[j,]^2/data[[j]]@data$nhaul[i]

                else
                data[[j]]@data$variance[i]<- sqrt(-1)

            }

      }

     for (j in 1:length(data))#variance by gam
        {
          for (i in 1:length(data[[j]]@data$Hom_m2_day.Pipe2))
            {
              if   (!is.na(data[[j]]@data$method[i])&data[[j]]@data$method[i]=="S")
              data[[j]]@data$var_gam[i]<- data[[j]]@data$Hom_m2_day.Pipe2[i]^2*(sum(data[[j]]@data$Area[i],data[[j]]@data$contr_Area[i], na.rm=T))^2*cvgam^2/data[[j]]@data$nhaul[i]

              else
              data[[j]]@data$variance[i]<- sqrt(-1)

            }

        }
          data
   }
 
 
 ################################################################################ 
 #  7. Daily EGG PRODUCTION by PERIOD
#Dates of start (dates) and end of each period (datee)

dateperiod <-
  function(data)
  {
    out<- data.frame(cbind(ddply(data[ ,c("Period","Date")], .(Period), summarise, Per_dates=min(Date)), Per_datee=ddply(data[ ,c("Period","Date")], .(Period), summarise,max(Date))[ ,2]))
    out$days<-as.numeric(as.character((out$Per_datee-out$Per_dates )+1))
    out
  }

 
survper<-
function(data)
{
  
  tdate<-subset(data, year==y_date&component==spw_comp)
  
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
 

 print(tdate)

}




################################################################################
  
  


tableDEP<-
  function(data)
  {
    out<-  ldply(data,function(x)data.frame(DEP = sum(x@data$egg_prod, na.rm=T), var_trad = sum(x@data$variance, na.rm=T), var_gam = sum(x@data$var_gam, na.rm=T)))
    out<-dplyr::rename(out, period=.id )     #;pp$period<-as.character(pp$period)
    out<-join(tdate_per,out)
    out<-rbind.fill(out,tdate_iper)
    out<-arrange(out,period)
    
    if (is.na( out$DEP[2]))
      
      out$DEP[2]<-(out$DEP[2+1]/2)*(out$days[2]/(out$days[2]+out$days[2+1]/2))     # daily production between start date #and first sampled period
    for (i in 3:length(out$DEP)-1)
      
      
      if (is.na( out$DEP[i]))
      {
        if ( out$DEP[i-1]<out$DEP[i+1])
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
    
  }