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
     
         data$Mac_m2_day.Mend[i]<-24*data$Mac1_m2[i]/(exp(-1.31*log(data$Temp5m[i])+6.90))
         
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