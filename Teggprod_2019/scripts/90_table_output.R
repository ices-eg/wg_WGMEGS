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
