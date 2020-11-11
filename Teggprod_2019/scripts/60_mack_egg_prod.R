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

