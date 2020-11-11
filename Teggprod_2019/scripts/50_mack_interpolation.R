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



