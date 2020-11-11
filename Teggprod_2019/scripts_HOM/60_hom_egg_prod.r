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
