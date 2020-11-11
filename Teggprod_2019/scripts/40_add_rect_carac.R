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
