#' 30.2 OVERLAPPING: SPATIAL AGGREGATION ON RECTANGLE
#' overlapping rectangles  and haul positions. Future developments
#' @param data. frame list
#' @return spatialobject list
#' @export


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



#' 30.1 OVERLAPPING: SPATIAL AGGREGATION ON RECTANGLE
#' overlapping rectangles  and haul positions. Current use
#' @param data. frame list
#' @return spatialobject list
#' @export


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
#' 40. CHARACTERISTICS OF RECTANGLES
#' adding  rectangle names and characteristics
#' @param data. frame list
#' @return spatialobject list
#' @export


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

#' Combining overlap and adding rectangle characteristics: overlap + add_sr
#' @param
#' @return
#' @export



overlap.grid <- function(data)

  {
     data1<-overlap(data)

     add_sr(data1)

   }


