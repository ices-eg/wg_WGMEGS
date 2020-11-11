#' 20. SPATIAL OBJECT ---
#' Transforming data into  Spatial object. this spatial object will be a list of spatial objects where each spatial object is a period
#' @param data.frame
#' @return spatialobject list
#' @export


ls_spat <- function(data)

{

  data<- plyr::dlply(data, c("Period"), as.data.frame)

  data<-plyr::llply(data,function(x){sp::SpatialPointsDataFrame(x[ ,c("Lon","Lat")],
                                         x,proj4string=CRS("+proj=longlat   +ellps=WGS84 +datum=WGS84"))})

  data

}
