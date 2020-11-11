#91. PLOTS ---

## WESTERN  HORSE MACKEREL

## Maps of sampling rectange by period

###saving plot

plothaul_save_hom<- function (data)
{
  
  png("images/survey.png",
      width = 7, height = 11, units = "in", pointsize = 12,
      bg = "white", res = 700,
      type = "cairo-png")
  
  par(mfrow=c(A,B), mar=c(1,1,1,1) + 0.2)
  for(j in 1:length(data))
    
  {
    maps::map(database = "worldHires",  xlim = c(-18,.5), ylim = c(43,68.5),fill=T, col="darkgreen", lwd=.5)
    plot(RECT_p, border="grey",  xlim = c(-18,.5), ylim = c(43,68.5), add=T, lwd=.5)
    degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)
    degAxis(1, at = c(seq(-18,.5, by=3)), cex.axis = 0.5, las=2)
    maps::map(database = "worldHires",  xlim = c(-18,.5), ylim = c(43,68.5),fill=T, col="darkgreen", add=T,  lwd=.5)
    points(data [[j]] @coords,col="red", pch=20,cex=.7)
    title(paste("Year= ", mean(data [[j]]@data$Year, na.rm=T),  "| Period= " ,  mean(data [[j]]@data$Period , na.rm=T)))
    box()
    
  }
  
  dev.off()
  
}


###View plot

plothaul_view_hom<- function (data)
  
{
  
  X11(width=5,height=7)
  par(mar=c(1,1,1,1) + 0.1, oma=c(1,1,1,1),mfrow=c(A,B))
  
  
  for(j in 1:length(data))
    
  {
    
    maps::map(database = "worldHires",  xlim = c(-18,.5), ylim = c(43,68.5),fill=T, col="darkgreen", lwd=.5)
    plot(RECT_p, border="grey",  xlim = c(-18,.5), ylim = c(43,68.5), add=T, lwd=.5)
    degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)
    degAxis(1, at = c(seq(-18,.5, by=3)), cex.axis = 0.5, las=2)
    maps::map(database = "worldHires",  xlim = c(-18,.5), ylim = c(43,68.5),fill=T, col="darkgreen", add=T,  lwd=.5)
    points(data [[j]] @coords,col="red", pch=20,cex=.7)
    title(paste("Year= ", mean(data [[j]]@data$Year, na.rm=T),  "| Period= " ,  mean(data [[j]]@data$Period , na.rm=T)))
    box()
    
  }
  
  
}


## survey grid for western horse mackerel  stock

###showing western grid figure

hom_grid<- function (data)
  
{
  
  X11(width=5,height=7)
  
  par(mar=c(2,2,2,2) + 0.1)
  
  maps::map(database = "worldHires",  xlim = c(-18,.5), ylim = c(43,68.5),fill=T, type="n")
  plot(data, border="grey",  xlim = c(-18,.5), ylim = c(43,68.5))
  degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)
  degAxis(1, at = c(seq(-18,.5, by=3)), cex.axis = 0.5, las=2)
  maps::map(database = "worldHires",  xlim = c(-18,.5), ylim = c(43,68.5),fill=T, col="darkgreen",add=T)
  title("Western Horse Mackerel grid")
  box()
  
}


###saving western grid figure

hom.grid_save<- function (data)
{
  
  png("western_hom_survey_grid.png",
      width = 5, height = 7, units = "in", pointsize = 10,
      bg = "white", res = 700,
      type = "cairo-png")
  
  par(mar=c(2,2,2,2) + 0.1)
  maps::map(database = "worldHires",  xlim = c(-18,.5), ylim = c(43,68.5),fill=T, type="n")
  plot(data, border="grey",  xlim = c(-18,.5), ylim = c(43,68.5))
  degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)
  degAxis(1, at = c(seq(-18,.5, by=3)), cex.axis = 0.5, las=2)
  maps::map(database = "worldHires",  xlim = c(-18,.5), ylim = c(43,68.5),fill=T, col="darkgreen",add=T)
  title("Western Horse Mackerel  area grid")
  box()
  dev.off()
}


#####


plot.neighbour_w<-      function (data)
{
  X11(width=5,height=7)
  par(mar=c(1,1,1,1) + 0.1, oma=c(1,1,1,1), mfrow=c(1,2))
  
  
  
  plot(RECT_p, border="grey",  xlim = c(-18,.5), ylim = c(43,68.5),  lwd=.5)
  degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)
  degAxis(1, at = c(seq(-18,.5, by=3)), cex.axis = 0.5, las=2)
  coords <- coordinates(RECT_p)
  plot(RECT_nb_rook, coordinates(RECT_p), pch=19, cex=0.6,lwd=0.3, col="blue",add=TRUE)
  title("Rook neighborhood")
  box()
  
  plot(RECT_p, border="grey",  xlim = c(-18,.5), ylim = c(43,68.5),  lwd=.5)
  degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)
  degAxis(1, at = c(seq(-18,.5, by=3)), cex.axis = 0.5, las=2)
  coords <- coordinates(RECT_p)
  plot(RECT_nb, coordinates(RECT_p), pch=19, cex=0.6,lwd=0.3, col="blue",add=TRUE)
  title("Queen neighborhood")
  box()
  
}


  