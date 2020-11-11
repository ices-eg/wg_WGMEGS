#' 70. VARIANCE for horse mackerel.  When there's extrapolation the variance is adjusted appropiately ( including contributed area of rectangles used for extrapolation)
#'  this function need some work.


#'  cv estimation
#'
coef_var_trad   <- function(data)

{

cv_trad <-

{

  data_11<-data

  data1<- llply(data, function(x) over(x[ ,c("Period","VolFilt","Temp5m","Temp20m","Mac1_m2","Hom1_m2")],RECT_p, returnList = F ))

  data01 <- lapply(1:length(data1), function(i)cbind(as.data.frame(data_11[[i]]),data1[[i]]))

  data01 <- llply(data01, function(x){subset(x, Hom1_m2 > 0)})

  tab<-llply(data01,function (x)subset(data.frame(table(x$RECT)), Freq > 1))

  data1 <- lapply(1:length(data01), function(i)

                    {

                      subset(data01[[i]], RECT %in% as.vector(unlist(tab[[i]][1])))

                    }

                 )


  out <- ldply(data1,failwith(NA, function(x)

                              {

                                (sqrt( exp(c( summary(lm(log(Hom1_m2) ~ RECT,x))$ sigma^2 )) - 1))

                              }

                            )

               )

 names(out)<-c("cv")

 out

}


#'  estimation cv. traditional method

cv_trad_all <-

{

   data_12<-data

   data2<- llply(data, function(x) over(x[ ,c("Period","VolFilt","Temp5m","Temp20m","Mac1_m2","Hom1_m2")],RECT_p, returnList = F ))

   data02 <- lapply(1:length(data2), function(i)cbind(as.data.frame(data_12[[i]]),data2[[i]]))

   data02 <- llply(data02, function(x){subset(x, Hom1_m2 > 0)})

   tab<-llply(data02,function (x)subset(data.frame(table(x$RECT)), Freq > 1))

   data2 <- lapply(1:length(data02), function(i)

                   {

                     subset(data02[[i]], RECT %in% as.vector(unlist(tab[[i]][1])))
                   }
                 )

   data2<-ldply(data2,data.frame)

   data2$RECT<-paste(data2$RECT,data2$Period,sep="-")

   out <-  sqrt( exp(c( summary(lm(log(Hom1_m2) ~ RECT,data2))$ sigma^2 )) - 1)

   names(out)<-c("cv_tot")

   out

}


#' cv_trad_all
#' In case some period without cv assign the cv all periods together

for (i in 1:nrow(cv_trad))
{
    if (is.na(cv_trad[i,]))
    cv_trad[i,]<-cv_trad_all
  else
    cv_trad[i,]<-cv_trad[i,]
}

cv_trad

print(cv_trad)
}


#' Estimation of cv using gam.

cv_gam <- function(data)

{

  data_1<-data

  data<- llply(data, function(x) over(x[ ,c("Period","VolFilt","Temp5m","Temp20m","Mac1_m2","Hom1_m2")],RECT_p, returnList = F ))

  data0 <- lapply(1:length(data), function(i)cbind(as.data.frame(data_1[[i]]),data[[i]]))

  data <- ldply(data0, function(x){subset(x, Hom1_m2 > 0)})

  out<-  sqrt( exp( c(gamm4(log(Hom1_m2) ~ s(Period, Lon, Lat), random = ~ (1|RECT), data = data) $ gam $ sig2)- 1))

  names(out)<-c("cv")

  out
  print(out)
}





#´VARIANCE IN RECTANGLES.  variance in egg densities for each sampling square (rectangle)

 var_hom <- function(data)

{

   for (j in 1:length(data))

    {

      for (i in 1:length(data[[j]]@data$Hom1_m2))

       {

          data[[j]]@data$variance[i]<-sqrt(-1)

          data[[j]]@data$var_gam[i]<-sqrt(-1)

       }

     }

   for (j in 1:length(data))#variance by traditional method

   {

     for (i in 1:length(data[[j]]@data$Hom_m2_day.Pipe2))

       {
         if(!is.na(data[[j]]@data$method[i])&data[[j]]@data$method[i]=="S")

           data[[j]]@data$variance[i]<- data[[j]]@data$Hom_m2_day.Pipe2[i]^2*(sum(data[[j]]@data$Area[i],data[[j]]@data$contr_Area[i], na.rm=T))^2*cv_traditional[j,]^2/data[[j]]@data$nhaul[i]

         else

           data[[j]]@data$variance[i]<- sqrt(-1)

        }

    }

   for(j in 1:length(data))#variance by gam

     {

        for (i in 1:length(data[[j]]@data$Hom_m2_day.Pipe2))

            {

              if(!is.na(data[[j]]@data$method[i])&data[[j]]@data$method[i]=="S")

                data[[j]]@data$var_gam[i]<- data[[j]]@data$Hom_m2_day.Pipe2[i]^2*(sum(data[[j]]@data$Area[i],data[[j]]@data$contr_Area[i], na.rm=T))^2*cv_gam^2/data[[j]]@data$nhaul[i]

              else

                data[[j]]@data$variance[i]<- sqrt(-1)

            }

     }

  data

}

