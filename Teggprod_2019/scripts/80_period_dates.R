#80. DATES  ---

#Dates of start (dates) and end of each period (datee)



dateperiod <- function(data)

{

  out<- data.frame(cbind(ddply(data[ ,c("Period","Date")], .(Period), summarise, Per_dates=min(Date)), Per_datee=ddply(data[ ,c("Period","Date")], .(Period), summarise,max(Date))[ ,2]))

  out$days<-as.numeric(as.character((out$Per_datee-out$Per_dates )+1))
  
  print(paste("NOTE!!!! period dates in imported file survey_megs",component,year, sep="_"))
  
  print(out, row.names = FALSE)
  
  print(paste("NOTE!!!! start and finish dates assumed in periods from date file. component:",component,"Year:",year,sep=" "))
  
  print("(period 0 means complete spawning season)")
  
  print(    select (tdate, Year, period,Component,dates,datee,days_per), row.names = FALSE)

}


#function: period length


survper<-
  function(data)
  {

    tdate<-subset(data, Year==year&Component==component)

    for (i  in 1:length(tdate$period))
    {
      tdate$days_per<-sqrt(-1)  #v
    }


    for (i  in 1:length(tdate$period))
    {
      tdate$days_per[i]<-as.numeric((tdate$datee[i]-tdate$dates[i] )+1)
    }

    for (i  in 1:length(tdate$period))
    {
      tdate$inter_period<-sqrt(-1)  #v
    }

    for (i  in 1:length(tdate$period))
    {
      tdate$inter_period[i]<-paste(tdate[i,2],tdate[i+1,2],sep="$")
    }

    for (i  in 1:length(tdate$period))
    {
      tdate$days_interper<-sqrt(-1)  #v
    }

    for (i in 2:length(tdate$period)-1)

    {
      tdate$days_interper[1]<-as.numeric(tdate[2,4]-tdate[1,4])
      tdate$days_interper[i]<-as.numeric(tdate[i+1,4]-tdate[i,5]-1)
      tdate$days_interper[length(tdate$period)]<-as.numeric(tdate[1,5]-tdate[i+1,5])
    }


    tdate

  }

