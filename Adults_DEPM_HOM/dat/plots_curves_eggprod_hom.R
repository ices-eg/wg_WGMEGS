
library(tidyverse)
library(lubridate)

tabdep_hom <- bind_rows( read.csv("tabDEP2_whom_92_19.csv") %>% mutate(datee=dmy(datee), dates=dmy(dates)) %>%  rename(Year=year, Component=component),
                          read.csv("output/output_table_midday_HOM.csv") %>% mutate(datee=ymd(datee), dates=ymd(dates), Component=recode(Component,"W"="western")))


summary(tabdep_hom)



tabdep_hom%>%filter(period=="1"|period=="2"|period=="3"|period=="4"|period=="5"|period=="6"|period=="7")



ggplot(tabdep_hom%>%filter(Component=="western", Year>2006)%>%
         filter(period=="1"|period=="2"|period=="3"|period=="4"|period=="5"|period=="6"|period=="7")%>%mutate(ydays=yday(ymd(dates)),ydaye=yday(ymd(datee)),rel_AEP=(AEP/days)/1e9 ),
       aes(xmin =ydays,xmax = ydaye,ymin = 0, ymax = rel_AEP), expand=F )+theme_bw()+
  
  geom_rect(aes(xmin =1,
                xmax = 31,
                ymin = -Inf, ymax = Inf), fill = 'grey',color = NA, alpha = .1)+
  geom_rect(aes(xmin =60,
                xmax = 90,
                ymin = -Inf, ymax = Inf), fill = 'grey',color = NA, alpha = .1)+
  geom_rect(aes(xmin =121,
                xmax = 151,
                ymin = -Inf, ymax = Inf), fill = 'grey',color = NA, alpha = .1)+
  geom_rect(aes(xmin =182,
                xmax = 212,
                ymin = -Inf, ymax = Inf), fill = 'grey',color = NA, alpha = .1)+
  
  annotate(geom="text", x=15, y=45000, label="Jan",
           color="grey20", size=1)+
  annotate(geom="text", x=75, y=45000, label="Mar",
           color="grey20", size=1)+
  annotate(geom="text", x=136, y=45000, label="May",
           color="grey20", size=1)+
  annotate(geom="text", x=197, y=45000, label="Jul",
           color="grey20", size=1)+
  
  geom_rect( fill = 'darkblue',color = 'black') +
  facet_wrap(~ Year, ncol=2)+
  
  # geom_point(size=3 )+
  theme(legend.position="none")+
  scale_x_continuous(limits =c(0,213), expand = c(0, 0))+
  ylim(0, 25000)+
  xlab("Julian day") +
  ylab(" Egg prod.(*10^9)")+
  labs(color='')+
  ggtitle("Western  horse mackerel \nEgg production by period ")

ggsave("eggprod_byperiod_HOM_western_ts_aep_23.png", width=9, heigh = 6, dpi=400)





#%>%filter(tabdep_hom,!grepl("$",period))

# ggplot(bind_rows(tabdep_hom%>%filter(Component=="western", Year>2005)%>%
#                    filter(period=="1"|period=="2"|period=="3"|period=="4"|period=="5"|period=="6"|period=="7")),
#           aes(x=mid_day,y=AEP/1e13, group=factor(Year),colour=factor(Year) ))+theme_bw()+
#   
#   geom_rect(aes(xmin =1,
#                 xmax = 31,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .1)+
#   geom_rect(aes(xmin =60,
#                 xmax = 90,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .1)+
#   geom_rect(aes(xmin =121,
#                 xmax = 151,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .1)+
#   geom_rect(aes(xmin =182,
#                 xmax = 212,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .1)+
#   
#   annotate(geom="text", x=15, y=150, label="Jan",
#            color="grey20", size=2)+
#   annotate(geom="text", x=75, y=150, label="Mar",
#            color="grey20", size=2)+
#   annotate(geom="text", x=136, y=150, label="May",
#            color="grey20", size=2)+
#   annotate(geom="text", x=197, y=150, label="Jul",
#            color="grey20", size=2)+
#   
#   geom_bar(stat="identity", width = 20, fill="darkblue") +
#   facet_wrap(~ Year, ncol=2,  dir="v")+
#  
#  # geom_point(size=3 )+
#   theme(legend.position="none")+
#   scale_x_continuous(limits =c(0,213), expand = c(0, 0))+
#     ylim(0, 150)+
#   xlab("Julian day") +
#   ylab(" Egg prod.(e13)")+
#   labs(color='')+
#   ggtitle("Western  Horse mackerel \n  egg production by period ")
# #ggsave("eggprod_byperiod_mack_western_ts_aep.png", width=9, heigh = 6, dpi=400)





#all no possible  mid point periods are differents southern-western

# #ggplot(bind_rows(tabdep_hom%>%filter( year>2003)%>%
#                    filter(period=="1"|period=="2"|period=="3"|period=="4"|period=="5"|period=="6"|period=="7")),
#        aes(x=mid_day,y=AEP/1e13, fill=Component ))+theme_bw()+
#   
#   geom_rect(aes(xmin =1,
#                 xmax = 31,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .1)+
#   geom_rect(aes(xmin =60,
#                 xmax = 90,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .1)+
#   geom_rect(aes(xmin =121,
#                 xmax = 151,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .1)+
#   geom_rect(aes(xmin =182,
#                 xmax = 212,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .1)+
#   
#   annotate(geom="text", x=15, y=50, label="Jan",
#            color="grey20", size=2)+
#   annotate(geom="text", x=75, y=50, label="Mar",
#            color="grey20", size=2)+
#   annotate(geom="text", x=136, y=50, label="May",
#            color="grey20", size=2)+
#   annotate(geom="text", x=197, y=50, label="Jul",
#            color="grey20", size=2)+
#   
#   geom_bar( width = 20) +
#   facet_wrap(~ year, ncol=2,  dir="v")+
#   
#   # geom_point(size=3 )+
#   theme(legend.position="none")+
#   scale_x_continuous(limits =c(0,213), expand = c(0, 0))+
#   ylim(0, 250)+
#   xlab("Julian day") +
#   ylab(" Egg prod.(e13)")+
#   labs(color='')+
#   ggtitle("Southern  mackerel \n  egg production by period ")
# ggsave("eggprod_byperiod_mack_southern_ts_aep.png", width=9, heigh = 6, dpi=400)



# geom_line( bind_rows(tabdep_hom%>%filter(Component=="western", year==2019)%>%
# filter(period=="1"|period=="2"|period=="3"|period=="4"|period=="5"|period=="6"|period=="7"),
# tabdep_hom%>%filter(Component=="western", year==2019)%>%filter(period=="0")%>%
#   mutate(mid_day=yday(dates), AEP=0)) ,mapping=aes(x=mid_day,y=AEP/1e13 ), colour="black", size=1.5)+

##WESTERN MACK DEP
#+xlim(0, 250)
ggplot(bind_rows(tabdep_hom%>%filter(Component=="western", Year>2007, days>0)%>%
                   filter(period=="1"|period=="2"|period=="3"|period=="4"|period=="5"|period=="6"|period=="7"|period=="7$NA"|period=="6$NA"|period=="5$NA" ),
                 tabdep_hom%>%
                   filter(Component=="western", Year>2007)%>%filter(period=="0")%>%mutate(mid_day=yday(dates), DEP=0)),
       aes(x=mid_day,y=DEP/1e12, group=factor(Year),colour=factor(Year), shape=factor(Year) ))+
  theme_bw()+
  
  geom_rect(aes(xmin =1,
                xmax = 31,
                ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .01)+
  geom_rect(aes(xmin =60,
                xmax = 90,
                ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .01)+
  geom_rect(aes(xmin =121,
                xmax = 151,
                ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .01)+
  geom_rect(aes(xmin =182,
                xmax = 212,
                ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .01)+
  
  annotate(geom="text", x=15, y=20, label="Jan",
           color="grey20", size=2)+
  annotate(geom="text", x=75, y=20, label="Mar",
           color="grey20", size=2)+
  annotate(geom="text", x=136, y=20, label="May",
           color="grey20", size=2)+
  annotate(geom="text", x=197, y=20, label="Jul",
           color="grey20", size=2)+
  
  geom_line(aes(color=factor(Year)), size=1.5) +
  geom_point(size=3 )+
  theme(legend.position=c(.2,.8))+
  xlab("Julian day") +
  ylab("Daily egg prod.(e12)")+
  labs(color='')+ 
  scale_x_continuous(limits =c(0,213), expand = c(0, 0))+
  ylim(0,20)+
  scale_colour_manual(name = "",
                    
                      values = c("blue", "purple", "darkolivegreen", "red", "black")) +   
  scale_shape_manual(name = "",
                     
                     values = c(15, 16, 17, 18, 8))+
  ggtitle("Western  horse mackerel \n Daily egg production ")


ggsave("eggprod_byperiod_HOM_ts_dep_23.png", width=9, heigh = 6, dpi=400)


tabdep_hom%>%filter(Component=="western")%>%group_by(Year)%>%summarise(AEP_tot=sum(AEP, na.rm=T),sd=sqrt(sum(A_var_trad, na.rm=T)))
##HORSE MACK AEP
ggplot(tabdep_hom%>%filter(Component=="western")%>%group_by(Year)%>%
         summarise(TAEP=sum(AEP, na.rm=T),sd=sqrt(sum(A_var_trad, na.rm=T))),
       aes(x=factor(Year), y=TAEP/1e13))+ geom_bar(stat="identity", fill="cornflowerblue")+
 # geom_errorbar(aes(ymin=(TAEP/1e13)-(sd/1e13), ymax=(TAEP/1e13)+(sd/1e13)), width=.3,  position=position_dodge(.9))+
  xlab("Year") + theme_bw()+
  ylab(" Annual Egg prod.(e13)")+
  ggtitle("Western  horse mackerel \n Annual egg production ")

ggsave("TAEP_HOM_ts_23.png", width=9, heigh = 6, dpi=300)

          

#
##comparative

assess<-tabdep_hom%>%group_by(Year)%>%summarise(TAEP=sum(AEP, na.rm=T),sd=sqrt(sum(A_var_trad, na.rm=T)))%>%right_join(read.csv("result_assess.csv"), by=c("Year"="Year"))
#%>%gather(type,value, TAEP:MEGS_SSB)

ggplot(assess, aes(x=Year))+ geom_ribbon(aes(ymin=SSB_2.5_assess/10, ymax=SSB_97.5_assess/10), fill = "grey70")+ geom_line(aes(y= SSB_assess/10))+geom_point(aes(y= SSB_assess/10), color="black", sixe=2)+geom_point(aes(y= MEGS_SSB/10   ), color="red", sixe=3)+geom_point(aes(y= TAEP/1e13), color="blue", sixe=3, shape=17) + labs(y="SSB ('0000 tonnes)", x="Year")+scale_y_continuous(sec.axis = sec_axis(~.*1e13, name= "Total egg prod"))+ scale_x_continuous(breaks=seq(1980, 2019,3), limits=c(1980, 2019)) +
  scale_colour_manual(name="",values=c( "black","red","blue"))


+ theme(legend.position = c(0.8, 0.9))
ggsave("assess_mack.png", width=7, heigh = 5, dpi=300)

##HORSE MACKEREL

#tabdep_hom <- read.csv("tabDEP2_whom_92_19.csv")%>%mutate(datee=dmy(datee), dates=dmy(dates))
aep_hom <- read.csv("HOM_TAEP_ts.csv")



# ggplot(bind_rows(tabdep_hom%>%filter(Component=="western", Year>2007, days>0)%>% filter(period=="1"|period=="2"|period=="3"|period=="4"|period=="5"|period=="6"|period=="7")%>%mutate(ydays=yday(ymd(dates)),ydaye=yday(ymd(datee)),rel_AEP=(AEP/days)/1e9 ),
#                  tabdep_hom%>%filter(Component=="western", Year>2007)%>%filter(period=="0")%>%mutate(mid_day=yday(dates), DEP=0)),
#        aes(x=mid_day,y=DEP/1e12, group=factor(Year),colour=factor(Year), shape=factor(Year) ))+
#   theme_bw()+
#   
#   geom_rect(aes(xmin =1,
#                 xmax = 31,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .01)+
#   geom_rect(aes(xmin =60,
#                 xmax = 90,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .01)+
#   geom_rect(aes(xmin =121,
#                 xmax = 151,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .01)+
#   geom_rect(aes(xmin =182,
#                 xmax = 212,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .01)+
#   
#   annotate(geom="text", x=15, y=25, label="Jan",
#            color="grey20", size=2)+
#   annotate(geom="text", x=75, y=25, label="Mar",
#            color="grey20", size=2)+
#   annotate(geom="text", x=136, y=25, label="May",
#            color="grey20", size=2)+
#   annotate(geom="text", x=197, y=25, label="Jul",
#            color="grey20", size=2)+
#   geom_line(aes(color=factor(Year)), size=1.5) +
#   geom_point(size=3 )+
#   theme(legend.position=c(.2,.7))+
#   xlab("Julian day") +
#   ylab("Daily egg prod.(e12)")+
#   labs(color='')+ 
#   ylim(0,25)+
#   scale_x_continuous(limits =c(0,213), expand = c(0, 0))+
#   scale_colour_manual(name = "",
#                       
#                       values = c("blue", "purple", "darkgreen", "red", "black")) +   
#   scale_shape_manual(name = "",
#                      
#                      values = c(15, 16, 17, 18, 8))+
#      ggtitle("Western Horse mackerel \n Daily egg production ")
# 
# ggsave("eggprod_byperiod_hom_ts_dep.png", width=9, heigh = 6, dpi=400)

#AEP

# ggplot(aep_HOM,aes(x=factor(Year), y=TAEP/1e13))+ geom_bar(stat="identity", fill="steelblue")+
#  # geom_errorbar(aes(ymin=(TAEP/1e13)-(sd/1e13), ymax=(TAEP/1e13)+(sd/1e13)), width=.3, position=position_dodge(.9))+
#   xlab("Year") +
#   theme_bw()+
#   ylab(" Annual Egg prod.(e13)")+
#    ggtitle("Western Horse mackerel \n Annual egg production ")
# 
# ggsave("AEP_hom_ts.png", width=9, heigh = 6, dpi=300)


##taep hom by period

# ggplot(bind_rows(tabdep_hom%>%filter(Component=="western", Year>2003)%>%
#                    filter(period=="1"|period=="2"|period=="3"|period=="4"|period=="5"|period=="6"|period=="7")),
#        aes(x=mid_day,y=AEP/1e13, group=factor(Year),colour=factor(Year) ))+theme_bw()+
#   
#   geom_rect(aes(xmin =1,
#                 xmax = 31,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .1)+
#   geom_rect(aes(xmin =60,
#                 xmax = 90,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .1)+
#   geom_rect(aes(xmin =121,
#                 xmax = 151,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .1)+
#   geom_rect(aes(xmin =182,
#                 xmax = 212,
#                 ymin = -Inf, ymax = Inf), fill = 'grey',color = 'white', alpha = .1)+
#   
#   annotate(geom="text", x=15, y=75, label="Jan",
#            color="grey20", size=2)+
#   annotate(geom="text", x=75, y=75, label="Mar",
#            color="grey20", size=2)+
#   annotate(geom="text", x=136, y=75, label="May",
#            color="grey20", size=2)+
#   annotate(geom="text", x=197, y=75, label="Jul",
#            color="grey20", size=2)+
#   
#   geom_bar(stat="identity", width = 18, fill="darkblue") +
#   facet_wrap(~ Year, ncol=2,  dir="v")+
#   
#   # geom_point(size=3 )+
#   theme(legend.position="none")+
#   scale_x_continuous(limits =c(0,213), expand = c(0, 0))+
#   ylim(0, 75)+
#   xlab("Julian day") +
#   ylab(" Egg prod.(e13)")+
#   labs(color='')+
#   ggtitle("Western  HORSE mackerel \n  egg production by period ")
# ggsave("eggprod_byperiod_HOM_western_ts_aep.png", width=9, heigh = 6, dpi=400)

####################################LEVELPLOTS




taep_southern<-bind_rows(read.csv("df/south_df1992.csv")%>%mutate(Year_megs=1992,Date=ymd_hms(Date),month=month(Date)),
                         read.csv("df/south_df1995.csv")%>%mutate(Year_megs=1995,Date=ymd_hms(Date),month=month(Date)),
                         read.csv("df/south_df1998.csv")%>%mutate(Year_megs=1998,Date=ymd_hms(Date),month=month(Date)),
                         read.csv("df/south_df2001.csv")%>%mutate(Year_megs=2001,Date=ymd_hms(Date),month=month(Date)),
                         read.csv("df/south_df2004.csv")%>%mutate(Year_megs=2004,Date=ymd_hms(Date),month=month(Date)),
                         read.csv("df/south_df2007.csv")%>%mutate(Year_megs=2007,Date=ymd_hms(Date),month=month(Date)),
                         read.csv("df/south_df2010.csv")%>%mutate(Year_megs=2010,Date=ymd_hms(Date),month=month(Date)),
                         read.csv("df/south_df2013.csv")%>%mutate(Year_megs=2013,Date=ymd_hms(Date),month=month(Date)),
                         read.csv("df/south_df2016.csv")%>%mutate(Year_megs=2016,Date=ymd_hms(paste(Date,"02:00:00")),month=month(Date)),
                         read.csv("df/south_df2019.csv")%>%mutate(Year_megs=2019,Date=ymd_hms(paste(Date,"02:00:00")),month=month(Date)))%>%
  mutate( RECT=as.factor(as.character(RECT)))%>%
  filter(method!="NA")%>%
  rename("Period2"=".id")
  
summary(taep_southern)

taep_western<-bind_rows(read.csv("df/west_df1992.csv")%>%mutate(Year_megs=1992,Date=dmy_hm(Date),month=month(Date)),
                         read.csv("df/west_df1995.csv")%>%mutate(Year_megs=1995,Date=dmy_hm(Date),month=month(Date)),
                         read.csv("df/west_df1998.csv")%>%mutate(Year_megs=1998,Date=dmy_hm(Date),month=month(Date)),
                         read.csv("df/west_df2001.csv")%>%mutate(Year_megs=2001,Date=dmy_hm(Date),month=month(Date)),
                         read.csv("df/west_df2004.csv")%>%mutate(Year_megs=2004,Date=dmy_hm(Date),month=month(Date)),
                         read.csv("df/west_df2007.csv")%>%mutate(Year_megs=2007,Date=dmy_hm(Date),month=month(Date)),
                         read.csv("df/west_df2010.csv")%>%mutate(Year_megs=2010,Date=dmy_hm(Date),month=month(Date)),
                         read.csv("df/west_df2013.csv")%>%mutate(Year_megs=2013,Date=dmy_hm(Date),month=month(Date)),
                         read.csv("df/west_df2016.csv")%>%mutate(Year_megs=2016,Date=dmy(Date)),month=month(Date)),
                         read.csv("df/west_df2019.csv")%>%mutate(Year_megs=2019,Date=ymd(Date),month=month(Date)))%>%
  mutate( RECT=as.factor(as.character(RECT)))%>%
  filter(method!="NA")%>%
  rename("Period2"=".id")
  

summary(taep_western)

head(taep_western%>%mutate(lat1=str_sub(lat, 1,2)))
head(taep_western%>%mutate(lat1=month(Date)))

#kk<-taep_western%>%group_by(Year_megs, lat2=as.numeric(str_sub(lat, 1,2)),month=month(Date) )%>%summarise(egg_prod_tot=sum(egg_prod2,na.rm=T),egg_prod_samp=sum(egg_prod,na.rm=T),Mac_m2_day.Mend2=sum(Mac_m2_day.Mend2,na.rm=T))

kkw<-taep_western%>%group_by(Year_megs, lat2=as.numeric(str_sub(lat, 1,2)),month=month(Date) )%>%summarise(egg_prod_tot=mean(egg_prod2,na.rm=T),egg_prod_samp=mean(egg_prod,na.rm=T),Mac_m2_day.Mend2=mean(Mac_m2_day.Mend2,na.rm=T), n=n())%>%filter(month!="NA")%>%data.frame()

##hist(log(kk$egg_prod_tot+1),breaks=100)
#summary(log(kk$egg_prod_tot+1))
#lims <- c( 0,1,20,24,28,31)             #establecemos intervalos
#classe <- findInterval(log(kk$egg_prod_tot+1), lims)    #aplicamos los intervalos a nuestros datos
#table(classe)


hist(log(kkw$egg_prod_tot+1),breaks=100)
summary(log(kkw$egg_prod_tot+1))
lims <- c( 0,1,20,24,28,31)             #establecemos intervalos
classe <- findInterval(log(kk$egg_prod_tot+1), lims)    #aplicamos los intervalos a nuestros datos
table(classe)


#ddply(western2,c("Year", "lat2","month"), summarise,egg_prod=sum(egg_prod,na.rm=T),egg_prod2=sum(egg_prod2,na.rm=T),Mac_m2_day.Mend2=sum(Mac_m2_day.Mend2,na.rm=T),variance=sum(variance,na.rm=T),lat=mean(lat,na.rm=T),  Temp20m=round(mean(Temp20m,na.rm=T)))           


cores <- c("#999999","#B2DF8A"  ,"#4DAF4A","#FDBF6F" ,"#E41A1C" ,"#000000")


library (classInt)
library(RColorBrewer) 


#q3<-classIntervals(kk$egg_prod_tot, n=5, style="fisher") 
#diff(q3$brks)
#> diff(q3$brks)
#[1] 8.775786e+11 2.075668e+12 3.336947e+12 8.094909e+12 4.054081e+12
plot(q3,pal=brewer.pal(3, "Reds")) 

q3<-classIntervals(kkw$egg_prod_tot , n=3, style="fisher") 
diff(q3$brks)
#> diff(q3$brks)
#[1] 8.3648744976 e10 23.8275126237e10 81.60469488586e10
plot(q3,pal=brewer.pal(3, "Reds")) 

 
##ploting hist by segment of classintervals
#breaks=c(-Inf,0.1, 2.075668e+12 ,3.336947e+12, 8.094909e+12, 4.054081e+12, Inf)
#kkw<-kkw%>%mutate(cat_egg_prod_tot=cut(egg_prod_tot,breaks=c(-Inf,0.001, .03e+12 ,0.20e+12, 2.5e+12,  Inf), labels=c("0","0-2.1", "2.1-3.3","3.3- 8.1",">8.1")))

kkw<-kkw%>%mutate(cat_egg_prod_tot=cut(egg_prod_tot,breaks=c(-Inf,0.0001, .8e+10 ,2.3e+10, 23e+10,  Inf), labels=c("0","0-0.8", "0.8-2.3","2.3- 23",">23")))

library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")

library(viridis)
mid <- mean(kkw$egg_prod_tot)
ggplot(kkw%>%filter(month!="NA")%>%data.frame(), aes(factor(month), lat2))+
  geom_tile(aes( fill = egg_prod_tot)) +
  facet_wrap(~ Year_megs, ncol=2,  strip.position="right", dir="v")+
  #scale_y_continuous(expand = c(0, 0)) +
  theme(panel.border = element_rect(colour = "grey", fill = NA))+
  xlab("Month") +
  ylab("Latitude (N)")+
  theme_bw()+
  scale_fill_gradient( low = "lightblue",high = "darkred" )+
  geom_tile(kkw%>%filter(egg_prod_tot==0),mapping=aes(), fill = "grey30")+ 
  theme(legend.position="top")+
  geom_text(aes(label = n,colour = "white", fontface = "bold"), size=2 , colour="white")+
    theme(strip.text.y = element_text(face="bold"))+
  ggtitle("Western  mackerel \n mean DEP (number=obs.) ")

ggsave("levelplot_western_ts_mean3.png", width=9, heigh = 11, dpi=600)

#scale_fill_gradientn(colours = c("blue", "lightblue", "lightgreen", "green"),breaks=c(0,25,50,75,Inf),na.value = "red")


kks<-taep_southern%>%group_by(Year_megs, lon2=as.numeric(str_sub(lon, 1,2)),month=month(Date) )%>%summarise(egg_prod_tot=sum(egg_prod2,na.rm=T),egg_prod_samp=sum(egg_prod,na.rm=T),Mac_m2_day.Mend2=sum(Mac_m2_day.Mend2,na.rm=T), n=n())%>%mutate(cat_egg_prod_tot=cut(egg_prod_tot,breaks=c(-Inf,0.001, .03e+12 ,0.20e+12, 2.5e+12,  Inf), labels=c("0","0-2.1", "2.1-3.3","3.3- 8.1",">8.1")))


ggplot(kks%>%filter(month!="NA")%>%data.frame(), aes(factor(month), lon2))+
  geom_tile(aes( fill = cat_egg_prod_tot)) +
  facet_wrap(~ Year_megs, ncol=2,  strip.position="right", dir="v")+
  #scale_y_continuous(expand = c(0, 0)) +
  theme(panel.border = element_rect(colour = "grey", fill = NA))+
  xlab("Month") +
  ylab("Longitude (W)")+
  theme_bw()+
  scale_fill_manual(name="Mack egg Prod.\n ( *e10)",values=c("#999999","#B2DF8A"  ,"#4DAF4A","#FDBF6F" ,"#E41A1C" ,"#000000"), labels=c("  0","< 3"  ,"3 - 20","20 - 250" ,"250 - 1850"),guide = guide_legend(reverse=TRUE))+
  theme(legend.position="top")+
  theme(strip.text.y = element_text(face="bold"))

ggsave("levelplot_southern_ts_mean2.png", width=9, heigh = 11, dpi=600)

ggplot(kks%>%filter(month!="NA")%>%data.frame(), aes(factor(month), lon2))+
  geom_tile(aes( fill = egg_prod_tot)) +
  facet_wrap(~ Year_megs, ncol=2,  strip.position="right", dir="v")+
  #scale_y_continuous(expand = c(0, 0)) +
  theme(panel.border = element_rect(colour = "grey", fill = NA))+
  xlab("Month") +
  ylab("Longitude (W)")+
   #geom_rug( aes(x= n),sides="b",color="red", position = "jitter")+
  theme_bw()+
  scale_fill_gradient( low = "lightblue",high = "darkred" )+
  geom_tile(kks%>%filter(egg_prod_tot==0),mapping=aes(), fill = "grey30")+ 
  geom_text(aes(label = n,colour = "white", fontface = "bold"), size=3 , colour="white")+
  theme(legend.position="top")+
  theme(strip.text.y = element_text(face="bold"))+
  ggtitle("Southern  mackerel \n mean DEP (number=obs) ")

ggsave("levelplot_southern_ts_mean3.png", width=9, heigh = 11, dpi=600)



###SSB



library(tidyverse)
library(lubridate)

ssb_ts <- read.csv("Final_table_MACK_all Years_2019_prov_25.04.20.csv")
summary(ssb_ts)




ggplot(ssb_ts%>%filter(Component=="Combined"),aes(x=factor(Year),y=SSB))+
  geom_bar(stat="identity", fill="grey")+ theme_bw()+
  xlab("Year") +
  ylab("SSB ( '000 ton)")+
  ggtitle("SSB  mackerel \n (thousand tons) ")


ggsave("ssb_mack_combined_ts.png", width=9, heigh = 6, dpi=300)


# ggplot(ssb_ts%>%filter(Component!="Combined"),aes(x=Year,y=SSB, fill=Component))+
#   geom_area(stat="identity", position="stack")+ theme_bw()+
#   xlab("Year") +
#   ylab("SSB ( '000 ton)")+
#   ggtitle("SSB  mackerel \n (thousand tons) ") no sense no continuous Year->triennnial

ggplot(ssb_ts%>%filter(Component!="Combined"),aes(x=factor(Year),y=SSB, fill=Component))+
  geom_bar(stat="identity", position="stack")+ theme_bw()+
  xlab("Year") +
  ylab("SSB ( '000 ton)")+
  ggtitle("SSB  mackerel by Components\n (thousand tons) ")+ theme(legend.position="top",legend.title = element_blank())

ggsave("ssb_mack_bycomp_ts.png", width=9, heigh = 6, dpi=300)


