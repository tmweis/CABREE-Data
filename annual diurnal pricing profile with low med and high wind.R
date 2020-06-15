library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(scales)



#SORTING FOR HIGH WIND VS LOW WIND SPEEDS
AESO2 <- NULL
AESO2 <- read.csv("prices_wind2019_formatted.csv", header = TRUE)%>%
  mutate(date=mdy_hm(date),hour_start=hour(date),month=factor(month.abb[month(date)],levels = month.abb),year=year(date),day=day(date))
#View(AESO2)


##### this finds the category breaks for 3 even splits#########
quantile(AESO2$wind, probs = c(0.33,0.66,0.999,.9999))

windcat=c(-Inf,200,600,Inf)
AESO2$windy = cut (AESO2$wind, breaks=windcat,labels = c("Low Wind","Medium Wind","High Wind"))

#### creates the 3 wind categories
lowwind <- AESO2[AESO2$windy=="Low Wind",]
medwind <- AESO2[AESO2$windy=="Medium Wind",]
highwind <- AESO2[AESO2$windy=="High Wind",]

######sets up timeframe for analysis
start_date<-format(min(AESO2$date),format = "%b %d, %Y")
end_date<-format(max(AESO2$date),format = "%b %d, %Y")


#### this creates the averages for entire year####
ave_data<-AESO2 %>% group_by(hour_start)%>% 
  summarise(min_price=min(price),max_price=max(price),mean_price=mean(price),
            sd_price=sd(price) )

###this creates avaerge of low####
low_data<-lowwind %>% group_by(hour_start)%>% 
  summarise(min_price=min(price),max_price=max(price),mean_price=mean(price),
            sd_price=sd(price) )

###this creates avaerge of medium####
med_data<-medwind %>% group_by(hour_start)%>% 
  summarise(min_price=min(price),max_price=max(price),mean_price=mean(price),
            sd_price=sd(price) )

###this creates avaerge of high####
high_data<-highwind %>% group_by(hour_start)%>% 
  summarise(min_price=min(price),max_price=max(price),mean_price=mean(price),
            sd_price=sd(price) )



ggplot(filter(ave_data)) +
  
  geom_line(data=filter(low_data),aes(hour_start,mean_price,colour="Low Wind <200 MW"),
            size=1)+
  geom_line(data=filter(med_data),aes(hour_start,mean_price,colour="Medium Wind <600 MW"),
            size=1)+
  geom_line(data=filter(high_data),aes(hour_start,mean_price,colour="High Wind >600 MW"),
            size=1)+
  geom_line(size=1.5,aes(hour_start,mean_price,color="Mean Price"))+
  scale_x_continuous(breaks=seq(0,23,4))+
  scale_y_continuous(breaks=pretty_breaks(),limits = c(0,175))+
  scale_colour_manual("",values = c("green","red","black","orange"))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 12,face = "bold", colour="black")
  )+    labs(y="Hourly Price ($/MWh)
             ",x="Hour",
             title="Hourly Pricing by Month",
             subtitle=paste("Alberta Pool Prices During Low, Medium and High Wind Periods from ",start_date," to ",end_date,sep=""),
             caption="Source: Generation data via NRGStream\nGraph by Tim Weis")
ggsave("aeso_diurnal_wind_pricing_2019.png",width = 16,height = 10)
