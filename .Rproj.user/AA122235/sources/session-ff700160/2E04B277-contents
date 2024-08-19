library(tidyverse)
library(dplyr)
library(lubridate)
library(hms)

#create Column names
column_names <- c("Date", "Time", "Open", "High", "Low", "Last", "Volume", "# of Trades", 
                  "OHLC Avg", "HLC Avg", "HL Avg", "Bid Volume", "Ask Volume", 
                  "Point of Control", "Value Area High Value", "Value Area Low Value", 
                  "Volume Weighted Average Price", "Previous Day Close Line", 
                  "Previous Day High Line", "Previous Day Low Line", 
                  "Daily Open at 08:00", "Cash Market Open at 09:00", 
                  "Open", "High", "Low", "Close")


#Import data, delimited by comma

data = read_delim("./F.US.DDU24 [C][M]  30 Min  #4_GraphData.txt", delim = ",", col_names=column_names,skip=1 )

rm(column_names)

#Data wrangling

data=data%>%
  mutate(Date=ymd(Date))%>%
  mutate(Time = as_hms(Time))%>%
  mutate(across(-c(Date,Time), as.numeric))%>%
  rename(Open="Open...3", High="High...4", Low="Low...5")

data=data%>%
  select(-"Previous Day Close Line",-"Previous Day Low Line", 
         -"Daily Open at 08:00", -"Cash Market Open at 09:00",-"Open...23",-"High...24",-"Low...25",-"Close",
         -"Point of Control", -"Value Area High Value", -"Value Area Low Value", 
         -"Volume Weighted Average Price", -"Previous Day High Line")

data=data%>%
  filter(Date !="2024-08-19")
  

#Explore data

colnames(data)
glimpse(data)
head(data)


#create new column with previous day close 

ClosingPrice = data %>%
  filter(Time == as_hms("21:30:00")) %>%
  select(Date, ClosingPrice = Last) %>%
  mutate(Date=Date+days(1))

data=data%>%
  left_join(ClosingPrice, by ="Date")

rm(ClosingPrice)

#create new column with previous day high and low 

highprice = data%>%
  group_by(Date)%>%
  summarise(PreviousDayHigh=max(High, na.rm = TRUE))%>%
  ungroup%>%
  mutate(Date+days(1))%>%
  select(-"Date")%>%
  rename(Date="Date + days(1)")

lowprice = data%>%
  group_by(Date)%>%
  summarise(PreviousDayLow=min(Low, na.rm = TRUE))%>%
  ungroup%>%
  mutate(Date+days(1))%>%
  select(-"Date")%>%
  rename(Date="Date + days(1)")

data=data%>%
  left_join(highprice, by="Date")%>%
  left_join(lowprice, by="Date")
rm(lowprice,highprice)

#Create new column with current Day open

Open=data%>%
  filter(Time==as_hms("08:00:00"))%>%
  select(Date,CurrentDayOpen=Open)

data=data%>%
  left_join(Open, by="Date")
rm(Open)

data=data%>%
  filter(ClosingPrice!="NA")



