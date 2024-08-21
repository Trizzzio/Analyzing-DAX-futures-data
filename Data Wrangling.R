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



#Explore data

colnames(data)
glimpse(data)
head(data)


#create new column with previous day close and current day open 

ClosingPrice = data %>%
  filter(Time == as_hms("21:30:00")) %>%
  select(Date, ClosingPrice = Last) %>%
  mutate(ShiftedClosingPrice =lag(ClosingPrice, n=1))%>%
  select(-"ClosingPrice")%>%rename("ClosingPrice"=ShiftedClosingPrice)

Open=data%>%
  filter(Time==as_hms("08:00:00"))%>%
  select(Date,CurrentDayOpen=Open)

data=data%>%
  left_join(ClosingPrice, by="Date")%>%
  left_join(Open, by="Date")

rm(ClosingPrice, Open)

#create new column with previous day high and low 

highprice = data%>%
  group_by(Date)%>%
  summarise(DayHigh=max(High, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(PreviousDayHigh =lag(DayHigh, n=1))%>%
  select(-"DayHigh")

lowprice = data%>%
  group_by(Date)%>%
  summarise(DayLow=min(Low, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(PreviousDayLow =lag(DayLow, n=1))%>%
  select(-"DayLow")

data=data%>%
  left_join(highprice, by="Date")%>%
  left_join(lowprice, by="Date")

rm(lowprice,highprice)


data<-data%>%
  filter(!is.na(ClosingPrice))%>%
  filter(!is.na(PreviousDayHigh))%>%
  filter(!is.na(PreviousDayLow))

##Eliminating outlier days, eliminating days with too little observations

data <- data %>%
  group_by(Date) %>%
  filter(n() == 40) %>%   # Keep only groups with exactly 40 rows
  ungroup()



