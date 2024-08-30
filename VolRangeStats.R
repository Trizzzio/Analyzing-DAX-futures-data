library(tidyverse)
library(dplyr)
library(lubridate)
library(hms)

#Upload FDAX dataset 

#create Column names
column_names <- c("Date", "Time", "Open", "High", "Low", "Last", "Volume", "# of Trades", 
                  "OHLC Avg", "HLC Avg", "HL Avg", "Bid Volume", "Ask Volume")

data = read_delim("./F.US.DDU24(5000Days).txt", delim = ",", col_names=column_names,skip=1 )


rm(column_names)


data=data%>%
  mutate(Date=ymd(Date))%>%
  mutate(Time = as_hms(Time))%>%
  mutate(across(-c(Date,Time), as.numeric))%>%
  select(-"# of Trades", -"OHLC Avg",-"HLC Avg",-"HL Avg", -"Bid Volume", -"Ask Volume")


#Upload vol data and merge with FDAX data 

column_names <- c("Date","Symbol","VDAX")


voldata = read_delim("./VDAX.txt", delim = ";", col_names=column_names,skip=1 )%>%
  select(-"Symbol")%>%
  mutate(Date=dmy(Date))


#Merge Dataframes

df <- data %>%
  left_join(voldata, by ="Date")


#Create Range variable

#create new column with previous day high and low 

df=df%>%
  group_by(Date)%>%
  mutate(DayHigh=max(High, na.rm = TRUE))%>%
  mutate(DayLow=min(Low, na.rm = TRUE))%>%
  mutate(Range=abs(DayHigh-DayLow))%>%
  ungroup()


#Calculating average range conditional on volatility


df=df%>%
  group_by(Date)%>%
  summarise(vol=first(VDAX),
            range=first(Range))%>%
  ungroup()%>%
  mutate(volbin=cut(vol, breaks = seq(5, max(vol, na.rm=TRUE)+5, by=5),include.lowest = TRUE, right =FALSE))


average_range_table <- df%>%
  group_by(Date)%>%
  group_by(volbin)%>%
  summarise(Average_Range = mean(range, na.rm = TRUE),
            Std_Dev_Range = sd(range, na.rm = TRUE),
      Days=n())%>%
  ungroup()

print(average_range_table)


