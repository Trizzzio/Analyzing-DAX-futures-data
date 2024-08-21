library(tidyverse)
library(dplyr)
library(lubridate)
library(hms)
library(xtable)

###Creating Statistics for 10+ years of data of DAX futures


###Uploading and cleaning data

#create Column names
column_names <- c("Date", "Time", "Open", "High", "Low", "Last", "Volume", "# of Trades", 
                  "OHLC Avg", "HLC Avg", "HL Avg", "Bid Volume", "Ask Volume")

data = read_delim("./F.US.DDU24(5000Days).txt", delim = ",", col_names=column_names,skip=1 )


rm(column_names)

#Data wrangling

data=data%>%
  mutate(Date=ymd(Date))%>%
  mutate(Time = as_hms(Time))%>%
  mutate(across(-c(Date,Time), as.numeric))%>%
  select(-"# of Trades", -"OHLC Avg",-"HLC Avg",-"HL Avg", -"Bid Volume", -"Ask Volume")

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

#Eliminate NAs


data<-data%>%
  filter(!is.na(ClosingPrice))%>%
  filter(!is.na(PreviousDayHigh))%>%
  filter(!is.na(PreviousDayLow))

##Eliminating outlier days, eliminating days with too little observations

data <- data %>%
  group_by(Date) %>%
  filter(n() >= 28) %>%   # Keep only groups with exactly 40 rows
  ungroup()


#Create Gap Variable

datagap=data%>%
  mutate(Gap= CurrentDayOpen-ClosingPrice)%>%
  filter(!is.na(Gap))

#Calculate Stats with function
#Positive gap

Pos10to50=analyze_gap_closure(datagap,"positive",10,50)
Pos10to40=analyze_gap_closure(datagap,"positive",10,40)
Pos10to30=analyze_gap_closure(datagap,"positive",10,30)
Pos0to10=analyze_gap_closure(datagap,"positive",0,10)
Pos10to20=analyze_gap_closure(datagap,"positive",10,20)
Pos20to30=analyze_gap_closure(datagap,"positive",20,30)
Pos30to40=analyze_gap_closure(datagap,"positive",30,40)
Pos20tomax=analyze_gap_closure(datagap,"positive",20)
Pos30tomax=analyze_gap_closure(datagap,"positive",30)
Pos40tomax=analyze_gap_closure(datagap,"positive",40)

#Negative gap

Neg10to50=analyze_gap_closure(datagap,"negative",10,50)
Neg10to40=analyze_gap_closure(datagap,"negative",10,40)
Neg10to30=analyze_gap_closure(datagap,"negative",10,30)
Neg0to10=analyze_gap_closure(datagap,"negative",0,10)
Neg10to20=analyze_gap_closure(datagap,"negative",10,20)
Neg20to30=analyze_gap_closure(datagap,"negative",20,30)
Neg30to40=analyze_gap_closure(datagap,"negative",30,40)
Neg20tomax=analyze_gap_closure(datagap,"negative",20)
Neg30tomax=analyze_gap_closure(datagap,"negative",30)
Neg40tomax=analyze_gap_closure(datagap,"negative",40)


##Create table 

GapResults<- rbind(Pos10to50,Pos10to40,Pos10to30,Pos0to10,Pos10to20,Pos20to30,Pos30to40,Pos20tomax,Pos30tomax,Pos40tomax,
                   Neg10to50,Neg10to40,Neg10to30,Neg0to10,Neg10to20,Neg20to30,Neg30to40,Neg20tomax,Neg30tomax,Neg40tomax)


# Convert the data frame to an xtable object
table <- xtable(GapResults, caption = "Gap Closing Statistics for Diffeerent Gaps (2010-12-14:2024-08-19)")

# Write the LaTeX code to a .tex file
tex_file <- "combined_df_table.tex"
print(table, file = tex_file, type = "latex", include.rownames = FALSE)

# Compile the .tex file to a PDF using system call to pdflatex
system(paste("pdflatex", tex_file))

