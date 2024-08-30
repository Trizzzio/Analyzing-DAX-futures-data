library(tidyverse)
library(dplyr)
library(lubridate)
library(hms)
library(xtable)


#Calculating trending stats

#create Column names
column_names <- c("Date", "Time", "Open", "High", "Low", "Last", "Volume", "# of Trades", 
                  "OHLC Avg", "HLC Avg", "HL Avg", "Bid Volume", "Ask Volume")

data = read_delim("./F.US.DDU24(5000Days).txt", delim = ",", col_names=column_names,skip=1 )


rm(column_names)
