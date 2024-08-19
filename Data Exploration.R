library(dplyr)

##Checking how many times within the dataset a gap between previous day close and current day open is being closed

datagap=data%>%
  mutate(Gap= CurrentDayOpen-ClosingPrice,
         GapMagnitude=abs(Gap))%>%
  mutate(IsGapGreaterThan10=GapMagnitude>10)

#Check if gap has been closed during the day