library(dplyr)

##Checking how many times within the dataset a gap between previous day close and current day open is being closed

datagap=data%>%
  mutate(Gap= CurrentDayOpen-ClosingPrice,
         GapMagnitude=abs(Gap))%>%
  mutate(IsGapGreaterThan10=GapMagnitude>=20)

#Check if gap has been closed during the day

datagap = datagap%>%
  filter(Time>=as_hms("08:00:00"))%>%
group_by(Date) %>%
  group_by(Date)%>%
  mutate(GapClosed=case_when(IsGapGreaterThan10 & Gap >0 &Low <= ClosingPrice ~ TRUE,
                             IsGapGreaterThan10 & Gap <0 &High >= ClosingPrice ~ TRUE,
                             TRUE ~ FALSE))%>% ungroup()
  

##Summarizing


Gap_Results = datagap %>%
  group_by(Date) %>%
  summarize(
    ClosingPrice=first(ClosingPrice),
    CurrentDayOpen = first(CurrentDayOpen),
    Gap=first(Gap),
    IsGapGreaterThan10=first(IsGapGreaterThan10),
    GapClosed=any(GapClosed)
  )



Gap_Results=Gap_Results%>%
  mutate(GapInd=if_else(GapClosed & IsGapGreaterThan10 , 1, 0))%>%
  mutate(noGap=if_else(IsGapGreaterThan10,0,1))

SumGapClosed=sum(Gap_Results$GapInd)
SumNoGap=sum(Gap_Results$noGap,na.rm=TRUE)
SumGapClosed/(248-SumNoGap)
