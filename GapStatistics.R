library(dplyr)

##Creating a function that outputs statistics for different gaps

analyze_gap_closure <- function(data, gap_type = c("both", "positive", "negative"), gap_size_min = 10, gap_size_max=1500) {
  
  #Creating error output if gap type input is inputted wrong
  if(!gap_type %in% c("both", "positive", "negative")){
    stop("Invalid gap_type. Choose from 'both', 'positive' or 'negative'")
  }
  
  
  #Filter data based on gap type and size
  
  if (gap_type=="positive"){
    filtered_data=data%>%
      filter (Gap >=gap_size_min & Gap <= gap_size_max)%>%
      filter(Time>=as_hms("08:00:00"))%>%
      mutate(c1130 = if_else(Time <= as_hms("11:30:00") & Low<= ClosingPrice, 1, 0),
             c1430 = if_else(Time <= as_hms("14:30:00") & Low<= ClosingPrice, 1, 0),
             c2130 = if_else(Time <= as_hms("21:30:00") & Low<= ClosingPrice, 1, 0))%>%
      group_by(Date)%>%
      mutate(Before_1130=if_else(any(c1130==1),1,0),
             Before_1430=if_else(any(c1430==1),1,0),
             Before_2130=if_else(any(c2130==1),1,0))%>%
      ungroup()%>%
      select(-"c1130",-"c1430",-"c2130")
    } else if (gap_type=="negative"){
    filtered_data=datagap%>%
      filter (Gap <= -gap_size_min & Gap >=-gap_size_max )%>%
      filter(Time>=as_hms("08:00:00"))%>%
      mutate(c1130 = if_else(Time <= as_hms("11:30:00") & High>= ClosingPrice, 1, 0),
             c1430 = if_else(Time <= as_hms("14:30:00") & High>=  ClosingPrice, 1, 0),
             c2130 = if_else(Time <= as_hms("21:30:00") & High>=  ClosingPrice, 1, 0))%>%
      group_by(Date)%>%
      mutate(Before_1130=if_else(any(c1130==1),1,0),
             Before_1430=if_else(any(c1430==1),1,0),
             Before_2130=if_else(any(c2130==1),1,0))%>%
      ungroup()%>%
      select(-"c1130",-"c1430",-"c2130")
  }else {
    filtered_data=data%>%
      filter(abs(Gap) >= gap_size_min & abs(Gap) <= gap_size_max)%>%
      filter(Time>=as_hms("08:00:00"))%>%
      group_by(Date)%>%
      mutate(c11=case_when(Time <= as_hms("11:30:00") & Gap >0 & Low <= ClosingPrice ~ TRUE,
                           Time <= as_hms("11:30:00") & Gap <0 & High >= ClosingPrice ~ TRUE,
                           TRUE ~ FALSE),
             c14=case_when(Time <= as_hms("14:30:00") & Gap >0 & Low <= ClosingPrice ~ TRUE,
                           Time <= as_hms("14:30:00") & Gap <0 & High >= ClosingPrice ~ TRUE,
                           TRUE ~ FALSE),
             c21=case_when(Time <= as_hms("21:30:00") & Gap >0 & Low <= ClosingPrice ~ TRUE,
                           Time <= as_hms("21:30:00") & Gap <0 & High >= ClosingPrice ~ TRUE,
                           TRUE ~ FALSE))%>%
       mutate(c1130=if_else(c11==TRUE,1,0),
             c1430=if_else(c14==TRUE,1,0),
             c2130=if_else(c21==TRUE,1,0)
      )%>%
      mutate(Before_1130=if_else(any(c1130==1),1,0),
             Before_1430=if_else(any(c1430==1),1,0),
             Before_2130=if_else(any(c2130==1),1,0))%>%
      ungroup()%>%
      select(-"c1130",-"c1430",-"c2130",-"c11",-"c14",-"c21")
  }

    
  
  #Create Summary and Ratios
  
  gap_string <- paste(gap_type, gap_size_min, gap_size_max)
  
  Results_df<-filtered_data%>%
    group_by(Date) %>%
    summarize(
      Date=first(Date),
      Gap=first(Gap),
      Before_1130=first(Before_1130),
      Before_1430=first(Before_1430),
      Before_2130=first(Before_2130)
    )
  
  
  result <- data.frame(
    Gap = gap_string,
    `Total Days` = nrow(Results_df),
    `Gap Closed b/1130` = sum(Results_df$Before_1130)/nrow(Results_df),
    `Gap Closed b/1430` = sum(Results_df$Before_1430)/nrow(Results_df),
    `Gap Closed b/2130` = sum(Results_df$Before_2130)/nrow(Results_df),
    stringsAsFactors = FALSE
  )
  
  print(result)
  
  
  }
