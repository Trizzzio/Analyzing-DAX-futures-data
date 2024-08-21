# Analyzing-DAX-futures-data
This code uses historical DAX futures data (obtained through the proprietary Denaly Exchange Data Feed for Sierra Chart)  and looks for statistical anomalies/patterns 

Data Wrangling: Uploads Data, Cleans it and creates new variables

Data Exploration: Testing Script, just used to test some code

GapStatistics: Contains the function that used as input (dataset,type-of-gap, minimum range of gap, max range of gap) and outputs a dataframe containing the ratio of times the gap is closed vs. not closed for different time ranges (eg. before 11:30, before 14:30 and before 21:30)

10YearGapStats: Uploads and cleans the dataset containing data from 2010-2024 and creates a table with the aforementioned ratios for different types of gaps.


