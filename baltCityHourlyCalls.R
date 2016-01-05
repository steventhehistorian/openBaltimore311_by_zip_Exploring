## Reshapes the original Baltimore City 311 data to include the department and agency requested in each observation in order to look at the distribution of demand for departments.
## Written by Steven Reddick, 1/5/2015.
## Last update, 1/5/2015.
library(dplyr)
library(ggplot2)
library(stringr)

setwd("C:/Users/sreddick/Desktop/311/baltimore_city_311_analysis")

        if(!exists("allBaltData")) {
                ## Download the dataset (originally obtained 1/4/2016).  Be patient, it's a big file.
                dlURLBaltData <- "https://data.baltimorecity.gov/api/views/9agw-sxsr/rows.csv?accessType=DOWNLOAD"
                download.file(dlURLBaltData, destfile = paste0(c("balt311"),gsub("-","",Sys.Date()),c(".csv")))
                allBaltData <- read.csv(paste0(c("balt311"),gsub("-","",Sys.Date()),c(".csv")),header = TRUE)
                allBaltData <- tbl_df(allBaltData)
                allBaltData <- mutate(allBaltData,createdDate = as.character(createdDate))
                
                        ## Create a new variable of the date of call in time format using "Lubridate".
                        allBaltData <- mutate(allBaltData, dateOfCall = mdy(substr(allBaltData$createdDate,1,10)))
                        ## Create a new variable of the month of call in text format.
                        allBaltData <- mutate(allBaltData, month = month(dateOfCall , label=TRUE))
                        
                                ## Create a new variable for time of the call
                                allBaltData <-  mutate(allBaltData,
                                                       timeOfCall = parse_date_time(substr(allBaltData$createdDate,1,22),
                                                                                    c('%m/%d/%Y %I:%M:%S %p')),
                                                       locale = Sys.getlocale("LC_TIME"))
                                ## Create a new variable of the hour of the call
                                        allBaltData <- mutate(allBaltData, Hour = as.factor(hour(timeOfCall)))
        }

                ## This is the .csv that contains the table of aggregated and unique code prefixes and their dept/agencies.  I compied the information in Excel based on independent research on the web.
                cityDeptCodePrefixAndDeptTable <- read.csv("cityDeptAgencyTable.csv", header = TRUE)
                ## Turn it into a tbl_df
                cityDeptCodePrefixAndDeptTable <- tbl_df(cityDeptCodePrefixAndDeptTable)
                
                
##<------- END SECTION -------->


                
                
                
## <----- Parsing the Agency codes from the phone codes.  Creating a new table once I discover what the call code initials mean. --->
        
        ## Group aggregate Baltimore 311
        callCodeTotals<- group_by(allBaltData,codeDescription) %>%
                summarise(count = n()) %>%
                arrange(codeDescription)
        
                ## Unique Baltimore 311 data by code description
                uniqueCallTypes <- group_by(allBaltData,codeDescription) %>%
                                        select(codeDescription) %>%
                                                unique()
               
        
                ## Make a dataframe from the unique call codes so we can parse out the agency in the next lines of code.
                uniqueCallTypes <- data.frame(uniqueCallTypes)
                        ## Finds the 1st word in the unique code vector, separated by "-" and then again by " ".                    
                        allCodePrefixes <-  unique(word(uniqueCallTypes$codeDescription, 1, sep = "-"))
                        allCodePrefixes <-  data.frame(unique(word(allCodePrefixes, 1, sep = " ")))
                        allCodePrefixes <- tbl_df(allCodePrefixes)
                                ## Creates the variable where I'll store the name of the actual agency that the city dept belongs to.  I plan to do this with a table join in dplyr.
                                allCodePrefixes$departmentAgency <-c("0")
                                ## Fixes the names up.
                                names(allCodePrefixes) <- c("abbreviation","departmentAgency")
                       
        
        ##<------- END SECTION -------->      
                


       
                                
                                
                                
                                
                                
                                
                                
############  How I went about merging/ joining tables. #########
                                
                                
        ## Creates a dummy small version of the big 311 dataset so I can test the table merge. 
        dummyOriginalData <- allBaltData[1:100,] 
                                        
                ## Uses word in the stringr package to parse the 1st word of the code description column and populate a new column with it so we can match it.                
                dummyOriginalData$departmentAgency <-  word(dummyOriginalData$codeDescription, 1, sep = "-")
                dummyOriginalData$departmentAgency <-  word(dummyOriginalData$departmentAgency, 1, sep = " ")
                dummyOriginalData$departmentAgency <- as.factor(dummyOriginalData$departmentAgency)
                
                        ## Attempting to merge the dummy data and the city code/department index tables.
                        dummyMergedTable_From_dummyOriginal_and_cityDeptCode <- left_join(dummyOriginalData, cityDeptCodePrefixAndDeptTable)
                
                        dummyMergedTable_From_dummyOriginal_and_cityDeptCode$cityDept
