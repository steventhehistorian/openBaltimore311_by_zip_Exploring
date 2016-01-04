library(dplyr)
library(lubridate)
library(ggplot2)


        setwd("C:/Users/sreddick/Desktop/311")
        
        ## Download the dataset (originally obtained 1/4/2016).
        dlURLBaltData <- "https://data.baltimorecity.gov/api/views/9agw-sxsr/rows.csv?accessType=DOWNLOAD"
        download.file(dlURLBaltData, destfile = paste0(c("balt311"),gsub("-","",Sys.Date()),c(".csv")))        
        
        ## Load the data from .csv
        allBaltData <- read.csv("balt31120160104.csv",header = TRUE)
        
                ## Use dplyr to wrap df in a tbl for dplyr SQL commands.
                allBaltData <- tbl_df(allBaltData)
                allBaltData <- mutate(allBaltData,createdDate = as.character(createdDate))

                        ## Create a new variable of the date of call in time format using "Lubridate".
                        allBaltData <- mutate(allBaltData, dateOfCall <- mdy(substr(allBaltData$createdDate,1,10)))
                        ## Create a new variable of the month of call in text format.
                        allBaltData <- mutate(allBaltData, month = month(dateOfCall , label=TRUE))
                        
                        ## Create a new variable for time of the call
                        allBaltData <-  mutate(allBaltData,
                                               timeOfCall = parse_date_time(substr(allBaltData$createdDate,1,22),
                                                c('%m/%d/%Y %I:%M:%S %p')),
                                               locale = Sys.getlocale("LC_TIME"))
                        ## Create a new variable of the hour of the call
                        allBaltData <- mutate(allBaltData, Hour = as.factor(hour(timeOfCall)))

                        
        
# <-------- Begin plotting total 2014 calls by dept ---------->
        
        ## Group data by code description
        by_callType <- group_by(allBaltData,codeDescription)
        
                ## Summarize to create 2 columns, 1. the call type and 2. total call counts for each type.
                activitiesTotal <- summarise(by_callType,
                                                count = n())
        
                        ## Filter out calls less than some threshold and sort decending.   
                        allBaltRequestsDescending <- filter(arrange(activitiesTotal,desc(count)),count > 10000)
        
                                ## Plot it using ggplot2
                                ggplot(allBaltRequestsDescending, aes(codeDescription, count)) +
                                        geom_point(aes(size = count, colour = count), alpha = 1/2) +
                                        geom_smooth() +
                                        scale_size_area(max_size = 12) + 
                                        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15)) + 
                                        ggtitle("Baltimore City 311 Calls, 2014") + 
                                        theme(plot.title = element_text(lineheight=.8, face="bold"))
        
        
        #@ <-------- End plotting total 2014 calls by dept ---------->

                                
                                
                                
        by_callTypeAndMonth <- group_by(allBaltData,codeDescription,month)
               
        
        by_Month <- group_by(allBaltData,month)
        
        
        activitiesByMonth <- summarise(by_callTypeAndMonth,
                                         count = n())
        
        filter(activitiesByMonth, grep("Rat" ,codeDescription, value = TRUE))
        
        

        
        
##  <--------------- Plotting total monthly calls, monthly "rat" calls, and calls without the "rat" queue.  --------------->
        
        
        ratRuboutCalls <- activitiesByMonth[activitiesByMonth$codeDescription =="SW-Rat Rubout - Proactive",]
        
        
        ratsdf <- data.frame(ratRuboutCalls[,3],callsOverMonths$count)
        plot(ratsdf[,1],)
        
        
                
                # Balt. co calls from targeted extensions, June through November. 
                baltCountyMonthlyTotals <- c(0,0,0,0,0,42029, 55977, 44124, 45981, 36453, 32762,0 )
                
                
                        ggplot(callsOverMonths, aes(month, count)) +
                                geom_point(aes(size = count, colour = count), alpha = 1/2) +
                                #geom_point(aes(y=ratsdf[,1], size = ratsdf[,1])) +
                                #geom_point(aes(y = callsOverMonths$count-ratsdf[,1])) +
                                geom_point(aes(y = baltCountyMonthlyTotals, size = baltCountyMonthlyTotals)) +
                                geom_smooth() +
                                scale_size_area(max_size = 12) + 
                                theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15)) + 
                                ggtitle("Baltimore City Monthly 311 Calls, 2014") + 
                                theme(plot.title = element_text(lineheight=.8, face="bold")) + 
               
                                
                                ### Revist if I have time: just for aesthetics.
                                
                                 #                 scale_colour_discrete(name  ="Government",
                #                                       breaks=c("Baltimore City", "Baltimore "),
                #                                       labels=c("Woman", "Man")) +
                #                 scale_shape_discrete(name  ="Payer",
                #                                      breaks=c("Female", "Male"),
                #                                      labels=c("Woman", "Man"))   
                

#                                         callsOverMonths <- summarise(by_Month,
#                                                                      count = n())
#         
#                                                 plot(callsOverMonths, type = "l", col = "red", ylim = c(15000,90000))
#                                                 abline(ratRuboutCalls, col = "blue")
        
####  <--------------- END SECTION --------->      
                                      
                                                
                                                          
                                                
        activitiesByMonthFiltered <- filter(activitiesByMonth, count > 2000)
        View(activitiesByMonth)
        print(activitiesByMonthFiltered, n = 100)
        table(activitiesByMonthFiltered)
        table(activitiesByMonthFiltered$codeDescription)
        
        mean(activitiesByMonthFiltered$count)

        
        ggplot(activitiesByMonthFiltered, aes(codeDescription, count)) +
                geom_point(aes(size = count), alpha = 1/2) +
                geom_smooth() +
                scale_size_area() + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        
        head(activitiesByMonthFiltered)
        

        month(allBaltData[,17])
        
        
        
        ###################################
        
#### <--------  Plot balt city total month/hour calls broken down by internet/phone methods. Creates datatables and subsets used in later code. -------- >
        
        by_callTypeAndHourAndMonth <- group_by(allBaltData,codeDescription,month,Hour, methodReceived)
                callsOverMonths <- summarise(by_callTypeAndHourAndMonth,
                                     count = n())

                        phones <- filter(callsOverMonths, methodReceived =="Phone",month=="Jun" | month=="Jul" | month=="Aug"|month=="Sep" |month=="Oct"|month=="Nov")
                        internets <- filter(callsOverMonths, methodReceived =="Internet",month=="Jun" | month=="Jul" | month=="Aug"|month=="Sep" |month=="Oct"|month=="Nov")
                        spot311 <- filter(callsOverMonths, methodReceived =="Spot 311",month=="Jun" | month=="Jul" | month=="Aug"|month=="Sep" |month=="Oct"|month=="Nov")
        
                                plot(spot311[,c(3,5)], main = "spot311")
                                plot(internets[,c(3,5)], main = "Internet")
                                plot(phones[,c(3,5)],main = "Phone")
        
        
        
#### <-------- END SECTION --------------> 
        
        
                                
#### <-------- Balt City Zip code/ populaiton/income and 311 contact methods.  Are there correlations? --------------> 
                                
                                
        ## File location for the 2010 census, income/pop by Balt city zip. 
                                
        #dlURL <- url("https://data.baltimorecity.gov/api/views/cix3-h4cy/rows.csv?accessType=DOWNLOAD")
        download.file(baltNeighborhoodCensus10, destfile = paste0(c("baltNeighborhoodCensus10"),gsub("-","",Sys.Date()),c(".csv")))      
        baltNeighborhoodCensus10 <- read.csv(paste0(c("baltNeighborhoodCensus10"),gsub("-","",Sys.Date()),c(".csv")), header = TRUE)                
        
        # 2 additional data sets with Zip data.
        zipcsv <-  read.csv("baltimoreCityZipIncomeAndCalls.csv", header = TRUE)
        baltZip <- read.csv("baltIncomeByZip.csv", header = TRUE)  
        
        # Tidying the dataset
        baltNeighborhoodCensus10 <- tbl_df(baltNeighborhoodCensus10)
        baltNeighborhoodCensus10 <- select(baltNeighborhoodCensus10, CSA2010 , totalpop10 , mhhi10)
        baltNeighborhoodCensus10 <- rename(baltNeighborhoodCensus10, Neighborhood = CSA2010)
        
        
                by_Zip <- group_by(allBaltData,zip)
                
                        methods <- tbl_df(data.frame(table(by_Zip$methodReceived , by_Zip$zip)))
        
                        zipAggregate <- summarise(by_Zip,
                                  count = n()) %>%
                                filter(count > 1000, methodReceived == "Internet" | methodReceived == "spot311"))
                                
                                        


                #         by_Zip %>%
                #         group_by(methodReceived) %>%
                #                 select(methodReceived) %>%
                #                 arrange(desc(count))
                        
                        
                        ## Opted out of neighborhoods since the data is too mis-matched.  Went with zips because the data was available.
                        
                #         by_Neighborhood <- group_by(allBaltData,Neighborhood)
                        
                #         View(summarise(by_Neighborhood, 
                #                   count = n()) %>%
                #                 filter(count > 1000)
                #         )
        
                                
        

       baltNeigh <- allBaltData %>%
         arrange(Neighborhood) %>%
                select (Neighborhood) %>%
                unique %>%
                bind_rows(baltNeighborhoodCensus10[,1]) %>%
               
               
       ##On the 1st run thru, I saved this to .csv and manually matched zip codes & data in Excel.  I later wrote better code that does it for you. 
               
       ## Turn baltZip into a tabledf
       baltZip <- tbl_df(baltZip)
       ## Select the important columns
        baltZip <- select(baltZip, Zip.Code, Population, mhhi)
        ## Make zipcode a factor and rename it
        baltZip <- mutate(baltZip , Zip.Code = as.factor(Zip.Code))
        baltZip <- rename(baltZip , zip = Zip.Code)
        ## Create columns in both tables so they can merge (Note that the dummy values will all be NA's when the tables merge.)
        baltZip$count <- rep(0,nrow(baltZip))  ## 
        zipAggregate$Population <- rep(0,nrow(zipAggregate))
        zipAggregate$mhhi <- rep(0,nrow(zipAggregate))
        ## Make a new table called baltZipTable using 
        baltZipTable <- bind_rows(baltZip,zipAggregate)
        baltZipTable <- baltZipTable %>%
                        group_by(zip) %>%
                        arrange(desc(count))
       
        ##<---- END SECTION. -------->
        
## <-------- Merging 2 different zip code tables. () Take 2 different data tables and SQL merge using dplyr) -------->
        
        ## Create a df from the saved zip/income csv file that I manually edited.
        #plot(zipcsv[zipcsv$mhhi > 0 ,c(1,2,3,4)])
        zipAndIncomeBaltimore <- zipcsv[zipcsv$mhhi > 0 ,c(1,2,3,4)]
        zipAndIncomeBaltimore <- mutate(zipAndIncomeBaltimore , zip = as.factor(zip))
        
                ## Take balt city 311 zip data and isolate it down to the internet methods of contact. 
                zips2 <-methods %>% 
                        arrange(desc(Freq) ) %>%
                        filter(Var1 =="Internet" | Var1 =="spot311" | Var1 =="Mobile Apps") %>%
                        arrange(desc(Freq) )
                
                        ## Get rid of generic names and replace with meaningful ones. ("zip" is the key value.)
                        zips2 <- rename(zips2, contactMethod = Var1, zip = Var2 )
                        
        
                                ## Join tables (SQL) using dplyr's "right_join"
                                baltContactMethodAndZip <- na.omit(right_join(zipAndIncomeBaltimore, zips2, by="zip"))

        ### <---- END SECTION --------#  
        

                                
                                
        
## <-------- # Create the list of total internet-type contacts by balt city zip. (i.e. 1 value for each zip) And Plot. -------->       
                                
        #
        ## You have to run these two consecutively...
        allInternetByZip <- baltContactMethodAndZip %>%
                mutate(zip = as.factor(zip)) %>%
                group_by(zip,Freq)
        allInternetByZip <-  summarize(allInternetByZip,
                count = sum(Freq))

                               
                        ## Plot scatterplot colored by zip, sized by population, of contact method and # of contacts.
                        
                        ggplot(baltContactMethodAndZip, aes(x = mhhi, y = Freq)) + 
                                geom_point(aes(color = factor(zip), size = Population)) +
                                geom_smooth() +
                                scale_size_area() + 
                                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                ggtitle("Baltimore City 311 Internet/Mobile Requests by Income")
                        
        ### <---- END SECTION --------# 
        
################
