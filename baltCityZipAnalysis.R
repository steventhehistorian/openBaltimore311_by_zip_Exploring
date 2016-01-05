## Second attempt at making a more fluid way of joining the 2 tables containins information about Baltimore City zip codes.
## Written by Steven Reddick, 1/5/2015.
## Last update, 1/5/2015.

library(dplyr)
library(ggplot2)

        if(!exists("allBaltData")) {
                ## Download the dataset (originally obtained 1/4/2016).  Be patient, it's a big file.
                dlURLBaltData <- "https://data.baltimorecity.gov/api/views/9agw-sxsr/rows.csv?accessType=DOWNLOAD"
                        download.file(dlURLBaltData, destfile = paste0(c("balt311"),gsub("-","",Sys.Date()),c(".csv")))
                                allBaltData <- read.csv(paste0(c("balt311"),gsub("-","",Sys.Date()),c(".csv")),header = TRUE)
                                        allBaltData <- tbl_df(allBaltData)
        }

## <---- This section of code load the data and creates the 3 tables that you can use to analyze the population & income by zip code and how that relates to Baltimore City 311 contacts.  


# Data saved to .csv in working directory from "http://zipatlas.com/us/md/baltimore/zip-code-comparison/median-household-income.htm"
baltZip <- read.csv("baltIncomeByZip.csv", header = TRUE) 
        ## Turn baltZip into a tabledf
        baltZip <- tbl_df(baltZip)
                ## Select the important columns
                baltZip <- select(baltZip, Zip.Code, Population, mhhi)
                        ## Make zipcode a factor and rename it so we can join tables by the zip" variable.
                        baltZip <- mutate(baltZip , Zip.Code = as.factor(Zip.Code))
                                baltZip <- rename(baltZip , zip = Zip.Code)



## Takes the full Baltimore city 311 Dataset and groups by zipcode. 
by_Zip <- group_by(allBaltData,zip)
        ## Creates a new table which adds up all of the instances of contact methods from each zip code.
        contactMethodByZipTable <- tbl_df(data.frame(table(by_Zip$methodReceived , by_Zip$zip)))
                ## Rename the variables so they will sync when the tables merge.
                contactMethodByZipTable <- rename(contactMethodByZipTable, contactMethod = Var1, zip = Var2 )
                        ## Filter down to e-contact only.  Note that you could omit the filter if you wanted to look at phone contacts, too.
                        contactMethodByZipTable <- contactMethodByZipTable %>% 
                                arrange(desc(Freq) ) %>%
                                filter(contactMethod =="Internet" | contactMethod =="spot311" | contactMethod =="Mobile Apps")

                                ## dlpyr right_join: table x, table y... omit rows where x!=y.  Wrap in a tbl_df for better prrocessing.
                                zipBalt311AndPopIncomeDf <- tbl_df(na.omit(right_join(baltZip, contactMethodByZipTable, by="zip")))
                                        ## Turn "zip" back into a factor (the table join coerces the variable into a character... This fixes it).
                                        zipBalt311AndPopIncomeDf <- zipBalt311AndPopIncomeDf %>%
                                                                        mutate(zip = as.factor(zip))

                        ## Sums up all the non-phone contacts by zip in case you want to work with aggregated data.  Note: you have to run all 3 of these commands to get the non-duplicated values.
                        sumZipBalt311AndPopIncomeDf <-zipBalt311AndPopIncomeDf %>%
                                mutate(zip = as.factor(zip)) %>%
                                group_by(zip,Freq)
                                sumZipBalt311AndPopIncomeDf <- select(sumZipBalt311AndPopIncomeDf,Population,  mhhi, Freq)
                                        sumZipBalt311AndPopIncomeDf <-  group_by(sumZipBalt311AndPopIncomeDf, zip) %>%
                                                                summarize(
                                                                        Freq = sum(Freq),
                                                                        Population = mean(Population),
                                                                        mhhi = mean(mhhi)) %>%
                                                                                unique()
### <---- END DATA GROOMING SECTION ------->



## <------ Creates a plot of zipcode population, income, and method of contact. 

### Plot the methods of non-phone contacts by zip
ggplot(zipBalt311AndPopIncomeDf, aes(x = mhhi, y = Freq)) + 
        geom_point(aes(color = factor(zip), size = Population, shape = factor(contactMethod))) +
        guides(colour = guide_legend(override.aes = list(size=10))) +
        geom_smooth() +
        scale_size_area() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("Baltimore City 311 Internet/Mobile Requests by Income")



### Plot the total non-phone contacts by zip
ggplot(sumZipBalt311AndPopIncomeDf, aes(x = mhhi, y = Freq)) + 
        geom_point(aes(color = factor(zip), size = Population)) +
        guides(colour = guide_legend(override.aes = list(size=10))) +
        geom_smooth() +
        scale_size_area() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("Aggregate Baltimore City 311 Internet/Mobile Requests by Income")




