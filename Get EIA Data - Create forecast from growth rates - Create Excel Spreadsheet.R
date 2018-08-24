# Note: You may have to download a Java Developer kit from APS for any writing to work
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_51\\jre")

#Load Packages
packages <- c("dplyr", "jsonlite", "lubridate", "XLConnect", "xlsx", "data.table", "zoo")

install <- function(packages){ 
        for(i in packages) {
                if(!(i %in% installed.packages()[, "Package"])) {
                        pack <- i
                        install.packages(pack, dependencies = TRUE)
                }
                else 
                        print(paste(i, "is already installed", sep = " "))
        }
}

install(packages) #Run install function
lapply(packages, require, character.only = TRUE)

#Set where JSON files should be saved

setwd("\\EIA JSON Gas Price Files\\")

#Montly energy review coal consumption

getUSNatGasPrices <- function(res, com, ind, state, name, key = "") {
        
        #Residential
        url <- paste("http://api.eia.gov/series?series_id=", res, "&api_key=", key, "&out=json", sep="")
        
        setwd("\\EIA JSON Gas Price Files\\")
        
        if(!file.exists(paste(state, " res gas prices.json", sep=""))) {
                download.file(url, destfile = paste(state, " res gas prices.json", sep=""))
        }

        data <- fromJSON(paste(state, " res gas prices.json", sep=""))

        df <- as.data.frame(sapply(data$series, rbind))
        res <- select(df, data.1, data.2) %>%
                mutate(date = as.Date(paste(data.1, 1, sep = ""), format = "%Y%m%d"),
                        year = year(date),
                        month = month(date)) %>%
                arrange(year, month) %>%
                select(year, month, data.2)
        res <- rename_(res, .dots = setNames("data.2", paste("pr_", match.call()[6], collapse=" ")))
        
        #Commercial
        url <- paste("http://api.eia.gov/series?series_id=", com, "&api_key=", key, "&out=json", sep="")

        if(!file.exists(paste(state, " com gas prices.json", sep=""))) {
                download.file(url, destfile = paste(state, " com gas prices.json", sep=""))
        }

        data <- fromJSON(paste(state, " com gas prices.json", sep=""))

        df <- as.data.frame(sapply(data$series, rbind))
        com <- select(df, data.1, data.2) %>%
                mutate(date = as.Date(paste(data.1, 1, sep = ""), format = "%Y%m%d"),
                       year = year(date),
                       month = month(date)) %>%
                arrange(year, month) %>%
                select(year, month, data.2)
        com <- rename_(com, .dots = setNames("data.2", paste("pc_", match.call()[6], collapse=" ")))
        
        # #Industrial
        url <- paste("http://api.eia.gov/series?series_id=", ind, "&api_key=", key, "&out=json", sep="")

        if(!file.exists(paste(state, " ind gas prices.json", sep=""))) {
                download.file(url, destfile = paste(state, " ind gas prices.json", sep=""))
        }

        data <- fromJSON(paste(state, " ind gas prices.json", sep=""))

        df <- as.data.frame(sapply(data$series, rbind))
        ind <- select(df, data.1, data.2) %>%
                mutate(date = as.Date(paste(data.1, 1, sep = ""), format = "%Y%m%d"),
                       year = year(date),
                       month = month(date)) %>%
                arrange(year, month) %>%
                select(year, month, data.2)
        ind <- rename_(ind, .dots = setNames("data.2", paste("pi_", match.call()[6], collapse=" ")))

        #Combine
        all <- full_join(res, com, by = c("year", "month")) %>%
                full_join(ind, by = c("year", "month"))
        
        setwd("\\natural gas\\state\\")
        write.xlsx(all, file = paste(state, ".xls", sep=""), row.names = FALSE)
}

#Get Data
arkansas <- getUSNatGasPrices(res = "NG.N3010AR3.M", com = "NG.N3020AR3.M", ind = "NG.N3035AR3.M", state = "arkansas", name = "ar")
indiana <- getUSNatGasPrices(res = "NG.N3010IN3.M", com = "NG.N3020IN3.M", ind = "NG.N3035IN3.M", state = "indiana", name = "in")
kentucky <- getUSNatGasPrices(res = "NG.N3010KY3.M", com = "NG.N3020KY3.M", ind = "NG.N3035KY3.M", state = "kentucky", name = "ky")
louisiana <- getUSNatGasPrices(res = "NG.N3010LA3.M", com = "NG.N3020LA3.M", ind = "NG.N3035LA3.M", state = "louisiana", name = "la")
michigan <- getUSNatGasPrices(res = "NG.N3010MI3.M", com = "NG.N3020MI3.M", ind = "NG.N3035MI3.M", state = "michigan", name = "mi")
ohio <- getUSNatGasPrices(res = "NG.N3010OH3.M", com = "NG.N3020OH3.M", ind = "NG.N3035OH3.M", state = "ohio", name = "oh")
oklahoma <- getUSNatGasPrices(res = "NG.N3010OK3.M", com = "NG.N3020OK3.M", ind = "NG.N3035OK3.M", state = "oklahoma", name = "ok")
tennessee <- getUSNatGasPrices(res = "NG.N3010TN3.M", com = "NG.N3020TN3.M", ind = "NG.N3035TN3.M", state = "tennessee", name = "tn")
texas <- getUSNatGasPrices(res = "NG.N3010TX3.M", com = "NG.N3020TX3.M", ind = "NG.N3035TX3.M", state = "texas", name = "tx")
virginia <- getUSNatGasPrices(res = "NG.N3010VA3.M", com = "NG.N3020VA3.M", ind = "NG.N3035VA3.M", state = "virginia", name = "va")
west_virginia <- getUSNatGasPrices(res = "NG.N3010WV3.M", com = "NG.N3020WV3.M", ind = "NG.N3035WV3.M", state = "west virginia", name = "wv")


#Write to a compiled file
setwd("\\auxillary\\natural gas\\")

sheets <- c("ark", "ind", "ky", "la", "mi", "oh", "ok", "tn", "tx", "va", "wv")
states <- c("arkansas", "indiana", "kentucky", "louisiana", "michigan", "ohio", "oklahoma", "tennessee", "texas", "virginia", "west virginia")
east <- c("indiana", "kentucky", "michigan", "ohio", "tennessee", "virginia", "west virginia")

#Save final file
wb <- createWorkbook()
j <- 0
lastact <- 2017

for(x in states) {
        print(x)
        j <- j + 1
        
        z <- sheets[[j]]
        print(z)
        
        #Bring in latest data
        
        sheet <- createSheet(wb, sheetName = paste(x))
        df <- read.xlsx(file = paste("\\natural gas\\state\\", x, ".xls", sep = ""), 1)
        names(df) <- gsub(".", "", names(df), fixed = TRUE) #Remove period from column names
        df <- filter(df, year >= 2001)
        
        #Fix missing values and convert from factor to numeric (inner character conversion keeps decimal point):
        df[[3]] <- as.numeric(as.character(df[[3]]))
        df[[3]] <- ifelse(is.na(df[,3]), (lag(df[,3], 1) + lag(df[,3], 2))/2, df[,3])
        df[[4]] <- as.numeric(as.character(df[[4]]))
        df[[4]] <- ifelse(is.na(df[,4]), (lag(df[,4], 1) + lag(df[,4], 2))/2, df[,4])
        df[[5]] <- as.numeric(as.character(df[[5]]))
        df[[5]] <- ifelse(is.na(df[,5]), (lag(df[,5], 1) + lag(df[,5], 2))/2, df[,5])
        
        #Bring in historical data
        old <- read.xlsx(file = paste("natural gas\\stategasprice.xls", sep = ""), sheetName = paste(z), 
                         colIndex = 1:4)
        old$year <- year(old$date)
        old$month <- month(old$date)
        old <- select(old, -date)
        old <- old[c(4, 5, 1, 2, 3)]
        old <- filter(old, year < 2001)
        
        #Fix missing values and convert from factor to numeric (inner character conversion keeps decimal point):
        old[[3]] <- as.numeric(as.character(old[[3]]))
        old[[3]] <- ifelse(is.na(old[,3]), (lag(old[,3], 1) + lag(old[,3], 2))/2, old[,3])
        old[[4]] <- as.numeric(as.character(old[[4]]))
        old[[4]] <- ifelse(is.na(old[,4]), (lag(old[,4], 1) + lag(old[,4], 2))/2, old[,4])
        old[[5]] <- as.numeric(as.character(old[[5]]))
        old[[5]] <- ifelse(is.na(old[,5]), (lag(old[,5], 1) + lag(old[,5], 2))/2, old[,5])
        
        #Create empty data frame
        date <- seq(as.Date("2017-11-01"), as.Date("2048-12-31"), 1)
        fcst <- data.frame(date)
        fcst$year <- year(fcst$date)
        fcst$month <- month(fcst$date)
        fcst <- select(fcst, -date) %>%
                distinct(year, month)
        
        #Concatenate the two datasets (new forecast and historical data)
        df2 <- bind_rows(old, df)
        df2 <- bind_rows(df2, fcst)
        df2 <- arrange(df2, year, month)
        
        #Final dataset with fcst values filled in using growth rates
        
        #East growth rates (key growth rates in the ifelse statement)
        if(x %in% east) {
                for (row in 2:dim(df2)) {
                        df2[row, 3] <- ifelse(df2$year == lastact & is.na(df2[,3]), lag(df2[,3], 12) * .9907, df2[,3])[row] #Res
                        df2[row, 4] <- ifelse(df2$year == lastact & is.na(df2[,4]), lag(df2[,4], 12) * .8646, df2[,4])[row] #Com
                        df2[row, 5] <- ifelse(df2$year == lastact & is.na(df2[,5]), lag(df2[,5], 12) * .9164, df2[,5])[row] #Ind
                        df2[row, 3] <- ifelse(df2$year > lastact & is.na(df2[,3]), lag(df2[,3], 12) * 1.0314, df2[,3])[row] #Res
                        df2[row, 4] <- ifelse(df2$year > lastact & is.na(df2[,4]), lag(df2[,4], 12) * 1.0386, df2[,4])[row] #Com
                        df2[row, 5] <- ifelse(df2$year > lastact & is.na(df2[,5]), lag(df2[,5], 12) * 1.0367, df2[,5])[row] #Ind
                }
        }
        
        #West growth rates (key growth rates in the ifelse statement)
        else 

                for (row in 2:dim(df2)) {
                        df2[row, 3] <- ifelse(df2$year == lastact & is.na(df2[,3]), lag(df2[,3], 12) * .9649, df2[,3])[row] #Res
                        df2[row, 4] <- ifelse(df2$year == lastact & is.na(df2[,4]), lag(df2[,4], 12) * .9199, df2[,4])[row] #Com
                        df2[row, 5] <- ifelse(df2$year == lastact & is.na(df2[,5]), lag(df2[,5], 12) * .9823, df2[,5])[row] #Ind
                        df2[row, 3] <- ifelse(df2$year > lastact & is.na(df2[,3]), lag(df2[,3], 12) * 1.031189, df2[,3])[row] #Res
                        df2[row, 4] <- ifelse(df2$year > lastact & is.na(df2[,4]), lag(df2[,4], 12) * 1.039538, df2[,4])[row] #Com
                        df2[row, 5] <- ifelse(df2$year > lastact & is.na(df2[,5]), lag(df2[,5], 12) * 1.0497, df2[,5])[row] #Ind
                }
        
        addDataFrame(df2, sheet, startRow=1, startColumn=1, row.names = FALSE)
}

saveWorkbook(wb, "stategasprices.xls")