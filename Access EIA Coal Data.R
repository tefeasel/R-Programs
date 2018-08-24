#Load Packages
packages <- c("dplyr", "jsonlite", "lubridate")

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

setwd("K:\\FCST\\FCST17_6+6\\long term\\auxillary\\coal\\EIA JSON Files")

#Montly energy review coal consumption

getUSCoal <- function(ID, name, key = "") {
        url <- paste("http://api.eia.gov/series?series_id=", ID, "&api_key=", key, "&out=json", sep="")
        
        if(!file.exists(paste(name, " coal consumption.json", sep=""))) {
                download.file(url, destfile = paste(name, " coal consumption.json", sep=""))
        }

        data <- fromJSON(paste(name, " coal consumption.json", sep=""))

        df <- as.data.frame(sapply(data$series, rbind))
        df2 <- select(df, data.1, data.2) %>%
                mutate(date = as.Date(paste(data.1, 1, sep = ""), format = "%Y%m%d"),
                        year = year(date),
                        month = month(date)) %>%
                arrange(year, month) %>%
                select(year, month, data.2)
        df2 <- rename_(df2, .dots = setNames("data.2", paste(match.call()[3], collapse=" ")))
}

#Get Data
residential <- getUSCoal("TOTAL.CLRCPUS.M", name = "cc_res")
commercial <- getUSCoal("TOTAL.CLCCPUS.M", name = "cc_com")
coke <- getUSCoal("TOTAL.CLKCPUS.M", name = "cc_coke")
industrial <- getUSCoal("TOTAL.CLOCPUS.M", name = "cc_ind")
electric <- getUSCoal("TOTAL.CLEIPUS.M", name = "cc_elec")
exports <- getUSCoal("TOTAL.CLEXPUS.M ", name = "exports")
usprod <- getUSCoal("TOTAL.CLPRPUS.M", name = "us prod")

all <- right_join(residential, commercial, by = c("year", "month")) %>%
        right_join(coke, by = c("year", "month")) %>%
        right_join(industrial, by = c("year", "month")) %>%
        right_join(electric, by = c("year", "month")) %>%
        right_join(exports, by = c("year", "month")) %>%
        right_join(usprod, by = c("year", "month")) %>%
        filter(year >= 1991)

#Set where final file should be saved

setwd("K:\\FCST\\FCST17_6+6\\long term\\auxillary\\coal")

#Save file

#write.csv(all, "consumption.csv", row.names = FALSE)
#Data is not tidy because it much match format previously used in order not to break other programs



