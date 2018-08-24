install.packages("rnoaa")
library(rnoaa)
library(dplyr)
library(lubridate)

options(noaakey = "")

min <- data.frame()

for(i in 1950:2016) {

minout <- ncdc(datasetid='GHCND', datatypeid = "TMIN", stationid='GHCND:USW00013977', startdate = paste(i, "-01-01", sep = ""), enddate = paste(i, "-12-31", sep = ""), limit=366)

data <- head(minout$data, 366)

min <- rbind(min, data)

}


max <- data.frame()

for(i in 1950:2016) {
        
        maxout <- ncdc(datasetid='GHCND', datatypeid = "TMAX", stationid='GHCND:USW00013977', startdate = paste(i, "-01-01", sep = ""), enddate = paste(i, "-12-31", sep = ""), limit=366)
        
        data <- head(maxout$data, 366)
        
        max <- rbind(max, data)
        
}

min <- rename(min, min = value)

min <- select(min, date, station, min)
max <- rename(max, max = value) %>%
        select(date, station, max)

problem1 <- setdiff(max$date, min$date)
problem2 <- setdiff(min$date, max$date)

combined <- full_join(min, max, by = c("date", "station"))

combined2 <- mutate(combined, year = year(date), month = month(date), day = day(date),
                    max = ((max/10)*1.8) + 32, min = ((min/10)*1.8) + 32, avg = (max+min)/2,
                    cdd_base65 = avg - 65, hdd_base55 = 55 - avg,
                    cdd_base65 = ifelse(cdd_base65 < 0 , 0, cdd_base65),
                    hdd_base55 = ifelse(hdd_base55 < 0 , 0, hdd_base55)) %>%
             select(date, year, month, day, station, min, max, avg, cdd_base65, hdd_base55) %>%
             mutate(flag = ifelse(date %in% c(problem1, problem2), 1, 0)) %>%
             arrange(date)

sum(combined2$flag == 1)

write.csv(combined2, "\\Texarkana Ark Weather Data.csv")

