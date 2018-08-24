library(dygraphs)
library(haven)
library(xts)
library(dplyr)
library(tidyr)
library(htmlwidgets)
library(rmarkdown)

options(scipen = 999)

data <- read_sas("")
data <- data[data$YEAR > 2000,]

data$pubdate <-as.Date(data$pubdate, origin = "1960-01-01")
data$qtr_year <-as.Date(data$qtr_year, origin = "1960-01-01")

data_new_chg <- data[data$pubdate == "2016-11-01" & !(is.na(data$change)),]
data_new_chg <- select(data_new_chg, JURIS, STATE, var, Description, qtr_year, change) %>%
                rename(new_chg = change)
data_old_chg <- data[data$pubdate == "2015-12-01" & !(is.na(data$change)),]
data_old_chg <- select(data_old_chg, JURIS, STATE, var, Description, qtr_year, change) %>%
                 rename(old_chg = change)

data_new_lvl <- data[data$pubdate == "2016-11-01" & !(is.na(data$levels)),]
data_new_lvl <- select(data_new_lvl, JURIS, STATE, var, Description, qtr_year, levels) %>%
                rename(new_level = levels)
data_old_lvl <- data[data$pubdate == "2015-12-01" & !(is.na(data$levels)),]
data_old_lvl <- select(data_old_lvl, JURIS, STATE, var, Description, qtr_year, levels) %>%
                rename(old_level = levels)

all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(data_new_chg, data_old_chg, data_new_lvl, data_old_lvl))
all$juris <- paste(all$JURIS, all$STATE, sep = " - ")
all <- select(all, -JURIS, -STATE, -var)

jur <- as.list(unique(all$juris))
juris_list <- jur <- as.list(unique(all$juris))
var <- as.list(unique(all$Description))

#If wanting only one juris, overwrite jur here:
jur <- ""

#Graphs
for(i in jur) { 
  for(j in var) {

data2 <- filter(all, juris == i, Description == j) %>%
         select(-juris, -Description)
minmax <- data2[, 2:3]
minimum <- apply(minmax, 2, min, na.rm = TRUE)
low <-max(sapply(minimum, min))
maximum <- apply(minmax, 2, max, na.rm = TRUE)
high <-max(sapply(maximum, max))

data2 <- as.xts(data2, order.by = data2$qtr_year)
data2 <- data2[,-1]

graph <- dygraph(data2, main = paste(i, ":", j, sep = "")) %>%
  dySeries(name = "new_chg", axis = "y",
           label = "November '16 Vintage Annual % Chg.",
           strokeWidth = 2) %>%
  dySeries(name = "new_level", axis = "y2",
            label = "November '16 Vintage Levels",
            strokePattern = "dashed") %>%
  dySeries(name = "old_chg", axis = "y",
           label = "December '15 Vintage Annual % Chg.",
           strokeWidth = 2) %>%
  dySeries(name = "old_level", axis = "y2",
           label = "December '15 Vintage Levels",
           strokePattern = "dashed") %>%
  dyAxis("y", label = "Year-Over-Year % Chg.", 
         valueRange = c(low, high),
         rangePad = 50,
         independentTicks = TRUE,
         axisLabelFormatter = 'function(d){return Math.round(d*10000)/1e2 + "%"}',
         valueFormatter = 'function(d){return Math.round(d*10000)/1e2 + "%"}') %>%
  dyAxis("y2", label = "Levels") %>%
  dyShading(from = "2007-12-01", to = "2009-06-01") %>%
  dyShading(from = "2003-03-01", to = "2001-11-01") %>%
  dyLegend(width = 800,
           show = "onmouseover") %>%
  dyOptions(colors = c("red", "red", "gray", "gray"),
            includeZero = TRUE) %>%
  dyRangeSelector(height = 20)

 print(graph)
# saveWidget(graph, 
#             file = paste("J:\\STAFF\\Feasel\\R Programs\\Moodys Graphs\\", i, ".html", sep = ""),
#             selfcontained = TRUE)
}
}
