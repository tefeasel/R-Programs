library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(dplyr)
library(leaflet)
library(scales)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(sf)
library(readxl)


### Begin data prep
setwd("")

fips <- read.csv("US County FIPS Codes.csv") %>%
        filter(State == "OH")      

cnty_data <- read.csv("Ohio Income Data.csv")

ohio_data <- read_excel("Ohio Data by County.xlsx") %>%
                filter(year == 2017) %>%     
                select(COUNTY_NAME, avg_bill, cust_gr, CUST) %>%
                mutate(cust_gr = percent(cust_gr))

county_dat <- cnty_data %>%
        mutate(Mnemonic = gsub("$", "", fixed = TRUE, Mnemonic),
               Geography = gsub("County", "", fixed = TRUE, Geography),
               Geography = gsub("(OH)", "", fixed = TRUE, Geography)) %>%
        separate(Mnemonic, into = paste("V", 1:2, sep = ".")) %>%
        rename(var = V.1,
               countyfips = V.2,
               countyname = Geography,
               value = Dec.2017) %>%
        mutate(countyfips = gsub("OH", "", fixed = TRUE, countyfips),
               countyfips = paste("39", countyfips, sep = ""),
               diff = (value/59768)-1,
               value = round(value, digits = 0),
               value2 = dollar(value),
               countyname = trimws(countyname, which = "both")) %>%
        filter(var == "FYHHMEDA")


county_dat2  <- inner_join(county_dat, ohio_data, by = c("countyname" = "COUNTY_NAME")) %>%
                select(countyfips, value, value2, diff, avg_bill, cust_gr, CUST) %>%
                mutate(burden = percent(avg_bill/value))


county_dat2$countyfips <- as.numeric(county_dat2$countyfips)


keep <- c(39001,39003,39005,39009,39011,39013,39015,39019,39029,39031,39033,39039,39041,39045,39047,39049,
          39053,39059,39063,39065,39067,39069,39071,39073,39075,39077,39079,39081,39083,39087,39089,39091,
          39097,39101,39105,39111,39115,39117,39119,39121,39125,39127,39129,39131,39137,39139,39141,39143,
          39145,39147,39151,39153,39157,39159,39161,39163,39167,39169,39173,39175)

county_dat2 <- filter(county_dat2, countyfips %in% keep)


# Colnames tolower
names(county_dat2) <- tolower(names(county_dat2))

# Rename columns to make for a clean df merge later.
colnames(county_dat2) <- c("GEOID", "hhinc", "hhinc_label", "pct_diff", "avg_bill", "cust_gr", "cust","burden")

# Have to add leading zeos to any FIPS code that's less than 5 digits long to get a good match.
county_dat2$GEOID <- formatC(county_dat2$GEOID, width = 5, format = "d", flag = "0")
### End data prep

# Download county shape file from Tiger.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
us.map <- readOGR(dsn = "cb_2017_us_county_20m", layer = "cb_2017_us_county_20m", stringsAsFactors = FALSE)

# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]

#Select state (Ohio = 39)
us.map <- us.map[us.map$STATEFP %in% c("39"),]

# Make sure other outling islands are removed.

leafmap <- merge(us.map, county_dat2, by=c("GEOID"))
centers <- data.frame(gCentroid(leafmap, byid = TRUE)) #get centers for text labels
centers$label <- ifelse(is.na(leafmap$hhinc), "", paste(as.character(dollar(round(leafmap$hhinc/1000, 0))), "k", sep = ""))

leafmap$hhinc_label <- ifelse(is.na(leafmap$hhinc_label), "County not in AEP Ohio Service Territory", leafmap$hhinc_label)
leafmap$pct_diff_label <- ifelse(is.na(leafmap$pct_diff), "County not in AEP Ohio Service Territory", percent(leafmap$pct_diff))
leafmap$avg_bill <- ifelse(is.na(leafmap$avg_bill), "County not in AEP Ohio Service Territory", dollar(leafmap$avg_bill, accuracy = 1))
leafmap$cust <- ifelse(is.na(leafmap$cust), "County not in AEP Ohio Service Territory", comma(leafmap$cust))
leafmap$burden <- ifelse(is.na(leafmap$burden), "County not in AEP Ohio Service Territory", leafmap$burden)
leafmap$cust_gr <- ifelse(is.na(leafmap$cust_gr), "County not in AEP Ohio Service Territory", leafmap$cust_gr)

# Format popup data for leaflet map.
popup_dat <- paste0("<strong>County: </strong>", 
                    leafmap$NAME,
                    "<br><strong># of Residential Customers: </strong>", 
                    leafmap$cust,
                    "<br><strong>Median Household Income: </strong>", 
                    leafmap$hhinc_label,
                    "<br><strong>% Difference from State Median: </strong>", 
                    leafmap$pct_diff_label,
                    "<br><strong>Average Annual Residential Bill: </strong>", 
                    leafmap$avg_bill,
                    "<br><strong>Residential Wallet Share: </strong>", 
                    leafmap$burden,
                    "<br><strong>Residential Customer Count CAGR (2012-2017): </strong>", 
                    leafmap$cust_gr
                    )

icon1 <- awesomeIcons(icon = "money", iconColor = 'black', library = "fa")
icon2 <- awesomeIcons(icon = "money", iconColor = 'black', library = "fa")

bins <- c(min(leafmap$pct_diff, na.rm = TRUE), -.10, -.03, .03, .10,max(leafmap$pct_diff, na.rm = TRUE))
pal <- colorBin(c("#E41A1C", "#D6604D", "#D1E5F0", "#92C5DE" ,"#2166AC"), domain = leafmap$pct_diff, bins = bins)

# Render final map in leaflet.
leaflet(data = leafmap) %>% 
        addTiles() %>%
        addPolygons(fillColor = ~pal(pct_diff), 
                    fillOpacity = 0.8, 
                    color = "#BDBDC3", 
                    weight = 1,
                    popup = popup_dat,
                    group = "All AEP Ohio Counties") %>%
        addLegend("bottomright", colors = c("#E41A1C", "#D6604D", "#D1E5F0", "#92C5DE" ,"#2166AC", "gray"),
                  title = "AEP Ohio Median HH Income (2017)",
                  labels = c("Well Below State Median", "Below State Median", "Within +/- 3% of State Median", "Above State Median", 
                             "Well Above State Median", "County not in AEP Ohio Service Territory"),
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 1) %>%
        # addAwesomeMarkers(lat = 39.7666, lng = -81.1145, icon = icon1, label = "Monroe County had the lowest median household income in AEP Ohio territory in 2017.") %>%
        # addAwesomeMarkers(lat = 40.2958, lng = -83.0649, icon = icon2, label = "Delaware County had the highest median household income in AEP Ohio territory in 2017.") %>%
        addLabelOnlyMarkers(data = centers, lng = ~x, lat=~y, label =  ~label, 
                            labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, interactive = FALSE))
        # addLayersControl(overlayGroups = c("All AEP Ohio Counties"),
        #                  options = layersControlOptions(collapsed = FALSE))

#Source: https://gist.github.com/keberwein/05f633a2be293b01b52bf05553d24b93

