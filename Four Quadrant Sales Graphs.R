#This function will install any packages that are needed that you don't have installed
install <- function(pack){ 
       not_installed <- pack[!(pack %in% installed.packages()[, "Package"])] 
       if(length(not_installed)) 
             install.packages(not_installed, dependencies = TRUE) 
       sapply(pack, require, character.only = TRUE) 
} 

#List the packages to use in this program
packages <- c("ggplot2", "ggradar", "scales", "dplyr", "tidyr", "readxl", "stringr",
              "grid", "gridExtra", "ggthemes", "haven") 
install(packages)
lapply(packages, require, character.only = TRUE) #This loads the packages; avoids all of the library/require statements

not_installed <- packages[!(packages %in% installed.packages())] 

#Data manipulation part
setwd("J:\\RVR\\Data\\")

#Read sales by division
east <- read_sas()
eastalt <- read_sas()
west <- read_sas()

east <- filter(east, YEAR <= 2014) %>% 
        select(YEAR, MONTH, JURIS, REVCLS, nmwh)

eastalt <- eastalt %>% select(YEAR, MONTH, JURIS, REVCLS, nmwh)
west <- west %>% select(YEAR, MONTH, JURIS, REVCLS, nmwh)

all <- rbind(east, eastalt, west)

#Data through 2015
all_2015 <- all %>%
        filter(YEAR %in% c(2012,2013,2014,2015) & REVCLS <= 4 & !(REVCLS %in% c())) %>%
        mutate(JURIS = ifelse(JURIS %in% c(), "", JURIS), #Manipulate juris names here
               REVCLS = floor(REVCLS)) %>% 
        group_by(YEAR, REVCLS, JURIS) %>%
        summarise(nmwh = sum(nmwh)) %>%
        ungroup() %>%
        arrange(JURIS, REVCLS, YEAR) 

#Get budget data
six_and_six <- read_sas() 
six_and_sixa <- read_sas()

six_and_six_16 <- select(six_and_six, YEAR, MONTH, JURIS, REVCLS, tkwh) %>%
                  filter(YEAR == 2016)
six_and_six_17 <- select(six_and_sixa, YEAR, MONTH, JURIS, REVCLS, tkwh) %>%
                  filter(YEAR == 2017)

#six_and_six <- rbind(six_and_six_16, six_and_six_17)

#Pull in actuals through certain date and forecast thereafter
last2_17 <- filter(six_and_six, REVCLS <= 4 & ((YEAR == 2016 & MONTH >= 10) | (YEAR == 2017))) %>%
            filter(!(REVCLS %in% c())) %>%
            mutate(JURIS = ifelse(JURIS %in% c(""), "", JURIS), #Manipulate juris names here
                   REVCLS = floor(REVCLS)) %>% 
            select(YEAR, MONTH, JURIS, REVCLS, tkwh) %>%
            rename(nmwh = tkwh) %>%
            mutate(nmwh = nmwh/1000) %>%
            arrange(JURIS, REVCLS, YEAR, MONTH)

actuals16 <- filter(all, YEAR %in% c(2016) & MONTH <=9 & REVCLS <= 4) %>%
             filter(!(REVCLS %in% c())) %>%
             mutate(JURIS = ifelse(JURIS %in% c(""), "", JURIS), #Manipulate juris names here
                    REVCLS = floor(REVCLS)) %>% 
              select(YEAR, MONTH, JURIS, REVCLS, nmwh) %>%
              arrange(JURIS, REVCLS, YEAR, MONTH)

ten_plus_two <- rbind(actuals16, last2_17)

ten_plus_two <- arrange(ten_plus_two, JURIS, REVCLS, YEAR, MONTH)

ten_plus_two_17 <- ten_plus_two %>%
                   group_by(YEAR, REVCLS, JURIS) %>%
                   summarise(nmwh = sum(nmwh)) %>%
                   ungroup() %>%
                   arrange(JURIS, REVCLS, YEAR) %>%
                   mutate(nmwh = nmwh)

changes <- rbind(all_2015, ten_plus_two_17)

sum_all <- changes %>%
           group_by(YEAR, JURIS) %>%
           summarise(nmwh = sum(nmwh)) %>%
           mutate(REVCLS = 99) %>%
           select(YEAR, REVCLS, JURIS, nmwh) %>%
           arrange(JURIS, REVCLS, YEAR)

changes <- bind_rows(changes, sum_all)

changes1 <- changes %>%
            arrange(JURIS, REVCLS, YEAR) %>%
            mutate(pct_chg = ((nmwh/lag(nmwh, n = 1)-1)),
                   YEAR = ifelse(YEAR == 2013, "2013A", YEAR),
                   YEAR = ifelse(YEAR == 2014, "2014A", YEAR),
                   YEAR = ifelse(YEAR == 2015, "2015A", YEAR),
                   YEAR = ifelse(YEAR == 2016, "2016E*", YEAR),
                   YEAR = ifelse(YEAR == 2017, "2017E", YEAR)) %>%
            filter(YEAR != 2012)

#Four quadrant waterfall graphs
jur <- c() #List segments here (code will loop through and produce a four quadrant graph for each)

for(i in seq_along(jur)) {

graph_res <- filter(changes1, JURIS == jur[i] & REVCLS == 1)

residential <- ggplot(graph_res, aes(YEAR, pct_chg)) +
                geom_bar(stat = "identity", fill = "darkorange", color = "black") +
                coord_cartesian(ylim = c(-.1,.05)) +
                geom_text(aes(x = YEAR, y = ifelse(pct_chg>.05, pct_chg - .05, pct_chg), 
                              label = paste(round(pct_chg*100, digits = 1), "%", sep = "")),
                              vjust = ifelse(sign(graph_res$pct_chg)<0, 1, -.25),
                              fontface = "bold", size = 3) +
                geom_hline(yintercept = 0) +
                scale_y_continuous(labels = percent) +
                labs(y = "Growth vs Prior Year", title = paste(jur[i], "Residential", "Normalized GWh Sales"),
                      subtitle = "% Change vs. Prior Year") +
                theme_classic() +
                theme(plot.title = element_text(hjust = .5, face = "bold", size = 10),
                      plot.subtitle = element_text(hjust = .5, size = 6),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.line.x = element_blank(),
                      axis.text = element_text(color = "black")) 

graph_com <- filter(changes1, JURIS == jur[i] & REVCLS == 2)

commercial <- ggplot(graph_com, aes(YEAR, pct_chg, label = pct_chg)) +
                geom_bar(stat = "identity", fill = "yellow", color = "black") +
                coord_cartesian(ylim = c(-.1,.05)) +
                geom_text(aes(x = YEAR, y = ifelse(pct_chg>.05, pct_chg - .05, pct_chg), 
                              label = paste(round(pct_chg*100, digits = 1), "%", sep = "")),
                              vjust = ifelse(sign(graph_com$pct_chg)<0, 1, -.25),
                              fontface = "bold", size = 3) +
                geom_hline(yintercept = 0) +
                scale_y_continuous(labels = percent) +
                labs(y = "Growth vs Prior Year", title = paste(jur[i], "Commercial", "Normalized GWh Sales"),
                      subtitle = "% Change vs. Prior Year") +
                theme_classic() +
                theme(plot.title = element_text(hjust = .5, face = "bold", size = 10),
                      plot.subtitle = element_text(hjust = .5, size = 6),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.line.x = element_blank(),
                      axis.text = element_text(color = "black")) 

graph_ind <- filter(changes1, JURIS == jur[i] & REVCLS == 3)

industrial <- ggplot(graph_ind, aes(YEAR, pct_chg, label = pct_chg)) +
                geom_bar(stat = "identity", fill = "blue", color = "black") +
                coord_cartesian(ylim = c(-.1,.05)) +
                geom_text(aes(x = YEAR, y = ifelse(pct_chg>.05, pct_chg - .05, pct_chg), 
                              label = paste(round(pct_chg*100, digits = 1), "%", sep = "")),
                              vjust = ifelse(sign(graph_ind$pct_chg)<0, 1, -.25),
                              fontface = "bold", size = 3) +
                geom_hline(yintercept = 0) +
                scale_y_continuous(labels = percent) +
                labs(y = "Growth vs Prior Year", title = paste(jur[i], "Industrial", "Normalized GWh Sales"),
                      subtitle = "% Change vs. Prior Year") +
                theme_classic() +
                theme(plot.title = element_text(hjust = .5, face = "bold", size = 10),
                plot.subtitle = element_text(hjust = .5, size = 6),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.line.x = element_blank(),
                axis.text = element_text(color = "black")) 

graph_total <- filter(changes1, JURIS == jur[i] & REVCLS == 99)

total <- ggplot(graph_total, aes(YEAR, pct_chg, label = pct_chg)) +
          geom_bar(stat = "identity", fill = "darkgreen", color = "black") +
          coord_cartesian(ylim = c(-.1,.05)) +
          geom_text(aes(x = YEAR, y = ifelse(pct_chg>.05, pct_chg - .05, pct_chg), 
                        label = paste(round(pct_chg*100, digits = 1), "%", sep = "")),
                        vjust = ifelse(sign(graph_total$pct_chg)<0, 1, -.25),
                        fontface = "bold", size = 3) +
          geom_hline(yintercept = 0) +
          scale_y_continuous(labels = percent) +
          labs(y = "Growth vs Prior Year", title = paste(jur[i], "Total", "Normalized GWh Sales"),
          subtitle = "% Change vs. Prior Year") +
          theme_classic() +
          theme(plot.title = element_text(hjust = .5, face = "bold", size = 10),
                plot.subtitle = element_text(hjust = .5, size = 6),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.line.x = element_blank(),
                axis.text = element_text(color = "black")) 

setwd()
g <- grid.arrange(residential, commercial, industrial, total, ncol = 2)}

#footnote <- "*2016 includes 9 months weather normalized actual results plus 3 months of forecasted values."

#png(paste(jur[i], " Orig 6 and 6 no Ormet.png", sep = ""), width = 1600, height = 900, res = 175)
#final <- arrangeGrob(g, bottom = textGrob(footnote, x = 0, hjust = -.009, vjust = .005, gp = gpar(fontsize = 6)))
#grid.draw(final)
#dev.off()


               
