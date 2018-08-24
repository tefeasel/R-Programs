#This requires a package that interfaces with Java. Make sure Sys.setenv is set to your "jre" Java folder so the package works properly
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.7.0_71\\jre")

#Load Packages
packages <- c("dplyr", "haven", "lubridate", "rJava", "XLConnect")

install <- function(pack){ 
  not_installed <- pack[!(pack %in% installed.packages()[, "Package"])] 
  if(length(not_installed)) 
    install.packages(not_installed, dependencies = TRUE) 
  sapply(pack, require, character.only = TRUE) 
} 

install(packages) #Run install function
lapply(packages, require, character.only = TRUE)

#Set graphs you want
juris <- c("")

#Set your west lan drive and where you want the graphs to be saved in setwd()
drive <- "I"
setwd(paste(drive, "", sep = ""))

#Graph code (Note: the legend looks off in RStudio, but it's fine once plot is exported)
peak_graph <- function(x) {

peak <- read_sas(paste(drive, ":\\.sas7bdat", sep = "")) %>%
        mutate(DATE = as.Date(DATE, origin = "1960-01-01", format = "%Y-%m-%d")) %>% #Making sure it imports date correctly
        select(DATE, PEAK, ENERGY, MIN, MAX, AVGTEMP, TEMP24_C) %>%
        filter(DATE >= "2014-01-01" & DATE <= today()-1)

DATE <- seq.Date(today(), as.Date("2017-2-28", format = "%Y-%m-%d"), by = 1)
DATE <- as.data.frame(DATE)
DATE$PEAK <- 0
peak1 <- bind_rows(peak, DATE) %>%
         filter(DATE >= "2016-11-01")

#png(paste(x, ".png", sep = ""), width = 1600, height = 900, res = 175) #Type of file I want to save and its name

par(mar = c(2.5, 4, 2.5, 2.5)) #Set margins (bottom, left, top, right)
with(peak1, plot(DATE, PEAK, type = "h", col = "gold2", xaxt = 'n', xlab = "", ylab = "Peak", #Peak bar graph
                 main = toupper(x)))
axis(side = 2) #Put it on left axis
par(new=TRUE) #Add new layer, true or false? True to add another layer (temperature line(s))

with(peak1, plot(DATE, MIN, type="l", col="blue", ylim = c(0, 100), xaxt = 'n', lwd = 2, axes = FALSE, ylab = "")) #Layer min temp line
axis(side = 4) #Put it on right axis
par(new=TRUE) #Add new layer, true or false? True to add another layer (temperature line(s))

with(peak1, plot(DATE, MAX, type="l", col="red",  ylim = c(0, 100), xaxt = 'n', lwd = 2, axes = FALSE, ylab = ""))#Layer max temp line
par(new=TRUE) #Add new layer, true or false? True to add another layer (temperature line(s))

with(peak1, plot(DATE, AVGTEMP, type="l", col="green4", ylim = c(0, 100), xaxt = 'n', lwd = 2, axes = FALSE, ylab = "")) #Layer avg temp line

axis.Date(side = 1, at = seq(peak1$DATE[1], max(peak1$DATE), "weeks"), format = "%m/%d", labels = TRUE) #Major ticks
axis.Date(side = 1, at = seq(peak1$DATE[1], max(peak1$DATE), "days"), labels = FALSE, tcl = -.2) #Minor ticks

legend("topright",col=c("blue", "green4", "red"),lty=1,legend=c("Min. Temp.","Average Temp.", "Max. Temp."),
       lwd = c(2, 2, 2), cex = .7, ncol = 1, pt.cex = 1, bty = "n")

#dev.off()
}

#Loop through the list of jurisdictions to graph 
lapply(juris, peak_graph)

##########################################################################################################

#This will save all of the graphs to an excel workbook.
save_excel <- function(x) {
  peak_graph_wb <- loadWorkbook("test.xlsx", create = FALSE)
  wb_file <- system.file("test.xlsx", package = "xlsx")
  pic <- paste("PATH", x, ".png", sep = "")
  createSheet(peak_graph_wb, name = x)
  createName(peak_graph_wb, name = x, formula = paste(x, "!","$B$2", sep = ""))
  addImage(peak_graph_wb, filename = pic, name = x, originalSize = TRUE)
  saveWorkbook(peak_graph_wb)
}

lapply(juris, save_excel)




