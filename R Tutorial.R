#Installing

#1.) https://cran.r-project.org/
#Wouldn't bother using just R, but use a good IDE, like R Studio (the most popular). It will be a better experience.
#2.) https://www.rstudio.com/

#Like SAS, R has a base package and add-ons (e.g., ETS, STAT, SAS/ACCESS). For R those add-ons are called packages, and they come from 
#users who write and develop them. There are at least 8,000 of them as of APR2016. 

#R is totally different from SAS. But anything you can do in SAS, you can do in R. And vice versa. But I'll try to focus
#on showing parallels to SAS here. 

#Somethings, however, are better in SAS. And somethings are better in R. So I see them as complements, not substitutes
#as some do.

#R is a vectorized language. As such, it excels at data analysis, but struggles with data management/manipulation. A few
#packages help to overcome this limitation. 

#R is case sensitive

#Why learn R? http://r4stats.com/articles/popularity/
#Python is probably the fastest growing language now
#Especially popular for analytics at tech firms

#Hadley Wickham: https://statistics.rice.edu/feed/FacultyDisplay.aspx?FID=3340

#We've seen how to comment. Use: # before whatever you want commented.

#The assignment operator. Can use either, but the first is better.
a <- 10
b = 7

#Printing (to the console, which is effectively the SAS log and Output in one.)
print(a)
a

#Basic operations
10+7
c <- a+b
d <- c*2
e <- 2**2
e <- 2^3
f <- 10/5
f <- (10/5)*2 + (4)
g <- log(f)
h <- exp(g)
i <- sum(c,d)
j <- sum(e + f)
k <- mean(i, j)
k <- mean(3 + 6 + 9)
l <- median()
m <- max(h, i)
n <- min(h, i)
floor(3.4)
fl(3.4)
ceiling(3.4)
round(3.56)

#What to do if you don't remember a function?
#?mean
#Auto complete

#Notice that median argument is empty?
#It requires a vector greater than [1,1] or a list.
combine <- c(1, 2, 3, 27, -11, 3, 7, 0, 1, 2, 1, 1)
median(combine)
var(combine)
sd(combine)
combine <- c(1 2 3 27)

#Vectorized language advantage - compact code and the less often it has to evaluate before sending to compiled code
s<- c(1,4,6) 
p <-c(1,4,6)

s+p
s*p

1+1
4+4
6+6

#R is not a compiled language. Rather, each function calls compiled C or C++ code (primarily).
#Thus, it takes advantage of their speed as all R is really doing is determining the data type and then passing 
#it to a C or C++ program. This just speeds up R code relative to R code written in pure R. 
#Big data = slow

#Boolean and other useful operators
c < d #Note the all caps
c > d #0 also for FALSE, 1 for TRUE
c >= d
1 == 1 #Note double equal signs; this is how you ask if something is equal.
1 = 1 #See the problem of using 1 equal sign. It's a declaration.
10 != 2 
10 != 10 #Not equals operator
1 %in% c(1, 2, 3)
1 | 2 >= 3 #Or
1 & 2 <= -3 #And

#Characters
who <- "AEP"
juris <- c("APV", "APW", "CSP", "IMI", "PSO")
paste(who, "Operates in 11 States") #Concatenating
paste(who, "Operates in 11 States", sep = "")

#"abc" or 'abc'

char <- "Oklahoma"
substr(char, 1, 2) 
toupper(char) #Convert to all upper case
tolower(char) #Lower case

#Data
#R has three different types of data: lists, matrices, and data frames
#data frame is nothing more than a data set (e.g. SAS, Excel, etc.) in tabular/relational form
#Matrix is the same, but you'll want to use data frames b/c matrix must have same data type and same length

class(juris)
juris_list <- as.list(juris)
class(juris_list)
juris_matrix <- as.matrix(juris)
class(juris_matrix)
juris_df <- as.data.frame(juris)
class(juris_df)

#Your df can be as big as your computer's RAM on a 64 bit system becasue all data is loaded into RAM. 
#There are workarounds other than buying more RAM. I'm just not well versed in them. 

#You could also have an array, but it's not too useful/used commonly
#Vector limit is approximately: (2^31)-1, or something near that. 

#Create a variable
juris_df$kwh <- c(100, 200, 300, 400, 500)
#Very little limitations on variable naming. Just can't use reserved words: TRUE, FALSE, NULL, Inf, NaN, etc.
#Notice NaN
#No formal length limit. Only limited by your system resources. 
object.size(colnames(juris_df))
?make.names

#Archaic way to rename a variable
names(juris_df)[names(juris_df)=="kwh"] <- "kwh_renamed"
str(juris_df)

#Libname's inferior equivalent
setwd("J:\\STAFF\\Feasel\\R Programs\\Data")
getwd() #Check your current working directory

base_import <- read.csv("gdp.csv", header = TRUE, nrows = 4, stringsAsFactors = FALSE, na.strings = "")
base_import <- read.csv("J:\\STAFF\\Feasel\\R Programs\\Data\\gdp.csv")

#R importing is junk
#Packages!
install.packages("readxl")
require("readxl")
library(readxl)

setwd("J:\\FCST\\Fcst16_6+6\\Demand\\IMI\\profile")

readxl_import <- read_excel("ResProfile.xls", sheet = 1, col_names = TRUE)

#XLSB not supported by readxl package, but there's a package for that: "excel.link"

#Printing, part deux
readxl_import
head(readxl_import) #Prints first 6 rows
tail(readxl_import) #Prints last 6 rows
summary(readxl_import)
str(readxl_import) #Sort of like PROC CONTENTS

#Printing, part deux-subsetting
readxl_import[1,] #1st row
readxl_import[,2] #2nd column
readxl_import[3,4] #3rd row, 4th column

#More useful:
readxl_import[readxl_import$Year == 2013,] #Subset rows
readxl_import_2013_jan <- readxl_import[readxl_import$Year == 2013 & readxl_import$Month == 1,] 
readxl_import_2013_jan_first5 <- readxl_import_2013_jan[,1:5] #Subset columns
readxl_import_subsetted <- readxl_import[,colnames(readxl_import_2013_jan_first5)] #Subset with another df
readxl_import_2013_jan_sansthird <- readxl_import_2013_jan[,-3] #Or
readxl_import_2013_jan_sansthird <- readxl_import_2013_jan[,!names(readxl_import_2013_jan) %in% c("Month", "Day")]

#Or
readxl_import_jan <- subset(readxl_import, Year == 2013)
readxl_import_jan_ymd <- subset(readxl_import, Year == 2013, select = c(Year, Month, Day))
readxl_import_jan_no_ymd <- subset(readxl_import, Year == 2013, select = -c(Year, Month, Day))

#"dplyr" makes all of this and all data manipulation a lot easier

#ODBC database connectivity: use "RODBC" package. It includes the sqlQuery function where you can write queries as well. 

#Dates are a pain; "lubridate" 

#Some base date functions
date_example <- "11-29-2016"
class(date_example)

date_example_fixed <- as.Date(date_example, format = "%m-%d-%Y")
class(date_example_fixed) #Or

#POSIXct & POSIXlt are also date types
date_example_posixct <- as.POSIXct(date_example, format = "%m-%d-%Y")
class(date_example_posixct)

#R's date origin: Jan 1, 1970 (vs. SAS 1/1/1960 and Excel 1/1/1900)

?strptime
#Note: no day = no date

#lubridate
install.packages("lubridate")
library(lubridate)

year <- year(date_example_fixed)
month <- month(date_example_fixed)
day <- day(date_example_fixed)

#SAS data!
install.packages("haven")
library(haven)

sas_import <- read_sas("J:\\FCST\\Fcst16_6+6\\Demand\\IMI\\imi_mthpeak.sas7bdat")
str(sas_import)

#Easily fix SAS dates (or Excel)
sas_import$MODATE <- as.Date(sas_import$MODATE, origin = "1960-01-01")
str(sas_import)

#Data management the easy way, or the dplyr way
install.packages("dplyr")
library(dplyr)

#dplyr is largely analagous to sql with 4 primary functions that can be used in any order: select, mutate, filter, arrange
#Has a lot of other functions, too. It can merge, rename, summarise, subset, and provides many other functions (lead, lag, first, last) 
#but second most useful, is it can function like PROC MEANS with these functions: summary & group_by

west <- read_sas("J:\\Database\\Aep_West\\database\\aep_west.sas7bdat")

#Non-archaic way to rename a variable with dplyr
juris_df <- rename(juris_df, kwh = kwh_renamed)
str(juris_df)

#Subset rows
west_row_subset <- filter(west, YEAR >= 2012)
west_col_subset <- select(west, YEAR, MONTH, juris, REVCLS, DAYS, KWH)
west_ordered <- arrange(west, REVCLS, juris) #Ascending is default
west_ordered_desc <- arrange(west, desc(REVCLS), juris)
west_mutated <- mutate(west, new_var = 1, DAYS = log(DAYS))

#Case sensitivity is a pain. One work around:
colnames(west_col_subset) <- tolower(colnames(west_col_subset))

#Even easier with one of R's greatest additions: the piping operator
#No need to go one by one, or specify data set to alter in each argument with %>%

west_pipe <- west %>%
              filter(YEAR >= 2012) %>%
              select(YEAR, MONTH, juris, REVCLS, DAYS, KWH) %>%
              mutate(dummy_summer = ifelse(MONTH %in% c(6, 7, 8, 9), 1, 0)) %>% #Creating a dummy here: ifelse(condition, if true, if false)
              arrange(YEAR, MONTH, juris, REVCLS)
head(west_pipe)

#Imitate PROC MEANS
west_sum <- west %>%
              group_by(YEAR, juris, REVCLS) %>%
              summarise(KWH = sum(KWH),
                        DAYS = sum(DAYS)) %>%
              filter(juris == "PSO" & REVCLS == 1 & YEAR >= 2005)
head(west_sum)

#Merge some data
rvr <- read_sas("J:\\RVR\\Data\\aep_west_rvr.sas7bdat")

rvr_pso <- rvr %>%
            select(YEAR, MONTH, JURIS, REVCLS, wnbase) %>%
            filter(JURIS == "PSO" & REVCLS == 1) %>%
            group_by(YEAR, JURIS, REVCLS) %>%
            summarise(wnbase = sum(wnbase))

merged <- merge(x = west_sum, y = rvr_pso, by = "YEAR")
merged_better <- merge(x = west_sum, y = rvr_pso, by.x = c("YEAR", "juris", "REVCLS"),
                       by.y = c("YEAR", "JURIS", 'REVCLS'), all = FALSE, all.x = FALSE, all.y = FALSE)

#PROC TRANSPOSE
#Haven't found my preferred package amongst "tidyr" and "reshape2". Find "reshape2" to be a little more simple and elegant
#but tend to use a combo of both.
install.packages("tidyr")
library(tidyr)
install.packages("reshape2")
library(reshape2)

shape_data <- select(west, YEAR, MONTH, REVCLS, juris, KWH, REV)

#long to wide:
head(shape_data)
shape_melt <- melt(shape_data, id = 1:4) #R package authors are insistent data is in this format before going wide

#lhs: variables I want to be in long format; rhs: variable I want to be new col name
shape_wide <- dcast(shape_melt, YEAR + REVCLS + MONTH + variable ~ juris, value.var = "value", sum)
head(shape_wide, 13)

#wide to long
#id is variables I don't want to alter. Everything else collapses to a column.
re_melt <- melt(shape_wide, id = 1:4, variable.name = "juris")
#Telling it to take re_melt and split variable values (KWH and REV), then pair them with the value column
original <- spread(re_melt, variable, value)
head(original)

#By default, merge performs an inner join. Change all statemenets for full, right, or left joins.
#Alternatively, use dplyr's left_join, right_join, inner_join, or full_join functions
#For SAS SET statement equivalent, use rbind or cbind functions (r for rows or c for columns). These can be tricky to execute how you want.

#Graphs: par excellence
hist(west_sum$KWH)
plot(west_sum$KWH)
plot(x = west_sum$YEAR, y = west_sum$KWH, type = "l")


#Numerous packages. Lattice and ggplot2 are the best. ggplot2 THE best, and easiest
#Based on the grammar of graphics (gg), Leland Wilkinson
#gg: data > geoms > coordinate system
#geom = visual marks (e.g., lines, dots, bars, bubbles, shapes, et. al)
#geom aesthetics represent variables (colors, etc.)
#don't need to work with coordinate much (catesian by default). But change when mapping, etc.

#General flow of a ggplot statement specify data, add layers (lines, etc), modify layers (visuals), additional elements (themes, labels, etc)

#Let's plot a quick plot of PSO kwh over the years
install.packages("ggplot2")
library(ggplot2)

#Really quick way
qplot(data = west_sum, x = YEAR, y = KWH, geom = "point")

#Better way, with a lot of customization
ggplot(data = west_sum, aes(x = YEAR, y = KWH)) + #aes() is simply mapping the asthetics 
  geom_bar(stat = "identity", aes(fill = "KWH"))

#Formatting isn't easy, but there's an app for that
install.packages("scales")
library(scales)

ggplot(data = west_sum, aes(x = YEAR, y = KWH)) + 
  geom_line(col = "blue") +
  labs(title = "PSO Sales Since 2005") + #Titles and labels
  scale_y_continuous(labels = comma) + #Had to load scales package in order to get comma formatted values on the y axis
  theme_minimal() 

#Show better examples in my "Graphs for Christine" program where I replicate Chad's 4 Quadrant Graph for each Juris
#There are also packages out there that make it easy to make interactive and animated graphs
#Example: http://lenkiefer.com/2016/11/21/consumer-price-viz

#Modeling - another area where R excels. You can run a lot of models that you can't run in SAS (without Enterprise Miner).
#Base package is limited

base_lm <- lm(KWH ~ DAYS + wnbase, data = merged_better)
summary(base_lm)
coef(base_lm)
plot(resid(base_lm))
plot(density(resid(base_lm)))
qqline(resid(base_lm))

#If I wanted to predict/forecast, I'd run it through the predict() function
predict(base_lm, merged_better)

#Eat your caret
install.packages("caret")
library(caret)
#Package written by Max Kuhn, Director of Nonclinical Statistics @ Pfizer

set.seed(3)

caret_lm <- train(KWH ~ DAYS + wnbase, method = "lm", data = merged_better)
summary(caret_lm)
coef(caret_lm$finalModel)

caret_lm <- train(KWH ~ DAYS + wnbase, method = "lm", data = merged_better, preProcess = "BoxCox")
caret_lm
coef(caret_lm$finalModel)

caret_knn <- train(KWH ~ DAYS + wnbase, method = "knn", data = merged_better, preProcess = c("center", "scale"))
caret_knn

cntrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "random")
caret_nn <- train(KWH ~ DAYS + wnbase, method = "nnet", data = merged_better, preProcess = c("center", "scale"), trControl = cntrl)
caret_nn

#ARIMA - Haven't used - but know these are popular.
install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)

tsdata <- ts(merged_better$KWH, start = c(1990,1), end = c(2016,1), frequency = 1)
adf.test(tsdata)

model_arima <- Arima(tsdata, order = c(0,1,0))
model_arima
fcst <- forecast(model_arima, 3)
autoplot(fcst)

#Lazy way
lazy <- auto.arima(tsdata)
lazy
autoplot(ggAcf(lazy))

#Some advanced functionality, of which I'm not that skilled

#Basic loop
states <- c("Virginia", "West Virginia", "Ohio", "Indiana", "Oklahoma")

#then = {}

for(i in 1:length(juris)) {
  this <- paste(juris[i], "is in", states[i])
  print(this)
}

for(i in 1:length(juris)) {
  if("TCC" %in% juris) {
    this <- paste(juris[i], "is in", states[i])
    print(this)
  }
  else print("not here")
  break
}

#Loops are not all that efficient in R since R has to repeadetdly resize vector(s) and reallocate memory, i.e., they are slow.
#However, they can't always be voided. For example, the second iteration is dependent on the result of the first, or also
#some functions actually don't take a vector argument (I don't know what those are)

#Apply statements, or loops that pre-allocate memory
log <- sapply(rvr_pso, is.numeric)
rvr_pso[log] <- lapply(rvr_pso[log], log)

str(log)
head(rvr_pso)

#Functions
match <- function(jur, st) {
  paste(jur, "is in", st)
}

match(juris, states)
match("KPCo", "Kentucky")

query_rvr <- function(jur, yr, mo) {
  filter(rvr, JURIS == jur & YEAR == yr & MONTH == mo) %>%
  select(YEAR, MONTH, JURIS, REVCLS, nmwh, wnbase)
}

query_rvr("TCC", 2016, 4)
query_rvr("SWA", 2012, 7)


#A more functional function - this alters a ggplot2 coordinate to make a radar plot with linear lines
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}




