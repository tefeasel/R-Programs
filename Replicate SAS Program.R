#This program replicates a SAS program that merges two types of sales data.
#Used for simple illustrative purposes for co-workers.

drive <- "J" #set your drive letter for the west lan (make sure it is capitalized)

packages <- c("dplyr", "readxl", "haven", "knitr") #List of packages used

#Wrote this function that will install any packages that are needed that which you don't have installed
install <- function(pack){ 
  not_installed <- pack[!(pack %in% installed.packages()[, "Package"])] 
  if(length(not_installed)) 
    install.packages(not_installed, dependencies = TRUE) 
  sapply(pack, require, character.only = TRUE) 
}

install(packages) #Run install function
lapply(packages, require, character.only = TRUE) #This loads the packages; avoids all of the library/require statements

billed_kwh_rev_jun16 <- read_sas(paste(drive, ".sas7bdat", sep = ""))

#You'll see the %>% operator a lot. It's just telling it to pass the result of the preceding statement/agrument to the next.
#It's formally called pipe operator. I think of it as chaining my code together.
#With the exception of importing and printing, this whole program is written with the "dplyr" package.
#Data step replication (SAS equivalent commented next to each step):

blend_dsm <- read_sas(paste(drive, ".sas7bdat", sep = "")) %>% #DATA step - set & merge;
             filter((YEAR >= 2018) & !(JURIS %in% c() & revcls >=5)) %>% #WHERE statement
             mutate(JURIS = ifelse(JURIS %in% c(), , JURIS), #Data manipulation
                    revcls = ifelse(JURIS == "IMI" & revcls == 3.4, 3.6, revcls), #Data manipulation
                    revcls = ifelse(revcls %in% c(3, 3.1, 3.2, 3.3), 3, revcls), #Data manipulation
                    datatype = "Forecast") %>%
            group_by(JURIS, YEAR, MONTH, revcls) %>% #Proc Means (don't need to separate from data step). This is like the class/by statement
            summarise(KWH = sum(KWH)) %>% #Proc Means
            select(JURIS, YEAR, MONTH, revcls, KWH) %>% #Keep/drop statement
            setNames(toupper(names(.))) #This sets all column names to upper case to help prevent merging issues

revcls5.3 <- read_sas(paste(drive,".sas7bdat", sep = "")) %>% #Data step set/merge
             filter(YEAR >= 2018) %>% #Where statement
             group_by(JURIS, YEAR, MONTH, REVCLS) %>% #PROC MEANS class statement
             summarise(KWH = sum(KWH)) #PROC MEANS

three <- bind_rows(list(billed_kwh_rev_jun16, blend_dsm, revcls5.3)) #Lines 46 & 47 is SAS. This is equivalent to concatenating/
#using SET statement in a data step, or equally, vertically combining with an outer union in SQL

#PROC IMPORT (piped to a DATA STEP with a WHERE statement):
two <- read_excel(paste(drive, "\\kWh by Class.xls", sep = ""), sheet = "Output", na = "#REF!") %>%
              mutate(UBASE = as.numeric(UBASE), #UBASE has a bunch of rows with and error b/c of trouble handiling #REF!. Forcing those to NA here.
                     UBASE = ifelse(is.na(UBASE), 0, UBASE)) %>% #Data manipulation: No need to type = 0 for each variable
              filter((YEAR > 2016) | (YEAR == 2016 & MONTH >= 5)) #Where
              
#write.csv(dataset) or write.___(dataset) whatever format you wanted to save the dataset to.

five <- full_join(three, two, by = c("JURIS", "YEAR", "MONTH", "REVCLS")) %>% #Data step merge. Line 81-89 in SAS.
        mutate_each(funs(replace(., is.na(.), 0))) %>% #Replace missing values with 0
        mutate(TKWH = KWH + UKWH, #Creating new variables
               TBASE = BASE + UBASE)

seven <- five %>% #PROC MEANS. Lines 94-97 in SAS
         group_by(JURIS, YEAR, MONTH, REVCLS) %>% #CLASS/BY statement
         summarise(KWH = sum(KWH), UKWH = sum(UKWH), BASE = sum(BASE), UBASE = sum(UBASE), TKWH = sum(TKWH), 
                   TBASE = sum(TBASE), PASSTHRU = sum(PASSTHRU)) 
         
total_bill_and_unbill <- seven %>% #Data step manipulation, followed by PROC SORT. Lines 100-113 in SAS.
            mutate(eei = "Reg East",
                   eei = ifelse(JURIS %in% c(), "Reg West", eei),
                   eei = ifelse(JURIS %in% c(), "OH", eei),
                   eei = ifelse(JURIS %in% c(), "TX",eei)) %>%
            arrange(JURIS, YEAR, REVCLS)

table <- bind_rows( #Most of this is a wonky way to replicate sum statement in PROC PRINT. It also replicates PROC SUMMARY in lines 118-125
          total_bill_and_unbill %>%
            group_by(JURIS, YEAR, REVCLS) %>% #SUM by juris & Revcls
            summarise_each(funs(sum), c(-1:-4, -12)) %>%
            filter(YEAR == 2017),
          total_bill_and_unbill %>%
            group_by(JURIS, YEAR) %>% #SUM by juris
            summarise_each(funs(sum), c(-1:-4, -12)) %>%
            filter(YEAR == 2017),
          total_bill_and_unbill %>%
            group_by(YEAR) %>% #SUM all for year
            summarise_each(funs(sum), c(-1:-4, -12)) %>%
            filter(YEAR == 2017)) %>% arrange(JURIS, YEAR)

table <- format(table, big.mark = ",", decimal.mark = ".") #FORMAT statement
table <- mutate(table, JURIS = ifelse(JURIS == "NA", 'Z-Total', JURIS)) #Make sure total is last in print
print(by(table, table$JURIS, kable)) #PROC PRINT, with by JURIS statement. 
#I'm using the "knitr" package function kable to print a better table than the default print can. 



