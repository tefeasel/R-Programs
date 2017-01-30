#Load Packages
packages <- c()

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