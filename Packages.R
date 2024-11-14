#ensure the libraries are loaded
#global file to call APLRS- KWS template for

#install libraries----

#Check list, if not there install.----
libs<-c("htmltools",
        "plyr",
        "tidyverse",
        "tidyr",
        "ggplot2",
        "tidyverse",
        "maptools",
        "leaflet",
        "rgdal",
        "devtools",
        "curl",
        "reshape2",
        "rmdformats",
        "knitr",
        "rmarkdown",
        "kableExtra",
        "rbokeh",
        "RColorBrewer",
        "plotly",
        "dplyr",
        "collapsibleTree",
        "DT",
        "zoo",
        "lubridate")

ins<-which(libs %in% rownames(installed.packages())==FALSE)#check if the packagesd have been loaded locally
install.packages(libs[ins]) #load those that havent been linstalled.
lapply(libs, require, character.only = TRUE)
#call the rmd