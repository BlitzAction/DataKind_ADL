
# initial setup

setwd("C:/Users/jeremyd/My Cloud/DATAKIND-ADL/")
CountDemographicFIPS <- readRDS("county-demoraphics-with_fips.rds")
#install.packages(c('shinythemes','tidyr','shiny','shinydashboard','leaflet','magrittr', 
#                   'sp','RColorBrewer','DT','highcharter','plyr','scales','plotrix'))


library(shinythemes)
library(tidyr)
library(shiny)
library(shinydashboard)
library(leaflet)
library(magrittr)
library(sp)
library(RColorBrewer)
library(DT)
library(highcharter)
library(plyr)
library(scales)
library(plotrix)
# CHECKS- CHECK EACH PIECE HERE TO GET WORKING
# explore (1st) page check
# score   (2nd) page check  
# email   (3rd) page check
runApp("adl-shiny",   display.mode = "showcase")