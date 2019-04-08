#####################################
## 01/23/2019 Justin Rajendra
## app to read from RAD8 pulse ox
## global V5 small

library(shiny)
library(data.table)
library(shinydashboard)
library(serial)
library(dashboardthemes)


# library(devtools)
# install_github("nik01010/dashboardthemes")

## clean up
closeAllConnections()
rm(list=ls())

#####################################
## save console output
con_out <- paste0("julian_sats_",format(Sys.time(),"%d-%m-%Y-%H-%M-%S"),".txt")
sink(file.path("www",con_out),append=FALSE,split=TRUE)

#####################################
## create serial connection

con <- serialConnection(name = "test_con",
                        port = "COM4",
                        mode = "9600,N,8,1",
                        buffering = "none",
                        newline = 1,
                        translation = "cr")

open(con)

