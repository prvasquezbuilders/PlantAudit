library(DBI)
library(RODBC)
library(RMySQL)
library(xts)
library(ggplot2)
library(hydroTSM) #call the hydrology package for time series analysis


#source("thickness.R")


source("energyaudit.R")


stop()

data <- read.csv(file="reliability.csv", header=TRUE, sep=",")
data=data.frame(data)

colors=c("chocolate","red","coral4","cyan1","cyan4","darkblue","blue","chartreuse4")

#Simulation
library(WeibullR)
source("simulation.R")

source("chart-survivalprobability.R")
source("totalcost.R")

#sensitivity analysis
#source("sa_alpha.R")
#source("sa_m.R")
#source("sa_cipi.R")

