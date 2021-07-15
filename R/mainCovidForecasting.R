#Title: COVID-19 forecasting
#Author: Jair Paulino
#Date: 2021/Jul/09

# Setup ----
# Cleaning R environment
rm(list=ls()); graphics.off() 

# Libraries
library(trend)

# Importing functions
source("R/linearAnalisys.R")

# Importing data
data = read.csv(file = "Data/Brazil_210319.csv", sep = ";")#View(data)
incDia = data$inc_dia ; plot.ts(incDia) 

generateImg(incDia)


