# Ben Glasner
# Minimum wage and nonemployers analysis
# Industry analysis of nonemployers
# Perform panel analysis in R to get the stargazer output for latex

##################
###  Library   ###
##################
library(stargazer)
library(ggplot2)
library(broom)
library(scales)
library(plm)
#################
### Set paths ###
#################
if(Sys.info()[["user"]]=="bglasner"){
  # Root folder
  path_project <- "C:/Users/bglasner/Dropbox/PhD Requirements"
}
if(Sys.info()[["user"]]=="bngla"){
  # Root folder
  path_project <- "C:/Users/bngla/Dropbox/PhD Requirements"
}
# Path to saved cohort data 
path_data <- paste0(path_project,"\\Nonemployer data\\Data\\")
# Path where matched samples should be saved 
path_output <- paste0(path_project,"\\Minimum wage and alt\\output\\Jakes comments")

setwd(paste0(path_data))

options(scipen=10000)
set.seed(42)

#################################################################
#                         Load the Data                         #
#################################################################
load("check_clust_00.RData")

using$Uber_active[using$Uber_active==0] <- "No Uber"
using$Uber_active[using$Uber_active==1] <- "Uber Active"

using <- subset(using, years >2016)

test <- using
test$minimum_wage_inf[test$years == 2018] <- 15


##################################
###     Turn it into a panel   ###
##################################

### Make the treatment - change in the real minimum wage
test_panel <- pdata.frame(test, index=c("id","YEAR.id"), drop.index=FALSE, row.names=TRUE) # create a panel df for lags and leads

### did the minimum wage increase?
test_panel$min_change <- diff(test_panel$minimum_wage_inf) # calc change in inflation adj minwage

test_panel$additional_estabpop[test_panel$quant_avg<=25] <- test_panel$min_change[test_panel$quant_avg<=25]*.0030 # using the effect estimate from 2010-2018 from de_chaise
test_panel$additional_estabpop[test_panel$quant_avg>25 & test_panel$quant_avg<=50] <- test_panel$min_change[test_panel$quant_avg>25 & test_panel$quant_avg<=50]*.0019 # using the effect estimate from 2010-2018 from de_chaise
test_panel$additional_estabpop[test_panel$quant_avg>50 & test_panel$quant_avg<=75] <- test_panel$min_change[test_panel$quant_avg>50 & test_panel$quant_avg<=75]*-0.0008 # using the effect estimate from 2010-2018 from de_chaise
test_panel$additional_estabpop[test_panel$quant_avg>75] <- test_panel$min_change[test_panel$quant_avg>75]*-0.0024 # using the effect estimate from 2010-2018 from de_chaise

test_panel$additional_estab <- test_panel$additional_estabpop*test_panel$Labor.Force

test_panel <- subset(test_panel, years ==2018)
sum(test_panel$additional_estab)

