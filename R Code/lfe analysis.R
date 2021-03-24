# Ben Glasner
# Minimum wage and nonemployers analysis
# Industry analysis of nonemployers
# Perform panel analysis in R to get the stargazer output for latex

##################
###  Library   ###
##################
library(stargazer)
library(lfe)
library(estimatr)
library(ggplot2)
library(broom)
library(scales)
library(Hmisc)
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
# load("check_clust_48-49.RData")
# load("check_clust_two_digit.RData")

using$Uber_active[using$Uber_active==0] <- "No Uber"
using$Uber_active[using$Uber_active==1] <- "Uber Active"

allnonemployers  <- using
allnonemployers$min_change <- allnonemployers$minimum_wage_inf
estab_HHI_list <- list()
rcp_HHI_list <- list()

industry_list <- c(sort(unique(as.character(allnonemployers$naics))))
industry_name <- data.frame(cbind(c("11","21","22","23","31-33","42","44-45","48-49","51","52",
                         "53","54","55","56","61","62","71","72","81","92"),
                       c("Agr./Forestry/Fish/Hunting","Mining/Quarrying/Oil/Gas","Utilities","Construction",
                         "Manufacturing","Wholesale Trade","Retail Trade","Trans./Warehousing",
                         "Information","Finance/Insurance","Real Estate/Rental/Leasing","Prof./Sci./Technical Services",
                         "Management of Companies and Enterprises","Admin./Support/Waste/Remediation","Educational Services","Health Care/Social Assist.",
                         "Arts/Entertainment/Recreation","Accommodation/Food Services","Other Services","Public Administration")))
colnames(industry_name)[1]<- "industry_list"
colnames(industry_name)[2]<- "industry_names"

#################################################################
#                         Estab All                             #
#################################################################

estab_HHI_list[[1]] <- felm(estab_pop ~ min_change + quantile + factor(Uber_active) | id + YEAR.id | 0 | st,
                             data = allnonemployers,
                             weights = allnonemployers$lf_avg)

estab_HHI_list[[2]] <- felm(estab_pop ~ min_change*Uber_active + quantile  | id + YEAR.id | 0 | st,
                             data = allnonemployers,
                             weights = allnonemployers$lf_avg)

estab_HHI_list[[3]] <- felm(estab_pop ~ min_change*quantile + factor(Uber_active) | id + YEAR.id | 0 | st,
                             data = allnonemployers,
                             weights = allnonemployers$lf_avg)

estab_HHI_list[[4]] <- felm(estab_pop ~ min_change*quantile*Uber_active | id + YEAR.id | 0 | st,
                             data = allnonemployers,
                             weights = allnonemployers$lf_avg)

#################################################################
#                         Rcp   All                             #
#################################################################

rcp_HHI_list[[1]] <- felm(receipt_estab ~ min_change + quantile + factor(Uber_active) | id + YEAR.id | 0 | st,
                           data = allnonemployers,
                           weights = allnonemployers$lf_avg)

rcp_HHI_list[[2]] <- felm(receipt_estab ~ min_change*Uber_active + quantile | id + YEAR.id | 0 | st,
                           data = allnonemployers,
                           weights = allnonemployers$lf_avg)

rcp_HHI_list[[3]] <- felm(receipt_estab ~ min_change*quantile + factor(Uber_active)| id + YEAR.id | 0 | st,
                           data = allnonemployers,
                           weights = allnonemployers$lf_avg)

rcp_HHI_list[[4]] <- felm(receipt_estab ~ min_change*quantile*Uber_active | id + YEAR.id | 0 | st,
                           data = allnonemployers,
                           weights = allnonemployers$lf_avg)

#################################################################
#                         Stargazer                             #
#################################################################

stargazer(estab_HHI_list[[1]], estab_HHI_list[[3]],
          keep = "min_change",
          keep.stat = c("n","rsq","adj.rsq"),
          font.size = "small",
          no.space = TRUE,
          digits = 4)


stargazer(rcp_HHI_list[[1]],  rcp_HHI_list[[3]],
          keep = "min_change",
          keep.stat = c("n","rsq","adj.rsq"),
          font.size = "small",
          no.space = TRUE,
          digits = 3)

# stargazer(estab_HHI_list[[1]], estab_HHI_list[[2]], estab_HHI_list[[3]], estab_HHI_list[[4]],
#           keep = "min_change",
#           keep.stat = c("n","rsq","adj.rsq"),
#           font.size = "small",
#           no.space = TRUE,
#           digits = 4)
# 
# 
# stargazer(rcp_HHI_list[[1]], rcp_HHI_list[[2]], rcp_HHI_list[[3]], rcp_HHI_list[[4]],
#           keep = "min_change",
#           keep.stat = c("n","rsq","adj.rsq"),
#           font.size = "small",
#           no.space = TRUE,
#           digits = 3)



#################################################################
#                         Load the Data                         #
#################################################################


load("check_clust_two_digit.RData")
using <- subset(using, industry !="Trans./Warehousing")

using$Uber_active[using$Uber_active==0] <- "No Uber"
using$Uber_active[using$Uber_active==1] <- "Uber Active"

allnonemployers  <- using
allnonemployers$min_change <- allnonemployers$minimum_wage_inf

industry_list <- c(sort(unique(as.character(allnonemployers$naics))))

#################################################################
#                         Estab All                             #
#################################################################

estab_HHI_list[[1]] <- felm(estab_pop ~ min_change + quantile + factor(Uber_active) + factor(industry) | cty_st + YEAR.id | 0 | st,
                            data = allnonemployers,
                            weights = allnonemployers$lf_avg)

estab_HHI_list[[2]] <- felm(estab_pop ~ min_change*Uber_active + quantile + factor(industry) | cty_st + YEAR.id | 0 | st,
                            data = allnonemployers,
                            weights = allnonemployers$lf_avg)

estab_HHI_list[[3]] <- felm(estab_pop ~ min_change*quantile + factor(Uber_active) + factor(industry) | cty_st + YEAR.id | 0 | st,
                            data = allnonemployers,
                            weights = allnonemployers$lf_avg)

estab_HHI_list[[4]] <- felm(estab_pop ~ min_change*quantile*Uber_active + factor(industry) | cty_st + YEAR.id | 0 | st,
                            data = allnonemployers,
                            weights = allnonemployers$lf_avg)

#################################################################
#                         Rcp   All                             #
#################################################################

rcp_HHI_list[[1]] <- felm(receipt_estab ~ min_change + quantile + factor(Uber_active) + factor(industry) | cty_st + YEAR.id | 0 | st,
                          data = allnonemployers,
                          weights = allnonemployers$lf_avg)

rcp_HHI_list[[2]] <- felm(receipt_estab ~ min_change*Uber_active + quantile  + factor(industry)| cty_st + YEAR.id | 0 | st,
                          data = allnonemployers,
                          weights = allnonemployers$lf_avg)

rcp_HHI_list[[3]] <- felm(receipt_estab ~ min_change*quantile + factor(Uber_active) + factor(industry) | cty_st + YEAR.id | 0 | st,
                          data = allnonemployers,
                          weights = allnonemployers$lf_avg)

rcp_HHI_list[[4]] <- felm(receipt_estab ~ min_change*quantile*Uber_active + factor(industry) | cty_st + YEAR.id | 0 | st,
                          data = allnonemployers,
                          weights = allnonemployers$lf_avg)


#################################################################
#                         Stargazer                             #
#################################################################

stargazer(estab_HHI_list[[1]], estab_HHI_list[[2]], estab_HHI_list[[3]], estab_HHI_list[[4]],
          keep = "min_change",
          keep.stat = c("n","rsq","adj.rsq"),
          font.size = "small",
          no.space = TRUE,
          digits = 4)

stargazer(rcp_HHI_list[[1]], rcp_HHI_list[[2]], rcp_HHI_list[[3]], rcp_HHI_list[[4]],
          keep = "min_change",
          keep.stat = c("n","rsq","adj.rsq"),
          font.size = "small",
          no.space = TRUE,
          digits = 3)
