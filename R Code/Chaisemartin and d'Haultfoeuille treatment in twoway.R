# Ben Glasner
# Minimum wage and nonemployers analysis
# Industry analysis of nonemployers
# Perform panel analysis in R to get the stargazer output for latex

##################
###  Library   ###
##################
library(stargazer)
library(stringr)
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

# using$Uber_active[using$Uber_active==0] <- "No Uber"
# using$Uber_active[using$Uber_active==1] <- "Uber Active"

allnonemployers  <- using

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

allnonemployers$minimum_wage <- pmax(allnonemployers$Annual_Federal_Minimum,allnonemployers$Annual_State_Minimum, allnonemployers$local_minimum_wage) 
allnonemployers$min_inc <- 0
allnonemployers$min_inc[allnonemployers$min_change>0] <- 1

# allnonemployers <- subset(allnonemployers, years>2009)
# allnonemployers <- subset(allnonemployers, years<2007)
#################################################################
#                         Estab All                             #
#################################################################

estab_HHI_list[[1]] <- felm(estab_pop ~ minimum_wage + quantile + Uber_active | id + YEAR.id | 0 | st,
                             data = allnonemployers,
                             weights = allnonemployers$lf_avg)

estab_HHI_list[[2]] <- felm(estab_pop ~ minimum_wage_inf + quantile + Uber_active | id + YEAR.id | 0 | st,
                            data = allnonemployers,
                            weights = allnonemployers$lf_avg)
#################################################################
#                         Rcp   All                             #
#################################################################

rcp_HHI_list[[1]] <- felm(receipt_estab ~ minimum_wage + quantile + Uber_active | id + YEAR.id | 0 | st,
                           data = allnonemployers,
                           weights = allnonemployers$lf_avg)

rcp_HHI_list[[2]] <- felm(receipt_estab ~ minimum_wage_inf + quantile + Uber_active | id + YEAR.id | 0 | st,
                          data = allnonemployers,
                          weights = allnonemployers$lf_avg)
#################################################################
#                         Summary                               #
#################################################################
summary(estab_HHI_list[[1]])
summary(rcp_HHI_list[[1]])
summary(estab_HHI_list[[2]])
summary(rcp_HHI_list[[2]])



#################################################################
# Chaisemartin & d'Haultfoeuille v Callaway & Sant'Anna         #
#################################################################
# Chaisemartin & d'Haultfoeuille done in stata using did_multplegt package
setwd(path_output)
library(readr)
did_advanced_results <- read_csv("did_advanced_results.csv")
did_advanced_results$ID <- seq.int(nrow(did_advanced_results))

did_advanced_results$TSM <- factor(did_advanced_results$TSM,
                                   levels = c("Pre 2007, Real Min. Wage, Two-Way", "Full, Real Min. Wage, Two-Way", "Post 2009, Real Min. Wage, Two-Way",
                                              "Pre 2007, Min Wage, Two-Way","Full, Min Wage, Two-Way","Post 2009, Min Wage, Two-Way",
                                              "Pre 2007, Min Wage, C&d'H","Full, Min Wage, C&d'H","Post 2009, Min Wage, C&d'H",
                                              "Pre 2007, Min. Wage Increase, C&S", "Post 2009, Min. Wage Increase, C&S"))

transport_2018_estab <- 0.0155 
transport_2018_rcp <- 47305

total_2018_estab <- 0.162 
total_2018_rcp <- 46439

did_advanced_results$ATE_perc <- 0
did_advanced_results$ATE_perc[did_advanced_results$Industry == "All Nonemployers" & did_advanced_results$Dependent =="Estab/Labor Force"] <- did_advanced_results$ATE[did_advanced_results$Industry == "All Nonemployers" & did_advanced_results$Dependent =="Estab/Labor Force"]/total_2018_estab
did_advanced_results$ATE_perc[did_advanced_results$Industry == "Transportation and Warehousing" & did_advanced_results$Dependent =="Estab/Labor Force"] <- did_advanced_results$ATE[did_advanced_results$Industry == "Transportation and Warehousing" & did_advanced_results$Dependent =="Estab/Labor Force"]/transport_2018_estab
did_advanced_results$ATE_perc[did_advanced_results$Industry == "All Nonemployers" & did_advanced_results$Dependent =="Average Receipts"] <- did_advanced_results$ATE[did_advanced_results$Industry == "All Nonemployers" & did_advanced_results$Dependent =="Average Receipts"]/total_2018_rcp
did_advanced_results$ATE_perc[did_advanced_results$Industry == "Transportation and Warehousing" & did_advanced_results$Dependent =="Average Receipts"] <- did_advanced_results$ATE[did_advanced_results$Industry == "Transportation and Warehousing" & did_advanced_results$Dependent =="Average Receipts"]/transport_2018_rcp

did_advanced_results$Lower_perc <- 0
did_advanced_results$Lower_perc[did_advanced_results$Industry == "All Nonemployers" & did_advanced_results$Dependent =="Estab/Labor Force"] <- did_advanced_results$Lower[did_advanced_results$Industry == "All Nonemployers" & did_advanced_results$Dependent =="Estab/Labor Force"]/total_2018_estab
did_advanced_results$Lower_perc[did_advanced_results$Industry == "Transportation and Warehousing" & did_advanced_results$Dependent =="Estab/Labor Force"] <- did_advanced_results$Lower[did_advanced_results$Industry == "Transportation and Warehousing" & did_advanced_results$Dependent =="Estab/Labor Force"]/transport_2018_estab
did_advanced_results$Lower_perc[did_advanced_results$Industry == "All Nonemployers" & did_advanced_results$Dependent =="Average Receipts"] <- did_advanced_results$Lower[did_advanced_results$Industry == "All Nonemployers" & did_advanced_results$Dependent =="Average Receipts"]/total_2018_rcp
did_advanced_results$Lower_perc[did_advanced_results$Industry == "Transportation and Warehousing" & did_advanced_results$Dependent =="Average Receipts"] <- did_advanced_results$Lower[did_advanced_results$Industry == "Transportation and Warehousing" & did_advanced_results$Dependent =="Average Receipts"]/transport_2018_rcp

did_advanced_results$Upper_perc <- 0
did_advanced_results$Upper_perc[did_advanced_results$Industry == "All Nonemployers" & did_advanced_results$Dependent =="Estab/Labor Force"] <- did_advanced_results$Upper[did_advanced_results$Industry == "All Nonemployers" & did_advanced_results$Dependent =="Estab/Labor Force"]/total_2018_estab
did_advanced_results$Upper_perc[did_advanced_results$Industry == "Transportation and Warehousing" & did_advanced_results$Dependent =="Estab/Labor Force"] <- did_advanced_results$Upper[did_advanced_results$Industry == "Transportation and Warehousing" & did_advanced_results$Dependent =="Estab/Labor Force"]/transport_2018_estab
did_advanced_results$Upper_perc[did_advanced_results$Industry == "All Nonemployers" & did_advanced_results$Dependent =="Average Receipts"] <- did_advanced_results$Upper[did_advanced_results$Industry == "All Nonemployers" & did_advanced_results$Dependent =="Average Receipts"]/total_2018_rcp
did_advanced_results$Upper_perc[did_advanced_results$Industry == "Transportation and Warehousing" & did_advanced_results$Dependent =="Average Receipts"] <- did_advanced_results$Upper[did_advanced_results$Industry == "Transportation and Warehousing" & did_advanced_results$Dependent =="Average Receipts"]/transport_2018_rcp

p1 <- ggplot(data = did_advanced_results, 
       aes(x = TSM,
           color = Method,
           shape = Treatment)) +
  geom_point(aes(y = ATE), size =5) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), size = 1)+
  geom_hline(yintercept = 0, color = "black") +
  # labs(x = "HHI Quantile") +
  theme(axis.text.x  = element_text(size = 15, angle = 90),
        axis.text.y  = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y.right = element_text(margin = margin(r=15)),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        panel.spacing = unit(2, "lines"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.box="vertical", 
        legend.margin=margin(),
        legend.background = element_rect(fill=alpha("white", 0.4)),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 25),
        legend.title = element_blank()) + 
  scale_y_continuous(label=comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  facet_grid( Dependent ~ Industry, 
              scales = "free_y") 

p2 <- ggplot(data = did_advanced_results, 
             aes(x = TSM,
                 color = Method,
                 shape = Treatment)) +
  geom_point(aes(y = ATE_perc), size =5) +
  geom_errorbar(aes(ymin=Lower_perc, ymax=Upper_perc), size = 1)+
  geom_hline(yintercept = 0, color = "black") +
  # labs(x = "HHI Quantile") +
  theme(axis.text.x  = element_text(size = 15, angle = 90),
        axis.text.y  = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y.right = element_text(margin = margin(r=15)),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        panel.spacing = unit(2, "lines"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.box="vertical", 
        legend.margin=margin(),
        legend.background = element_rect(fill=alpha("white", 0.4)),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 25),
        legend.title = element_blank()) + 
  scale_y_continuous(label=percent) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  facet_grid( Dependent ~ Industry, 
              scales = "free_y") 

pdf(file = paste0("did_comparison.pdf"), width = 25, height = 15)
plot(p1)
dev.off()

pdf(file = paste0("did_comparison_perc.pdf"), width = 25, height = 15)
plot(p2)
dev.off()