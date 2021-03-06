# Ben Glasner
# Medicaid and nonemployers analysis
# Industry analysis of nonemployers
# Perform panel analysis in R to get the stargazer output for latex

##################
###  Library   ###
##################
library(devtools)
# install_github("synth-inference/synthdid")
library(synthdid)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(scales)

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
load("check_clust_00.RData")
# load("check_clust_48-49.RData")

#######################################################
###             local Min Wage increases            ###
#######################################################
setwd(paste0(path_output))
using$has_local <- 0
using$has_local[using$local_minimum_wage!=0 & 
                  using$local_minimum_wage>using$Annual_State_Minimum &
                  using$local_minimum_wage>using$Annual_Federal_Minimum] <- 1
using_local <- using

sapply(using_local, function(x) sum(is.na(x)))
using_local <- using_local[complete.cases(using_local),]

county_counts <- as.data.frame(table(using_local$id)) # count how many times a county-state appears in the sample
county_counts <- subset(county_counts, Freq==18) # keep only the counties that are in the full sample for balance
colnames(county_counts)[1]<- "id" # relabel the id for the merge
using_local <- using_local[using_local$id %in% county_counts$id, ]

using_local <- using_local %>% select("id", "id_numeric","years", "YEAR.id","estab_pop","receipt_estab",
                                      "quantile", "HHI_lower", "Uber_active","has_local","min_change",
                                      "Labor.Force","population","Unemployment.Rate","lf_avg","st")

#################################################################
#                         DID set up                            #
#################################################################

###
local_subset <- subset(using, has_local==1)
local_subset <- local_subset[order(local_subset$id, abs(local_subset$years) ), ]
local_subset <- local_subset[ !duplicated(local_subset$id), ]  
local_subset$first.treat <- local_subset$years
local_subset <- local_subset %>% select("id","first.treat")

local_subset$keep <- "No"
# local_subset$keep[local_subset$first.treat==2004] <- "Yes"
# local_subset$keep[local_subset$first.treat==2005] <- "Yes"
# local_subset$keep[local_subset$first.treat==2007] <- "Yes"
# local_subset$keep[local_subset$first.treat==2014] <- "Yes"
local_subset$keep[local_subset$first.treat==2015] <- "Yes"
# local_subset$keep[local_subset$first.treat==2016] <- "Yes"
# local_subset$keep[local_subset$first.treat==2017] <- "Yes"

using <- merge(using, local_subset, all = TRUE)
using$first.treat[is.na(using$first.treat)] <- 0
using$id_numeric <- as.numeric(using$id)

using$keep[is.na(using$keep)] <- "Yes"
using <- subset(using, keep!="No")

#################################################################
#                         Synth-DID set up                      #
#################################################################

### Create just the labor force estimates for weights:
using_lf <- using %>% dplyr::select("id", "lf_avg")
using_lf <- unique(using_lf)

### Create just the county ID list:
county_ids <- data.frame(table(using$id))
county_ids <- subset(county_ids, Freq>0)
names(county_ids) <- c("id","Freq")


# I need to stack the non-medicaid expansion counties on top of the expansion counties,
# and specify how many countrol counties their are using n_0
# I need to also specify the treatment years and control years using T_0
using_no_local_long <- subset(using, first.treat==0) %>% select("id","estab_pop", "years")
using_no_local_long <- spread(using_no_local_long, years, estab_pop)
using_no_local_long <- merge(using_no_local_long, county_ids)
n_0 <- as.numeric(nrow(using_no_local_long))

using_local_long <- subset(using, first.treat==2015) %>% select("id","estab_pop", "years")
using_local_long <- spread(using_local_long, years, estab_pop)
using_local_long <- merge(using_local_long, county_ids)
n_1 <- as.numeric(nrow(using_local_long))

using_wide <- rbind(using_no_local_long,using_local_long)
Y_estab <- using_wide %>% select("2001", "2002", "2003", "2004", "2005",
                           "2006", "2007", "2008", "2009", "2010", "2011",
                           "2012", "2013", "2014", "2015", "2016", "2017",
                           "2018")
Y_estab <- as.matrix(Y_estab)

using_no_local_long <- subset(using, first.treat==0) %>% select("id","receipt_estab", "years")
using_no_local_long <- spread(using_no_local_long, years, receipt_estab)
using_no_local_long <- merge(using_no_local_long, county_ids)
n_0 <- as.numeric(nrow(using_no_local_long))

using_local_long <- subset(using, first.treat==2015) %>% select("id","receipt_estab", "years")
using_local_long <- spread(using_local_long, years, receipt_estab)
using_local_long <- merge(using_local_long, county_ids)
n_1 <- as.numeric(nrow(using_local_long))

using_wide <- rbind(using_no_local_long,using_local_long)
Y_rcp <- using_wide %>% select("2001", "2002", "2003", "2004", "2005",
                           "2006", "2007", "2008", "2009", "2010", "2011",
                           "2012", "2013", "2014", "2015", "2016", "2017",
                           "2018")
Y_rcp <- as.matrix(Y_rcp)

### Modify the row/column names
T_0 <- 14
T_1 <- 4
n <- n_0 + n_1
T <- T_0 + T_1

rownames(Y_estab) = 1:n
colnames(Y_estab) = 1:T
rownames(Y_rcp) = 1:n
colnames(Y_rcp) = 1:T

#######################################################
# build an array of controls (quantile + Labor.Force + Uber_active) 
# of the form (rows = ids, column = years, cell = variable)
######
using_no_local_long <- subset(using, first.treat==0) %>% select("id","quantile", "years")
using_no_local_long <- spread(using_no_local_long, years, quantile)
using_no_local_long <- merge(using_no_local_long, county_ids)

using_local_long <- subset(using, first.treat==2015) %>% select("id","quantile", "years")
using_local_long <- spread(using_local_long, years, quantile)
using_local_long <- merge(using_local_long, county_ids)

using_wide <- rbind(using_no_local_long,using_local_long)
quant <- using_wide %>% select("2001", "2002", "2003", "2004", "2005",
                               "2006", "2007", "2008", "2009", "2010", "2011",
                               "2012", "2013", "2014", "2015", "2016", "2017",
                               "2018")
quant <- as.matrix(quant)
rownames(quant) = 1:n
colnames(quant) = 1:T

using_no_local_long <- subset(using, first.treat==0) %>% select("id","Uber_active", "years")
using_no_local_long <- spread(using_no_local_long, years, Uber_active)
using_no_local_long <- merge(using_no_local_long, county_ids)

using_local_long <- subset(using, first.treat==2015) %>% select("id","Uber_active", "years")
using_local_long <- spread(using_local_long, years, Uber_active)
using_local_long <- merge(using_local_long, county_ids)

using_wide <- rbind(using_no_local_long,using_local_long)
Uber <- using_wide %>% select("2001", "2002", "2003", "2004", "2005",
                              "2006", "2007", "2008", "2009", "2010", "2011",
                              "2012", "2013", "2014", "2015", "2016", "2017",
                              "2018")
Uber <- as.matrix(Uber)
rownames(Uber) = 1:n
colnames(Uber) = 1:T

z <- array(c(quant, Uber) , dim = c(nrow(Uber), ncol(Uber), 2 ))
#######################################################


#################################################################
#                         Synth-DID Analysis                    #
#################################################################
#list of control units which create the ATE
effect_lf <- using_no_local_long %>% dplyr::select("id")
effect_lf <- merge(effect_lf,using_lf)

# create unit level effect estimates for estab_pop
test <- synthdid_units_plot(synthdid_estimate(Y_estab,
                                              n_0,
                                              T_0), 
                            show.ci = TRUE,
                            negligible.alpha = .1)

    effect_estimate <- test$data
    effect_estimate$unit <- as.numeric(effect_estimate$unit)
    effect_estimate <- cbind(effect_lf, effect_estimate)
    effect_estimate$lf_avg_weight <- effect_estimate$lf_avg/sum(effect_estimate$lf_avg)
    effect_estimate$weight_2 <- effect_estimate$weight*effect_estimate$lf_avg_weight
    effect_estimate$plot_clean[effect_estimate$weight_2>as.numeric(quantile(effect_estimate$weight_2,.75))] <- 1
    effect_estimate$plot_clean[is.na(effect_estimate$plot_clean)] <- 0
    
    # Calculate LF adjusted ATE of the Synthetic results
    mean_weight_estab <- diagis::weighted_mean(effect_estimate$y, w = effect_estimate$weight_2)
    se_weight_estab <- diagis::weighted_se(effect_estimate$y, w = effect_estimate$weight_2)
    

        p1 <- ggplot(data = effect_estimate, 
               aes(x = unit, 
                   y = y,
                   size = weight_2)) +
          # geom_point(data = subset(effect_estimate, plot_clean==0), shape = 3, alpha = .1) +
          geom_point(data = subset(effect_estimate, plot_clean==1), aes(color = lf_avg)) +
          geom_hline(yintercept =  0, color = "black") +
          geom_hline(yintercept =  mean_weight_estab, color = "red") +
          geom_hline(yintercept = mean_weight_estab + 1.96*se_weight_estab, linetype = "dashed", color = "red") +
          geom_hline(yintercept = mean_weight_estab - 1.96*se_weight_estab, linetype = "dashed", color = "red") +
          ylab("Unit Effect Estimate") + xlab ("Units") +
          theme(plot.title = element_blank(),
                axis.text.x = element_text(size = 25, angle = 90),
                axis.text.y = element_text(size = 25),
                axis.title.y = element_text(size = 25),
                axis.title.x = element_text(size = 25),
                panel.background = element_rect(fill = "white"),
                panel.grid = element_line(colour = "grey"),
                axis.line = element_line(colour = "black"),
                legend.background = element_rect(fill=NA),
                legend.text = element_text(size = 20),
                legend.key = element_rect(fill="white"),
                legend.title = element_text(size = 20)) +
          scale_colour_steps(
            low = "grey80",
            high = "grey1",
            space = "Lab",
            na.value = "grey50",
            guide = "coloursteps",
            aesthetics = "colour",
            labels = comma
          ) +
          guides(colour = guide_colorbar(title = "County Labor Force"),
                 size = guide_legend(title = "Effect Weights"),
                 alpha = "none")


# create unit level effect estimates for Average Results
test <- synthdid_units_plot(synthdid_estimate(Y_rcp,
                                              n_0,
                                              T_0), 
                            show.ci = TRUE,
                            negligible.alpha = .1)

    effect_estimate <- test$data
    effect_estimate$unit <- as.numeric(effect_estimate$unit)
    effect_estimate <- cbind(effect_lf, effect_estimate)
    effect_estimate$lf_avg_weight <- effect_estimate$lf_avg/sum(effect_estimate$lf_avg)
    effect_estimate$weight_2 <- effect_estimate$weight*effect_estimate$lf_avg_weight
    effect_estimate$plot_clean[effect_estimate$weight_2>as.numeric(quantile(effect_estimate$weight_2,.75))] <- 1
    effect_estimate$plot_clean[is.na(effect_estimate$plot_clean)] <- 0
    
    # Calculate LF adjusted ATE of the Synthetic results
    mean_weight_rcp <- diagis::weighted_mean(effect_estimate$y, w = effect_estimate$weight_2)
    se_weight_rcp <- diagis::weighted_se(effect_estimate$y, w = effect_estimate$weight_2)

            p2 <- ggplot(data = effect_estimate, 
                   aes(x = unit, 
                       y = y,
                       size = weight_2)) +
              # geom_point(data = subset(effect_estimate, plot_clean==0), shape = 3, alpha = .1) +
              geom_point(data = subset(effect_estimate, plot_clean==1), aes(color = lf_avg)) +
              geom_hline(yintercept =  0, color = "black") +
              geom_hline(yintercept =  mean_weight_rcp, color = "red") +
              geom_hline(yintercept = mean_weight_rcp + 1.96*se_weight_rcp, linetype = "dashed", color = "red") +
              geom_hline(yintercept = mean_weight_rcp - 1.96*se_weight_rcp, linetype = "dashed", color = "red") +
              ylab("Unit Effect Estimate") + xlab ("Units") +
              theme(plot.title = element_blank(),
                    axis.text.x = element_text(size = 25, angle = 90),
                    axis.text.y = element_text(size = 25),
                    axis.title.y = element_text(size = 25),
                    axis.title.x = element_text(size = 25),
                    panel.background = element_rect(fill = "white"),
                    panel.grid = element_line(colour = "grey"),
                    axis.line = element_line(colour = "black"),
                    legend.background = element_rect(fill=NA),
                    legend.text = element_text(size = 20),
                    legend.key = element_rect(fill="white"),
                    legend.title = element_text(size = 20)) +
              scale_y_continuous(labels = comma) +
              scale_colour_steps(
                low = "grey80",
                high = "grey1",
                space = "Lab",
                na.value = "grey50",
                guide = "coloursteps",
                aesthetics = "colour",
                labels = comma
              ) +
              guides(colour = guide_colorbar(title = "County Labor Force"),
                     size = guide_legend(title = "Effect Weights"),
                     alpha = "none")

###############################################
mean_weight_estab
se_weight_estab
mean_weight_estab/se_weight_estab

mean_weight_rcp
se_weight_rcp
mean_weight_rcp/se_weight_rcp
###############################################
            
# synthdid_rmse_plot(synthdid_estimate(Y,
#                                      n_0,
#                                      T_0,
#                                      X = z))
# 
# synthdid_placebo_plot(synthdid_estimate(Y,
#                                         n_0,
#                                         T_0,
#                                         X = z,
#                                         update.lambda = TRUE,
#                                         update.omega = TRUE), overlay = FALSE, treated.fraction = NULL)
# synthdid_se(synthdid_estimate(Y,
#                               n_0,
#                               T_0,
#                               X = z,
#                               update.lambda = TRUE,
#                               update.omega = TRUE))






