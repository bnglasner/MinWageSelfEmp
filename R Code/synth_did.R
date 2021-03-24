# Ben Glasner
# Medicaid and nonemployers analysis
# Industry analysis of nonemployers
# Perform panel analysis in R to get the stargazer output for latex


##################
###  Library   ###
##################
library(devtools)
# install_github("synth-inference/synthdid")
# devtools::install_github("bcallaway11/did")
library(stargazer)
library(ggplot2)
library(broom)
library(scales)
library(mvtnorm)
library(data.table)
library(dplyr)
library(tidyr)
# library(synthdid)
library(DIDmultiplegt)
library(did)

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
# load("check_clust_00.RData")
load("check_clust_48-49.RData")

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
# using_local <- subset(using_local, years > 2009)
using_local <- subset(using_local, years < 2007)

###########  Create a measure of the first year a local minimum wage increase happens 

local_subset <- subset(using_local, has_local==1)
local_subset <- local_subset[order(local_subset$id, abs(local_subset$years) ), ]
local_subset <- local_subset[ !duplicated(local_subset$id), ]  
local_subset$first.treat.local <- local_subset$years
local_subset <- local_subset %>% select("id_numeric","first.treat.local")

using_local <- merge(using_local, local_subset, all = TRUE)
using_local[is.na(using_local)] <- 0

###########  Create a measure of the first year a local minimum wage increase happens with Uber active

using_local$has_local_uber <- using_local$has_local*using_local$Uber_active

local_subset <- subset(using_local, has_local_uber==1)
local_subset <- local_subset[order(local_subset$id, abs(local_subset$years) ), ]
local_subset <- local_subset[ !duplicated(local_subset$id), ]  
local_subset$first.treat.uber.local <- local_subset$years
local_subset <- local_subset %>% select("id","first.treat.uber.local")

using_local <- merge(using_local, local_subset, all = TRUE)
using_local$first.treat.uber.local[is.na(using_local$first.treat.uber.local)] <- 0
using_local$id_numeric <- as.numeric(using_local$id)

###########  Create a measure of the first year a minimum wage increase happens with Uber Active

using_local$min_increase_uber <- 0
using_local$min_increase_uber[using_local$min_change>0 & using_local$Uber_active==1] <- 1

local_subset <- subset(using_local, min_increase_uber==1)
local_subset <- local_subset[order(local_subset$id, abs(local_subset$years) ), ]
local_subset <- local_subset[ !duplicated(local_subset$id), ]  
local_subset$first.treat.uber.min_change <- local_subset$years
local_subset <- local_subset %>% select("id","first.treat.uber.min_change")

using_local <- merge(using_local, local_subset, all = TRUE)
using_local$first.treat.uber.min_change[is.na(using_local$first.treat.uber.min_change)] <- 0

###########  Create a measure of the first year a minimum wage increase happens 

using_local$min_increase <- 0
using_local$min_increase[using_local$min_change>0] <- 1

local_subset <- subset(using_local, min_increase==1)
local_subset <- local_subset[order(local_subset$id, abs(local_subset$years) ), ]
local_subset <- local_subset[ !duplicated(local_subset$id), ]  
local_subset$first.treat.min_change <- local_subset$years
local_subset <- local_subset %>% select("id","first.treat.min_change")

using_local <- merge(using_local, local_subset, all = TRUE)
using_local$first.treat.min_change[is.na(using_local$first.treat.min_change)] <- 0

###########################################
###

Med_diff_estabpop <-  att_gt(yname="estab_pop",
                             first.treat.name="first.treat.min_change",
                             idname="id_numeric",
                             tname="years",
                             # xformla=~quantile + receipt_estab,
                             data=using_local,
                             weightsname = "lf_avg",
                             control.group = "notyettreated",
                             # clustervars = "st",
                             bstrap = TRUE,
                             panel = TRUE,
                             estMethod = "reg",
                             printdetails=FALSE)

es_estabpop <- aggte(Med_diff_estabpop, type="dynamic")
summary(es_estabpop)


###
Med_diff_rcp <-  att_gt(yname="receipt_estab",
                        first.treat.name="first.treat.min_change",
                        idname="id_numeric",
                        tname="years",
                        # xformla=~quantile + estab_pop,
                        data=using_local,
                        weightsname = "lf_avg",
                        control.group = "notyettreated",
                        # clustervars = "st",
                        # bstrap = TRUE,
                        panel = TRUE,
                        # estMethod = "dr",
                        estMethod = "reg",
                        printdetails=FALSE)

es_rcp <- aggte(Med_diff_rcp, type="dynamic")
summary(es_rcp)

###
Med_diff_estabpop <-  att_gt(yname="estab_pop",
                             first.treat.name="first.treat.min_change",
                             idname="id_numeric",
                             tname="years",
                             # xformla=~quantile + receipt_estab,
                             data=using_local,
                             weightsname = "lf_avg",
                             control.group = "notyettreated",
                             # clustervars = "st",
                             bstrap = TRUE,
                             panel = TRUE,
                             estMethod = "reg",
                             printdetails=FALSE)

es_estabpop <- aggte(Med_diff_estabpop, type="dynamic")
es_estabpop_df <- data.frame(es_estabpop[["egt"]],es_estabpop[["att.egt"]],es_estabpop[["se.egt"]])
names(es_estabpop_df) <- c("Period", "ATT", "se")
es_estabpop_df$Var <- "Estab./Labor Force"
ggdid(es_estabpop)
summary(es_estabpop)


###
Med_diff_rcp <-  att_gt(yname="receipt_estab",
                        first.treat.name="first.treat.min_change",
                        idname="id_numeric",
                        tname="years",
                        # xformla=~quantile + estab_pop,
                        data=using_local,
                        weightsname = "lf_avg",
                        control.group = "notyettreated",
                        # clustervars = "st",
                        # bstrap = TRUE,
                        panel = TRUE,
                        # estMethod = "dr",
                        estMethod = "reg",
                        printdetails=FALSE)

es_rcp <- aggte(Med_diff_rcp, type="dynamic")
es_rcp_df <- data.frame(es_rcp[["egt"]],es_rcp[["att.egt"]],es_rcp[["se.egt"]])
names(es_rcp_df) <- c("Period", "ATT", "se")
es_rcp_df$Var <- "Avg. Rec."
summary(es_rcp)


es_df <- rbind(es_estabpop_df,es_rcp_df)
es_df$Treatment <- "No Change"
es_df$Treatment[es_df$Period>-1] <- "After Min. Wage Inc."


myColors <- c("green3","black")
names(myColors) <- levels(es_df$Treatment)
colScale <- scale_colour_manual(name = "grp",values = myColors)

p1 <-ggplot(data = es_df, 
            aes(x = Period,
                color = Treatment)) +
  geom_point(aes(y = ATT), size =3) +
  geom_errorbar(aes(ymin=ATT-1.96*se, ymax=ATT+1.96*se), size = 1)+
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "HHI Quantile") +
  theme(axis.text = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.y.right = element_text(margin = margin(r=15)),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        panel.spacing = unit(2, "lines"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.background = element_rect(fill=alpha("white", 0.4)),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 25),
        legend.title = element_blank()) + 
  colScale +
  scale_y_continuous(label=comma) +
  facet_grid( Var ~ ., 
              scales = "free_y") 



###

Med_diff_estabpop <-  att_gt(yname="estab_pop",
                             first.treat.name="first.treat.local",
                             idname="id_numeric",
                             tname="years",
                             # xformla=~quantile + receipt_estab,
                             data=using_local,
                             weightsname = "lf_avg",
                             control.group = "notyettreated",
                             # clustervars = "st",
                             bstrap = TRUE,
                             panel = TRUE,
                             estMethod = "reg",
                             printdetails=FALSE)

es_estabpop <- aggte(Med_diff_estabpop, type="dynamic")
es_estabpop_df <- data.frame(es_estabpop[["egt"]],es_estabpop[["att.egt"]],es_estabpop[["se.egt"]])
names(es_estabpop_df) <- c("Period", "ATT", "se")
es_estabpop_df$Var <- "Estab./Labor Force"

summary(es_estabpop)


###
Med_diff_rcp <-  att_gt(yname="receipt_estab",
                        first.treat.name="first.treat.local",
                        idname="id_numeric",
                        tname="years",
                        # xformla=~quantile + estab_pop,
                        data=using_local,
                        weightsname = "lf_avg",
                        control.group = "notyettreated",
                        # clustervars = "st",
                        # bstrap = TRUE,
                        panel = TRUE,
                        # estMethod = "dr",
                        estMethod = "reg",
                        printdetails=FALSE)

es_rcp <- aggte(Med_diff_rcp, type="dynamic")
es_rcp_df <- data.frame(es_rcp[["egt"]],es_rcp[["att.egt"]],es_rcp[["se.egt"]])
names(es_rcp_df) <- c("Period", "ATT", "se")
es_rcp_df$Var <- "Avg. Rec."
summary(es_rcp)


es_df <- rbind(es_estabpop_df,es_rcp_df)
es_df$Treatment <- "No Change"
es_df$Treatment[es_df$Period>-1] <- "After Min. Wage Inc."


myColors <- c("green3","black")
names(myColors) <- levels(es_df$Treatment)
colScale <- scale_colour_manual(name = "grp",values = myColors)

p2 <-ggplot(data = es_df, 
            aes(x = Period,
                color = Treatment)) +
  geom_point(aes(y = ATT), size =3) +
  geom_errorbar(aes(ymin=ATT-1.96*se, ymax=ATT+1.96*se), size = 1)+
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "HHI Quantile") +
  theme(axis.text = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.y.right = element_text(margin = margin(r=15)),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        panel.spacing = unit(2, "lines"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.background = element_rect(fill=alpha("white", 0.4)),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 25),
        legend.title = element_blank()) + 
  colScale +
  scale_y_continuous(label=comma) +
  facet_grid( Var ~ ., 
              scales = "free_y") 

###########################################

setwd(path_output)

pdf(file = paste0("Event_study_allnonemployer_2010_2018.pdf"), width = 12, height = 10)
plot(p1)
dev.off()

pdf(file = paste0("Event_study_allnonemployer_local_2010_2018.pdf"), width = 12, height = 10)
plot(p2)
dev.off()

# pdf(file = paste0("Event_study_allnonemployer_2000_2006.pdf"), width = 12, height = 10)
# plot(p1)
# dev.off()
# 
# pdf(file = paste0("Event_study_allnonemployer_local_2000_2006.pdf"), width = 12, height = 10)
# plot(p2)
# dev.off()

# pdf(file = paste0("Event_study_transportation.pdf"), width = 12, height = 10)
# plot(p1)
# dev.off()
# 
# pdf(file = paste0("Event_study_transportation_local.pdf"), width = 12, height = 10)
# plot(p2)
# dev.off()

###########################################

felm <- felm(estab_pop ~ has_local + quantile + factor(Uber_active) | id + YEAR.id | 0 | st,
             data = using_local,
             weights = using_local$lf_avg)

summary(felm)

using$min_increase <- 0
using$min_increase[using$min_change >0] <- 1

felm <- felm(estab_pop ~ min_increase + quantile + factor(Uber_active) | id + YEAR.id | 0 | st,
             data = using,
             weights = using$lf_avg)

summary(felm)


felm <- felm(estab_pop ~ min_change + quantile + factor(Uber_active) | id + YEAR.id | 0 | st,
             data = using,
             weights = using$lf_avg)

summary(felm)




# #################################################################
# #                         Synth-DID set up                      #
# #################################################################
# 
# using_local$treat <- using_local$has_local
# using_local_synth <- using_local
# # using_local_synth <- subset(using_local, 
# #                        first.treat!=2004 &
# #                        first.treat!=2005 &
# #                        first.treat!=2007 & 
# #                        first.treat!=2014 &
# #                        # first.treat!=2015 &
# #                        first.treat!=2016 &
# #                        first.treat!=2017
# #                      )
# temp <- subset(using_local_synth, Uber_active==1 & years==2015)
# county_ids <- data.frame(table(temp$id))
# county_ids <- subset(county_ids, Freq>0)
# names(county_ids) <- c("id","Freq")
# # Alaska
# # Indiana
# # Pennsylvania
# # Louisiana
# # Montana
# 
# 
# # I need to stack the non-medicaid expansion counties on top of the expansion counties, 
# # and specify how many countrol counties their are using n_0
# # I need to also specify the treatment years and control years using T_0
# 
# using_no_local_long <- subset(using_local_synth, first.treat==0) %>% select("id","estab_pop", "years")
# using_no_local_long <- spread(using_no_local_long, years, estab_pop)
# using_no_local_long <- merge(using_no_local_long, county_ids)
# n_0 <- as.numeric(nrow(using_no_local_long))
# 
# using_local_synth_long <- subset(using_local_synth, first.treat==2015) %>% select("id","estab_pop", "years")
# using_local_synth_long <- spread(using_local_synth_long, years, estab_pop)
# using_local_synth_long <- merge(using_local_synth_long, county_ids)
# n_1 <- as.numeric(nrow(using_local_synth_long))
# 
# T_0 <- 14
# T_1 <- 4
# 
# n <- n_0 + n_1
# T <- T_0 + T_1
# 
# using_wide <- rbind(using_no_local_long,using_local_synth_long)
# Y <- using_wide %>% select("2001", "2002", "2003", "2004", "2005",
#                                     "2006", "2007", "2008", "2009", "2010", "2011",
#                                     "2012", "2013", "2014", "2015", "2016", "2017",
#                                     "2018")
# Y <- as.matrix(Y)
# rownames(Y) = 1:n
# colnames(Y) = 1:T
# 
# tau.hat = synthdid_estimate(Y,n_0,T_0)
# se = synthdid_se(tau.hat)
# 
# print(paste0("point estimate: ", round(tau.hat, 6)))
# print(paste0("95% CI for tau: (", round(tau.hat - 1.96 * se, 6), ", ", round(tau.hat + 1.96 * se, 6), ")"))
# # plot(tau.hat)
# 
# did_estimate(Y,
#              n_0,
#              T_0)
# plot(did_estimate(Y,
#                   n_0,
#                   T_0))
# synthdid_estimate(Y,
#                   n_0,
#                   T_0,
#                   update.lambda = TRUE,
#                   update.omega = TRUE)
# plot(synthdid_estimate(Y,
#                        n_0,
#                        T_0,
#                        update.lambda = TRUE,
#                        update.omega = TRUE))
# 
# synthdid_placebo_plot(synthdid_estimate(Y,
#                                         n_0,
#                                         T_0,
#                                         update.lambda = TRUE,
#                                         update.omega = TRUE), overlay = FALSE, treated.fraction = NULL)
# synthdid_se(synthdid_estimate(Y,
#                               n_0,
#                               T_0,
#                               update.lambda = TRUE,
#                               update.omega = TRUE))
# #################################################################
# #                         Synth-DID set up                      #
# #################################################################
# 
# # I need to stack the non-local min wage expansion counties on top of the local min counties, 
# # and specify how many countrol counties their are using n_0
# # I need to also specify the treatment years and control years using T_0
# 
# using_no_local_long <- subset(using_local_synth, first.treat==0) %>% select("id","receipt_estab", "years")
# using_no_local_long <- spread(using_no_local_long, years, receipt_estab)
# using_no_local_long <- merge(using_no_local_long, county_ids)
# n_0 <- as.numeric(nrow(using_no_local_long))
# 
# using_local_synth_long <- subset(using_local_synth, first.treat==2015) %>% select("id","receipt_estab", "years")
# using_local_synth_long <- spread(using_local_synth_long, years, receipt_estab)
# using_local_synth_long <- merge(using_local_synth_long, county_ids)
# n_1 <- as.numeric(nrow(using_local_synth_long))
# 
# T_0 <- 14
# T_1 <- 4
# 
# n <- n_0 + n_1
# T <- T_0 + T_1
# 
# using_wide <- rbind(using_no_local_long,using_local_synth_long)
# Y <- using_wide %>% select("2001", "2002", "2003", "2004", "2005",
#                            "2006", "2007", "2008", "2009", "2010", "2011",
#                            "2012", "2013", "2014", "2015", "2016", "2017",
#                            "2018")
# Y <- as.matrix(Y)
# rownames(Y) = 1:n
# colnames(Y) = 1:T
# 
# tau.hat = synthdid_estimate(Y,n_0,T_0)
# se = synthdid_se(tau.hat)
# 
# print(paste0("point estimate: ", round(tau.hat, 6)))
# print(paste0("95% CI for tau: (", round(tau.hat - 1.96 * se, 6), ", ", round(tau.hat + 1.96 * se, 6), ")"))
# plot(tau.hat)
# 
# did_estimate(Y,n_0,T_0)
# synthdid_estimate(Y,n_0,T_0)
