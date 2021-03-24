# Ben Glasner
# Minimum wage and nonemployers analysis
# Industry analysis of nonemployers
# Create Descriptive Plots/Figures


##################
###  Library   ###
##################
library(ggplot2)
library(gganimate)
library(transformr)
library(gifski)
library(ggthemes)
library(RColorBrewer)
library(dplyr)
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
# Path where plots should be saved
path_output <- paste0(path_project,"\\Minimum wage and alt\\output\\Jakes comments")
# Path where animations should be saved 
path_animate <- paste0(path_project,"\\Minimum wage and alt\\output\\Jakes comments\\animation")

setwd(paste0(path_data))

options(scipen=10000)
set.seed(42)

industry_list <- c("00","48-49")
# industry_list <- c("31-33", "44-45","72","81")
#################################################################
#                         Load the Data                         #
#################################################################
load("check_clust_00.RData")
total <- using
load("check_clust_two_digit.RData")
allnonemployers  <- using
rm(using)
load("bin_labor.RData")

allnonemployers$min_change <- allnonemployers$minimum_wage_inf
total$min_change <- total$minimum_wage_inf

allnonemployers <- allnonemployers %>% dplyr::select("years",
                                                     "id",
                                                     "cty_st",
                                                     "min_change",
                                                     "Uber_active",
                                                     "estab",
                                                     "estab_pop",
                                                     "Labor.Force",
                                                     "receipt_estab",
                                                     "population",
                                                     "HHI_lower",
                                                     "quantile",
                                                     "naics")
total <- total %>% dplyr::select("years",
                                 "id",
                                 "cty_st",
                                 "min_change",
                                 "Uber_active",
                                 "estab",
                                 "estab_pop",
                                 "Labor.Force",
                                 "receipt_estab",
                                 "population",
                                 "HHI_lower",
                                 "quantile",
                                 "naics")
                                                     
using <- rbind(allnonemployers, total)
rm(allnonemployers, total)

industry_list <- sort(unique(using$naics))
data_list <- list()
annual_estab_plot <- list()
annual_estab <- list()
annual_rcp_plot <- list()
annual_rcp <- list()  
uber <- list()
no_uber <- list()
estab_uber_plot <- list()
rcp_uber_plot <- list()

for (i in seq_along(industry_list)) {
  data_list[[i]] <- subset(using, naics == industry_list[[i]])
  data_list[[i]]$comp <- "High Competition"
  data_list[[i]]$comp[data_list[[i]]$HHI_lower > 751] <- "Low Competition"
  
}

#################################################################
#                         Clean the Data                        #
#################################################################
setwd(path_output)

data_list[[1]]$Labor.Force_log <- log(data_list[[1]]$Labor.Force)
data_list[[1]]$population_log <- log(data_list[[1]]$population)

data_list[[1]]$group[data_list[[1]]$Uber_active==0] <- "No Uber"
data_list[[1]]$group[data_list[[1]]$Uber_active==1] <- "Uber Active"

data_list[[1]]$data_type <- "Observed"
labor_force_bins$data_type <- "Binned Total"
labor_force_bins$Labor.Force_log <- log(labor_force_bins$bin_labor)
labor_force_bins$group <- "Aggregate"

names(labor_force_bins) <- c("quantile", "Labor.Force","data_type","Labor.Force_log","group")

desc_quantile <- subset(data_list[[1]], years==2018) %>% dplyr::select("quantile",
                                 "Labor.Force_log",
                                 "Labor.Force",
                                 "group",
                                 "data_type")

labor_force_bins <- labor_force_bins %>% dplyr::select("quantile",
                                                  "Labor.Force_log",
                                                  "Labor.Force",
                                                  "group",
                                                  "data_type")
desc_quantile <- rbind(desc_quantile, labor_force_bins)
desc_quantile$group <- as.factor(desc_quantile$group)

scatter <- ggplot(data=desc_quantile, aes(x = quantile, y = Labor.Force_log))  + 
  geom_jitter(alpha=.25, aes(size = Labor.Force_log)) +
  # geom_rug(col=rgb(.5,0,0), alpha = .1) +
  ylab("Log(County Labor Force)") +
  xlab("HHI Quantile") +
  scale_y_continuous(label=comma,
                     limits=c(min(desc_quantile$Labor.Force_log),max(desc_quantile$Labor.Force_log))) + 
  scale_size_continuous(label=comma) +
  theme(axis.text = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
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
        legend.title = element_blank())  +
  guides(size = FALSE,
         color = guide_legend(override.aes = list(alpha = 1, size = 5),
                             ncol = 2),
         alpha = FALSE) +
  facet_grid(~data_type)
  

setwd(path_output)
pdf(file = paste0("population_HHI.pdf"), width = 12, height = 8)
scatter
dev.off()

scatter <- ggplot(data=data_list[[1]], aes(x = quantile, y = HHI_lower))  + 
  geom_point(alpha=.25, aes(size = Labor.Force)) +
  # geom_rug(col=rgb(.5,0,0), alpha = .1) +
  ylab("County HHI") +
  xlab("HHI Quantile") +
  scale_y_continuous(label=comma,
                     limits=c(min(data_list[[1]]$HHI_lower),max(data_list[[1]]$HHI_lower))) + 
  scale_size_continuous(label=comma) +
  theme(axis.text = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
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
        legend.title = element_blank())  +
  guides(size = FALSE,
         color = guide_legend(override.aes = list(alpha = 1, size = 5),
                              ncol = 2),
         alpha = FALSE) +
  facet_grid(~data_type)


setwd(path_output)
pdf(file = paste0("HHI_quantile.pdf"), width = 12, height = 8)
scatter
dev.off()


for (i in seq_along(industry_list)) {
  
  data_list[[i]]$Uber_active_factor <- "Uber Not Active" 
  data_list[[i]]$Uber_active_factor[data_list[[i]]$Uber_active==1] <- "Uber Active"
  data_list[[i]]$Uber_active_factor <- as.factor(data_list[[i]]$Uber_active_factor)

  annual_estab[[i]] <- data_list[[i]] %>% 
    group_by(years) %>%
    summarize(Estab = sum(estab),
              labor_force = sum(Labor.Force),
              y = weighted.mean(estab_pop, Labor.Force))


  annual_rcp[[i]] <- data_list[[i]] %>% 
    group_by(years) %>%
    summarize(Estab = sum(estab),
              labor_force = sum(Labor.Force),
              y = weighted.mean(receipt_estab, Labor.Force))

  annual_estab_plot[[i]] <- ggplot(data = annual_estab[[i]],
                                   aes(x = years,y = y)) +
                                  geom_point(size = 5) +
                                  geom_line(size = 1.5) +
                                  ylab("Establishments/Labor Force") + xlab("Year") +
                                  theme(plot.title = element_text(size=20),
                                        axis.text = element_text(size = 20),
                                        axis.title.y = element_text(size = 20),
                                        axis.title.x = element_text(size = 20),
                                        panel.background = element_rect(fill = "white"),
                                        panel.grid = element_line(colour = "grey"),
                                        axis.line = element_line(colour = "black"),
                                        legend.position = "bottom",
                                        legend.title = element_blank(),
                                        legend.key = element_rect(fill="white"),
                                        legend.text = element_text(size = 20),
                                        legend.background = element_rect(fill=NA))
  
  annual_rcp_plot[[i]] <- ggplot(data = annual_rcp[[i]],
                                aes(x = years,y = y)) +
                                geom_point(size = 5) +
                                geom_line(size = 1.5) +
                                coord_cartesian(ylim=c(45000, 80000)) +
                                ylab("Average Receipts") + xlab("Year") +
                                scale_y_continuous(label=comma) +
                                theme(plot.title = element_text(size=20),
                                      axis.text = element_text(size = 20),
                                      axis.title.y = element_text(size = 20),
                                      axis.title.x = element_text(size = 20),
                                      panel.background = element_rect(fill = "white"),
                                      panel.grid = element_line(colour = "grey"),
                                      axis.line = element_line(colour = "black"),
                                      legend.position = "bottom",
                                      legend.title = element_blank(),
                                      legend.key = element_rect(fill="white"),
                                      legend.text = element_text(size = 20),
                                      legend.background = element_rect(fill=NA))
  
  

  # data_list[[i]] <- subset(data_list[[i]], min_change<2.5)
  
  uber[[i]] <- subset(data_list[[i]], Uber_active_factor=="Uber Active")
  no_uber[[i]]  <- subset(data_list[[i]], Uber_active_factor=="Uber Not Active")
  
  estab_uber_plot[[i]] <- ggplot(data = data_list[[i]],
                                aes(x = min_change,
                                    y = estab_pop,
                                    weight = population,
                                    color=Uber_active_factor,
                                    size = population)) + 
                                geom_point(data = uber[[i]],alpha =.3) +
                                geom_smooth(data=uber[[i]], method = "lm",color = "green3") +
                                geom_point(data = no_uber[[i]],alpha =.3) +
                                geom_smooth(data= no_uber[[i]], method = "lm",color = "red") +
                                ylab("Establishments/Labor Force") + xlab("Real Minimum Wage (2016 $s)") +
                                theme(plot.title = element_text(size=20),
                                      axis.text = element_text(size = 20),
                                      axis.title.y = element_blank(),
                                      axis.title.x = element_text(size = 20),
                                      panel.background = element_rect(fill = "white"),
                                      panel.grid = element_line(colour = "grey"),
                                      axis.line = element_line(colour = "black"),
                                      legend.position = "bottom",
                                      legend.title = element_blank(),
                                      legend.key = element_rect(fill="white"),
                                      legend.text = element_text(size = 20),
                                      legend.background = element_rect(fill=NA)) + 
                                guides(colour = guide_legend(override.aes = list(size=5))) +
                                scale_color_manual(values = c("green3","black")) +
                                scale_radius()+
                                guides(size = FALSE)
  
  
  rcp_uber_plot[[i]] <- ggplot(data = data_list[[i]],
                              aes(x = min_change,
                                  y = receipt_estab,
                                  weight = population,
                                  color=Uber_active_factor,
                                  size = population)) + 
                              geom_point(data = uber[[i]],alpha =.3) +
                              geom_smooth(data=uber[[i]], method = "lm",color = "green3") +
                              geom_point(data = no_uber[[i]],alpha =.3) +
                              geom_smooth(data= no_uber[[i]], method = "lm",color = "red") +
                              ylab("Average Receipts") + xlab("Real Minimum Wage (2016 $s)") +
                              theme(plot.title = element_text(size=20),
                                    axis.text = element_text(size = 20),
                                    axis.title.y = element_blank(),
                                    axis.title.x = element_text(size = 20),
                                    panel.background = element_rect(fill = "white"),
                                    panel.grid = element_line(colour = "grey"),
                                    axis.line = element_line(colour = "black"),
                                    legend.position = "bottom",
                                    legend.title = element_blank(),
                                    legend.key = element_rect(fill="white"),
                                    legend.text = element_text(size = 20),
                                    legend.background = element_rect(fill=NA)) + 
                              guides(colour = guide_legend(override.aes = list(size=5))) +
                              scale_color_manual(values = c("green3","black")) +
                              scale_y_continuous(label=comma) +
                              scale_radius()+
                              guides(size = FALSE)

} 


annual_estab[[1]]$industry <- "County Total"
annual_estab[[9]]$industry <- "Trans./Warehousing"
annual_estab[[1]]$dependent <- "Estab./Labor Force"
annual_estab[[9]]$dependent <- "Estab./Labor Force"

annual_rcp[[1]]$industry <- "County Total"
annual_rcp[[9]]$industry <- "Trans./Warehousing"
annual_rcp[[1]]$dependent <- "Average Receipts"
annual_rcp[[9]]$dependent <- "Average Receipts"

annual <- rbind(annual_estab[[1]],
      annual_estab[[9]],
      annual_rcp[[1]],
      annual_rcp[[9]])



annual_plot <- ggplot(data = annual,
                      aes(x = years, y = y)) +
                      geom_point(size = 5) +
                      geom_line(size = 1.5) +
                      xlab("Year") +
                      scale_y_continuous(label=comma) +
                      theme(plot.title = element_text(size=20),
                            axis.text = element_text(size = 20),
                            axis.title.y = element_blank(),
                            axis.title.x = element_text(size = 30),
                            strip.text.x = element_text(size = 20),
                            strip.text.y = element_text(size = 20),
                            strip.background = element_rect(fill = "white"),
                            panel.background = element_rect(fill = "white"),
                            panel.grid = element_line(colour = "grey"),
                            panel.spacing = unit(2, "lines"),
                            axis.line = element_line(colour = "black"),
                            legend.position = "bottom",
                            legend.title = element_blank(),
                            legend.key = element_rect(fill="white"),
                            legend.text = element_text(size = 20),
                            legend.background = element_rect(fill=NA)) +
                      facet_wrap(dependent ~ industry,
                                 scales = "free")
    


pdf(file = paste0("annual_plot.pdf"), width = 15, height = 12)
plot(annual_plot)
dev.off()


uber_total_estab <- uber[[1]] %>% dplyr::select("min_change",
                                                 "estab_pop",
                                                 "population",
                                                 "Uber_active_factor")

uber_trans_estab <- uber[[9]] %>% dplyr::select("min_change",
                                                 "estab_pop",
                                                 "population",
                                                 "Uber_active_factor")
no_uber_total_estab <- no_uber[[1]] %>% dplyr::select("min_change",
                                                 "estab_pop",
                                                 "population",
                                                 "Uber_active_factor")

no_uber_trans_estab <- no_uber[[9]] %>% dplyr::select("min_change",
                                                "estab_pop",
                                                "population",
                                                "Uber_active_factor")

uber_total_rcp <- uber[[1]] %>% dplyr::select("min_change",
                                                 "receipt_estab",
                                                 "population",
                                                 "Uber_active_factor")

uber_trans_rcp <- uber[[9]] %>% dplyr::select("min_change",
                                                "receipt_estab",
                                                "population",
                                                "Uber_active_factor")

no_uber_total_rcp <- no_uber[[1]] %>% dplyr::select("min_change",
                                                       "receipt_estab",
                                                       "population",
                                                       "Uber_active_factor")

no_uber_trans_rcp <- no_uber[[9]] %>% dplyr::select("min_change",
                                                      "receipt_estab",
                                                      "population",
                                                      "Uber_active_factor")


uber_total_estab$industry <- "County Total"
no_uber_total_estab$industry <- "County Total"
uber_trans_estab$industry <- "Trans./Warehousing"
no_uber_trans_estab$industry <- "Trans./Warehousing"

uber_total_estab$dependent <- "Estab./Labor Force"
no_uber_total_estab$dependent <- "Estab./Labor Force"
uber_trans_estab$dependent <- "Estab./Labor Force"
no_uber_trans_estab$dependent <- "Estab./Labor Force"

uber_total_rcp$industry <- "County Total"
no_uber_total_rcp$industry <- "County Total"
uber_trans_rcp$industry <- "Trans./Warehousing"
no_uber_trans_rcp$industry <- "Trans./Warehousing"

uber_total_rcp$dependent <- "Average Receipts"
no_uber_total_rcp$dependent <- "Average Receipts"
uber_trans_rcp$dependent <- "Average Receipts"
no_uber_trans_rcp$dependent <- "Average Receipts"


names(uber_total_estab)[2] <- "y"
names(no_uber_total_estab)[2] <- "y"
names(uber_trans_estab)[2] <- "y"
names(no_uber_trans_estab)[2] <- "y"
names(uber_total_rcp)[2] <- "y"
names(no_uber_total_rcp)[2] <- "y"
names(uber_trans_rcp)[2] <- "y"
names(no_uber_trans_rcp)[2] <- "y"

uber_no_uber <- rbind(uber_total_estab,
                      no_uber_total_estab,
                      uber_trans_estab,
                      no_uber_trans_estab,
                      uber_total_rcp,
                      no_uber_total_rcp,
                      uber_trans_rcp,
                      no_uber_trans_rcp)



uber_no_uber$Uber_active_factor <- factor(uber_no_uber$Uber_active_factor, levels = c("Uber Not Active","Uber Active"))

p <- ggplot(data = uber_no_uber,
       aes(x = min_change,
           y = y,
           weight = population,
           # color=Uber_active_factor,
           size = population)) + 
  geom_point(data = subset(uber_no_uber, Uber_active_factor=="Uber Active"),alpha =.3) +
  geom_point(data = subset(uber_no_uber, Uber_active_factor=="Uber Not Active"),alpha =.3) +
  geom_smooth(data= subset(uber_no_uber, Uber_active_factor=="Uber Not Active"), method = "lm", aes(color = "Uber Not Active")) +
  geom_smooth(data= subset(uber_no_uber, Uber_active_factor=="Uber Active"), method = "lm", aes(color = "Uber Active")) +
  xlab("Real Minimum Wage (2016 $s)") +
  theme(plot.title = element_text(size=20),
        axis.text = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 30),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        panel.spacing = unit(2, "lines"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 25),
        legend.background = element_rect(fill="white")) +
  scale_color_manual(values = c("green3","red")) +
  scale_y_continuous(label=comma) +
  scale_radius()+
  guides(size = FALSE) +
facet_wrap(dependent ~ industry,
           scales = "free")


pdf(file = paste0("desc_estab_rcp_total_trans.pdf"), width = 15, height = 12)
plot(p)
dev.off()

png(file = paste0("desc_estab_rcp_total_trans.png"), width = 1250, height = 1000)
plot(p)
dev.off()
