################################################################################
#Outbreak scnenarios
#-COVER data
#-no waning
################################################################################

#set up
options(scipen = 999)
source("R/function_figures.R")
source("R/import_vaccination_data.R")

## Import libraries 
#devtools::install_github("alxsrobert/seirvodin")
library(seirvodin)
library(dplyr)
library(socialmixr)
library(odin.dust)
library(mcstate)
library(tictoc)
library(tidyr)
library(ggplot2)
library(data.table)

clean_mcmc_pars <- function(mcmc_pars){
  colnames(mcmc_pars)[which(colnames(mcmc_pars) == "catchup")] <- "catchup_10"
  colnames(mcmc_pars)[which(colnames(mcmc_pars) == "catchup2")] <- "catchup2_7"
  colnames(mcmc_pars)[which(colnames(mcmc_pars) == "recov11to15")] <- "recov_8"
  colnames(mcmc_pars)[which(colnames(mcmc_pars) == "recov16to20")] <- "recov_9"
  colnames(mcmc_pars)[which(colnames(mcmc_pars) == "recov21to30")] <- "recov_10"
  colnames(mcmc_pars)[which(colnames(mcmc_pars) == "recov31to40")] <- "recov_11"
  colnames(mcmc_pars)[which(colnames(mcmc_pars) == "recov40plus")] <- "recov_12"
  colnames(mcmc_pars)[which(colnames(mcmc_pars) == "v_70s")] <- "v_11"
  
  return(mcmc_pars)
}


create_scenario <- function(scenario_name, burnin = 5000, waning = "since_vax", vax = "cover"){
  scenario <- scenario_name
  # Number of simulations per sample
  n_part <- 100
  n_samples <- 25
  
  #### Import data and model fit ####
  
  year_start <- 2010
  N_year <- 10
  N_time <- t_tot <- 365 * N_year
  
  
  age <- c("[0,1)", "[1,2)", "[2,3)", "[3,4)", "[4,5)", "[5,6)", "[6,10)", "[10,15)",
           "[15,20)", "[20,30)", "[30,40)", "[40,100]")
  year_per_age <- c(1, 1, 1, 1, 1, 1, 4, 5, 5, 10, 10, 40)
  
  regions <- c("North East", "North West", "Yorkshire and The Humber", "East Midlands",
               "West Midlands", "East", "London", "South East", "South West")
  
  ## Import the different data streams into a list.
  # Use scenario to move between vaccine scenarios (early / early_timely etc..)
  all_data <- import_all_data(year_start = year_start, N_year = N_year, 
                              scenario = scenario, vax = "cover", regions = regions, 
                              year_per_age = year_per_age)
  
  
  all_specs <- seirvodin::specs_simulations(
    year_start = year_start, N_year = N_year, waning = waning, burnin = burnin, 
    n_samples = n_samples, nowane = FALSE, deterministic = FALSE
  )
  
  
  ## Import the parameter estimates
  if(waning == "no"){
    if(vax == "cover"){
      pmcmc_run <- readRDS("Output/cover_degree/no.RDS")
    } else if (vax == "cover"){
      pmcmc_run <- readRDS("Output/cover_degree/no.RDS")
    }
  } else if(waning %in% c("since_vax", "early")){
    if(vax == "cover"){
      pmcmc_run <- readRDS("Output/cover_degree/since_vax.RDS")
    } else if (vax == "cover"){
      pmcmc_run <- readRDS("Output/cover_degree/since_vax.RDS")
    }
  } else stop("wrong value of waning, should be `no`, `since_vax`, or `early")
  
  
  pmcmc_run$pars <- clean_mcmc_pars(mcmc_pars = pmcmc_run$pars)
  
  all_output <- 
    seirvodin::generate_outbreaks(
      model_run = pmcmc_run, model = seirvodin::seirv_age_region, 
      list_specs = all_specs, list_data = all_data, n_part = n_part, 
      verbose = T, aggreg_year = TRUE  )
  
  return(all_output)  
}


set.seed(1)
#### Analysis of all_output ####
reference <- create_scenario(scenario_name = "reference")
saveRDS(reference, file="Output/models/reference_waningCover.rda")
rm(reference)
gc()


early_second <- create_scenario(scenario_name = "early")
saveRDS(early_second, file="Output/models/early_second_waningCover.rda")
rm(early_second)
gc()

MMR2_as_MMR1 <- create_scenario(scenario_name = "MMR2_as_MMR1")
saveRDS(early_second_speedy, file="Output/models/MMR2_as_MMR1_waningCover.rda")
rm(early_second_speedy)
gc()


MMR2_at_5 <- create_scenario(scenario_name = "MMR2_at5")
saveRDS(MMR2_at_5, file="Output/models/MMR2_at_5_waningCover.rda")
rm(MMR2_at_5)
gc()


D2_earlyplus1 <- create_scenario(scenario_name = "cover_earlyMMR2plus1")
saveRDS(D2_earlyplus1, file="Output/models/D2_earlyplus1_waningCover.rda")
rm(D2_earlyplus1)
gc()

D2_earlyplus3 <- create_scenario(scenario_name = "cover_earlyMMR2plus3")
saveRDS(D2_earlyplus1, file="Output/models/D2_earlyplus3_waningCover.rda")
rm(D2_earlyplus1)
gc()



D1_05 <- create_scenario(scenario_name = "cover_MMR1plus05")
saveRDS(D1_05, file="Output/models/D1_05_waningCover.rda")
rm(D1_05)
gc()


D1_1 <- create_scenario(scenario_name = "cover_MMR1plus1")
saveRDS(D1_1, file="Output/models/D1_1_waningCover.rda")
rm(D1_1)
gc()

D2_1 <- create_scenario(scenario_name = "MMR2plus1")
saveRDS(D2_1, file="Output/models/D2_1_waningCover.rda")
rm(D2_1)
gc()


D2_3 <- create_scenario(scenario_name = "MMR2plus3")
saveRDS(D2_3, file="Output/models/D2_3_waningCover.rda")
rm(D2_3)
gc()



D2_minus3 <- create_scenario(scenario_name = "cover_earlyMMR2minus3")
saveRDS(D2_minus3, file="Output/models/D2_minus3_waningCover.rda")
rm(D2_minus3)
gc()

D2_minus5 <- create_scenario(scenario_name = "cover_earlyMMR2minus5")
saveRDS(D2_minus5, file="Output/models/D2_minus5_waningCover.rda")
rm(D2_minus5)
gc()



#---summary table for results
all_models <- list.files("Output/models/", pattern = "_waningCover")
tmp <- readRDS(paste0("Output/models/", all_models[1]))
tmp <- tmp[grep("new_I", rownames(tmp)), ,]
summary_table <- summary(apply(tmp, 2, sum))
rm(tmp)
gc()



for(i in 2:length(all_models)){
  tmp <- readRDS(paste0("Output/models/", all_models[i]))
  tmp <- tmp[grep("new_I", rownames(tmp)), ,]
  row <- summary(apply(tmp, 2, sum))
  rm(tmp)
  summary_table <- rbind(summary_table, row)
  gc()
}

#check mames
summary_table <- cbind(c("MMR1 +0.25","MMR1 +0.5","MMR1 +1",
                         "MMR2 +0.25","MMR2 +0.5","MMR2 +1",
                         "MMR2 + 3",
                         "early +0.25", "early +0.5","early +1",
                         "MMR2 -3","MMR2 -5","early second",
                         "early second fast","early MMR2 like MMR1",
                         "MMR2 at 5", "reference"), summary_table)
summary_table <- as.data.table(summary_table)
summary_table[, result := paste0(`Median`, " (", `1st Qu.`, " ;", `3rd Qu.`, ")")]
med_ref <- as.numeric(summary_table$Median[1])
summary_table[, Median := as.numeric(Median)]
summary_table[, `1st Qu.` := as.numeric(`1st Qu.`)]
summary_table[, `3rd Qu.` := as.numeric(`3rd Qu.`)]
summary_table[, diff_per := paste0(round(100-((Median/med_ref)*100),digits = 2),
                                   " (" ,round(100-((`1st Qu.`/med_ref)*100), digits = 2),
                                   " ;", round(100-((`3rd Qu.`/med_ref)*100), digits = 2), ")")]

write.csv2(summary_table, file = file_path)


#comparing the scenarios in graphs
#improving coverage
plot1 <- yearly_cases_fig_flexible_new("reference_waningCover.rda","D2_1_waningCover.rda"  , 
                                       "Reference","MMR2 +1%",
                                       "#2c5985", "#c4263e")
plot2 <- yearly_cases_fig_flexible_new("D2_1_waningCover.rda", "D2_3_waningCover.rda",
                                       "MMR2 + 1%","MMR2 +3%", 
                                       "#c4263e", "#3a95b1")
plot3 <- yearly_cases_fig_flexible_new("D2_3_waningCover.rda", "D1_1_waningCover.rda",
                                       "MMR2 +3%","MMR1 +1%", 
                                       "#3a95b1","#ed5f54" )

library(cowplot)
plt <- plot_grid(plot1, plot2, plot3,
                 ncol = 1, nrow = 3, 
                 labels = c('A', 'B', 'C'),
                 label_size = 22,
                 label_y = 1.01,
                 label_x = 0.01,
                 scale = 0.9)
ggsave("Figures/Coverage_COVER_waning.png",
       plt,
       width =  6,
       height = 14,
       bg = "white")

#changing schedule
plot1 <- yearly_cases_fig_flexible_higher_y("reference_waningCover.rda", "MMR2_at_5_waningCover.rda",
                                            "Reference","School entry MMR2", 
                                            "#2c5985","#c4263e")
plot2<- yearly_cases_fig_flexible_new("reference_waningCover.rda", "early_second_waningCover.rda",
                                      "Reference","Early MMR2", 
                                      "#2c5985","#ed5f54")
plot3 <- yearly_cases_fig_flexible_new("early_second_waningCover.rda", "D2_earlyplus1_waningCover.rda",
                                       "Early MMR2","Early MMR2 +1%", 
                                       "#ed5f54","#3a95b1")
plot4 <- yearly_cases_fig_flexible_new("D2_earlyplus1_waningCover.rda", "MMR2_as_MMR1_waningCover.rda",
                                       "Early MMR2 +1%","Early MMR2 like MMR1", 
                                       "#3a95b1","#f77964")
plot5 <- yearly_cases_fig_flexible_new("early_second_waningCover.rda", "D2_minus3_waningCover.rda",
                                       "Early MMR2","Early MMR2 -3%", 
                                       "#ed5f54","#2e5b88")
plot6 <- yearly_cases_fig_flexible_new("early_second_waningCover.rda", "D2_minus5_waningCover.rda",
                                       "Early MMR2","Early MMR2 -5%", 
                                       "#ed5f54","#2a5783")

#improving coverage vs the schedule
plt <- plot_grid(plot1, plot2, plot3, plot4, 
                 plot5, plot6,
                 ncol = 2, nrow = 3, 
                 labels = c('A', 'B', 'C', 'D', 'E', 'F'),
                 label_size = 22,
                 label_y = 1.01,
                 label_x = 0.01,
                 scale = 0.9)
ggsave("Figures/Schedule_COVER_waning.png",
       plt,
       width =  12,
       height = 14,
       bg = "white")

