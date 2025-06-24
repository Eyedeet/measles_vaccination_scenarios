#Sensivity analyses - simulation only from 2015 onwards
################################################################################
#Outbreak scnenarios
#-CPRD data
#- no waning
################################################################################

#set up
options(scipen = 999)
source("R/function_figures.R")
source("R/function_vaccination_data.R")

## Import libraries 
devtools::install_github("alxsrobert/seirvodin")
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


create_scenario <- function(scenario_name, burnin = 5000, waning = "no", vax = "cprd"){
  scenario <- scenario_name
  # Number of simulations per sample
  n_part <- 25
  n_samples <- 100
  
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
                              scenario = scenario, vax = "cprd", regions = regions, 
                              year_per_age = year_per_age)
  
  
  all_specs <- seirvodin::specs_simulations(
    year_start = year_start, N_year = N_year, waning = waning, burnin = burnin, 
    n_samples = n_samples, nowane = FALSE, deterministic = FALSE
  )
  
  
  ## Import the parameter estimates
  if(waning == "no"){
    if(vax == "cprd"){
      pmcmc_run <- readRDS("Output/cprd_degree/no.RDS")
    } else if (vax == "cover"){
      pmcmc_run <- readRDS("Output/cover_degree/no.RDS")
    }
  } else if(waning %in% c("since_vax", "early")){
    if(vax == "cprd"){
      pmcmc_run <- readRDS("Output/cprd_degree/since_vax.RDS")
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

##############################################################################
#### Analysis of all_output ####
reference <- create_scenario(scenario_name = "reference")
saveRDS(reference, file="Output/models/reference_2015.rda")
rm(reference)
gc()


early_second <- create_scenario(scenario_name = "CPRD_earlyMMR2_2015")
saveRDS(early_second, file="Output/models/early_second_2015.rda")
rm(early_second)
gc()

#compare the results
tmp <- readRDS("Output/models/reference_2015.rda")
tmp <- tmp[grep("new_I", rownames(tmp)), ,]
#filter only the last 5 years (2015-2020)
tmp <- tmp[, , 6:10]
summary_table <- summary(apply(tmp, 2, sum))

tmp2 <- readRDS("Output/models/early_second_2015.rda")
tmp2 <- tmp2[grep("new_I", rownames(tmp2)), ,]
#filter only the last 5 years (2015-2020)
tmp2 <- tmp2[, , 6:10]
summary_table <- rbind(summary_table, 
                       summary(apply(tmp2, 2, sum)))
summary_table <- cbind(summary_table, c("Reference 2015-2020",
                         "Early MMR2 2015-2020"))
summary_table <- as.data.table(summary_table)
summary_table[, result := paste0(`Median`, " (", `1st Qu.`, "; ", `3rd Qu.`, ")")]
med_ref <- as.numeric(summary_table$Median[1])
summary_table[, Median := as.numeric(Median)]
summary_table[, `1st Qu.` := as.numeric(`1st Qu.`)]
summary_table[, `3rd Qu.` := as.numeric(`3rd Qu.`)]
summary_table[, diff_per := paste0(round(  (((med_ref-Median)/med_ref)*100), digits = 2),
                                   " (" , round((((med_ref-`3rd Qu.`)/med_ref)*100), digits = 2),
                                   "; ", round((((med_ref-`1st Qu.`)/med_ref)*100), digits = 2),")")]


#table of yearly cases by scenario
yearly_cases <- function(output){
  tmp <- readRDS(paste0("Output/models/", output))
  rows_new_cases <- rownames(tmp)[grep("new_I", rownames(tmp))]
  yearly_cases <- data.table(year =  seq(2010, 2019, 1),
                             median = rep(0, times = 10),
                             lb = rep(0, times = 10),
                             ub = rep(0, times = 10))
  
  for(i in 1:10){
    
    dt <- tmp[, , i]
    summary_table <- summary(apply(dt, 2, sum))
    yearly_cases$median[i] <- summary_table[[3]]
    yearly_cases$lb[i] <- summary_table[[2]]
    yearly_cases$ub[i] <- summary_table[[5]]
  }
  
  return(yearly_cases)
}

tmp1 <- yearly_cases("reference_2015.rda")
tmp2 <- yearly_cases("early_second_2015.rda")
res <- cbind(tmp1, tmp2)
colnames(res) <- c("year", "median",  "lb", "ub", "year" ,"median.1" ,
                    "lb.1",    "ub.1"  )
res[, diff_MMR2early := paste0(round((((median-median.1)/median)*100), digits = 2),
                           " (" , round((((median-ub.1)/median)*100), digits = 2),
                           "; ", round((((median-lb.1)/median)*100), digits = 2),")")]
