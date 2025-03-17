#table of regional cases by scenario

regional_cases <- function(output){
  
  tmp <- readRDS(paste0("Output/models/", output))
  rows_new_cases <- rownames(tmp)[grep("new_I", rownames(tmp))]
  regional_cases <- data.table(region =  c("North East", "North West", "Yorkshire and The Humber", "East Midlands",
                                           "West Midlands", "East", "London", "South East", "South West"),
                               median = rep(0, times = 9),
                               lb = rep(0, times = 9),
                               ub = rep(0, times = 9))                        
  
  for(i in 1:9){
    
    dt <- tmp[grep(paste0("reg", i),  rownames(tmp)), , ]
    summary_table <- summary(apply(dt, 2, sum))
    regional_cases$median[i] <- summary_table[[3]]
    regional_cases$lb[i] <- summary_table[[2]]
    regional_cases$ub[i] <- summary_table[[5]]
  }
  
  return(regional_cases)
}

#table of yearly cases by scenario
yearly_cases <- function(output){
  tmp <- readRDS(paste0("Output/models/", "reference.rda"))
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

#############################################################################

