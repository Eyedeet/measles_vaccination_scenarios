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

#############################################################################

#regional cases for reference, early MMR2, and MMR1+0.5

reference <-regional_cases("reference.rda")
reference[, IQR_ref := paste0(median, " (", lb, "; ", ub, ")")]
earlyMMR2 <-regional_cases("early_second.rda")
earlyMMR2[, IQR_early := paste0(median, " (", lb, "; ", ub, ")")]
MMR1plu05 <- regional_cases("D1_05.rda")
MMR1plu05[, IQR_MMR1 := paste0(median, " (", lb, "; ", ub, ")")]

res <- cbind(reference, earlyMMR2, MMR1plu05)
colnames(res) <- c("region", "median",  "lb", "ub","IQR_ref" , "region" ,"median.1" ,
                   "lb.1",    "ub.1" , "IQR_early", "region", "median.2" ,  
                   "lb_2" ,  "ub.2" , "IQR_MMR1" )

res[, diff_MMR1 := paste0(round(100-((median.1/median)*100),digits = 2),
                                   " (" ,round(100-((ub.1/median)*100), digits = 2),
                                   "; ", round(100-((lb.1/median)*100), digits = 2), ")")]
res[, diff_MMR2 := paste0(round(100-((median.2/median)*100),digits = 2),
                                    " (" ,round(100-((ub.2/median)*100), digits = 2), digits = 2),
                                    "; ", round(100-((lb_2/median)*100), ")")]
res <- res[, c("region", "IQR_ref" ,  "IQR_early", "IQR_MMR1",
                                   "diff_MMR1", "diff_MMR2")]


write.csv2(res, file = "Output/regional_cases.csv")

#yearly cases by scenario
reference <-yearly_cases("reference.rda")
reference[, IQR_ref := paste0(median, " (", lb, "; ", ub, ")")]
earlyMMR2 <-yearly_cases("early_second.rda")
earlyMMR2[, IQR_early := paste0(median, " (", lb, "; ", ub, ")")]
MMR1plu05 <- yearly_cases("D1_05.rda")
MMR1plu05[, IQR_MMR1 := paste0(median, " (", lb, "; " , ub, ")")]

res2 <- cbind(reference, earlyMMR2, MMR1plu05)
colnames(res2) <- c("year", "median",  "lb", "ub","IQR_ref" , "year" ,"median.1" ,
                   "lb.1",    "ub.1" , "IQR_early", "year", "median.2" ,  
                   "lb.2" ,  "ub.2" , "IQR_MMR1" )

res2[, diff_MMR1 := paste0(round(100-((median.1/median)*100),digits = 2),
                          " (" ,round(100-((ub.1/median)*100), digits = 2),
                          "; ", round(100-((lb.1/median)*100), digits = 2), ")")]
res2[, diff_MMR2 := paste0(round(100-((median.2/median)*100),digits = 2),
                          " (" ,round(100-((ub.2/median)*100), digits = 2), digits = 2),
    "; ", round(100-((lb.2/median)*100), ")")]
res2 <- res2[, c("year", "IQR_ref" ,  "IQR_early", "IQR_MMR1",
               "diff_MMR1", "diff_MMR2")]

write.csv2(res, file = "Output/yearly_cases.csv")


