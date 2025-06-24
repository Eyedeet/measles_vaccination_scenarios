####Generating all plots
source("R/function_figures.R")
library(cowplot)
library(tidyverse)
library(data.table)


###############################################################################
#Figure 3 - original cases against reference scenario
cases <- data.table(year = c(2010:2019),
                    cases_surv = c(374,  1064,  1897,  1447,  104,   92,   522,   248,   964,   792))

ref <- readRDS(paste0("Output/models/reference.rda"))

rows_new_cases <- rownames(ref)[grep("new_I", rownames(ref))]
## Sum number of new infected per day
new_cases <- cbind.data.frame(reg = "National", iter = seq_len(ncol(ref)), 
                              ref[rows_new_cases,,] %>% colSums())
time <- time <- seq(2010, 2019, 1) 
colnames(new_cases) <- c("reg", "iter", as.character(time))

## Change new_cases to long format (to then use ggplot) 
long_new_cases <- pivot_longer(as.data.frame(new_cases), 
                               cols = c(as.character(time)), 
                               names_to = "time",
                               values_to = "new_cases")
long_new_cases <- as.data.table(long_new_cases)
long_new_cases$time <- long_new_cases$time %>% as.numeric 

## Aggregate by region / iteration / year
cases_per_year <- long_new_cases[, lapply(.SD, sum), by = .(iter, time, reg)]

#average number of cases by iteration
tmp1 <- data.table()
for(i in 2010:2019){
  
  vec <- quantile(cases_per_year[time == i, new_cases], probs = c(0.025, 0.125, 0.25, 0.5, 0.75, 0.875, 0.975))
  new_row <- data.table(year = i, lb_95 = vec[[1]], lb_75= vec[[2]], lb_50 = vec[[3]], 
                        median = vec[[4]], ub_50 = vec[[5]], ub_75 = vec[[6]], ub_95 = vec[[7]])
  tmp1 <- rbind(tmp1, new_row)
}

tmp<- merge(tmp1, cases, by = "year")

plot <- tmp %>%
  ggplot(aes(x = year))+
  geom_line(aes(y = median), color = "#2c5985" )+
    geom_point(aes(y = cases_surv), color = "darkgrey", size = 3)+
  scale_color_manual(values = c("#2c5985"))+ 
  geom_ribbon (aes(ymin = lb_50, ymax = ub_50),  alpha = 0.5, linetype = 0, fill = "#2c5985")+
  geom_ribbon (aes(ymin = lb_95, ymax = ub_95),  alpha = 0.2, linetype = 0, fill = "#2c5985")+
  scale_fill_manual(values = c("#2c5985"))+
  scale_x_continuous(name = "Year", breaks = c(2011, 2013, 2015, 2017, 2019))+
  scale_y_continuous(name = "N measles cases", breaks = seq(0, 4000, by = 500), limits = c(0, 4000))+
  ylab("N measles cases")+
  xlab("Year")+
  theme_classic()+
  theme(legend.position="bottom",
        axis.text.x = element_text(color = "grey20", size = 20, angle = 45, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 22, angle = 0, hjust = .5, vjust = 0, face = "italic"),
        axis.title.y = element_text(color = "grey20", size = 22, angle = 90, hjust = .5, vjust = 1, face = "italic"),
        legend.text = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_blank())

ggsave("Figures/Reference_Surveillance.png",
       plot,
       width =  7,
       height = 6,
       bg = "white", dpi = 300)


################################################################################
#CPRD - no waning
#imrproving coverage
plot1 <- yearly_cases_fig_flexible_new("reference.rda","D2_1.rda"  , 
                                       "Reference","MMR2 +1%",
                                       "#2c5985", "#c4263e")
plot2 <- yearly_cases_fig_flexible_new("D2_1.rda", "D2_3.rda",
                                       "MMR2 + 1%","MMR2 +3%", 
                                       "#c4263e", "#3a95b1")
plot3 <- yearly_cases_fig_flexible_new("D2_3.rda", "D1_1.rda",
                                       "MMR2 +3%","MMR1 +1%", 
                                       "#3a95b1","#ed5f54" )


plt <- plot_grid(plot1, plot2, plot3,
                 ncol = 1, nrow = 3, 
                 labels = c('A', 'B', 'C'),
                 label_size = 22,
                 label_y = 1.01,
                 label_x = 0.01,
                 scale = 0.9)
ggsave("Figures/Coverage_CPRD_no_waning.png",
       plt,
       width =  6,
       height = 14,
       bg = "white", dpi = 300)


#changing schedule
plot1 <- yearly_cases_fig_flexible_higher_y("reference.rda", "MMR2_at_5.rda",
                                            "Reference","School entry MMR2", 
                                            "#2c5985","#c4263e")
plot2<- yearly_cases_fig_flexible_new("reference.rda", "early_second.rda",
                                      "Reference","Early MMR2", 
                                      "#2c5985","#ed5f54")
plot3 <- yearly_cases_fig_flexible_new("early_second.rda", "D2_earlyplus1.rda",
                                       "Early MMR2","Early MMR2 +1%", 
                                       "#ed5f54","#3a95b1")
plot4 <- yearly_cases_fig_flexible_new("D2_earlyplus1.rda", "MMR2_as_MMR1.rda",
                                       "Early MMR2 +1%","Early MMR2 like MMR1", 
                                       "#3a95b1","#f77964")
plot5 <- yearly_cases_fig_flexible_new("early_second.rda", "D2_minus3.rda",
                                       "Early MMR2","Early MMR2 -3%", 
                                       "#ed5f54","#2e5b88")
plot6 <- yearly_cases_fig_flexible_new("early_second.rda", "D2_minus5.rda",
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
ggsave("Figures/Schedule_CPRD_no_waning.png",
       plt,
       width =  12,
       height = 14,
       bg = "white", dpi = 300)

###############################################################################

#comparing the scenarios in graphs
#improving coverage
plot1 <- yearly_cases_fig_flexible_new("reference_cover.rda","D2_1_cover.rda"  , 
                                       "Reference","MMR2 +1%",
                                       "#2c5985", "#c4263e")
plot2 <- yearly_cases_fig_flexible_new("D2_1_cover.rda", "D2_3_cover.rda",
                                       "MMR2 + 1%","MMR2 +3%", 
                                       "#c4263e", "#3a95b1")
plot3 <- yearly_cases_fig_flexible_new("D2_3_cover.rda", "D1_1_cover.rda",
                                       "MMR2 +3%","MMR1 +1%", 
                                       "#3a95b1","#ed5f54" )


plt <- plot_grid(plot1, plot2, plot3,
                 ncol = 1, nrow = 3, 
                 labels = c('A', 'B', 'C'),
                 label_size = 22,
                 label_y = 1.01,
                 label_x = 0.01,
                 scale = 0.9)
ggsave("Figures/Coverage_COVER_no_waning.png",
       plt,
       width =  6,
       height = 14,
       bg = "white", dpi = 300)

#changing schedule
plot1 <- yearly_cases_fig_flexible_higher_y("reference_cover.rda", "MMR2_at_5_cover.rda",
                                            "Reference","School entry MMR2", 
                                            "#2c5985","#c4263e")
plot2<- yearly_cases_fig_flexible_new("reference_cover.rda", "early_second_cover.rda",
                                      "Reference","Early MMR2", 
                                      "#2c5985","#ed5f54")
plot3 <- yearly_cases_fig_flexible_new("early_second_cover.rda", "D2_earlyplus1_cover.rda",
                                       "Early MMR2","Early MMR2 +1%", 
                                       "#ed5f54","#3a95b1")
plot4 <- yearly_cases_fig_flexible_new("D2_earlyplus1_cover.rda", "MMR2_as_MMR1_cover.rda",
                                       "Early MMR2 +1%","Early MMR2 like MMR1", 
                                       "#3a95b1","#f77964")
plot5 <- yearly_cases_fig_flexible_new("early_second_cover.rda", "D2_minus3_cover.rda",
                                       "Early MMR2","Early MMR2 -3%", 
                                       "#ed5f54","#2e5b88")
plot6 <- yearly_cases_fig_flexible_new("early_second_cover.rda", "D2_minus5_cover.rda",
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
ggsave("Figures/Schedule_COVER_no_waning.png",
       plt,
       width =  12,
       height = 14,
       bg = "white", dpi = 300)

################################################################################
#CPRD with waning 

#changing schedule
plot1 <- yearly_cases_fig_flexible_higher_y("reference_waningCPRD.rda", "MMR2_at_5_waningCPRD.rda",
                                            "Reference","School entry MMR2", 
                                            "#2c5985","#c4263e")
plot2<- yearly_cases_fig_flexible_new("reference_waningCPRD.rda", "early_second_waningCPRD.rda",
                                      "Reference","Early MMR2", 
                                      "#2c5985","#ed5f54")
plot3 <- yearly_cases_fig_flexible_new("early_second_waningCPRD.rda", "D2_earlyplus1_waningCPRD.rda",
                                       "Early MMR2","Early MMR2 +1%", 
                                       "#ed5f54","#3a95b1")
plot4 <- yearly_cases_fig_flexible_new("D2_earlyplus1_waningCPRD.rda", "MMR2_as_MMR1_waningCPRD.rda",
                                       "Early MMR2 +1%","Early MMR2 like MMR1", 
                                       "#3a95b1","#f77964")
plot5 <- yearly_cases_fig_flexible_new("early_second_waningCPRD.rda", "D2_minus3_waningCPRD.rda",
                                       "Early MMR2","Early MMR2 -3%", 
                                       "#ed5f54","#2e5b88")
plot6 <- yearly_cases_fig_flexible_new("early_second_waningCPRD.rda", "D2_minus5_waningCPRD.rda",
                                       "Early MMR2","Early MMR2 -5%", 
                                       "#ed5f54","#2a5783")
plot7<- yearly_cases_fig_flexible_new("reference_waningCPRD.rda", "early_second_waning3CPRD.rda",
                                      "Reference","Early MMR2 waning (3y)", 
                                      "#2c5985","#ed5f54")
plot8<- yearly_cases_fig_flexible_new("early_second_waningCPRD.rda", "early_second_waning3CPRD.rda",
                                      "Early MMR2 (5y) ","Early MMR2 waning (3y)", 
                                      "#2c5985","#3a95b1")

#improving coverage vs the schedule
plt <- plot_grid(plot1, plot2, plot3, plot4, 
                 plot5, plot6, plot7, plot8,
                 ncol = 2, nrow = 4, 
                 labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
                 label_size = 22,
                 label_y = 1.01,
                 label_x = 0.01,
                 scale = 0.9)
ggsave("Figures/Schedule_CPRD_waning_all.png",
       plt,
       width =  12,
       height = 16,
       bg = "white", dpi = 300)



################################################################################
#age proportiongs

#% of caseses
#reference & early MMR2
A <- plot_age_prop("reference.rda", "early_second.rda",
                   "Reference", "Early MMR2",
                   "black", "#3690ae")
#Reference and MMR1 + 0.5
B <-plot_age_prop("reference.rda", "D1_05.rda",
                        "Reference", "MMR1 + 0.5%",
                        "black", "#a90c38")

# Reference and school entry MMR2
C <-plot_age_prop("reference.rda", "MMR2_at_5.rda",
                  "Reference", "School entry MMR2",
                  "black", "#589a80")


#N cases
#reference & early MMR2
D <- plot_age_abs("reference.rda", "early_second.rda",
                   "Reference", "Early MMR2",
                   "black", "#3690ae")
#Reference and MMR1 + 0.5
E <-plot_age_abs("reference.rda", "D1_05.rda",
                  "Reference", "MMR1 + 0.5%",
                  "black", "#a90c38")

# Reference and school entry MMR2
Fe <- plot_age_abs("reference.rda", "MMR2_at_5.rda",
                    "Reference", "School entry MMR2",
                    "black", "#589a80")

#improving coverage vs the schedule
plt <- plot_grid(A, B, C,  D, E, Fe,
                 ncol = 3, nrow = 2, 
                 labels = c('A', 'B', 'C', 'D', 'E', 'F'),
                 label_size = 22,
                 label_y = 1.01,
                 label_x = 0.01,
                 scale = 0.9)

ggsave("Figures/Age_dist.png",
       plt,
       width =  14,
       height = 11,
       bg = "white", dpi = 300)
###############################################################################
#age proportiongs for  cover

#% of caseses
#reference & early MMR2
A <- plot_age_prop("reference_cover.rda", "early_second_cover.rda",
                   "Reference", "Early MMR2",
                   "black", "#3690ae")
#Reference and MMR1 + 0.5
B <-plot_age_prop("reference_cover.rda", "D1_05_cover.rda",
                  "Reference", "MMR1 + 0.5%",
                  "black", "#a90c38")

# Reference and school entry MMR2
C <-plot_age_prop("reference_cover.rda", "MMR2_at_5_cover.rda",
                  "Reference", "School entry MMR2",
                  "black", "#589a80")


#N cases
#reference & early MMR2
D <- plot_age_abs("reference_cover.rda", "early_second_cover.rda",
                  "Reference", "Early MMR2",
                  "black", "#3690ae")
#Reference and MMR1 + 0.5
E <-plot_age_abs("reference_cover.rda", "D1_05_cover.rda",
                 "Reference", "MMR1 + 0.5%",
                 "black", "#a90c38")

# Reference and school entry MMR2
Fe <- plot_age_abs("reference_cover.rda", "MMR2_at_5_cover.rda",
                   "Reference", "School entry MMR2",
                   "black", "#589a80")

#improving coverage vs the schedule
plt <- plot_grid(A, B, C,  D, E, Fe,
                 ncol = 3, nrow = 2, 
                 labels = c('A', 'B', 'C', 'D', 'E', 'F'),
                 label_size = 22,
                 label_y = 1.01,
                 label_x = 0.01,
                 scale = 0.9)

ggsave("Figures/Age_dist_cover.png",
       plt,
       width =  14,
       height = 11,
       bg = "white", dpi = 300)

##############################################################################
#age proportiongs for  CPRD with waning 

#% of caseses
#reference & early MMR2
A <- plot_age_prop("reference_waningCPRD.rda", "early_second_waningCPRD.rda",
                   "Reference", "Early MMR2",
                   "black", "#3690ae")
#Reference and MMR1 + 0.5
B <-plot_age_prop("reference_waningCPRD.rda", "D1_05_waningCPRD.rda",
                  "Reference", "MMR1 + 0.5%",
                  "black", "#a90c38")

# Reference and school entry MMR2
C <-plot_age_prop("reference_waningCPRD.rda", "MMR2_at_5_waningCPRD.rda",
                  "Reference", "School entry MMR2",
                  "black", "#589a80")

#waning from 3
D<- plot_age_prop("reference_waning3CPRD.rda", "early_second_waning3CPRD.rda",
                   "Reference", "Early MMR2 (3y)",
                   "black", "orange")


#N cases
#reference & early MMR2
E <- plot_age_abs("reference_waningCPRD.rda", "early_second_waningCPRD.rda",
                  "Reference", "Early MMR2",
                  "black", "#3690ae")
#Reference and MMR1 + 0.5
Fe <-plot_age_abs("reference_waningCPRD.rda", "D1_05_waningCPRD.rda",
                 "Reference", "MMR1 + 0.5%",
                 "black", "#a90c38")

# Reference and school entry MMR2
G <- plot_age_abs("reference_waningCPRD.rda", "MMR2_at_5_waningCPRD.rda",
                   "Reference", "School entry MMR2",
                   "black", "#589a80")
#waning from 3 
H <- plot_age_abs("reference_waning3CPRD.rda", "early_second_waning3CPRD.rda",
                   "Reference", "Early MMR2 (3y)",
                   "black", "orange")


#improving coverage vs the schedule
plt <- plot_grid(A, B, C,  D, E, Fe, G, H, 
                 ncol = 4, nrow = 2, 
                 labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
                 label_size = 22,
                 label_y = 1.01,
                 label_x = 0.01,
                 scale = 0.9)

ggsave("Figures/Age_dist_CPRD_waning.png",
       plt,
       width =  18,
       height = 11,
       bg = "white", dpi = 300)

###############################################################################
#plot yearly coverage - Figure 1
df <- data.table(read.csv2(paste0("Data/",
                                      "Coverage_reg_year_orig_extrapol.csv")))
df[, cov5y := as.numeric(cov5y)]
df[, cov5y := cov5y*100]
df[, cov2y := as.numeric(cov2y)*100]
ex <- df[n_dose == 1][, cov5y := cov2y][, n_dose := 3]
df <- rbind(df, ex)

df[, region := factor(region, levels = c("east midlands", "east of england",
                                         "london" ,"north east", "north west", 
                                         "south east" , "south west", "west midlands",
                                         "yorkshire and the humber"), 
                      labels = c("EM", "EE", "LND", "NE", "NW", "SE", "SW", "WM",
                                 "YH") )]
df[, n_dose := factor(n_dose, levels = c(3, 1, 2), 
                      labels = c( "MMR1 - age 2", "MMR1 - age 5", "MMR2 - age 5"))]

plot <- df%>%
  ggplot(aes(x = year, group = region))+
  geom_line(aes(y = cov5y, color = region), linewidth = 1)+
  facet_grid(~n_dose)+
  scale_color_manual(values = c("#ef476f", "#F78C8B", "#ffd166", "#83d483", "#06d6a0",
                                "#0cb0a9", "#118ab2", "#0c637f", "#073b4c"))+ 
  labs(color = "Region")+
  scale_x_continuous(name = "Year", breaks = c(2005, 2010, 2015, 2019))+
  ylab("Coverage (%)")+
  ylim(0,100)+
  geom_vline(xintercept = 2010, linetype = "dashed")+
  theme_classic()+
  theme(legend.position="bottom",
        axis.text.x = element_text(color = "grey20", size = 20, angle = 45, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "italic"),
        axis.title.y = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = 1, face = "italic"),
        legend.text = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(size=18),
        plot.title = element_text(vjust = 1,hjust = 1, size = 20),
        panel.spacing.y = unit(5, "lines"),
        strip.text.x = element_text(size = 18))


ggsave("Figures/Fig_1_coverage.png",
       plot,
       width =  18,
       height = 8,
       bg = "white", dpi = 300)
