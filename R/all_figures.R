####Generating all plots
source("R/function_figures.R")

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

library(cowplot)
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
       bg = "white")


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
       bg = "white")

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
       bg = "white")

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
       bg = "white")

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
       bg = "white")



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
       bg = "white")
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
       bg = "white")

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
       bg = "white")

#