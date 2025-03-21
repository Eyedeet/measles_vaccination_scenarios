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
#CPRD with waning from age 5


################################################################################
#CPRD with waning from age 3

