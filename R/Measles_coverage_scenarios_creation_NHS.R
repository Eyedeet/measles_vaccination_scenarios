####--- MEASLES SCNENARIOS with cover data
library(tidyverse)
library(rlang)
options(scipen = 999)

#reading in extrapolated COVER data
df <- data.table(read.csv2(file =  paste0("Data/",
                               "Coverage_reg_year_nhs_extrapol.csv")))
df[, cov1y := as.numeric(cov1y)]
df[, cov2y := as.numeric(cov2y)]
df[, cov3y := as.numeric(cov3y)]
df[, cov4y := as.numeric(cov4y)]
df[, cov5y := as.numeric(cov5y)]

################################################################################
###-only improving MMR2 coverage
dt1 <- data.table::copy(df)
dt1[n_dose == 2, cov5y := cov5y+0.01]
dt1[n_dose ==2, cov3y := cov3y + 0.01]
dt1[n_dose ==2, cov4y := cov4y + 0.01]

write.table(dt1, file =  paste0("Data/",
                                 "COVER_MMR2plus1.csv"),
            sep = ";", dec = ".", row.names = FALSE)


dt2 <- data.table::copy(df)
dt2[n_dose == 2, cov5y := cov5y+0.03]
dt2[n_dose ==2, cov3y := cov3y + 0.03]
dt2[n_dose ==2, cov4y := cov4y + 0.03]


write.table(dt2, file =  paste0("Data/",
                                 "COVER_MMR2plus3.csv"),
            sep = ";", dec = ".", row.names = FALSE)


################################################################################
###---bringing second dose forward
#assumptions:
#-coverage at age 2 == coverage at age 4
#-coverage at age 3 == coverage at age 5, and remains constant after
#-coverage at age 1 == 75% of cov2

tmp1 <- data.table::copy(df)
tmp1[n_dose == 2, cov2y := cov4y]
tmp1[n_dose ==2, cov3y := cov5y]
tmp1[n_dose ==2, cov4y := cov5y]
tmp1[n_dose == 2, cov1y := 0]

write.table(tmp1, file =  paste0("Data/",
                                         "COVER_earlyMMR2.csv"),
            sep = ";", dec = ".", row.names = FALSE)



#sub secenario, MMR2 like MMR1
d1 <- tmp1[n_dose == 1]
d2 <- tmp1[n_dose == 1]
d2[, cov5y:= cov4y]
d2[, cov4y:= cov3y]
d2[, cov3y:= cov2y]
d2[, cov2y:= cov1y]
d2[, cov1y:= 0]
d2[, n_dose := 2]

tmp1a <- rbind(d1, d2)

write.table(tmp1a, file =  paste0("Data/",
                                 "COVER_earlyMMR2_likeMMR1.csv"),
            sep = ";", dec = ".", row.names = FALSE)

#MMR2 minus 3%

tmp1b <- data.table::copy(tmp1)
tmp1b[n_dose == 2, cov5y := cov5y-0.03]
tmp1b[n_dose ==2, cov3y := cov3y - 0.03]
tmp1b[n_dose ==2, cov4y := cov4y - 0.03]
tmp1b[n_dose ==2, cov2y := cov2y - 0.03]
tmp1b[n_dose == 2, cov1y := 0]
write.table(tmp1b, file =  paste0("Data/",
                                  "COVER_earlyMMR2_minus3.csv"),
            sep = ";", dec = ".", row.names = FALSE)

#MMR2 minus 5%
tmp1c <- data.table::copy(tmp1)
tmp1c[n_dose == 2, cov5y := cov5y-0.05]
tmp1c[n_dose ==2, cov3y := cov3y - 0.05]
tmp1c[n_dose ==2, cov4y := cov4y - 0.05]
tmp1c[n_dose ==2, cov2y := cov2y - 0.05]
tmp1c[n_dose == 2, cov1y := 0]
write.table(tmp1c, file =  paste0("Data/",
                                  "COVER_earlyMMR2_minus5.csv"),
            sep = ";", dec = ".", row.names = FALSE)

#MMR2 early plus 1%
tmp1d <- data.table::copy(tmp1)
tmp1d[n_dose == 2, cov5y := cov5y+0.01]
tmp1d[n_dose ==2, cov3y := cov3y + 0.01]
tmp1d[n_dose ==2, cov4y := cov4y + 0.01]
tmp1d[n_dose ==2, cov2y := cov2y + 0.01]
tmp1d[n_dose == 2, cov1y := 0]
write.table(tmp1d, file =  paste0("Data/",
                                  "COVER_earlyMMR2plus1.csv"),
            sep = ";", dec = ".", row.names = FALSE)

#MMR2 early plus 3%
tmp1e <- data.table::copy(tmp1)
tmp1e[n_dose == 2, cov5y := cov5y+0.03]
tmp1e[n_dose ==2, cov3y := cov3y + 0.03]
tmp1e[n_dose ==2, cov4y := cov4y + 0.03]
tmp1e[n_dose ==2, cov2y := cov2y + 0.03]
tmp1e[n_dose == 2, cov1y := 0]
write.table(tmp1e, file =  paste0("Data/",
                                  "COVER_earlyMMR2plus3.csv"),
            sep = ";", dec = ".", row.names = FALSE)

###############################################################################
###--- second dose at age of five
#assumptions: coverage at age of 5 == 0.75% of age 5, rest 0

tmp2 <- data.table::copy(df)
tmp2[n_dose == 2, cov5y := 0.75*cov5y]
tmp2[n_dose ==2, cov3y := 0]
tmp2[n_dose ==2, cov4y := 0]
tmp2[n_dose == 2, cov2y := 0]
tmp2[n_dose == 2, cov1y := 0]

write.table(tmp2, file =  paste0("Data/",
                                 "COVER_lateMMR2.csv"),
            sep = ";", dec = ".", row.names = FALSE)

###############################################################################
####-improving MMR1 by 1%

tmp3 <- data.table::copy(df)
tmp3[n_dose == 1, cov5y := cov5y+0.01]
tmp3[n_dose ==1, cov3y := cov3y + 0.01]
tmp3[n_dose ==1, cov4y := cov4y + 0.01]
tmp3[n_dose ==1, cov2y := cov2y + 0.01]
tmp3[n_dose == 1, cov1y := cov1y + 0.01]

write.table(tmp3, file =  paste0("Data/",
                                 "COVER_MMR1plus1.csv"),
            sep = ";", dec = ".", row.names = FALSE)


#improving MMR1 by 0.5%

tmp4 <- data.table::copy(df)
tmp4[n_dose == 1, cov5y := cov5y+0.005]
tmp4[n_dose ==1, cov3y := cov3y + 0.005]
tmp4[n_dose ==1, cov4y := cov4y + 0.005]
tmp4[n_dose ==1, cov2y := cov2y + 0.005]
tmp4[n_dose == 1, cov1y := cov1y + 0.005]

write.table(tmp4, file =  paste0("Data/",
                                 "COVER_MMR1plus05.csv"),
            sep = ";", dec = ".", row.names = FALSE)

#improving MMR1 by 0.25%

tmp5 <- data.table::copy(df)
tmp5[n_dose == 1, cov5y := cov5y+0.0025]
tmp5[n_dose ==1, cov3y := cov3y + 0.0025]
tmp5[n_dose ==1, cov4y := cov4y + 0.0025]
tmp5[n_dose ==1, cov2y := cov2y + 0.0025]
tmp5[n_dose == 1, cov1y := cov1y + 0.0025]

write.table(tmp5, file =  paste0("Data/",
                                 "COVER_MMR1plus025.csv"),
            sep = ";", dec = ".", row.names = FALSE)


