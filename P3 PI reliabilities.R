##P3 1st response reliabilities - Broad Category - Beth and Nic##

options(scipen=999) ##remove scientific notation

library(lpSolve)
library(irr)
P3dat <- RELIABILITIES_NB_P3_ParentProsocialInterview

## Helping opinion first responses ##
kappa2(P3dat[,c(51,52)], "unweighted")

## Cooperation opinion first responses ##
kappa2(P3dat[,c(70,71)], "unweighted")

##Data excluding 999 vectors##
P3dat999 <- P3dat[ which(P3dat$Op_1_help_BC_NB < 12, P3dat$Op_1_help_BC_BG < 12, P3dat$Op_1_coop_BC_NB < 12 & P3dat$Op_1_help_BC_BG < 12), ]


## Helping opinion first responses - excludes 999 ##
kappa2(P3dat999[,c(51,52)], "unweighted")

## Cooperation opinion first responses - excludes 999 ##
kappa2(P3dat999[,c(70,71)], "unweighted")



