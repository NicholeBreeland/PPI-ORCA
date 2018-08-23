##P1 1st response reliabilities - Broad Category and REL - Beth and Nic##

options(scipen=999) ##remove scientific notation

library(lpSolve)
library(irr)
P1dat <- MasterDOC_ParentInterview_P1toP3

## Helping opinion first responses ##
kappa2(P1dat[,c(51,52)], "unweighted")

## Cooperation opinion first responses ##
kappa2(P1dat[,c(70,71)], "unweighted")

##Data excluding 999 vectors##
P1dat999 <- P1dat[ which(P1dat$Op_1_help_BC_NB < 12, P1dat$Op_1_help_REL_NB < 12, P1dat$Op_1_help_BC_BG < 12, P1dat$Op_1_help_REL_BG < 12, P1dat$Op_1_coop_BC_NB < 12, P1dat$Op_1_coop_REL_NB < 12, P1dat$Op_1_coop_REL_BG < 12 & P1dat$Op_1_coop_BC_BG < 12), ]


## Helping opinion first responses - excludes 999 ##
kappa2(P3dat999[,c(51,52)], "unweighted")

## Cooperation opinion first responses - excludes 999 ##
kappa2(P3dat999[,c(70,71)], "unweighted")

