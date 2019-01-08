##P3 1st response reliabilities - Broad Category - Beth and Nic##

options(scipen=999) ##remove scientific notation

library(lpSolve)
library(irr)
P3dat <- RELIABILITIES_NB_P3_ParentProsocialInterview

## Helping opinion first responses ##
kappa2(P3dat[,c(51,52)], "unweighted")

## Cooperation opinion first responses ##
kappa2(P3dat[,c(70,71)], "unweighted")

##Beth and Nichole Reliabilities for relationship responses 30% of sample##

P3dat_eg_rel <- RELIABILITIES_NB_P3_ParentProsocialInterview

##reliability sample##

rsamp_P3dat_rel <- subset(P3dat_eg_rel, Reliability == 1, select = c("Op_1_help_REL_NB", "Op_1_help_REL_BG", "Op_1_coop_REL_NB", "Op_1_coop_REL_BG"))

#help responses#
kappa2(rsamp_P3dat_rel[,c(1,2)], "unweighted")

#coop responses#
kappa2(rsamp_P3dat_eg_rel[,c(3,4)], "unweighted")

#################

## Examples reliabilities Nic and Sasha - first answers only ##


##reliability sample##

rsamp_P3dat_eg_rel <- subset(P3dat_eg_rel, Reliability == 1, select = c("Eg_1_help_NB", "Eg_1_help_SC", "Eg_1_coop_NB", "Eg_1_coop_SC", "Op_1_help_REL_NB", "Op_1_help_REL_SC", "Op_1_coop_REL_NB", "Op_1_coop_REL_SC"))

##reliability helping examples##

kappa2(rsamp_P3dat_eg_rel[,c(1,2)], "unweighted")

##reliability cooperating examples##

kappa2(rsamp_P3dat_eg_rel[,c(3,4)], "unweighted")

##reliability help opinion relationship##

kappa2(rsamp_P3dat_eg_rel[,c(5,6)], "unweighted")

##reliability cooperation opinion relationship##

kappa2(rsamp_P3dat_eg_rel[,c(7,8)], "unweighted")