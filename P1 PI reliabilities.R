##P1 1st response reliabilities - Broad Category and REL - Beth and Nic##

options(scipen=999) ##remove scientific notation

library(lpSolve)
library(irr)
P1dat <- MasterDOC_ParentInterview_P1toP3

## Helping opinion first responses ##
kappa2(P1dat[,c(64,65)], "unweighted")

## Helping relationship first responses ##
kappa2(P1dat[,c(68,69)], "unweighted")

## Cooperation opinion first responses ##
kappa2(P1dat[,c(111,112)], "unweighted")

## Cooperation relationship first responses ##
kappa2(P1dat[,c(115,116)], "unweighted")


