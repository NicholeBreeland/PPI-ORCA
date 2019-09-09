all.data = read.csv("P1_PP1_P3_Reliability_SocialContext_PPIP1.csv", stringsAsFactors = FALSE)

options(scipen = 999)

all.data[all.data == 999] <- NA
all.data[all.data == 9999] <- NA
all.data[all.data == 99] <- NA
all.data[all.data == 777] <- NA


# PPI Phase 1 ego/alt reliabilities -----------------------------------------------


##P1 response reliabilities - ego/alt category and REL - Beth and Nic##
##FIRST PASS## - BOTH CODE ALL AND DISCUSS DISAGREEMENTS

library(lpSolve)

library(irr)


## Cooperation indep/egp all 999s removed reliability P1 ##
kappa2(all.data[,c(10,11)], "unweighted")

## Cooperation indep/egp all 999s removed reliability P3 ##
kappa2(all.data[,c(134,135)], "unweighted")

##P1 response reliabilities - ego/alt category and REL - Beth and Nic##
##SECOND PASS## - NB CODE 30%

library(lpSolve)

library(irr)

#datasubset for 30%

rel.df <- all.data[which(all.data$Reliability_2P_inout == 1),]
View(rel.df)

## Cooperation indep/egp all 999s removed reliability P1 ##
kappa2(rel.df[,c(13,14)], "unweighted")


##P1 response reliabilities - ego/alt category and REL - Beth and Nic##
##THIRD PASS## - NB CODE 30%


library(lpSolve)

library(irr)

#datasubset for 30%

rel.df <- all.data[which(all.data$Reliability_3P_inout == 1),]
rel.df_naomit <- subset(rel.df, select = c("Reliability_3P_inout", "Op_coop_REL_NB_all_3", "Op_coop_REL_BG_all_3"))
View(rel.df_naomit)

rel.df_naomit <- na.omit(rel.df_naomit)

## Cooperation indep/egp all 999s removed reliability P1 ##
kappa2(rel.df_naomit[,c(2,3)], "unweighted")


# P3 Social context reliabilities -----------------------------------------

#Coder categories for reliability check

#NB_OB = 1
#NB_BB = 2
#NB_MP = 3
#OB_BB = 4
#OB_MP = 5
#BB_MP = 6

all.data$P3_SocialContext_CoderCategeory[all.data$P3_SocialContext_Coders == "NB_OB"] <- 1
all.data$P3_SocialContext_CoderCategeory[all.data$P3_SocialContext_Coders == "NB_BB"] <- 2
all.data$P3_SocialContext_CoderCategeory[all.data$P3_SocialContext_Coders == "NB_MP"] <- 3
all.data$P3_SocialContext_CoderCategeory[all.data$P3_SocialContext_Coders == "OB_BB"] <- 4
all.data$P3_SocialContext_CoderCategeory[all.data$P3_SocialContext_Coders == "OB_MP"] <- 5
all.data$P3_SocialContext_CoderCategeory[all.data$P3_SocialContext_Coders == "BB_MP"] <- 6
  

##Personal final scores per coding block

#Nichole Scores
all.data$PR_Afill_NB <- with(all.data, (PR_T1_Afill_NB+PR_T2_Afill_NB)/2)
all.data$PR_Antag_NB <- with(all.data, (PR_T1_Antag_NB+PR_T2_Antag_NB)/2)
all.data$PR_Eng_NB <- with(all.data, (PR_T1_Eng_NB+PR_T2_Eng_NB)/2)
all.data$PR_JCE_NB <- with(all.data, (PR_T1_JCE_NB+PR_T2_JCE_NB)/2)

all.data$CR_Afill_NB <- with(all.data, (CR_T1_Afill_NB+CR_T2_Afill_NB)/2)
all.data$CR_Antag_NB <- with(all.data, (CR_T1_Antag_NB+CR_T2_Antag_NB)/2)
all.data$CR_Eng_NB <- with(all.data, (CR_T1_Eng_NB+CR_T2_Eng_NB)/2)
all.data$CR_JCE_NB <- with(all.data, (CR_T1_JCE_NB+CR_T2_JCE_NB)/2)

#Olivia Scores
all.data$PR_Afill_OB <- with(all.data, (PR_T1_Afill_OB+PR_T2_Afill_OB)/2)
all.data$PR_Antag_OB <- with(all.data, (PR_T1_Antag_OB+PR_T2_Antag_OB)/2)
all.data$PR_Eng_OB <- with(all.data, (PR_T1_Eng_OB+PR_T2_Eng_OB)/2)
all.data$PR_JCE_OB <- with(all.data, (PR_T1_JCE_OB+PR_T2_JCE_OB)/2)

all.data$CR_Afill_OB <- with(all.data, (CR_T1_Afill_OB+CR_T2_Afill_OB)/2)
all.data$CR_Antag_OB <- with(all.data, (CR_T1_Antag_OB+CR_T2_Antag_OB)/2)
all.data$CR_Eng_OB <- with(all.data, (CR_T1_Eng_OB+CR_T2_Eng_OB)/2)
all.data$CR_JCE_OB <- with(all.data, (CR_T1_JCE_OB+CR_T2_JCE_OB)/2)

#Bri Scores
all.data$PR_Afill_BB <- with(all.data, (PR_T1_Afill_BB+PR_T2_Afill_BB)/2)
all.data$PR_Antag_BB <- with(all.data, (PR_T1_Antag_BB+PR_T2_Antag_BB)/2)
all.data$PR_Eng_BB <- with(all.data, (PR_T1_Eng_BB+PR_T2_Eng_BB)/2)
all.data$PR_JCE_BB <- with(all.data, (PR_T1_JCE_BB+PR_T2_JCE_BB)/2)

all.data$CR_Afill_BB <- with(all.data, (CR_T1_Afill_BB+CR_T2_Afill_BB)/2)
all.data$CR_Antag_BB <- with(all.data, (CR_T1_Antag_BB+CR_T2_Antag_BB)/2)
all.data$CR_Eng_BB <- with(all.data, (CR_T1_Eng_BB+CR_T2_Eng_BB)/2)
all.data$CR_JCE_BB <- with(all.data, (CR_T1_JCE_BB+CR_T2_JCE_BB)/2)

#Meera scores
all.data$PR_Afill_MP <- with(all.data, (PR_T1_Afill_MP+PR_T2_Afill_MP)/2)
all.data$PR_Antag_MP <- with(all.data, (PR_T1_Antag_MP+PR_T2_Antag_MP)/2)
all.data$PR_Eng_MP <- with(all.data, (PR_T1_Eng_MP+PR_T2_Eng_MP)/2)
all.data$PR_JCE_MP <- with(all.data, (PR_T1_JCE_MP+PR_T2_JCE_MP)/2)

all.data$CR_Afill_MP <- with(all.data, (CR_T1_Afill_MP+CR_T2_Afill_MP)/2)
all.data$CR_Antag_MP <- with(all.data, (CR_T1_Antag_MP+CR_T2_Antag_MP)/2)
all.data$CR_Eng_MP <- with(all.data, (CR_T1_Eng_MP+CR_T2_Eng_MP)/2)
all.data$CR_JCE_MP <- with(all.data, (CR_T1_JCE_MP+CR_T2_JCE_MP)/2)

##subset for just P3 E-C social context codes
sc.df_finals <- subset(all.data, select = c(1, 147:179))

write.csv(sc.df_finals, file = "P3_sc_finalscores.csv")

#created new datafile to transform data file from wide to long using multiple columns...(probably a code for this but can't sort it out)

#Create datasets for each group of coders and add back in 999 values 0's for reliability
##used 0 instead of 999 as 999 overinflates the ICC agreement rating

#sc.df_finals.csv = LONG DATA FILE 

sc.df_finals.csv = read.csv("P3_sc_finalscores.csv", stringsAsFactors = FALSE)

sc.df_NB_OB <- sc.df_finals.csv[which(sc.df_finals.csv$P3_SocialContext_CoderCategeory == 1),]
sc.df_NB_OB[is.na(sc.df_NB_OB)] <- 0

sc.df_NB_BB <- sc.df_finals.csv[which(sc.df_finals.csv$P3_SocialContext_CoderCategeory == 2),]
sc.df_NB_BB[is.na(sc.df_NB_BB)] <- 0

sc.df_NB_MP <- sc.df_finals.csv[which(sc.df_finals.csv$P3_SocialContext_CoderCategeory == 3),]
sc.df_NB_MP[is.na(sc.df_NB_MP)] <- 0

sc.df_OB_BB <- sc.df_finals.csv[which(sc.df_finals.csv$P3_SocialContext_CoderCategeory == 4),]
sc.df_OB_BB[is.na(sc.df_OB_BB)] <- 0

sc.df_OB_MP <- sc.df_finals.csv[which(sc.df_finals.csv$P3_SocialContext_CoderCategeory == 5),]
sc.df_OB_MP[is.na(sc.df_OB_MP)] <- 0

sc.df_BB_MP <- sc.df_finals.csv[which(sc.df_finals.csv$P3_SocialContext_CoderCategeory == 6),]
sc.df_BB_MP[is.na(sc.df_BB_MP)] <- 0

# ICCs P3 C-E PR and CR tasks ---------------------------------------------


##ICCs for Nichole and Olivia

#NB_OB affiliation subset

sc.df_NB_OB_afill <- subset(sc.df_NB_OB, select = c("Afill_NB", "Afill_OB"))
sc.df_NB_OB_antag <- subset(sc.df_NB_OB, select = c("Antag_NB", "Antag_OB"))
sc.df_NB_OB_eng <- subset(sc.df_NB_OB, select = c("Eng_NB", "Eng_OB"))
sc.df_NB_OB_jce <- subset(sc.df_NB_OB, select = c("JCE_NB", "JCE_OB"))

library(irr)

#Affiliation reliability for NB_OB
icc(sc.df_NB_OB_afill, model="twoway", type="agreement")

#Antag reliability for NB_OB
icc(sc.df_NB_OB_antag, model="twoway", type="agreement")

#Engagement reliability for NB_OB
icc(sc.df_NB_OB_eng, model="twoway", type="agreement")

#Joint coordinated engagement reliability for NB_OB
icc(sc.df_NB_OB_jce, model="twoway", type="agreement")


##ICCs for Nichole and Bri

#NB_BB category subsets

sc.df_NB_BB_afill <- subset(sc.df_NB_BB, select = c("Afill_NB", "Afill_BB"))
sc.df_NB_BB_antag <- subset(sc.df_NB_BB, select = c("Antag_NB", "Antag_BB"))
sc.df_NB_BB_eng <- subset(sc.df_NB_BB, select = c("Eng_NB", "Eng_BB"))
sc.df_NB_BB_jce <- subset(sc.df_NB_BB, select = c("JCE_NB", "JCE_BB"))

library(irr)

#Affiliation reliability for NB_BB
icc(sc.df_NB_BB_afill, model="twoway", type="agreement")

#Antag reliability for NB_BB
icc(sc.df_NB_BB_antag, model="twoway", type="agreement")

#Engagement reliability for NB_BB
icc(sc.df_NB_BB_eng, model="twoway", type="agreement")

#Joint coordinated engagement reliability for NB_BB
icc(sc.df_NB_BB_jce, model="twoway", type="agreement")


##ICCs for Nichole and Meera

#NB_MP category subsets

sc.df_NB_MP_afill <- subset(sc.df_NB_MP, select = c("Afill_NB", "Afill_MP"))
sc.df_NB_MP_antag <- subset(sc.df_NB_MP, select = c("Antag_NB", "Antag_MP"))
sc.df_NB_MP_eng <- subset(sc.df_NB_MP, select = c("Eng_NB", "Eng_MP"))
sc.df_NB_MP_jce <- subset(sc.df_NB_MP, select = c("JCE_NB", "JCE_MP"))

library(irr)

#Affiliation reliability for NB_MP
icc(sc.df_NB_MP_afill, model="twoway", type="agreement")

#Antag reliability for NB_MP
icc(sc.df_NB_MP_antag, model="twoway", type="agreement")

#Engagement reliability for NB_MP
icc(sc.df_NB_MP_eng, model="twoway", type="agreement")

#Joint coordinated engagement reliability for NB_MP
icc(sc.df_NB_MP_jce, model="twoway", type="agreement")

##ICCs for Olivia and Bri

#OB_BB category subsets

sc.df_OB_BB_afill <- subset(sc.df_OB_BB, select = c("Afill_OB", "Afill_BB"))
sc.df_OB_BB_afill [sc.df_OB_BB_afill  == 999] <- NA
sc.df_OB_BB_antag <- subset(sc.df_OB_BB, select = c("Antag_OB", "Antag_BB"))
sc.df_OB_BB_eng <- subset(sc.df_OB_BB, select = c("Eng_OB", "Eng_BB"))
sc.df_OB_BB_jce <- subset(sc.df_OB_BB, select = c("JCE_OB", "JCE_BB"))

library(irr)

#Affiliation reliability for OB_BB
icc(sc.df_OB_BB_afill, model="twoway", type="agreement")

#Antag reliability for OB_BB
icc(sc.df_OB_BB_antag, model="twoway", type="agreement")

#Engagement reliability for OB_BB
icc(sc.df_OB_BB_eng, model="twoway", type="agreement")

#Joint coordinated engagement reliability for OB_BB
icc(sc.df_OB_BB_jce, model="twoway", type="agreement")

##ICCs for Olivia and Meera

#OB_MP category subsets

sc.df_OB_MP_afill <- subset(sc.df_OB_MP, select = c("Afill_OB", "Afill_MP"))
sc.df_OB_MP_afill [sc.df_OB_MP_afill  == 999] <- NA
sc.df_OB_MP_antag <- subset(sc.df_OB_MP, select = c("Antag_OB", "Antag_MP"))
sc.df_OB_MP_eng <- subset(sc.df_OB_MP, select = c("Eng_OB", "Eng_MP"))
sc.df_OB_MP_jce <- subset(sc.df_OB_MP, select = c("JCE_OB", "JCE_MP"))

library(irr)

#Affiliation reliability for OB_MP
icc(sc.df_OB_MP_afill, model="twoway", type="agreement")

#Antag reliability for OB_MP
icc(sc.df_OB_MP_antag, model="twoway", type="agreement")

#Engagement reliability for OB_MP
icc(sc.df_OB_MP_eng, model="twoway", type="agreement")

#Joint coordinated engagement reliability for OB_MP
icc(sc.df_OB_MP_jce, model="twoway", type="agreement")

##ICCs for Bri and Meera

#BB_MP category subsets

sc.df_BB_MP_afill <- subset(sc.df_BB_MP, select = c("Afill_BB", "Afill_MP"))
sc.df_BB_MP_afill [sc.df_BB_MP_afill  == 999] <- NA
sc.df_BB_MP_antag <- subset(sc.df_BB_MP, select = c("Antag_BB", "Antag_MP"))
sc.df_BB_MP_eng <- subset(sc.df_BB_MP, select = c("Eng_BB", "Eng_MP"))
sc.df_BB_MP_jce <- subset(sc.df_BB_MP, select = c("JCE_BB", "JCE_MP"))

library(irr)

#Affiliation reliability for BB_MP
icc(sc.df_BB_MP_afill, model="twoway", type="agreement")

#Antag reliability for BB_MP
icc(sc.df_BB_MP_antag, model="twoway", type="agreement")

#Engagement reliability for BB_MP
icc(sc.df_BB_MP_eng, model="twoway", type="agreement")

#Joint coordinated engagement reliability for BB_MP
icc(sc.df_BB_MP_jce, model="twoway", type="agreement")