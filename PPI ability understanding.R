# ALWAYS RUN FIRST! -------------------------------------------------------

all.data = read.csv("P1_PPI_Ability_Anticipation_Workingset.csv", stringsAsFactors = FALSE)


options(scipen = 999)

all.data[all.data == 999] <- NA
all.data[all.data == 777] <- NA


#Difference score for altruistic/egotistic responses P1

all.data$response.diff = with(all.data, Op_coop_Alt_Avg_p1 - Op_coop_Ego_Avg_p1)

#response difference score for P3

all.data$response.diff.p3 = with(all.data, Op_coop_Alt_Avg_p3 - Op_coop_Ego_Avg_p3)


#Gender category assign
all.data$Gender_Label[all.data$Gender == 1] <- "Male"
all.data$Gender_Label[all.data$Gender == 2] <- "Female"

#so, 1 = 100% altruistic and -1 = 100% egotistic

##BIG 5/SDO/RWA SCORING AND CODING

#reverse scoring items

library(car)

#Personality

all.data$NeurVol_2 = recode(all.data$NeurVol_2, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$NeurVol_4 = recode(all.data$NeurVol_4, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$NeurVol_6 = recode(all.data$NeurVol_6, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$NeurVol_8 = recode(all.data$NeurVol_8, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$NeurWith_1 = recode(all.data$NeurWith_1, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$NeurWith_3 = recode(all.data$NeurWith_3, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$NeurWith_5 = recode(all.data$NeurWith_5, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$NeurWith_8 = recode(all.data$NeurWith_8, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$AgreeComp_1 = recode(all.data$AgreeComp_1, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$AgreeComp_4 = recode(all.data$AgreeComp_4, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$AgreeComp_6 = recode(all.data$AgreeComp_6, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$AgreeComp_7 = recode(all.data$AgreeComp_7, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$AgreeComp_9 = recode(all.data$AgreeComp_9, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$AgreePol_2 = recode(all.data$AgreePol_2, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$AgreePol_4 = recode(all.data$AgreePol_4, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$AgreePol_7 = recode(all.data$AgreePol_7, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$AgreePol_8 = recode(all.data$AgreePol_8, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$AgreePol_9 = recode(all.data$AgreePol_9, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$AgreePol_10 = recode(all.data$AgreePol_10, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ConsInd_2 = recode(all.data$ConsInd_2, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ConsInd_3 = recode(all.data$ConsInd_3, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ConsInd_4 = recode(all.data$ConsInd_4, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ConsInd_6 = recode(all.data$ConsInd_6, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ConsInd_9 = recode(all.data$ConsInd_9, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ConsInd_10 = recode(all.data$ConsInd_10, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ConsOrd_1 = recode(all.data$ConsOrd_1, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ConsOrd_5 = recode(all.data$ConsOrd_5, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ConsOrd_7 = recode(all.data$ConsOrd_7, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ConsOrd_8 = recode(all.data$ConsOrd_8, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ExtraEnth_2 = recode(all.data$ExtraEnth_2, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ExtraEnth_3 = recode(all.data$ExtraEnth_3, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ExtraEnth_4 = recode(all.data$ExtraEnth_4, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ExtraEnth_6 = recode(all.data$ExtraEnth_6, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ExtraEnth_7 = recode(all.data$ExtraEnth_7, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ExtraAssert_3 = recode(all.data$ExtraAssert_3, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ExtraAssert_5 = recode(all.data$ExtraAssert_5, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ExtraAssert_8 = recode(all.data$ExtraAssert_8, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$ExtraAssert_10 = recode(all.data$ExtraAssert_10, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$OpenIntel_2 = recode(all.data$OpenIntel_2, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$OpenIntel_5 = recode(all.data$OpenIntel_5, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$OpenIntel_6 = recode(all.data$OpenIntel_6, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$OpenIntel_9 = recode(all.data$OpenIntel_9, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$Openness_5 = recode(all.data$Openness_5, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$Openness_8 = recode(all.data$Openness_8, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$Openness_9 = recode(all.data$Openness_9, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$Openness_10 = recode(all.data$Openness_10, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")

#SDO and RWA

all.data$SDO_INCSOCEQUAL_r = recode(all.data$SDO_INCSOCEQUAL_r, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$SDO_GROUPEQUAL_r = recode(all.data$SDO_GROUPEQUAL_r, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$SDO_DOEQUAL_r = recode(all.data$SDO_DOEQUAL_r, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$RWA_DEVOWN_r = recode(all.data$RWA_DEVOWN_r, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
all.data$RWA_ATHEIGOOD_r = recode(all.data$RWA_ATHEIGOOD_r, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$RWA_OPPGOOD_r = recode(all.data$RWA_OPPGOOD_r, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")
all.data$EP_ALLNZ_r= recode(all.data$EP_ALLNZ_r, "1=7; 2=6; 3=5; 4=4; 5=3;6=2; 7=1")


#Scoring Big 5, RWA, SDO from P5 data

all.data$Neur = rowMeans(all.data[,54:73], na.rm=TRUE)
all.data$Agree = rowMeans(all.data[,74:93], na.rm=TRUE)
all.data$Cons = rowMeans(all.data[,94:113], na.rm=TRUE)
all.data$Extra = rowMeans(all.data[,114:133], na.rm=TRUE)
all.data$Open = rowMeans(all.data[,134:153], na.rm=TRUE)

all.data$SDO = rowMeans(all.data[,156:161], na.rm=TRUE)
all.data$RWA = rowMeans(all.data[,162:167], na.rm=TRUE)
all.data$EP = rowMeans(all.data[,168:170], na.rm=TRUE)

p3_sc.df <- read.csv("ORCA_P3_CAAT_SC.csv", stringsAsFactors = FALSE)

p3_sc.df[p3_sc.df == 999] <- NA
p3_sc.df[p3_sc.df == 9999] <- NA
p3_sc.df[p3_sc.df == 99] <- NA

#PR final SC scores calc.
p3_sc.df$PR_Afill <- rowMeans(p3_sc.df[,c("PR_T1_Afill_Final", "PR_T2_Afill_Final")], na.rm = TRUE)
p3_sc.df$PR_Antag <- rowMeans(p3_sc.df[,c("PR_T1_Antag_Final", "PR_T2_Antag_Final")], na.rm = TRUE)
p3_sc.df$PR_Eng <- rowMeans(p3_sc.df[,c("PR_T1_Eng_Final", "PR_T2_Eng_Final")], na.rm = TRUE)
p3_sc.df$PR_JCE <- rowMeans(p3_sc.df[,c("PR_T1_JCE_Final", "PR_T2_JCE_Final")], na.rm = TRUE)

#FT final SC scores calc.
p3_sc.df$CR_Afill <- rowMeans(p3_sc.df[,c("CR_T1_Afill_Final", "CR_T2_Afill_Final")], na.rm = TRUE)
p3_sc.df$CR_Antag <- rowMeans(p3_sc.df[,c("CR_T1_Antag_Final", "CR_T2_Antag_Final")], na.rm = TRUE)
p3_sc.df$CR_Eng <- rowMeans(p3_sc.df[,c("CR_T1_Eng_Final", "CR_T2_Eng_Final")], na.rm = TRUE)
p3_sc.df$CR_JCE <- rowMeans(p3_sc.df[,c("CR_T1_JCE_Final", "CR_T2_JCE_Final")], na.rm = TRUE)

library(dplyr)
all.data <- right_join(all.data, p3_sc.df, by = "ID_num")

# IBQ data import ---------------------------------------------------------

p1_IBQ.df <- read.csv("ORCA_p1_IBQ_FinalScores.csv", stringsAsFactors = FALSE)

p1_IBQ.df[p1_IBQ.df == 999] <- NA
p1_IBQ.df[p1_IBQ.df == 9999] <- NA
p1_IBQ.df[p1_IBQ.df == 99] <- NA

all.data <- right_join(all.data, p1_IBQ.df, by = "ID_num")

all.data$Avg_Anti_P3

tail(all.data)

# PPI P1 Analyses ---------------------------------------------------------

# Data subsets by P3 Cooperative ability task -----------------------------

#Main data set for ORCA all study in group

P1.data <-all.data[which(all.data$Study_inout == 1 & all.data$PCG_p1 == 1),]

P1.data_pcgm <- all.data[which(all.data$Study_inout == 1 & all.data$PCG_p1 == 1 & all.data$Parent_Type_p1 == 2),]

#Indep study in data subset

IT.data <- all.data[which(all.data$InOut_IT_P3 == 1, all.data$Study_inout == 1 & all.data$PCG_p1 == 1),]

#Parallel roles study in data subset - ALL PCG

PR.data <- all.data[which(all.data$InOut_PR_P3 == 1 & all.data$Study_inout == 1 & all.data$PCG_p1 == 1),]

#Parallel roles study in data subset - ALL PCG AND Moms only

PR_pcgm.data <- all.data[which(all.data$InOut_PR_P3 == 1 & all.data$Study_inout == 1 & all.data$PCG_p1 == 1 & all.data$Parent_Type_p1 == 2),]

#Froggy task study in data subset

FT.data <- all.data[which(all.data$InOut_FT_P3 == 1 & all.data$Study_inout == 1 & all.data$PCG_p1 == 1),]

#Froggy task study in data subset - ALL PCG AND Moms only

FT_pcgm.data <- all.data[which(all.data$InOut_FT_P3 == 1 & all.data$Study_inout == 1 & all.data$PCG_p1 == 1 & all.data$Parent_Type_p1 == 2),]

# Visulizing the data - Ability & Social Context -----------------------------------------------------

#response diff

with(P1.data, hist(response.diff,breaks = 10,
                   xlab = "ego/alt score",
                   main = "hist parent response"))

skewness(P1.data$response.diff, na.rm = TRUE)

#IT task - ability data

with(IT.data, hist(Avg_SC_IT_P3,breaks = 10,
                   xlab = "spatial coordination scores",
                   main = "indep spatial coordination"))

skewness(IT.data$Avg_SC_IT_P3, na.rm = TRUE)


with(IT.data, hist(Avg_Succ_IT_P3,breaks = 5,
                   xlab = "success scores",
                   main = "indep success"))

with(IT.data, hist(Avg_Lat_IT_P3,breaks = 5,
                   xlab = "latency scores",
                   main = "indep latency"))

with(IT.data, plot(response.diff, Avg_SC_IT_P3))
with(IT.data, plot(response.diff, Avg_Succ_IT_P3))
with(IT.data, plot(response.diff, Avg_Lat_IT_P3))

#PR task - Ability data

with(PR.data, hist(Avg_SC_PR_P3,breaks = 5,
                   xlab = "spatial coordination scores",
                   main = "parallel roles spatial coordination"))

with(PR.data, boxplot(Avg_SC_PR_P3,
                   xlab = "spatial coordination scores",
                   main = "parallel roles spatial coordination"))
?boxplot

skewness(PR.data$Avg_SC_PR_P3, na.rm = TRUE)


with(PR.data, hist(Avg_SC_PR_P3,breaks = 5,
                   xlab = "log spatial coordination scores",
                   main = "parallel roles spatial coordination"))

with(PR.data, hist(Avg_Succ_PR_P3,breaks = 5,
                   xlab = "success scores",
                   main = "parallel roles success"))

with(PR.data, hist(Avg_Lat_PR_P3,breaks = 5,
                   xlab = "latency scores",
                   main = "parallel roles latency"))

with(PR.data, plot(response.diff, Avg_SC_PR_P3))
with(PR.data, plot(response.diff, Avg_Succ_PR_P3))
with(PR.data, plot(response.diff, Avg_Lat_PR_P3))

library(ggplot2)

ggplot(data = PR.data,
       mapping = aes(x = Gender, y = Avg_SC_PR_P3, colour = response.cat)) +
  geom_boxplot() +
  labs(title = "Differences in SC for response type and gender",
       x = "Gender",
       y = "SC score",
       colour = "Parent\nresponse type") +
  theme(text = element_text(size = 14))

ggplot(data = PR.data,
       mapping = aes(x = Gender, y = Avg_Succ_PR_P3, colour = response.cat)) +
  geom_boxplot() +
  labs(title = "Differences in Succ for response type and gender",
       x = "Gender",
       y = "Succ score",
       colour = "Parent\nresponse type") +
  theme(text = element_text(size = 14))

ggplot(data = PR.data,
       mapping = aes(x = Gender, y = Avg_Lat_PR_P3, colour = response.cat)) +
  geom_boxplot() +
  labs(title = "Differences in Lat for response type and gender",
       x = "Gender",
       y = "Lat score",
       colour = "Parent\nresponse type") +
  theme(text = element_text(size = 14))


#Froggy task

with(FT.data, plot(response.diff, Avg_SC_FT_P3))
with(FT.data, plot(response.diff, Avg_Succ_FT_P3))
with(FT.data, plot(response.diff, Avg_LAT1_FT_P3))
with(FT.data, plot(response.diff, Avg_LAT2_FT_P3))

##histograms of Social context data P3 Parallel roles task

with(P1.data, hist(PR_Afill,breaks = 10,
                   xlab = "affiliation score",
                   main = "hist sc afill"))

with(P1.data, hist(PR_Antag, breaks = 10,
                   xlab = "antagonistic score",
                   main = "hist sc antag"))

with(P1.data, hist(PR_Eng,breaks = 10,
                   xlab = "engagement score",
                   main = "hist sc eng"))
shapiro.test(P1.data$PR_Eng)

with(P1.data, hist(PR_JCE,breaks = 10,
                   xlab = "joint coordinated engagement score",
                   main = "hist sc jce"))
shapiro.test(P1.data$PR_JCE)


#Froggy task SC distributions

with(P1.data, hist(CR_Afill,breaks = 5,
                   xlab = "affiliation score",
                   main = "hist sc afill"))

with(P1.data, hist(CR_Antag, breaks = 5,
                   xlab = "antagonistic score",
                   main = "hist sc antag"))

with(P1.data, hist(CR_Eng,breaks = 5,
                   xlab = "engagement score",
                   main = "hist sc eng"))
shapiro.test(P1.data$CR_Eng)

with(P1.data, hist(CR_JCE,breaks = 10,
                   xlab = "joint coordinated engagement score",
                   main = "hist sc jce"))
shapiro.test(P1.data$CR_JCE)

library(ggplot2)

ggplot(data = P1.data,
       mapping = aes(x = Gender_Label, y = PR_JCE, colour = Gender)) +
  geom_boxplot() +
  labs(title = "Differences in JCE by Gender",
       x = "Gender",
       y = "JCE score",
       colour = "") +
  theme(text = element_text(size = 14))

mean(P1.data$PR_JCE, na.rm = TRUE)
sd(P1.data$PR_JCE, na.rm = TRUE)

##No need to remove outliers as not +- 3 SD above/below mean.

# Preanalyses - Ability & Social Context -------------------------------------------------------------

#visualize data for PPI responses, groups female vs male children

with(P1.data, boxplot(response.diff ~ Gender)) 

#data for parent responses are non normally distributed

shapiro.test(P1.data$response.diff)

#check for difference bt responses by child gender

wilcox.test(response.diff ~ Gender, data = P1.data)
wilcox.test(response.diff ~ Gender, data = P1.data_pcgm)

##No diff bt mothers & pcg and just pcg <<<<

#visualize data for PPI reasoning and time spent playing social games

trend_line = lm((response.diff) ~ (Pi_social_games_freq_p1_fixed_777), data = all.data)

with(all.data, plot(Pi_social_games_freq_p1_fixed_777, response.diff,
                    main = "social games vs response diff",
                    xlab = "number of social games played per wk",
                    ylab = "response difference score",
                    xlim = c(0, 40), ylim = c(-1,1)))
abline(trend_line)

##Mothers and fathers that are PCG

table(P1.data$Parent_Type_p1)

#spearman correlation for freq social games and response dif#

resp_sg.corr <- cor.test(x = P1.data$Pi_social_games_freq_p1_fixed, y = P1.data$response.diff, method = 'spearman')
resp_sg.corr

resp_sg_pcgm.corr <- cor.test(x = P1.data_pcgm$Pi_social_games_freq_p1_fixed, y = P1.data_pcgm$response.diff, method = 'spearman')
resp_sg_pcgm.corr

#ability differences by gender IT

wilcox.test(Avg_SC_IT_P3 ~ Gender, data = IT.data)
wilcox.test(Avg_Succ_IT_P3 ~ Gender, data = IT.data)
wilcox.test(Avg_Lat_IT_P3 ~ Gender, data = IT.data)

#social context differences by gender PR

wilcox.test(Avg_SC_PR_P3 ~ Gender, data = PR_pcgm.data, conf.int = T, conf.level = .95, paired=FALSE)
Boys_PR<- PR_pcgm.data[which(PR_pcgm.data$Gender == 1),]
Girls_PR<- PR_pcgm.data[which(PR_pcgm.data$Gender == 2),]

?wilcox.test
median(Boys_PR$Avg_SC_PR_P3, na.rm = TRUE)
median(Girls_PR$Avg_SC_PR_P3, na.rm = TRUE)

wilcox.test(Avg_Succ_PR_P3 ~ Gender, data = PR.data, paired=FALSE)
wilcox.test(Avg_Lat_PR_P3 ~ Gender, data = PR.data, paired=FALSE)


wilcox.test(PR_Afill ~ Gender, data = PR.data)
wilcox.test(PR_Antag ~ Gender, data = PR.data)
wilcox.test(PR_Eng ~ Gender, data = PR.data)
wilcox.test(PR_JCE ~ Gender, data = PR.data)

#Social context differences by gender FT

wilcox.test(Avg_SC_FT_P3 ~ Gender, data = FT.data)
wilcox.test(Avg_Succ_FT_P3 ~ Gender, data = FT.data)
wilcox.test(Avg_LAT1_FT_P3 ~ Gender, data = FT.data)
wilcox.test(Avg_LAT2_FT_P3 ~ Gender, data = FT.data)
wilcox.test(CR_Afill ~ Gender, data = FT.data)
wilcox.test(CR_Antag ~ Gender, data = FT.data)
wilcox.test(CR_Eng ~ Gender, data = FT.data)
wilcox.test(CR_JCE ~ Gender, data = FT.data)

#test relationship between age and social context variables
plot(PR.data$Age_p3, PR.data$PR_Afill)
pr.afill.age.corr <- cor.test(x = PR.data$Age_p3, y = PR.data$PR_Afill, method = 'spearman')
pr.afill.age.corr

pr.antag.age.corr <- cor.test(x = PR.data$Age_p3, y = PR.data$PR_Antag, method = 'spearman')
pr.antag.age.corr

pr.eng.age.corr <- cor.test(x = PR.data$Age_p3, y = PR.data$PR_Eng, method = 'spearman')
pr.eng.age.corr

pr.jce.age.corr <- cor.test(x = PR.data$Age_p3, y = PR.data$PR_JCE, method = 'spearman')
pr.jce.age.corr


#Froggy task social context and age relationships

FT.afill.age.corr <- cor.test(x = FT.data$Age_p3, y = FT.data$CR_Afill, method = 'spearman')
FT.afill.age.corr

FT.antag.age.corr <- cor.test(x = FT.data$Age_p3, y = FT.data$CR_Antag, method = 'spearman')
FT.antag.age.corr

FT.eng.age.corr <- cor.test(x = FT.data$Age_p3, y = FT.data$CR_Eng, method = 'spearman')
FT.eng.age.corr
plot(FT.data$Age_p3, FT.data$CR_Eng)

FT.jce.age.corr <- cor.test(x = FT.data$Age_p3, y = FT.data$CR_JCE, method = 'spearman')
FT.jce.age.corr


##correlations of social context variables with one another by task
sc.df <- subset(P1.data, select = c("PR_Afill", "PR_Antag", "PR_Eng", "PR_JCE", "CR_Afill", 
                                    "CR_Antag", "CR_Eng", "CR_JCE"))

sc.df <- na.omit(sc.df)

sc.cor <- cor(sc.df, use = "complete.obs")

round(sc.cor, 2)

library(Hmisc)

sc.cor.pvalues <- rcorr(as.matrix(sc.df))
sc.cor.pvalues


#social games & CA IT
sg.sc.it.corr <- cor.test(x = IT.data$Pi_social_games_freq_p1_fixed, y = IT.data$Avg_SC_IT_P3, method = 'spearman')
sg.sc.it.corr

sg.succ.it.corr <- cor.test(x = IT.data$Pi_social_games_freq_p1_fixed, y = IT.data$Avg_Succ_IT_P3, method = 'spearman')
sg.succ.it.corr

sg.lat.it.corr <- cor.test(x = IT.data$Pi_social_games_freq_p1_fixed, y = IT.data$Avg_Lat_IT_P3, method = 'spearman')
sg.lat.it.corr

#social games & CA PR

sg.sc.pr.corr <- cor.test(x = PR.data$Pi_social_games_freq_p1_fixed, y = PR.data$Avg_SC_PR_P3, method = 'spearman')
sg.sc.pr.corr

sg.succ.pr.corr <- cor.test(x = PR.data$Pi_social_games_freq_p1_fixed, y = PR.data$Avg_Succ_PR_P3, method = 'spearman')
sg.succ.pr.corr

sg.lat.pr.corr <- cor.test(x = PR.data$Pi_social_games_freq_p1_fixed, y = PR.data$Avg_Lat_PR_P3, method = 'spearman')
sg.lat.pr.corr

#social games & CA FT

sg.sc.ft.corr <- cor.test(x = FT.data$Pi_social_games_freq_p1_fixed, y = FT.data$Avg_SC_FT_P3, method = 'spearman')
sg.sc.ft.corr

sg.succ.ft.corr <- cor.test(x = FT.data$Pi_social_games_freq_p1_fixed, y = FT.data$Avg_Succ_FT_P3, method = 'spearman')
sg.succ.ft.corr

sg.lat1.ft.corr <- cor.test(x = FT.data$Pi_social_games_freq_p1_fixed, y = FT.data$Avg_LAT1_FT_P3, method = 'spearman')
sg.lat1.ft.corr

sg.lat2.ft.corr <- cor.test(x = FT.data$Pi_social_games_freq_p1_fixed, y = FT.data$Avg_LAT2_FT_P3, method = 'spearman')
sg.lat2.ft.corr

#Check age and ability for IT

ag.sc.it.corr <- cor.test(x = IT.data$Age_p3, y = IT.data$Avg_SC_IT_P3, method = 'spearman')
ag.sc.it.corr

ag.succ.it.corr <- cor.test(x = IT.data$Age_p3, y = IT.data$Avg_Succ_IT_P3, method = 'spearman')
ag.succ.it.corr

ag.lat.it.corr <- cor.test(x = IT.data$Age_p3, y = IT.data$Avg_Lat_IT_P3, method = 'spearman')
ag.lat.it.corr

#Check age and ability for PR

ag.sc.pr.corr <- cor.test(x = PR.data$Age_p3, y = PR.data$Avg_SC_PR_P3, method = 'spearman')
ag.sc.pr.corr

ag.succ.pr.corr <- cor.test(x = PR.data$Age_p3, y = PR.data$Avg_Succ_PR_P3, method = 'spearman')
ag.succ.pr.corr

ag.lat.pr.corr <- cor.test(x = PR.data$Age_p3, y = PR.data$Avg_Lat_PR_P3, method = 'spearman')
ag.lat.pr.corr

#Check age and ability for FT

ag.sc.ft.corr <- cor.test(x = FT.data$Age_p3, y = FT.data$Avg_SC_FT_P3, method = 'spearman')
ag.sc.ft.corr

ag.succ.ft.corr <- cor.test(x = FT.data$Age_p3, y = FT.data$Avg_Succ_FT_P3, method = 'spearman')
ag.succ.ft.corr

ag.lat1.ft.corr <- cor.test(x = FT.data$Age_p3, y = FT.data$Avg_LAT1_FT_P3, method = 'spearman')
ag.lat1.ft.corr

ag.lat2.ft.corr <- cor.test(x = FT.data$Age_p3, y = FT.data$Avg_LAT2_FT_P3, method = 'spearman')
ag.lat2.ft.corr


##Test for differences in social context between tasks

##Make data into numeric vectors - have to use FT data because less observations

PR_Afill_task <- FT_pcgm.data$PR_Afill

CR_Afill_task <- FT_pcgm.data$CR_Afill

PR_Antag_task <- FT_pcgm.data$PR_Antag

CR_Antag_task <- FT_pcgm.data$CR_Antag

PR_Eng_task <- FT_pcgm.data$PR_Eng

CR_Eng_task <- FT_pcgm.data$CR_Eng

PR_JCE_task <- FT_pcgm.data$PR_JCE

CR_JCE_task <- FT_pcgm.data$CR_JCE


#create a dataframe


afill_time <- data.frame( 
  time = rep(c("PR_Afill_task", "CR_Afill_task"), each = 137),
  score = c(PR_Afill_task, CR_Afill_task)
)

#Ttest
afill.time.ttest  <- wilcox.test(score ~ time, data = afill_time, paired = FALSE)
afill.time.ttest


##visualize differences

library(ggplot2)
ggplot(data = afill_time,
       mapping = aes(x = time, y = score)) +
  geom_boxplot() +
  labs(title = "Differences in affiliation \n per task",
       x = "task",
       y = "afill score",
       colour = "") +
  theme(text = element_text(size = 14))

#Antag task differences (PR and FT)


antag_time <- data.frame( 
  time = rep(c("PR_Antag_task", "CR_Antag_task"), each = 137),
  score = c(PR_Antag_task, CR_Antag_task)
)


#Ttest
antag.time.ttest  <- wilcox.test(score ~ time, data = antag_time, paired = FALSE)
antag.time.ttest

#visualize differences

ggplot(data = antag_time,
       mapping = aes(x = time, y = score)) +
  geom_boxplot() +
  labs(title = "Differences in antag \n per task",
       x = "task",
       y = "antag score",
       colour = "") +
  theme(text = element_text(size = 14))

#Engagment differences by task

eng_time <- data.frame( 
  time = rep(c("PR_Eng_task", "CR_Eng_task"), each = 137),
  score = c(PR_Eng_task, CR_Eng_task)
)


#Ttest
eng.time.ttest  <- wilcox.test(score ~ time, data = eng_time, paired = FALSE)
eng.time.ttest

#visualize differences

ggplot(data = eng_time,
       mapping = aes(x = time, y = score)) +
  geom_boxplot() +
  labs(title = "Differences in eng \n per task",
       x = "task",
       y = "eng score",
       colour = "") +
  theme(text = element_text(size = 14))


##Joint coordinated engagement by task

jce_time <- data.frame( 
  time = rep(c("PR_JCE_task", "CR_JCE_task"), each = 137),
  score = c(PR_JCE_task, CR_JCE_task)
)


#Ttest
jce.time.ttest  <- wilcox.test(score ~ time, data = jce_time, paired = FALSE)
jce.time.ttest

#visualize differences

ggplot(data = jce_time,
       mapping = aes(x = time, y = score)) +
  geom_boxplot() +
  labs(title = "Differences in JCE \n per task",
       x = "task",
       y = "JCE score",
       colour = "") +
  theme(text = element_text(size = 14))


# Simple linear regression models - Ability -----------------------------------------

#IT task SC

IT_SC.model <- lm(Avg_SC_IT_P3 ~ response.diff, data = IT.data)
IT_SC.model
summary(IT_SC.model)

#IT Succ

IT_Succ.model <- lm(Avg_Succ_IT_P3 ~ response.diff + Gender, data = IT.data)
IT_Succ.model
summary(IT_Succ.model)

#IT Lat

IT_Lat.model <- lm(Avg_Lat_IT_P3 ~ response.diff + Gender, data = IT.data)
IT_Lat.model
summary(IT_Lat.model)


#PR task SC

PR_SC.model <- lm(Avg_SC_PR_P3 ~ response.diff + Gender, data = PR.data)
PR_SC.model
summary(PR_SC.model)

PR_SC.model_pcgm <- lm(Avg_SC_PR_P3 ~ response.diff + Gender, data = PR_pcgm.data)
PR_SC.model_pcgm
summary(PR_SC.model_pcgm)

#PR task Succ

PR_Succ.model <- lm(Avg_Succ_PR_P3 ~ response.diff, data = PR.data)
PR_Succ.model
summary(PR_Succ.model)

PR_Succ.model_pcgm <- lm(Avg_Succ_PR_P3 ~ response.diff, data = PR_pcgm.data)
PR_Succ.model_pcgm
summary(PR_Succ.model_pcgm)

#PR task Lat

PR_Lat.model <- lm(Avg_Lat_PR_P3 ~ response.diff, data = PR.data)
PR_Lat.model
summary(PR_Lat.model)

PR_Lat.model_pcgm <- lm(Avg_Lat_PR_P3 ~ response.diff, data = PR_pcgm.data)
PR_Lat.model_pcgm
summary(PR_Lat.model_pcgm)

#FT task SC*

FT_SC.model <- lm(Avg_SC_FT_P3 ~ response.diff, data = FT.data)
FT_SC.model
summary(FT_SC.model)

FT_SC.model_pcgm <- lm(Avg_SC_FT_P3 ~ response.diff, data = FT_pcgm.data)
FT_SC.model_pcgm
summary(FT_SC.model_pcgm)


#FT task Succ*

FT_Succ.model <- lm(Avg_Succ_FT_P3 ~ response.diff, data = FT.data)
FT_Succ.model
summary(FT_Succ.model)

FT_Succ.model_pcgm <- lm(Avg_Succ_FT_P3 ~ response.diff, data = FT_pcgm.data)
FT_Succ.model_pcgm
summary(FT_Succ.model_pcgm)
autoplot(FT_Succ.model_pcgm)

#FT task Lat1*

FT_Lat1.model <- lm(Avg_LAT1_FT_P3 ~ response.diff + Age_p3, data = FT.data)
FT_Lat1.model
summary(FT_Lat1.model)

FT_Lat1.model_pcgm <- lm(Avg_LAT1_FT_P3 ~ response.diff + Age_p3, data = FT_pcgm.data)
FT_Lat1.model_pcgm
summary(FT_Lat1.model_pcgm)
autoplot(FT_Lat1.model_pcgm)

#FT task Lat2

FT_Lat2.model <- lm(Avg_LAT2_FT_P3 ~ response.diff, data = FT.data)
FT_Lat2.model
summary(FT_Lat2.model)


FT_Lat2.model_pcgm <- lm(Avg_LAT2_FT_P3 ~ response.diff, data = FT_pcgm.data)
FT_Lat2.model_pcgm
summary(FT_Lat2.model_pcgm)


# Sig regression model check - Ability ---------------------------------------------

library(ggfortify)

autoplot(FT_SC.model_pcgm)
autoplot(FT_Succ.model_pcgm)
autoplot(FT_Lat1.model_pcgm)

library(gvlma)
gvlma::gvlma(FT_SC.model_pcgm)
gvlma::gvlma(FT_Succ.model_pcgm)
gvlma::gvlma(FT_Lat1.model_pcgm)



# Regression scatterplots (poster) ----------------------------------------

library(ggplot2)


ggplot(FT_pcgm.data, aes(response.diff, y=Avg_SC_FT_P3)) + 
  geom_point(color="blue",fill="blue",shape=21,alpha=0.5,size=4, stroke = 2) +
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, level=0.95) +
  ylab("Spatial Coordination") +
  xlab("CVS")+ 
  theme(axis.title.x = element_text(color="black", size=24, face="bold"),
        axis.title.y = element_text(color="black", size=24, face="bold"),
        axis.text = element_text(size=16)
  )

ggplot(FT_pcgm.data, aes(response.diff, y=Avg_Succ_FT_P3)) + 
  geom_point(color="blue",fill="blue",shape=21,alpha=0.5,size=6, stroke = 2) +
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, level=0.95) +
  ylab("Success") +
  xlab("CVS") + 
  theme(axis.title.x = element_text(color="black", size=24, face="bold"),
        axis.title.y = element_text(color="black", size=24, face="bold"),
        axis.text = element_text(size=16)
  )
library(ggplot2)
ggplot(FT_pcgm.data, aes(response.diff, y=Avg_LAT1_FT_P3)) + 
  geom_point(color="blue",fill="blue",shape=21,alpha=0.5,size=6, stroke = 2) +
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, level=0.95) +
  ylab("Latency - Toy Retrieval") +
  xlab("CVS")+ 
  theme(axis.title.x = element_text(color="black", size=24, face="bold"),
        axis.title.y = element_text(color="black", size=24, face="bold"),
        axis.text = element_text(size=16)
  )

library(ggplot2)
ggplot(FT_pcgm.data, aes(response.diff, y=Avg_LAT2_FT_P3)) + 
  geom_point(color="blue",fill="blue",shape=20,alpha=0.5,size=6, stroke = 2) +
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, level=0.95) +
  ylab("Latency 2 - End Goal") +
  xlab("CVS")+ 
  theme(axis.title.x = element_text(color="black", size=24, face="bold"),
        axis.title.y = element_text(color="black", size=24, face="bold"),
        axis.text = element_text(size=16)
  )


mean(FT_pcgm.data$Avg_SC_FT_P3, na.rm = TRUE)
mean(PR_pcgm.data$Avg_SC_PR_P3, na.rm = TRUE)
sd(FT_pcgm.data$Avg_SC_FT_P3, na.rm = TRUE)
sd(PR_pcgm.data$Avg_SC_PR_P3, na.rm = TRUE)

mean(FT_pcgm.data$Avg_Succ_FT_P3, na.rm = TRUE)
mean(PR_pcgm.data$Avg_Succ_PR_P3, na.rm = TRUE)
sd(FT_pcgm.data$Avg_Succ_FT_P3, na.rm = TRUE)
sd(PR_pcgm.data$Avg_Succ_PR_P3, na.rm = TRUE)


mean(FT_pcgm.data$Avg_LAT1_FT_P3, na.rm = TRUE)
mean(PR_pcgm.data$Avg_Lat_PR_P3, na.rm = TRUE)
sd(FT_pcgm.data$Avg_LAT1_FT_P3, na.rm = TRUE)
sd(PR_pcgm.data$Avg_Lat_PR_P3, na.rm = TRUE)

mean(P1.data_pcgm$response.diff, na.rm = TRUE)
sd(P1.data_pcgm$response.diff, na.rm = TRUE)

mean(P1.data_pcgm$Age_p1, na.rm = TRUE)
sd(P1.data_pcgm$Age_p1, na.rm = TRUE)

mean(P1.data_pcgm$Age_p3, na.rm = TRUE)
sd(P1.data_pcgm$Age_p3, na.rm = TRUE)

# Categorical analyses (DISREGARD) ----------------------------------------------------

#PR

PR.SC.aov <- aov(Avg_SC_PR_P3 ~ response.cat + Gender, data = PR.data)
summary(PR.SC.aov)

PR.data$response.cat <- as.factor(PR.data$response.cat)

kruskal.test(Avg_Succ_PR_P3 ~ response.cat, data = PR.data)

PR.Lat.aov <- aov(Avg_Lat_PR_P3 ~ response.cat + Gender, data = PR.data)
summary(PR.Lat.aov)

#FT

#++One way ANOVA

FT.SC.aov <- aov(Avg_SC_FT_P3 ~ response.cat, data = FT.data)
summary(FT.SC.aov)

FT.Succ.aov <- aov(Avg_Succ_FT_P3 ~ response.cat, data = FT.data)
summary(FT.Succ.aov)

#ANCOVA

FT.Lat1.aov <- aov(Avg_LAT1_FT_P3 ~ response.cat*Age_p3, data = FT.data)
summary(FT.Lat1.aov)

FT.Lat2.aov <- aov(Avg_LAT2_FT_P3 ~ response.cat*Age_p3, data = FT.data)
summary(FT.Lat2.aov)

#nonparametric FT - not incl latency because age differences

FT.data$response.cat <- as.factor(FT.data$response.cat)


kruskal.test(Avg_SC_FT_P3 ~ response.cat, data = FT.data)
kruskal.test(Avg_Succ_FT_P3 ~ response.cat, data = FT.data)


# Transformed data analyses - Ability -----------------------------------------------

###Only on significant slr models

install.packages("MASS")
library(MASS)

##IT roles task

##cannot do boxcox for Succ because response variables must be positive.

#boxcox transformation on IT_Lat model

boxcox(IT_Lat.model,lambda = seq(-3,3))

#lambda .5

bc.IT_Lat.model <- lm((Avg_Lat_IT_P3)^.5 ~ response.diff + Gender, data = IT.data)

summary(bc.IT_Lat.model)
bc.IT_Lat.model

library(ggfortify)
autoplot(bc.IT_Lat.model)


##Froggy Task

#boxcox transformation on FT_SC model

boxcox(FT_SC.model,lambda = seq(-3,3))

#lambda = 2.5


bc.FT_SC.model <- lm((Avg_SC_FT_P3)^2.5 ~ response.diff, data = FT_pcgm.data)

summary(bc.FT_SC.model)
bc.FT_SC.model


library(ggfortify)
autoplot(bc.FT_SC.model)

gvlma::gvlma(bc.FT_SC.model)




#boxcox transformation on FT_SucC model

boxcox(FT_Succ.model,lambda = seq(-3,3))

#lambda = 2.5

bc.FT_Succ.model <- lm((Avg_Succ_FT_P3)^2.5 ~ response.diff, data = FT_pcgm.data)

summary(bc.FT_Succ.model)
bc.FT_Succ.model

library(ggfortify)
autoplot(bc.FT_Succ.model)

gvlma::gvlma(bc.FT_Succ.model)

#boxcox transformation on FT_LAT1 model

boxcox(FT_Lat1.model,lambda = seq(-3,3))

#lambda = -.5

bc.FT_Lat1.model <- lm((Avg_LAT1_FT_P3)^-.5 ~ response.diff + Age_p3, data = FT_pcgm.data)

summary(bc.FT_Lat1.model)
bc.FT_Lat1.model

library(ggfortify)
autoplot(bc.FT_Lat1.model)
gvlma::gvlma(bc.FT_Lat1.model)


# P1 PPI P3 Ability Ordinal Reg Models (success) ------------------------------------

####ERROR NEED TO FIX - UNABLE TO RUN
#PR
library(MASS)

PR.data$Avg_SC_PR_P3 <- factor(PR.data$Avg_SC_PR_P3, ordered = TRUE)

PR_succ.pv1.OR_model <- polr(Avg_SC_PR_P3 ~ response.diff, data = PR.data, Hess=TRUE)
summary(PR_succ.pv1.OR_model)

# Visualizing data - Understanding ----------------------------------------

#Understanding freq

with(P1.data, hist(Avg_Anti_P3,breaks = 10,
                   xlab = "Cooperative understanding",
                   main = "Understanding score freq"))
library(e1071)
skewness(P1.data$Avg_Anti_P3, na.rm = TRUE)

#understanding versus response diff score

with(P1.data, plot(response.diff, Avg_Anti_P3))


# Preanalyses - Understanding ---------------------------------------------

#Anticipation scores non normally distributed

shapiro.test(P1.data$Avg_Anti_P3)

#check for difference bt understanding score by child gender

wilcox.test(Avg_Anti_P3 ~ Gender, data = P1.data)


#visualize data for understanding and time spent playing social games

cu.sg.trend_line = lm((Avg_Anti_P3) ~ (Pi_social_games_freq_p1_fixed_777), data = P1.data)

with(P1.data, plot(Pi_social_games_freq_p1_fixed_777, Avg_Anti_P3,
                    main = "social games vs CU",
                    xlab = "number of social games played per wk",
                    ylab = "anticipation score",
                    xlim = c(0, 40), ylim = c(-1,1)))
abline(cu.sg.trend_line)


#spearman correlation for freq social games and response dif#

cu_sg.corr <- cor.test(x = P1.data$Pi_social_games_freq_p1_fixed_777, y = P1.data$Avg_Anti_P3, method = 'spearman')
cu_sg.corr

freq.games.corr <- cor.test(x = P1.data$Pi_social_games_freq_p1_fixed, y = P1.data$Pi_social_games_freq_p1_fixed_777, method = 'pearson')
summary(freq.games.corr)

#visualize data for understanding and age at P1

cu.ag.trend_line = lm((Avg_Anti_P3) ~ (Age_p1), data = P1.data)

with(P1.data, plot(Age_p1, Avg_Anti_P3,
                    main = "Age vs CU",
                    xlab = "Age",
                    ylab = "anticipation score",
                    xlim = c(0, 40), ylim = c(-1,1)))
abline(cu.ag.trend_line)


#spearman correlation for freq social games and response dif#

cu_ag.corr <- cor.test(x = P1.data$Age_p1, y = P1.data$Avg_Anti_P3, method = 'spearman')
cu_ag.corr

plot(P1.data$Avg_Anti_P3, P1.data$response.diff)

# PPI 1 & Understanding SLR - Understanding -------------------------

CU.model <- lm(Avg_Anti_P3 ~ response.diff, data = P1.data_pcgm)
CU.model
summary(CU.model)
autoplot(CU.model)
library(gvlma)
gvlma::gvlma(CU.model)

P1.data_pcgm$Avg_Anti_P3_cat <- P1.data_pcgm$Avg_Anti_P3

library(MASS)

##create levels of anticipation variable
P1.data_pcgm$Avg_Anti_P3_cat <- factor(P1.data_pcgm$Avg_Anti_P3_cat)


CU.ordinal.model <- polr(Avg_Anti_P3_cat ~ response.diff, data = P1.data_pcgm, Hess=TRUE)
ctable <- coef(summary(CU.ordinal.model))
ctable

#P values and other add to table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

#confidence intervals
ci <- confint(CU.ordinal.model)
ci

##Models for cooperative understanding and cooperative ability

PR_CU_CA_SC <- lm(Avg_SC_PR_P3 ~ Avg_Anti_P3, data = PR.data)
PR_CU_CA_SC
summary(PR_CU_CA_SC)
library(ggfortify)
autoplot(PR_CU_CA_SC)

PR_CU_CA_Succ <- lm(Avg_Succ_PR_P3 ~ Avg_Anti_P3, data = PR.data)
PR_CU_CA_Succ
summary(PR_CU_CA_Succ)
autoplot(PR_CU_CA_Succ)

PR_CU_CA_Lat <- lm(Avg_Lat_PR_P3 ~ Avg_Anti_P3, data = PR.data)
PR_CU_CA_Lat
summary(PR_CU_CA_Lat)
autoplot(PR_CU_CA_Lat)


FT_CU_CA_SC <- lm(Avg_SC_FT_P3 ~ Avg_Anti_P3, data = FT.data)
FT_CU_CA_SC
summary(FT_CU_CA_SC)
autoplot(FT_CU_CA_SC)

FT_CU_CA_Succ <- lm(Avg_Succ_FT_P3 ~ Avg_Anti_P3, data = FT.data)
FT_CU_CA_Succ
summary(FT_CU_CA_Succ)
autoplot(FT_CU_CA_Succ)

FT_CU_CA_Lat <- lm(Avg_LAT1_FT_P3 ~ Avg_Anti_P3, data = FT.data)
FT_CU_CA_Lat
summary(FT_CU_CA_Lat)

FT_CU_CA_Lat2 <- lm(Avg_LAT2_FT_P3 ~ Avg_Anti_P3, data = FT.data)
FT_CU_CA_Lat2
summary(FT_CU_CA_Lat2)


##recode anticipation for both task subsets

PR.data$yn.Anti[PR.data$Avg_Anti_P3 == 0] <- 0
PR.data$yn.Anti[PR.data$Avg_Anti_P3 == .5] <- 1
PR.data$yn.Anti[PR.data$Avg_Anti_P3 == 1] <- 1

FT.data$yn.Anti[FT.data$Avg_Anti_P3 == 0] <- 0
FT.data$yn.Anti[FT.data$Avg_Anti_P3 == .5] <- 1
FT.data$yn.Anti[FT.data$Avg_Anti_P3 == 1] <- 1

#Parllel roles

model_sc.pr_UND <- lm(Avg_SC_PR_P3 ~ yn.Anti, data = PR.data) #+ Avg_Succ_PR_P3 + Avg_Lat_PR_P3
summary(model)
confint(model)
predict(model, type="response") # predicted values

model_succ.pr_UND <- lm(Avg_Succ_PR_P3 ~ yn.Anti, data = PR.data) 
summary(model_succ.pr_UND)

model_Lat.pr_UND <- lm(Avg_Lat_PR_P3 ~ yn.Anti, data = PR.data) 
summary(model_Lat.pr_UND)

#Froggy

model_SC.ft_UND <- lm(Avg_SC_FT_P3 ~ yn.Anti, data = FT.data) 
summary(model_SC.ft_UND)

model_Succ.ft_UND <- lm(Avg_Succ_FT_P3 ~ yn.Anti, data = FT.data) 
summary(model_Succ.ft_UND)

model_Lat1.ft_UND <- lm(Avg_LAT1_FT_P3 ~ yn.Anti, data = FT.data) 
summary(model_Lat1.ft_UND)

model_Lat2.ft_UND <- lm(Avg_LAT2_FT_P3 ~ yn.Anti, data = FT.data) 
summary(model_Lat2.ft_UND)


# P1 PPI P3 Social Context Analyses (SLRs) --------------------------------

##Parallel roles tasks - social context predicting cooperative ability

PR.SC.model.sc <- lm(Avg_SC_PR_P3 ~ PR_Afill + PR_Antag + PR_Eng + PR_JCE, data = PR_pcgm.data)
PR.SC.model.sc
summary(PR.SC.model.sc)
vif(PR.SC.model.sc)
autoplot(PR.SC.model.sc)
gvlma::gvlma(PR.SC.model.sc) #doesnt meet assumptions


PR.Succ.model.sc <- lm(Avg_Succ_PR_P3 ~ PR_Afill + PR_Antag + PR_Eng + PR_JCE, data = PR_pcgm.data)
PR.Succ.model.sc
summary(PR.Succ.model.sc)
vif(PR.Succ.model.sc)
gvlma::gvlma(PR.Succ.model.sc)

PR.Lat.model.sc <- lm(Avg_Lat_PR_P3 ~ PR_Afill + PR_Antag + PR_Eng + PR_JCE, data = PR_pcgm.data)
PR.Lat.model.sc
summary(PR.Lat.model.sc)
vif(PR.Lat.model.sc)
gvlma::gvlma(PR.Lat.model.sc)

##Froggy tasks - social context predicting cooperative ability

FT.SC.model.sc <- lm(Avg_SC_FT_P3 ~ CR_Afill + CR_Antag + CR_Eng + CR_JCE, data = FT_pcgm.data)
FT.SC.model.sc
summary(FT.SC.model.sc)
vif(FT.SC.model.sc)
gvlma::gvlma(FT.SC.model.sc)

FT.Succ.model.sc <- lm(Avg_Succ_FT_P3 ~ CR_Afill + CR_Antag + CR_Eng + CR_JCE, data = FT_pcgm.data)
FT.Succ.model.sc
summary(FT.Succ.model.sc)
vif(FT.Succ.model.sc)
gvlma::gvlma(FT.Succ.model.sc)

FT.Lat.model.sc <- lm(Avg_LAT1_FT_P3 ~ CR_Afill + CR_Antag + CR_Eng + CR_JCE, data = FT_pcgm.data)
FT.Lat.model.sc
summary(FT.Lat.model.sc)
vif(FT.Lat.model.sc)
gvlma::gvlma(FT.Lat.model.sc)

FT.Lat2.model.sc <- lm(Avg_LAT2_FT_P3 ~ CR_Afill + CR_Antag + CR_Eng + CR_JCE, data = FT_pcgm.data)
FT.Lat2.model.sc
summary(FT.Lat2.model.sc)
vif(FT.Lat2.model.sc)
gvlma::gvlma(FT.Lat2.model.sc)


# P3 PPI P3 Analyses ----------------------------

#Main data set for ORCA all study in group AND pcgs for P3s

P3.data <-all.data[which(all.data$Study_inout == 1 & all.data$PCG_p3 == 1),]
P3.data_pcgm <- all.data[which(all.data$Study_inout == 1 & all.data$PCG_p3 == 1 & all.data$Parent_Type_p3 == 2),]

#Parallel roles study in data subset - ALL PCG for P3 PPI

PR.data.p3 <- all.data[which(all.data$InOut_PR_P3 == 1 & all.data$Study_inout == 1 & all.data$PCG_p3 == 1),]

#Parallel roles study in data subset - ALL PCG AND Moms only for P3 PPI

PR_pcgm.data.p3 <- all.data[which(all.data$InOut_PR_P3 == 1 & all.data$Study_inout == 1 & all.data$PCG_p3 == 1 & all.data$Parent_Type_p3 == 2),]

#Froggy task study in data subset ALL PCG for P3 PPI

FT.data.p3 <- all.data[which(all.data$InOut_FT_P3 == 1 & all.data$Study_inout == 1 & all.data$PCG_p3 == 1),]

#Froggy task study in data subset - ALL PCG AND Moms only for P3 PPI

FT_pcgm.data.p3 <- all.data[which(all.data$InOut_FT_P3 == 1 & all.data$Study_inout == 1 & all.data$PCG_p3 == 1 & all.data$Parent_Type_p3 == 2),]


# P3 PPI P3 Ability - Visualizing/checking data ------------------------------------

#response diff distribution for P3 PPI

with(P3.data, hist(response.diff.p3,breaks = 10,
                   xlab = "ego/alt score",
                   main = "hist parent response"))
library(e1071)
skewness(P3.data$response.diff.p3, na.rm = TRUE)
shapiro.test(P3.data$response.diff.p3)


#Subset with PCGs for P1 and P3 who completed PPI at both time points for paired samples t test.

P1.P3_PCG_Bothforms <- all.data[which(all.data$Study_inout == 1 & all.data$PCG_p1 == 1 
                                      & all.data$PCG_p3 == 1 & all.data$response.diff >= -1
                                      & all.data$response.diff.p3 >= -1),]

##Make data into numeric vectors

T1 <- P1.P3_PCG_Bothforms$response.diff

T2 <- P1.P3_PCG_Bothforms$response.diff.p3

#create a dataframe

rd_time <- data.frame( 
  time = rep(c("T1", "T2"), each = 118),
  rd = c(T1,  T2)
)

#Ttest
wilcox.p1p3  <- wilcox.test(rd ~ time, data = rd_time, paired = TRUE)
wilcox.p1p3 


#visualize data for PPI responses, groups female vs male children

with(P3.data, boxplot(response.diff.p3 ~ Gender)) 

#data for parent responses are non normally distributed

shapiro.test(P3.data$response.diff)

#check for difference bt responses by child gender

wilcox.test(response.diff ~ Gender, data = P3.data)
wilcox.test(response.diff ~ Gender, data = P3.data_pcgm)

##No diff bt mothers & pcg and just pcg <<<<

#visualize data for PPI reasoning and time spent playing social games

trend_line = lm((response.diff.p3) ~ (Pi_social_games_freq_p3_fixed), data = P3.data)

with(P3.data, plot(Pi_social_games_freq_p3_fixed, response.diff.p3,
                    main = "social games vs response diff",
                    xlab = "number of social games played per wk",
                    ylab = "response difference score",
                    xlim = c(0, 40), ylim = c(-1,1)))
abline(trend_line)

##Mothers and fathers that are PCG

table(P3.data$Parent_Type_p3)

#spearman correlation for freq social games and response dif#

p3.resp_sg.corr <- cor.test(x = P3.data$Pi_social_games_freq_p3_fixed, y = P3.data$response.diff.p3, method = 'spearman')
p3.resp_sg.corr

p3.resp_sg_pcgm.corr <- cor.test(x = P3.data_pcgm$Pi_social_games_freq_p3_fixed, y = P3.data_pcgm$response.diff.p3, method = 'spearman')
p3.resp_sg_pcgm.corr

#ability differences by gender PR

wilcox.test(Avg_SC_PR_P3 ~ Gender, data = PR.data)
wilcox.test(Avg_Succ_PR_P3 ~ Gender, data = PR.data)
wilcox.test(Avg_Lat_PR_P3 ~ Gender, data = PR.data)

#ability differences by gender FT

wilcox.test(Avg_SC_FT_P3 ~ Gender, data = FT.data)
wilcox.test(Avg_Succ_FT_P3 ~ Gender, data = FT.data)
wilcox.test(Avg_LAT1_FT_P3 ~ Gender, data = FT.data)
wilcox.test(Avg_LAT2_FT_P3 ~ Gender, data = FT.data)


#social games & CA PR

p3.sg.sc.pr.corr <- cor.test(x = PR.data.p3$Pi_social_games_freq_p3_fixed, y = PR.data.p3$Avg_SC_PR_P3, method = 'spearman')
p3.sg.sc.pr.corr

p3.sg.succ.pr.corr <- cor.test(x = PR.data.p3$Pi_social_games_freq_p3_fixed, y = PR.data.p3$Avg_Succ_PR_P3, method = 'spearman')
p3.sg.succ.pr.corr

p3.sg.lat.pr.corr <- cor.test(x = PR.data.p3$Pi_social_games_freq_p3_fixed, y = PR.data.p3$Avg_Lat_PR_P3, method = 'spearman')
p3.sg.lat.pr.corr

#social games & CA FT

p3.sg.sc.ft.corr <- cor.test(x = FT.data.p3$Pi_social_games_freq_p3_fixed, y = FT.data.p3$Avg_SC_FT_P3, method = 'spearman')
p3.sg.sc.ft.corr

p3.sg.succ.ft.corr <- cor.test(x = FT.data.p3$Pi_social_games_freq_p3_fixed, y = FT.data.p3$Avg_Succ_FT_P3, method = 'spearman')
p3.sg.succ.ft.corr

p3.sg.lat1.ft.corr <- cor.test(x = FT.data.p3$Pi_social_games_freq_p3_fixed, y = FT.data.p3$Avg_LAT1_FT_P3, method = 'spearman')
p3.sg.lat1.ft.corr

p3.sg.lat2.ft.corr <- cor.test(x = FT.data.p3$Pi_social_games_freq_p3_fixed, y = FT.data.p3$Avg_LAT2_FT_P3, method = 'spearman')
p3.sg.lat2.ft.corr


#Check age and ability for PR

ag.sc.pr.corr <- cor.test(x = PR.data$Age_p3, y = PR.data$Avg_SC_PR_P3, method = 'spearman')
ag.sc.pr.corr

ag.succ.pr.corr <- cor.test(x = PR.data$Age_p3, y = PR.data$Avg_Succ_PR_P3, method = 'spearman')
ag.succ.pr.corr

ag.lat.pr.corr <- cor.test(x = PR.data$Age_p3, y = PR.data$Avg_Lat_PR_P3, method = 'spearman')
ag.lat.pr.corr

#Check age and ability for FT

ag.sc.ft.corr <- cor.test(x = FT.data$Age_p3, y = FT.data$Avg_SC_FT_P3, method = 'spearman')
ag.sc.ft.corr

ag.succ.ft.corr <- cor.test(x = FT.data$Age_p3, y = FT.data$Avg_Succ_FT_P3, method = 'spearman')
ag.succ.ft.corr

ag.lat1.ft.corr <- cor.test(x = FT.data$Age_p3, y = FT.data$Avg_LAT1_FT_P3, method = 'spearman')
ag.lat1.ft.corr

ag.lat2.ft.corr <- cor.test(x = FT.data$Age_p3, y = FT.data$Avg_LAT2_FT_P3, method = 'spearman')
ag.lat2.ft.corr


# P3 PPI P3 Ability SLR models --------------------------------------------

#PR task SC

#model includes PCG mothers and fathers
PR_SC.model.p3 <- lm(Avg_SC_PR_P3 ~ response.diff.p3, data = PR.data.p3)
PR_SC.model.p3
summary(PR_SC.model.p3)

##model for PCG and Mothers only
PR_SC.model_pcgm.p3 <- lm(Avg_SC_PR_P3 ~ response.diff.p3, data = PR_pcgm.data.p3)
PR_SC.model_pcgm.p3
summary(PR_SC.model_pcgm.p3)

#PR task Succ

#model includes PCG mothers and fathers
PR_Succ.model.p3 <- lm(Avg_Succ_PR_P3 ~ response.diff.p3, data = PR.data.p3)
PR_Succ.model.p3
summary(PR_Succ.model.p3)

##model for PCG and Mothers only
PR_Succ.model_pcgm.p3 <- lm(Avg_Succ_PR_P3 ~ response.diff.p3, data = PR_pcgm.data.p3)
PR_Succ.model_pcgm.p3
summary(PR_Succ.model_pcgm.p3)

#PR task Lat

PR_Lat.model.p3 <- lm(Avg_Lat_PR_P3 ~ response.diff.p3, data = PR.data.p3)
PR_Lat.model.p3
summary(PR_Lat.model.p3)

PR_Lat.model_pcgm.p3 <- lm(Avg_Lat_PR_P3 ~ response.diff.p3, data = PR_pcgm.data.p3)
PR_Lat.model_pcgm.p3
summary(PR_Lat.model_pcgm.p3)

#FT task SC

FT_SC.model.p3 <- lm(Avg_SC_FT_P3 ~ response.diff.p3, data = FT.data.p3)
FT_SC.model.p3
summary(FT_SC.model.p3)

FT_SC.model_pcgm.p3 <- lm(Avg_SC_FT_P3 ~ response.diff.p3, data = FT_pcgm.data.p3)
FT_SC.model_pcgm.p3
summary(FT_SC.model_pcgm.p3)


#FT task Succ*

FT_Succ.model.p3 <- lm(Avg_Succ_FT_P3 ~ response.diff.p3, data = FT.data.p3)
FT_Succ.model.p3
summary(FT_Succ.model.p3)

FT_Succ.model_pcgm.p3 <- lm(Avg_Succ_FT_P3 ~ response.diff.p3, data = FT_pcgm.data.p3)
FT_Succ.model_pcgm.p3
summary(FT_Succ.model_pcgm.p3)


#FT task Lat1*

FT_Lat1.model.p3 <- lm(Avg_LAT1_FT_P3 ~ response.diff.p3 + Age_p3, data = FT.data.p3)
FT_Lat1.model.p3
summary(FT_Lat1.model.p3)

FT_Lat1.model_pcgm.p3 <- lm(Avg_LAT1_FT_P3 ~ response.diff.p3 + Age_p3, data = FT_pcgm.data.p3)
FT_Lat1.model_pcgm.p3
summary(FT_Lat1.model_pcgm.p3)


#FT task Lat2

FT_Lat2.model_pcgm.p3 <- lm(Avg_LAT2_FT_P3 ~ response.diff.p3, data = FT_pcgm.data.p3)
FT_Lat2.model_pcgm.p3
summary(FT_Lat2.model_pcgm.p3)

library(ggfortify)
autoplot(FT_Lat2.model.p3)
autoplot(FT_Lat1.model_pcgm.p3)


# P3 PPI P3 Ability Transformed models (box cox) --------------------------

####PR

#parallel roles SC

boxcox(PR_SC.model.p3,lambda = seq(-3,3))

#lambda = 3

bc.PR_SC.model.p3<- lm((Avg_SC_PR_P3)^3 ~ response.diff.p3, data = PR_pcgm.data.p3)

summary(bc.PR_SC.model.p3)
bc.PR_SC.model.p3

library(ggfortify)
autoplot(bc.PR_SC.model.p3)

#parallel roles Lat

boxcox(PR_Lat.model.p3,lambda = seq(-3,3))

#lambda = .5

bc.PR_Lat.model.p3<- lm((Avg_Lat_PR_P3)^.5 ~ response.diff.p3, data = PR_pcgm.data.p3)

summary(bc.PR_Lat.model.p3)
bc.PR_Lat.model.p3

library(ggfortify)
autoplot(bc.PR_Lat.model.p3)


####FT
#spatial coordination

boxcox(FT_SC.model.p3,lambda = seq(-3,3))

#lambda = 2.5

bc.FT_SC.model.p3 <- lm((Avg_SC_FT_P3)^2.5 ~ response.diff.p3, data = FT_pcgm.data.p3)

summary(bc.FT_SC.model.p3) 
bc.FT_SC.model.p3

library(ggfortify)
autoplot(bc.FT_SC.model.p3)

#Success

boxcox(FT_Succ.model.p3,lambda = seq(-3,3))

#lambda = 2.5

bc.FT_Succ.model.p3 <- lm((Avg_Succ_FT_P3)^2.5 ~ response.diff.p3, data = FT_pcgm.data.p3)

summary(bc.FT_Succ.model.p3)
bc.FT_Succ.model.p3

library(ggfortify)
autoplot(bc.FT_Succ.model.p3)

#boxcox transformation on FT_LAT1 model PPI P3

boxcox(FT_Lat1.model.p3,lambda = seq(-3,3))

#lambda = -.5

bc.FT_Lat1.model.p3<- lm((Avg_LAT1_FT_P3)^-.5 ~ response.diff.p3 + Age_p3, data = FT_pcgm.data.p3)

summary(bc.FT_Lat1.model.p3)
bc.FT_Lat1.model.p3

library(ggfortify)
autoplot(bc.FT_Lat1.model.p3)

#FT LAT2

boxcox(FT_Lat2.model.p3,lambda = seq(-3,3))

#lambda = .25

bc.FT_Lat2.model.p3 <- lm((Avg_LAT2_FT_P3)^.25 ~ response.diff.p3, data = FT_pcgm.data.p3)

summary(bc.FT_Lat2.model.p3)
bc.FT_Lat2.model.p3

library(ggfortify)
autoplot(bc.FT_Lat2.model.p3)

# P1 IBQ and Ability Analyses ---------------------------------------------

# P1 IBQ visualising data -------------------------------------------------


with(P1.data, hist(SUR, breaks = 10,
                   xlab = "Infant surgency",
                   main = "IBQ - surg"))

boxplot(P1.data$SUR)

shapiro.test(P1.data$SUR) #nonnormal
library(rockchalk)
skewness(P1.data$SUR)

with(P1.data, hist(NEG, breaks = 10,
                        xlab = "Infant neg affectivity",
                        main = "IBQ - neg"))

boxplot(P1.data$NEG)

shapiro.test(P1.data$NEG) #nonnormal
library(rockchalk)
skewness(P1.data$NEG)

with(P1.data, hist(REG, breaks = 10,
                        xlab = "Infant reglatory capacity",
                        main = "IBQ - reg"))

boxplot(P1.data$REG)

shapiro.test(P1.data$REG) #nonnormal
library(rockchalk)
skewness(P1.data$REG)


# P1 IBQ & P3 Ability SLR (non transformed) -------------------------------------------------

#Parallel roles

#PR- Spatial coordination

ibq_pr.sc_model <- lm(Avg_SC_PR_P3 ~ SUR + NEG + REG, data = PR_pcgm.data)
ibq_pr.sc_model
summary(ibq_pr.sc_model)
library(ggfortify)
autoplot(ibq_pr.sc_model)

#PR - Success

ibq_pr.succ_model <- lm(Avg_Succ_PR_P3 ~ SUR + NEG + REG, data = PR_pcgm.data)
ibq_pr.succ_model
summary(ibq_pr.succ_model)
autoplot(ibq_pr.succ_model)

ibq_pr.lat_model <- lm(Avg_Lat_PR_P3 ~ SUR + NEG + REG, data = PR_pcgm.data)
ibq_pr.lat_model
summary(ibq_pr.lat_model)
autoplot(ibq_pr.lat_model)

##Froggy task

#FT- Spatial coordination
ibq_ft.sc_model <- lm(Avg_SC_FT_P3 ~ SUR + NEG + REG, data = FT_pcgm.data)
ibq_ft.sc_model
summary(ibq_ft.sc_model)
autoplot(ibq_ft.sc_model)

#FT- Success
ibq_ft.succ_model <- lm(Avg_Succ_FT_P3 ~ SUR + NEG + REG, data = FT_pcgm.data)
ibq_ft.succ_model
summary(ibq_ft.succ_model)
autoplot(ibq_ft.succ_model)

#FT- Lat 1
ibq_ft.lat1_model <- lm(Avg_LAT1_FT_P3 ~ SUR + NEG + REG, data = FT_pcgm.data)
ibq_ft.lat1_model
summary(ibq_ft.lat1_model)
autoplot(ibq_ft.lat1_model)

ibq_ft.lat2_model <- lm(Avg_LAT2_FT_P3 ~ SUR + NEG + REG, data = FT_pcgm.data)
ibq_ft.lat2_model
summary(ibq_ft.lat2_model)

# P5 Big 5 RWA SDO Data ---------------------------------------------------

personality.df <-all.data[which(all.data$Study_inout == 1 & all.data$PCG_p5 == 1),]
personality.df_pcgm <- all.data[which(all.data$Study_inout == 1 & all.data$PCG_p5 == 1 & all.data$Parent_Type_p5 == 2),]
personality.values.df_pcgm <- all.data[which(all.data$Study_inout == 1 & all.data$PCG_p5 == 1 & 
                                               all.data$Parent_Type_p5 == 2 & all.data$Parent_Type_p1 == 2 &
                                               all.data$PCG_p1 == 1 & all.data$PCG_p3 == 1 &
                                               all.data$Parent_Type_p3 == 2),]



# Big5 - Data subsets for tasks -------------------------------------------

#Indep roles study in data subset - ALL PCG AND Moms only

IT_pcgm.data.p3.p5 <- all.data[which(all.data$InOut_IT_P3 == 1 & all.data$Study_inout == 1 & all.data$PCG_p1 == 1 & all.data$Parent_Type_p1 == 2 & all.data$PCG_p5 == 1 & all.data$Parent_Type_p5 == 2),]


#Parallel roles study in data subset - ALL PCG

PR.data.p3.p5 <- all.data[which(all.data$InOut_PR_P3 == 1 & all.data$Study_inout == 1 & all.data$PCG_p1 == 1 & all.data$PCG_p5 == 1),]

#Parallel roles study in data subset - ALL PCG AND Moms only

PR_pcgm.data.p3.p5 <- all.data[which(all.data$InOut_PR_P3 == 1 & all.data$Study_inout == 1 & all.data$PCG_p1 == 1 & all.data$Parent_Type_p1 == 2 & all.data$PCG_p5 == 1 & all.data$Parent_Type_p5 == 2),]

#Froggy task study in data subset

FT.data.p3.p5 <- all.data[which(all.data$InOut_FT_P3 == 1 & all.data$Study_inout == 1 & all.data$PCG_p1 == 1 & all.data$PCG_p5 == 1),]

#Froggy task study in data subset - ALL PCG AND Moms only

FT_pcgm.data.p3.p5 <- all.data[which(all.data$InOut_FT_P3 == 1 & all.data$Study_inout == 1 & all.data$PCG_p1 == 1 & all.data$Parent_Type_p1 == 2 & all.data$PCG_p5 == 1 & all.data$Parent_Type_p5 == 2),]


# Big 5 RWA SDO - Visualiazing data and preanalyses ----------------------------

#Neur
with(personality.df, hist(Neur,breaks = 10,
                   xlab = "Neuroticism",
                   main = "Neur Personality \n distribution"))
library(e1071)
skewness(personality.df$Neur, na.rm = TRUE)
shapiro.test(personality.df$Neur)

#Openness
with(personality.df, hist(Open,breaks = 10,
                          xlab = "Openness",
                          main = "Openness Personality \n distribution"))
library(e1071)
skewness(personality.df$Open, na.rm = TRUE)
shapiro.test(personality.df$Open)

#Conscientiousness

with(personality.df, hist(Cons,breaks = 10,
                          xlab = "Conscientiousness",
                          main = "Cons Personality \n distribution"))
library(e1071)
skewness(personality.df$Cons, na.rm = TRUE)
shapiro.test(personality.df$Cons)

#Extraversion

with(personality.df, hist(Extra,breaks = 10,
                          xlab = "Extraversion",
                          main = "Extra Personality \n distribution"))
library(e1071)
skewness(personality.df$Extra, na.rm = TRUE)
shapiro.test(personality.df$Extra)

#Agreeableness

with(personality.df, hist(Agree,breaks = 10,
                          xlab = "Agreeableness",
                          main = "Agree Personality \n distribution"))
library(e1071)
skewness(personality.df$Agree, na.rm = TRUE)
shapiro.test(personality.df$Agree)

#Agreeableness

with(personality.df_pcgm, hist(Agree,breaks = 10,
                          xlab = "Agreeableness",
                          main = "Agree Personality \n distribution"))
library(e1071)
skewness(personality.df_pcgm$Agree, na.rm = TRUE)
shapiro.test(personality.df_pcgm$Agree)

#SDO
with(personality.df, hist(SDO,breaks = 10,
                          xlab = "Soc Dom Ori",
                          main = "SDO \n distribution"))
library(e1071)
skewness(personality.df$SDO, na.rm = TRUE)
shapiro.test(personality.df$SDO)

#RWA
with(personality.df, hist(RWA,breaks = 10,
                          xlab = "Right wing auth",
                          main = "RWA \n distribution"))
library(e1071)
skewness(personality.df$RWA, na.rm = TRUE)
shapiro.test(personality.df$RWA)

#EP
with(personality.df, hist(EP,breaks = 10,
                          xlab = "Ep",
                          main = "EP \n distribution"))
library(e1071)
skewness(personality.df$EP, na.rm = TRUE)
shapiro.test(personality.df$EP)

##scatter plots vis. data relationships

##Personality & response diff score

with(personality.df, plot(response.diff, Open))
with(personality.df, plot(response.diff, Cons))
with(personality.df, plot(response.diff, Extra))
with(personality.df, plot(response.diff, Agree))
with(personality.df, plot(response.diff, Neur))
with(personality.df, plot(response.diff, SDO))
with(personality.df, plot(response.diff, RWA))
with(personality.df, plot(response.diff, EP))

#Personality and child cooperative ability parallel roles

with(personality.df, plot(Avg_SC_PR_P3, Open))
with(personality.df, plot(Avg_SC_PR_P3, Cons))
with(personality.df, plot(Avg_SC_PR_P3, Extra))
with(personality.df, plot(Avg_SC_PR_P3, Agree))
with(personality.df, plot(Avg_SC_PR_P3, Neur))
with(personality.df, plot(Avg_SC_PR_P3, SDO))
with(personality.df, plot(Avg_SC_PR_P3, RWA))
with(personality.df, plot(Avg_SC_PR_P3, EP))

with(personality.df, plot(Avg_Succ_PR_P3, Open))
with(personality.df, plot(Avg_Succ_PR_P3, Cons))
with(personality.df, plot(Avg_Succ_PR_P3, Extra))
with(personality.df, plot(Avg_Succ_PR_P3, Agree))
with(personality.df, plot(Avg_Succ_PR_P3, Neur))
with(personality.df, plot(Avg_Succ_PR_P3, SDO))
with(personality.df, plot(Avg_Succ_PR_P3, RWA))
with(personality.df, plot(Avg_Succ_PR_P3, EP))

with(personality.df, plot(Avg_Lat_PR_P3, Open))
with(personality.df, plot(Avg_Lat_PR_P3, Cons))
with(personality.df, plot(Avg_Lat_PR_P3, Extra))
with(personality.df, plot(Avg_Lat_PR_P3, Agree))
with(personality.df, plot(Avg_Lat_PR_P3, Neur))
with(personality.df, plot(Avg_Lat_PR_P3, SDO))
with(personality.df, plot(Avg_Lat_PR_P3, RWA))
with(personality.df, plot(Avg_Lat_PR_P3, EP))

#Personality and child cooperative ability complementary roles

with(personality.df, plot(Avg_SC_FT_P3, Open))
with(personality.df, plot(Avg_SC_FT_P3, Cons))
with(personality.df, plot(Avg_SC_FT_P3, Extra))
with(personality.df, plot(Avg_SC_FT_P3, Agree))
with(personality.df, plot(Avg_SC_FT_P3, Neur))
with(personality.df, plot(Avg_SC_FT_P3, SDO))
with(personality.df, plot(Avg_SC_FT_P3, RWA))
with(personality.df, plot(Avg_SC_FT_P3, EP))

with(personality.df, plot(Avg_Succ_FT_P3, Open))
with(personality.df, plot(Avg_Succ_FT_P3, Cons))
with(personality.df, plot(Avg_Succ_FT_P3, Extra))
with(personality.df, plot(Avg_Succ_FT_P3, Agree))
with(personality.df, plot(Avg_Succ_FT_P3, Neur))
with(personality.df, plot(Avg_Succ_FT_P3, SDO))
with(personality.df, plot(Avg_Succ_FT_P3, RWA))
with(personality.df, plot(Avg_Succ_FT_P3, EP))

with(personality.df, plot(Avg_LAT1_FT_P3, Open))
with(personality.df, plot(Avg_LAT1_FT_P3, Cons))
with(personality.df, plot(Avg_LAT1_FT_P3, Extra))
with(personality.df, plot(Avg_LAT1_FT_P3, Agree))
with(personality.df, plot(Avg_LAT1_FT_P3, Neur))
with(personality.df, plot(Avg_LAT1_FT_P3, SDO))
with(personality.df, plot(Avg_LAT1_FT_P3, RWA))
with(personality.df, plot(Avg_LAT1_FT_P3, EP))

with(personality.df, plot(Avg_LAT2_FT_P3, Open))
with(personality.df, plot(Avg_LAT2_FT_P3, Cons))
with(personality.df, plot(Avg_LAT2_FT_P3, Extra))
with(personality.df, plot(Avg_LAT2_FT_P3, Agree))
with(personality.df, plot(Avg_LAT2_FT_P3, Neur))
with(personality.df, plot(Avg_LAT2_FT_P3, SDO))
with(personality.df, plot(Avg_LAT2_FT_P3, RWA))
with(personality.df, plot(Avg_LAT2_FT_P3, EP))

#correlation between dimensions tests

##subset personality df for correlation matrix

pp.df <- subset(personality.df, select = c("Open", "Cons", "Extra", "Agree", "Neur", "RWA", "SDO", "EP"))

pp.df <- na.omit(pp.df)

p5.cor <- cor(pp.df, use = "complete.obs")

round(p5.cor, 2)

library(Hmisc)

p5.cor.pvalues <- rcorr(as.matrix(pp.df))
p5.cor.pvalues


# Big 5 RWA SDO Coop ABILITY SLR models (untransformed)------------------------------------------------

#PR tasks - Multivariate multiple regression

PR_abil.pp.model <- lm(cbind(Avg_SC_PR_P3, Avg_Succ_PR_P3, Avg_Lat_PR_P3)  ~ Agree + Open + Cons + Extra + Neur, data = PR_pcgm.data.p3.p5)
PR_abil.pp.model
summary(PR_abil.pp.model)

PR_abil.ao.model <- lm(cbind(Avg_SC_PR_P3, Avg_Succ_PR_P3, Avg_Lat_PR_P3)  ~ Agree + Open, data = PR.data.p3.p5)
PR_abil.ao.model
summary(PR_abil.ao.model)

PR_abil.rse.model <- lm(cbind(Avg_SC_PR_P3, Avg_Succ_PR_P3, Avg_Lat_PR_P3)  ~ RWA + SDO + EP, data = PR.data.p3.p5)
PR_abil.rse.model
summary(PR_abil.rse.model)


#Froggy task - Multivariate multiple regression

FT_abil.pp.model <- lm(cbind(Avg_SC_FT_P3, Avg_Succ_FT_P3, Avg_LAT1_FT_P3, Avg_LAT2_FT_P3)  ~ Agree + Open + Cons + Extra + Neur, data = FT.data.p3.p5)
FT_abil.pp.model
summary(FT_abil.pp.model)

FT_abil.ao.model <- lm(cbind(Avg_SC_FT_P3, Avg_Succ_FT_P3, Avg_LAT1_FT_P3, Avg_LAT2_FT_P3)  ~ Agree + Open, data = FT.data.p3.p5)
FT_abil.ao.model
summary(FT_abil.ao.model)

FT_abil.rse.model <- lm(cbind(Avg_SC_FT_P3, Avg_Succ_FT_P3, Avg_LAT1_FT_P3, Avg_LAT2_FT_P3)  ~ RWA + SDO + EP, data = FT.data.p3.p5)
FT_abil.rse.model
summary(FT_abil.rse.model)

######Don't use multivariate regression because missing observations

#Spatial coordination - IT

IT_SC.pp.model.pcgm <- lm(Avg_SC_IT_P3 ~ Agree + Open + Cons + Extra + Neur, data = IT_pcgm.data.p3.p5)
IT_SC.pp.model.pcgm
summary(IT_SC.pp.model.pcgm)

#Success - IT

IT_Succ.pp.model.pcgm <- lm(Avg_Succ_IT_P3 ~ Agree + Open + Cons + Extra + Neur, data = IT_pcgm.data.p3.p5)
IT_Succ.pp.model.pcgm
summary(IT_Succ.pp.model.pcgm)

#Latency - IT

IT_Lat.pp.model.pcgm <- lm(Avg_Lat_IT_P3 ~ Agree + Open + Cons + Extra + Neur, data = IT_pcgm.data.p3.p5)
IT_Lat.pp.model.pcgm
summary(IT_Lat.pp.model.pcgm)

#Spatial coordination - PR

PR_SC.pp.model.pcgm <- lm(Avg_SC_PR_P3 ~ Agree + Open, data = PR_pcgm.data.p3.p5)
PR_SC.pp.model.pcgm
summary(PR_SC.pp.model.pcgm)

#Check assumptions
mean(PR_SC.pp.model.pcgm$residuals)
acf(PR_SC.pp.model.pcgm$residuals)
autoplot(PR_SC.pp.model)
var(PR_pcgm.data.p3.p5$Avg_SC_PR_P3, na.rm = TRUE)  
vif(PR_SC.pp.model.pcgm)
library(gvlma)
gvlma::gvlma(PR_SC.pp.model.pcgm)

PR_SC.ocean.model.pcgm <- lm(Avg_SC_PR_P3 ~ Agree + Open + Cons + Extra + Neur, data = PR_pcgm.data.p3.p5)
PR_SC.ocean.model.pcgm
summary(PR_SC.ocean.model.pcgm)


rse.PR_SC.pp.model.pcgm <- lm(Avg_SC_PR_P3 ~ RWA +SDO + EP, data = PR_pcgm.data.p3.p5)
rse.PR_SC.pp.model.pcgm
summary(rse.PR_SC.pp.model.pcgm)
autoplot(rse.PR_SC.pp.model.pcgm)

#Success 
PR_Succ.pp.model.pcgm <- lm(Avg_Succ_PR_P3 ~ Agree + Open, data = PR_pcgm.data.p3.p5)
PR_Succ.pp.model.pcgm
summary(PR_Succ.pp.model.pcgm)
autoplot(PR_Succ.pp.model.pcgm)

PR_Succ.ocean.model.pcgm <- lm(Avg_Succ_PR_P3 ~ Agree + Open + Cons + Extra + Neur, data = PR_pcgm.data.p3.p5)
PR_Succ.ocean.model.pcgm
summary(PR_Succ.ocean.model.pcgm)

rse.PR_Succ.pp.model.pcgm <- lm(Avg_Succ_PR_P3 ~ RWA + SDO + EP, data = PR_pcgm.data.p3.p5)
rse.PR_Succ.pp.model.pcgm
summary(rse.PR_Succ.pp.model.pcgm)
autoplot(rse.PR_Succ.pp.model.pcgm)

## Latency 
PR_Lat.pp.model.pcgm <- lm(Avg_Lat_PR_P3 ~ Agree + Open, data = PR_pcgm.data.p3.p5)
PR_Lat.pp.model.pcgm
summary(PR_Lat.pp.model.pcgm)
autoplot(PR_Lat.pp.model)

PR_Lat.ocean.model.pcgm <- lm(Avg_Lat_PR_P3 ~ Agree + Open + Cons + Extra + Neur, data = PR_pcgm.data.p3.p5)
PR_Lat.ocean.model.pcgm
summary(PR_Lat.ocean.model.pcgm)

PR_Lat.rse.model.pcgm <- lm(Avg_Lat_PR_P3 ~ RWA + SDO + EP, data = PR_pcgm.data.p3.p5)
PR_Lat.rse.model.pcgm
summary(PR_Lat.rse.model.pcgm)

#Froggy tasks - CR

#Spatial Coordination

FT_SC.pp.model <- lm(Avg_SC_FT_P3 ~ Agree + Open + Cons + Extra + Neur, data = FT.data.p3.p5)
FT_SC.pp.model
summary(FT_SC.pp.model)
autoplot(FT_SC.pp.model)

FT_SC.pp.model.pcgm <- lm(Avg_SC_FT_P3 ~ Agree + Open + Cons + Extra + Neur, data = FT_pcgm.data.p3.p5)
FT_SC.pp.model.pcgm
summary(FT_SC.pp.model.pcgm)
autoplot(FT_SC.pp.model.pcgm)

FT_SC.res.model.pcgm <- lm(Avg_SC_FT_P3 ~ RWA + SDO + EP, data = FT_pcgm.data.p3.p5)
FT_SC.res.model.pcgm
summary(FT_SC.res.model.pcgm)
autoplot(FT_SC.res.model.pcgm)

#success 

FT_Succ.pp.model.pcgm <- lm(Avg_Succ_FT_P3 ~ Agree + Open + Cons + Extra + Neur, data = FT_pcgm.data.p3.p5)
FT_Succ.pp.model.pcgm
summary(FT_Succ.pp.model.pcgm)
autoplot(FT_Succ.pp.model.pcgm)

mean(FT.data.p3.p5$Avg_Succ_FT_P3)
sd(FT.data.p3.p5$Avg_Succ_FT_P3)

rse.FT_Succ.pp.model.pcgm <- lm(Avg_Succ_FT_P3 ~ RWA + SDO + EP, data = FT_pcgm.data.p3.p5)
rse.FT_Succ.pp.model.pcgm
summary(rse.FT_Succ.pp.model.pcgm)
autoplot(rse.FT_Succ.pp.model.pcgm) #Need to transform ? Normality viol?
gvlma::gvlma(rse.FT_Succ.pp.model.pcgm)


#latency action 1 

FT_LAT1.pp.model.pcgm <- lm(Avg_LAT1_FT_P3 ~ Agree + Open + Cons + Extra + Neur, data = FT_pcgm.data.p3.p5)
FT_LAT1.pp.model.pcgm
summary(FT_LAT1.pp.model.pcgm)
autoplot(FT_LAT1.pp.model.pcgm)
gvlma::gvlma(FT_LAT1.pp.model.pcgm)


rwa.FT_LAT1.pp.model.pcgm <- lm(Avg_LAT1_FT_P3 ~ RWA + SDO+ EP, data = FT_pcgm.data.p3.p5)
rwa.FT_LAT1.pp.model.pcgm
summary(rwa.FT_LAT1.pp.model.pcgm)
autoplot(rwa.FT_LAT1.pp.model.pcgm)


#latency action 2 >>no sig<<
FT_LAT2.pp.model <- lm(Avg_LAT2_FT_P3 ~ Agree + Open + Cons + Extra + Neur, data = FT.data.p3.p5)
FT_LAT2.pp.model
summary(FT_LAT2.pp.model)
autoplot(FT_LAT2.pp.model)


FT_LAT2.pp.model.pcgm <- lm(Avg_LAT2_FT_P3 ~ Agree + Open + Cons + Extra + Neur, data = FT_pcgm.data.p3.p5)
FT_LAT2.pp.model.pcgm
vif(FT_LAT2.pp.model.pcgm)
summary(FT_LAT2.pp.model.pcgm)
autoplot(FT_LAT2.pp.model.pcgm)

rwa.FT_LAT2.pp.model.pcgm <- lm(Avg_LAT2_FT_P3 ~ RWA + SDO + EP, data = FT_pcgm.data.p3.p5)
rwa.FT_LAT2.pp.model.pcgm
summary(rwa.FT_LAT2.pp.model.pcgm)
autoplot(rwa.FT_LAT2.pp.model.pcgm)

##Response difference scores and sdo/rwa/ep/Big5
##personality.values.df_pcgm = same parent for all 3 phases and data points

#P1 values
rd.poli.model.pcgm <- lm(response.diff ~ SDO + RWA + EP, data = personality.values.df_pcgm)
rd.poli.model.pcgm
summary(rd.poli.model.pcgm)
autoplot(rd.poli.model.pcgm)
gvlma::gvlma(rd.poli.model.pcgm)

rd.pp.model.pcgm <- lm(response.diff ~ Agree + Open + Cons + Extra + Neur, data = personality.values.df_pcgm)
rd.pp.model.pcgm
summary(rd.pp.model.pcgm)
autoplot(rd.pp.model.pcgm)


#P3 values
rdp3.poli.model.pcgm <- lm(response.diff.p3 ~ SDO + RWA + EP, data = personality.values.df_pcgm)
rdp3.poli.model.pcgm
summary(rdp3.poli.model.pcgm)
autoplot(rdp3.poli.model.pcgm)

rdp3.pp.model.pcgm <- lm(response.diff.p3 ~ Agree + Open + Cons + Extra + Neur, data = personality.values.df_pcgm)
rdp3.pp.model.pcgm
summary(rdp3.pp.model.pcgm)
autoplot(rdp3.pp.model.pcgm)

##do parents high in agreeableness and openness play more social games?

#P1 PPI
p1.sg.pp_model.pcgm <- lm(Pi_social_games_freq_p1_fixed ~ Agree + Open + Cons + Extra + Neur, data = personality.values.df_pcgm)
p1.sg.pp_model.pcgm
summary(p1.sg.pp_model.pcgm)
autoplot(p1.sg.pp_model.pcgm)

cor.test(personality.values.df_pcgm$Pi_social_games_freq_p1_fixed, personality.values.df_pcgm$Agree)


#P3 PPI
p3.sg.pp_model.pcgm <- lm(Pi_social_games_freq_p3_fixed ~ Agree + Open + Cons + Extra + Neur, data = personality.values.df_pcgm)
p3.sg.pp_model.pcgm
summary(p3.sg.pp_model.pcgm)


# Big 5 RWA SDO Coop ABILITY SLR (Transformed) ----------------------------
####PR


#parallel roles SC
library(MASS)
boxcox(PR_SC.pp.model.pcgm,lambda = seq(-3,3))

#lambda = 3

bc.PR_SC.pp.model.pcgm <- lm((Avg_SC_PR_P3)^3 ~ Agree + Open, data = PR_pcgm.data.p3.p5)

summary(bc.PR_SC.pp.model.pcgm)
bc.PR_SC.pp.model.pcgm

library(ggfortify)
autoplot(bc.PR_SC.pp.model.pcgm)


# Big 5 RWA SDO & IBQ SLRs (untransformed) --------------------------------

pp.ibq.reg_model <- lm(REG ~ Agree + Open + Cons + Extra + Neur, data = personality.df_pcgm)
pp.ibq.reg_model
summary(pp.ibq.reg_model)

pp.ibq.sur_model <- lm(SUR ~ Agree + Open + Cons + Extra + Neur, data = personality.df_pcgm)
pp.ibq.sur_model
summary(pp.ibq.sur_model)

pp.ibq.neg_model <- lm(NEG ~ Agree + Open + Cons + Extra + Neur, data = personality.df_pcgm)
pp.ibq.neg_model
summary(pp.ibq.neg_model)





# Big5 > Coop UNDERSTANDING SLR -------------------------------------------

CU.model.pp <- lm(Avg_Anti_P3 ~ Agree + Open + Cons + Extra + Neur, data = personality.values.df_pcgm)
CU.model.pp
summary(CU.model.pp)
autoplot(CU.model.pp)


# Stepwise regression models p1 PPI - P3 ability - P5 pp ------------------


install.packages("tidyverse")
install.packages("caret")
install.packages("leaps")

library(tidyverse)
library(caret)
library(leaps)

###Parallel roles spatial coordination stepwise regression models

sr.pr.sc.df <- subset(PR_pcgm.data.p3.p5, select = c("Avg_SC_PR_P3",
                                                  "Open", "Cons", "Extra", "Agree", "Neur", "RWA",
                                                  "SDO", "EP", "response.diff",
                                                  "response.diff.p3", "PR_Afill", "PR_Antag", "PR_Eng", "PR_JCE",
                                                  "Age_p3", "Gender_Label"))
sr.pr.sc.df <- na.omit(sr.pr.sc.df)


library(MASS)
library(ggfortify)

sr.pr.sc_full.model <- lm(Avg_SC_PR_P3 ~., data = sr.pr.sc.df)
summary(sr.pr.sc_full.model)


sr.pr.sc_step.model <- stepAIC(sr.pr.sc_full.model, direction = "both", trace = FALSE)
summary(sr.pr.sc_step.model)
autoplot(sr.pr.sc_step.model)

##PR Success scores SWR

sr.pr.Succ.df <- subset(PR_pcgm.data.p3.p5, select = c("Avg_Succ_PR_P3",
                                                     "Open", "Cons", "Extra", "Agree", "Neur", "RWA",
                                                     "SDO", "EP", "response.diff",
                                                     "response.diff.p3", "PR_Afill", "PR_Antag", "PR_Eng", "PR_JCE",
                                                     "Age_p3", "Gender_Label"))
sr.pr.Succ.df <- na.omit(sr.pr.Succ.df)


library(MASS)
library(ggfortify)

sr.pr.Succ_full.model <- lm(Avg_Succ_PR_P3 ~., data = sr.pr.Succ.df)
summary(sr.pr.Succ_full.model)


sr.pr.Succ_step.model <- stepAIC(sr.pr.Succ_full.model, direction = "both", trace = FALSE)
summary(sr.pr.Succ_step.model)
autoplot(sr.pr.Succ_step.model)


##PR Latency stepwise regression

sr.pr.Lat.df <- subset(PR_pcgm.data.p3.p5, select = c("Avg_Lat_PR_P3",
                                                     "Open", "Cons", "Extra", "Agree", "Neur", "RWA",
                                                     "SDO", "EP", "response.diff",
                                                     "response.diff.p3", "PR_Afill", "PR_Antag", "PR_Eng", "PR_JCE",
                                                     "Age_p3", "Gender_Label"))
sr.pr.Lat.df <- na.omit(sr.pr.Lat.df)


library(MASS)
library(ggfortify)

sr.pr.Lat_full.model <- lm(Avg_Lat_PR_P3 ~., data = sr.pr.Lat.df)
summary(sr.pr.Lat_full.model)


sr.pr.Lat_step.model <- stepAIC(sr.pr.Lat_full.model, direction = "both", trace = FALSE)
summary(sr.pr.Lat_step.model)
autoplot(sr.pr.Lat_step.model)


###Froggy Task Stepwise regression models

##FT Spatial coordination

sr.ft.sc.df <- subset(FT_pcgm.data.p3.p5, select = c(Avg_SC_FT_P3, Open, Cons, Extra, Agree, Neur, RWA, SDO, EP, response.diff, response.diff.p3,  CR_Afill, CR_Antag, CR_Eng, CR_JCE, Age_p3, Gender))


sr.ft.sc.df <- na.omit(sr.ft.sc.df)


library(MASS)
library(ggfortify)

sr.ft.sc_full.model <- lm(Avg_SC_FT_P3 ~., data = sr.ft.sc.df)
summary(sr.ft.sc_full.model)


sr.ft.sc_step.model <- stepAIC(sr.ft.sc_full.model, direction = "both", trace = FALSE)
summary(sr.ft.sc_step.model)
autoplot(sr.ft.sc_step.model)

##Succ SWR models

sr.ft.Succ.df <- subset(FT_pcgm.data.p3.p5, select = c(Avg_Succ_FT_P3, Open, Cons, Extra, Agree, Neur, RWA, SDO, EP, response.diff, response.diff.p3,  CR_Afill, CR_Antag, CR_Eng, CR_JCE, Age_p3, Gender))


sr.ft.Succ.df <- na.omit(sr.ft.Succ.df)

sr.ft.Succ_full.model <- lm(Avg_Succ_FT_P3 ~., data = sr.ft.Succ.df)
summary(sr.ft.Succ_full.model)


sr.ft.Succ_step.model <- stepAIC(sr.ft.Succ_full.model, direction = "both", trace = FALSE)
summary(sr.ft.Succ_step.model)
autoplot(sr.ft.Succ_step.model)

##Lat1 models

sr.ft.LAT1.df <- subset(FT_pcgm.data.p3.p5, select = c(Avg_LAT1_FT_P3, Open, Cons, Extra, Agree, Neur, RWA, SDO, EP, response.diff, response.diff.p3,  CR_Afill, CR_Antag, CR_Eng, CR_JCE, Age_p3, Gender))


sr.ft.LAT1.df <- na.omit(sr.ft.LAT1.df)


sr.ft.LAT1_full.model <- lm(Avg_LAT1_FT_P3 ~., data = sr.ft.LAT1.df)
summary(sr.ft.LAT1_full.model)


sr.ft.LAT1_step.model <- stepAIC(sr.ft.LAT1_full.model, direction = "both", trace = FALSE)
summary(sr.ft.LAT1_step.model)
autoplot(sr.ft.LAT1_step.model)

#Lat2 models

sr.ft.LAT2.df <- subset(FT_pcgm.data.p3.p5, select = c(Avg_LAT2_FT_P3, Open, Cons, Extra, Agree, Neur, RWA, SDO, EP, response.diff, response.diff.p3,  CR_Afill, CR_Antag, CR_Eng, CR_JCE, Age_p3, Gender))

sr.ft.LAT2.df <- na.omit(sr.ft.LAT2.df)

sr.ft.LAT2_full.model <- lm(Avg_LAT2_FT_P3 ~., data = sr.ft.LAT2.df)
summary(sr.ft.LAT2_full.model)


sr.ft.LAT2_step.model <- stepAIC(sr.ft.LAT2_full.model, direction = "both", trace = FALSE)
summary(sr.ft.LAT2_step.model)
autoplot(sr.ft.LAT2_step.model)


# Significant regression models (all) -------------------------------------

#Froggy Task - Parent values P1 and Spatial Coordination 
FT_SC.model_pcgm <- lm(Avg_SC_FT_P3 ~ response.diff, data = FT_pcgm.data)
FT_SC.model_pcgm
summary(FT_SC.model_pcgm)

#Froggy Task - Parent values P1 and Success
FT_Succ.model_pcgm <- lm(Avg_Succ_FT_P3 ~ response.diff, data = FT_pcgm.data)
FT_Succ.model_pcgm
summary(FT_Succ.model_pcgm)

#Froggy Task - Parent values (+AGE) P1 and Latency Action 1
FT_Lat1.model_pcgm <- lm(Avg_LAT1_FT_P3 ~ response.diff + Age_p3, data = FT_pcgm.data)
FT_Lat1.model_pcgm
summary(FT_Lat1.model_pcgm)

#Froggy Task - Parent values P1 and Latency Action 2
FT_Lat2.model_pcgm <- lm(Avg_LAT2_FT_P3 ~ response.diff, data = FT_pcgm.data)
FT_Lat2.model_pcgm
summary(FT_Lat2.model_pcgm)

#Froggy Task - Parent values P1 and Latency Action 1
##Transformed to better meet assumptions
bc.FT_Lat1.model <- lm((Avg_LAT1_FT_P3)^-.5 ~ response.diff + Age_p3, data = FT_pcgm.data)
bc.FT_Lat1.model
summary(bc.FT_Lat1.model)

#Parallel Task - C-E Social Context P3 and Spatial Coordination
PR.SC.model.sc <- lm(Avg_SC_PR_P3 ~ PR_Afill + PR_Antag + PR_Eng + PR_JCE, data = PR_pcgm.data)
PR.SC.model.sc
summary(PR.SC.model.sc)
vif(PR.SC.model.sc)
autoplot(PR.SC.model.sc)
gvlma::gvlma(PR.SC.model.sc) #doesnt meet assumptions

#Parallel Task - C-E Social Context P3 and Success
PR.Succ.model.sc <- lm(Avg_Succ_PR_P3 ~ PR_Afill + PR_Antag + PR_Eng + PR_JCE, data = PR_pcgm.data)
PR.Succ.model.sc
summary(PR.Succ.model.sc)
vif(PR.Succ.model.sc)
gvlma::gvlma(PR.Succ.model.sc)

#Parallel Task - C-E Social Context P3 and Latency
PR.Lat.model.sc <- lm(Avg_Lat_PR_P3 ~ PR_Afill + PR_Antag + PR_Eng + PR_JCE, data = PR_pcgm.data)
PR.Lat.model.sc
summary(PR.Lat.model.sc)
vif(PR.Lat.model.sc)
gvlma::gvlma(PR.Lat.model.sc)

#Froggy Task - C-E Social Context P3 and Spatial Coordination
FT.SC.model.sc <- lm(Avg_SC_FT_P3 ~ CR_Afill + CR_Antag + CR_Eng + CR_JCE, data = FT_pcgm.data)
FT.SC.model.sc
summary(FT.SC.model.sc)
vif(FT.SC.model.sc)
gvlma::gvlma(FT.SC.model.sc)

#Froggy Task - C-E Social Context P3 and Success
FT.Succ.model.sc <- lm(Avg_Succ_FT_P3 ~ CR_Afill + CR_Antag + CR_Eng + CR_JCE, data = FT_pcgm.data)
FT.Succ.model.sc
summary(FT.Succ.model.sc)
vif(FT.Succ.model.sc)
gvlma::gvlma(FT.Succ.model.sc)

#Froggy Task - C-E Social Context P3 and Latency Action 1
FT.Lat.model.sc <- lm(Avg_LAT1_FT_P3 ~ CR_Afill + CR_Antag + CR_Eng + CR_JCE + Age_p3, data = FT_pcgm.data)
FT.Lat.model.sc
summary(FT.Lat.model.sc)
vif(FT.Lat.model.sc)
gvlma::gvlma(FT.Lat.model.sc)

#Froggy Task - C-E Social Context P3 and Latency Action 2
FT.Lat2.model.sc <- lm(Avg_LAT2_FT_P3 ~ CR_Afill + CR_Antag + CR_Eng + CR_JCE, data = FT_pcgm.data)
FT.Lat2.model.sc
summary(FT.Lat2.model.sc)
vif(FT.Lat2.model.sc)
gvlma::gvlma(FT.Lat2.model.sc)

#Froggy Task - Parent Values P3 (+AGE) and Latency Action 1
FT_Lat1.model_pcgm.p3 <- lm(Avg_LAT1_FT_P3 ~ response.diff.p3 + Age_p3, data = FT_pcgm.data.p3)
FT_Lat1.model_pcgm.p3
summary(FT_Lat1.model_pcgm.p3)

#Froggy Task - Parent Values P3 (+AGE) and Latency Action 1 TRANSFORMED
bc.FT_Lat1.model.p3<- lm((Avg_LAT1_FT_P3)^-.5 ~ response.diff.p3 + Age_p3, data = FT_pcgm.data.p3)
bc.FT_Lat1.model.p3
summary(bc.FT_Lat1.model.p3)

#Parallel Task - Parent Agreeableness & Openness and Spatial Coordination
PR_SC.pp.model.pcgm <- lm(Avg_SC_PR_P3 ~ Agree + Open, data = PR_pcgm.data.p3.p5)
PR_SC.pp.model.pcgm
summary(PR_SC.pp.model.pcgm)

#Check assumptions
mean(PR_SC.pp.model.pcgm$residuals)
acf(PR_SC.pp.model.pcgm$residuals)
autoplot(PR_SC.pp.model)
var(PR_pcgm.data.p3.p5$Avg_SC_PR_P3, na.rm = TRUE)  
vif(PR_SC.pp.model.pcgm)
library(gvlma)
gvlma::gvlma(PR_SC.pp.model.pcgm)

#Parallel Task - Parent Big 5 and Spatial Coordination
PR_SC.ocean.model.pcgm <- lm(Avg_SC_PR_P3 ~ Agree + Open + Cons + Extra + Neur, data = PR_pcgm.data.p3.p5)
PR_SC.ocean.model.pcgm
summary(PR_SC.ocean.model.pcgm)

#Parallel Task - Parent Agreeableness & Openness and Success
PR_Succ.pp.model.pcgm <- lm(Avg_Succ_PR_P3 ~ Agree + Open, data = PR_pcgm.data.p3.p5)
PR_Succ.pp.model.pcgm
summary(PR_Succ.pp.model.pcgm)
autoplot(PR_Succ.pp.model.pcgm)

#Parallel Task - Parent Big 5 and Success
PR_Succ.ocean.model.pcgm <- lm(Avg_Succ_PR_P3 ~ Agree + Open + Cons + Extra + Neur, data = PR_pcgm.data.p3.p5)
PR_Succ.ocean.model.pcgm
summary(PR_Succ.ocean.model.pcgm)

#Froggy Task - Parent Big 5  and Latency Action 1
FT_LAT1.pp.model.pcgm <- lm(Avg_LAT1_FT_P3 ~ Agree + Open + Cons + Extra + Neur, data = FT_pcgm.data.p3.p5)
FT_LAT1.pp.model.pcgm
summary(FT_LAT1.pp.model.pcgm)
autoplot(FT_LAT1.pp.model.pcgm)
gvlma::gvlma(FT_LAT1.pp.model.pcgm)

#Froggy Task - Parent political values and Latency Action 1
rwa.FT_LAT1.pp.model.pcgm <- lm(Avg_LAT1_FT_P3 ~ RWA + SDO+ EP, data = FT_pcgm.data.p3.p5)
rwa.FT_LAT1.pp.model.pcgm
summary(rwa.FT_LAT1.pp.model.pcgm)
autoplot(rwa.FT_LAT1.pp.model.pcgm)

##Parent political values and prosocial values p1
rd.poli.model.pcgm <- lm(response.diff ~ SDO + RWA + EP, data = personality.values.df_pcgm)
rd.poli.model.pcgm
summary(rd.poli.model.pcgm)
autoplot(rd.poli.model.pcgm)
gvlma::gvlma(rd.poli.model.pcgm)

##Parent political values and prosocial values p3
rdp3.poli.model.pcgm <- lm(response.diff.p3 ~ SDO + RWA + EP, data = personality.values.df_pcgm)
rdp3.poli.model.pcgm
summary(rdp3.poli.model.pcgm)
autoplot(rdp3.poli.model.pcgm)

##Parent prosocial values p3 and Parent Big 5
rdp3.pp.model.pcgm <- lm(response.diff.p3 ~ Agree + Open + Cons + Extra + Neur, data = personality.values.df_pcgm)
rdp3.pp.model.pcgm
summary(rdp3.pp.model.pcgm)
autoplot(rdp3.pp.model.pcgm)

##Parent social games frequency and Big 5
p1.sg.pp_model.pcgm <- lm(Pi_social_games_freq_p1_fixed ~ Agree + Open + Cons + Extra + Neur, data = personality.values.df_pcgm)
p1.sg.pp_model.pcgm
summary(p1.sg.pp_model.pcgm)
autoplot(p1.sg.pp_model.pcgm)

mean(p1.sg.pp_model.pcgm$residuals)
acf(p1.sg.pp_model.pcgm$residuals)
var(personality.values.df_pcgm$Pi_social_games_freq_p1_fixed, na.rm = TRUE)  

cor.test(personality.values.df_pcgm$Pi_social_games_freq_p1_fixed, personality.values.df_pcgm$Agree)

#Parent personality predicting child temperament
pp.ibq.reg_model <- lm(REG ~ Agree + Open + Cons + Extra + Neur, data = personality.df_pcgm)
pp.ibq.reg_model
summary(pp.ibq.reg_model)
autoplot(pp.ibq.reg_model)
gvlma::gvlma(pp.ibq.reg_model) #meets assumptions


# Mediation and Moderation Models -----------------------------------------

##Spatial Coordination - Parallel roles tasks

PR_pp.antag_medmodel <- lm(Avg_SC_PR_P3~ Agree + Open + Cons + Extra + Neur + PR_Antag, data = PR_pcgm.data.p3.p5)
PR_pp.antag_medmodel
summary(PR_pp.antag_medmodel)
##X fully predicts Y - no mediation effects with M = PR_Antag


PR_pp.sg_medmodel <- lm(Avg_SC_PR_P3 ~ Open + Agree + Pi_social_games_freq_p1_fixed, data = PR_pcgm.data.p3.p5)
PR_pp.sg_medmodel
summary(PR_pp.sg_medmodel)
summary(PR_SC.ocean.model.pcgm)
##X fully predicts Y - NO mediation effects



#Test for moderation effect SC ~ Agree * Social games P1

#Centering Data
Open.c   <- with(PR_pcgm.data.p3.p5, c(scale(Open, center=TRUE, scale=FALSE))) #Centering IV; Openness
Agree.c   <- with(PR_pcgm.data.p3.p5, c(scale(Agree,  center=TRUE, scale=FALSE))) #Center IV; Agreeableness
Games.c   <- with(PR_pcgm.data.p3.p5, c(scale(Pi_social_games_freq_p1_fixed,  center=TRUE, scale=FALSE))) #Centering moderator; Social games

fitMod <- lm(Avg_SC_PR_P3 ~ Gender + Agree.c + Agree.c * Games.c, data = PR_pcgm.data.p3.p5) #Model interacts IV & moderator
summary(fitMod)

gvlma(fitMod)

library(stargazer)
stargazer(fitMod,type="text", title = "Agreeableness and Social Games")
library(rockchalk)
ps  <- plotSlopes(fitMod, plotx="Agree.c", modx="Games.c", xlab = "Agreeableness", ylab = "PR - Spatial Coordination", modxVals = "std.dev")

#Test for moderation effect SC ~ Agree * Antag

#Centering Data
Open.c   <- with(PR_pcgm.data.p3.p5, c(scale(Open, center=TRUE, scale=FALSE))) #Centering IV; Openness
Agree.c   <- with(PR_pcgm.data.p3.p5, c(scale(Agree,  center=TRUE, scale=FALSE))) #Center IV; Agreeableness
Antag.c  <- with(PR_pcgm.data.p3.p5, c(scale(PR_Antag, center=TRUE, scale=FALSE))) #Centering moderator; PR Antag behav

fitMod2 <- lm(Avg_Succ_PR_P3 ~ Agree.c + Agree.c * Games.c, data = PR_pcgm.data.p3.p5) #Model interacts IV & moderator
summary(fitMod2)

gvlma(fitMod2)

library(stargazer)
stargazer(fitMod,type="text", title = "Agreeableness and Antagonism in PR")
library(rockchalk)
ps2 <- plotSlopes(fitMod2, plotx="Agree.c", modx="Antag.c", xlab = "Agreeableness", ylab = "Spatial Coodination", modxVals = "std.dev")


# STUDY 1 FINALIZED STATS -------------------------------------------------

# 00000000000000000000000000000000000
#Variables: Parent personality (Big 5), values (diff score P1), behavior (social game freq)
#Sample descriptives - M/SD social games, 1 sample ttest w diff score
#Correlations - (1) values*social games (2) personality*sg (3) personality*values
# 00000000000000000000000000000000000

## Sample Descriptives

#General
mean(P1.data_pcgm$Age_p1)
mean(P1.data_pcgm$Age_p3, na.rm = TRUE)
sd(P1.data_pcgm$Age_p1)
sd(P1.data_pcgm$Age_p3, na.rm = TRUE)

table(P1.data_pcgm$Gender)
table(P1.data_pcgm$Parent_Type_p1,P1.data_pcgm$PCG_p1)

#Social Games
mean(P1.data_pcgm$Pi_cooperate_freq_p1_fixed, na.rm = TRUE) #answers such as "often" "daily" not included
sd(P1.data_pcgm$Pi_cooperate_freq_p1_fixed, na.rm = TRUE) #answers such as "often" "daily" not included

#Cooperative Value Score P1
mean(P1.data_pcgm$response.diff, na.rm = TRUE)
sd(P1.data_pcgm$response.diff, na.rm = TRUE)

shapiro.test(P1.data_pcgm$response.diff) # non normal distribution

#????Do one-sample t-test????

#Parent N for personality measure

length(personality.df_pcgm$Agree)

#Parent personality - Openness
mean(personality.values.df_pcgm$Open, na.rm = TRUE)
sd(personality.values.df_pcgm$Open, na.rm = TRUE)

#Parent personality - Conscientiousness
mean(personality.values.df_pcgm$Cons, na.rm = TRUE)
sd(personality.values.df_pcgm$Cons, na.rm = TRUE)

#Parent personality - Extraversion
mean(personality.values.df_pcgm$Extra, na.rm = TRUE)
sd(personality.values.df_pcgm$Extra, na.rm = TRUE)

#Parent personality - Agreeableness
mean(personality.values.df_pcgm$Agree, na.rm = TRUE)
sd(personality.values.df_pcgm$Agree, na.rm = TRUE)

#Parent personality - Neuroticism
mean(personality.values.df_pcgm$Neur, na.rm = TRUE)
sd(personality.values.df_pcgm$Neur, na.rm = TRUE)

mean(P1.data_pcgm$no_of_op_p1, na.rm = TRUE)
sd(P1.data_pcgm$no_of_op_p1, na.rm = TRUE)

#Test correlations

#Cooperative Value Score (CVS) and Social Games (SG) #.ns
resp_sg_pcgm.corr <- cor.test(x = P1.data_pcgm$Pi_social_games_freq_p1_fixed, y = P1.data_pcgm$response.diff, method = 'spearman')
resp_sg_pcgm.corr

##Personality and CVS

#Agreeableness Personality and Cooperative Value Score (CVS) #.ns
resp_agree.corr <- cor.test(x = P1.data_pcgm$response.diff, y = P1.data_pcgm$Agree, method = 'spearman')
resp_agree.corr

#Openness Personality and Cooperative Value Score (CVS) #.ns
resp_open.corr <- cor.test(x = P1.data_pcgm$response.diff, y = P1.data_pcgm$Open, method = 'spearman')
resp_open.corr

#Conscienciousness Personality and Cooperative Value Score (CVS) #.ns
resp_cons.corr <- cor.test(x = P1.data_pcgm$response.diff, y = P1.data_pcgm$Cons, method = 'spearman')
resp_cons.corr

#Extraversion Personality and Cooperative Value Score (CVS) #.ns

resp_extra.corr <- cor.test(x = P1.data_pcgm$response.diff, y = P1.data_pcgm$Extra, method = 'spearman')
resp_extra.corr

#Neuroticism Personality and Cooperative Value Score (CVS) #.ns
resp_neur.corr <- cor.test(x = P1.data_pcgm$response.diff, y = P1.data_pcgm$Neur, method = 'spearman')
resp_neur.corr

##Personality and Social Games

#Agreeableness Personality (CVS) and Social Games (SG) #SIG p < .001, r = .31

plot(P1.data_pcgm$Pi_social_games_freq_p1_fixed_777, P1.data_pcgm$Agree)
resp_agree.corr <- cor.test(x = P1.data_pcgm$Pi_social_games_freq_p1_fixed, y = P1.data_pcgm$Agree, method = 'pearson')
resp_agree.corr

#Openness Personality (CVS) and Social Games (SG) #.ns
resp_open.corr <- cor.test(x = P1.data_pcgm$Pi_social_games_freq_p1_fixed, y = P1.data_pcgm$Open, method = 'spearman')
resp_open.corr

#Conscienciousness Personality (CVS) and Social Games (SG) #SIG p <.05, r = .20
resp_cons.corr <- cor.test(x = P1.data_pcgm$Pi_social_games_freq_p1_fixed, y = P1.data_pcgm$Cons, method = 'pearson')
resp_cons.corr

#Extraversion Personality (CVS) and Social Games (SG) #.ns
resp_extra.corr <- cor.test(x = P1.data_pcgm$Pi_social_games_freq_p1_fixed, y = P1.data_pcgm$Extra, method = 'spearman')
resp_extra.corr

#Neuroticism Personality (CVS) and Social Games (SG) #.ns
resp_neur.corr <- cor.test(x = P1.data_pcgm$Pi_social_games_freq_p1_fixed, y = P1.data_pcgm$Neur, method = 'spearman')
resp_neur.corr

# STUDY 2 FINALIZED STATS -------------------------------------------------
#Descriptives

mean(P1.data_pcgm$Avg_Anti_P3, na.rm = TRUE)
sd(P1.data_pcgm$Avg_Anti_P3, na.rm = TRUE)
length(P1.data_pcgm$Avg_Anti_P3)

#PR task - spatial coordination
mean(PR.data$Avg_SC_PR_P3, na.rm = TRUE)
sd(PR.data$Avg_SC_PR_P3, na.rm = TRUE)
length(PR.data$Avg_SC_PR_P3)

#PR task - success
mean(PR.data$Avg_Succ_PR_P3, na.rm = TRUE)
sd(PR.data$Avg_Succ_PR_P3, na.rm = TRUE)
length(PR.data$Avg_Succ_PR_P3)

#PR task - latency
mean(PR.data$Avg_Lat_PR_P3, na.rm = TRUE)
sd(PR.data$Avg_Lat_PR_P3, na.rm = TRUE)
length(PR.data$Avg_Lat_PR_P3)

#Complementary roles task (FT)
#FT task - spatial coordination
mean(FT.data$Avg_SC_FT_P3, na.rm = TRUE)
sd(FT.data$Avg_SC_FT_P3, na.rm = TRUE)
length(FT.data$Avg_SC_FT_P3)

#FT task - success
mean(FT.data$Avg_Succ_FT_P3, na.rm = TRUE)
sd(FT.data$Avg_Succ_FT_P3, na.rm = TRUE)
length(FT.data$Avg_Succ_FT_P3)

#FT task - latency action 1
mean(FT.data$Avg_LAT1_FT_P3, na.rm = TRUE)
sd(FT.data$Avg_LAT1_FT_P3, na.rm = TRUE)

#FT task - latency action 2
mean(FT.data$Avg_LAT2_FT_P3, na.rm = TRUE)
sd(FT.data$Avg_LAT2_FT_P3, na.rm = TRUE)

# STUDY 2 - Parent + CU ---------------------------------------------------

##Does parent CVS at P1 predict CU (anticipation) at P3? No.

library(ggfortify)
?autoplot

CU.model <- lm(Avg_Anti_P3 ~ response.diff, data = P1.data_pcgm)
CU.model
summary(CU.model)
autoplot(CU.model)
library(gvlma)

#Correlation as suggested by stats consultants
cu_cvs.corr <- cor.test(x = P1.data_pcgm$response.diff, y = P1.data_pcgm$Avg_Anti_P3, method = 'spearman')
cu_cvs.corr

plot(x = P1.data_pcgm$response.diff, y = P1.data_pcgm$Avg_Anti_P3)


##Does parent personality at P5 predict CU (anticipation) at P3? Yes. 
  #Full model: Agreeableness p = .05, r2 (full model) = .05, model non sig
  #Prosocial Personality model: Agreeableness p = .05, r2 (full model) = .04, model non sig.

##Model with all Big 5##
CU_pp.model.pcgm <- lm(Avg_Anti_P3 ~ Agree + Open + Cons + Extra + Neur, data = personality.values.df_pcgm)
CU_pp.model.pcgm
summary(CU_pp.model.pcgm)
autoplot(CU_pp.model.pcgm)


#Model with only Agree and Open (prosocial personality)
CU_ao.model.pcgm <- lm(Avg_Anti_P3 ~ Agree + Open, data = personality.values.df_pcgm)
CU_ao.model.pcgm
summary(CU_ao.model.pcgm)

CU_sg.model.pcgm <- lm(Avg_Anti_P3 ~ Pi_social_games_freq_p1_fixed, data = personality.values.df_pcgm)
CU_sg.model.pcgm
summary(CU_sg.model.pcgm)

citation(package = "stats")


# STUDY 2 - Parent + CA ---------------------------------------------------


##Does parent CVS predict CA (SC, Succ, Lat) at P3? Yes, but only in the Complementary Roles Task

#Parallel roles - Spatial Coordination linear regression
PR_SC.model_pcgm <- lm(Avg_SC_PR_P3 ~ response.diff + Gender, data = PR_pcgm.data)
PR_SC.model_pcgm
summary(PR_SC.model_pcgm)
autoplot(PR_SC.model_pcgm)

#Parallel roles - Spatial Coordination Ordinal regression

library(MASS)
PR_pcgm.data$Avg_SC_PR_P3_cat <- factor(PR_pcgm.data$Avg_SC_PR_P3, ordered = TRUE)

PR_sc.cvs.OR_model <- polr(Avg_SC_PR_P3_cat ~ response.diff, data = PR_pcgm.data, Hess=TRUE)
summary(PR_sc.cvs.OR_model)
PR_SC.model.table <- coef(summary(PR_sc.cvs.OR_model))
PR_SC.model.table

#P values and other add to table
p <- pnorm(abs(PR_SC.model.table[, "t value"]), lower.tail = FALSE) * 2
PR_SC.model.table <- cbind(PR_SC.model.table, "p value" = p)
PR_SC.model.table

#confidence intervals
ci <- confint(PR_sc.cvs.OR_model)
ci
exp(ci)
#odds ratio
exp(coef(PR_sc.cvs.OR_model))
#For every unit increase in parent CVS, the odds of being more spatially coordinated on the 
#PR task  is multiplied by 1.28 (28% [1.28-1]) times, holding constant all other variables.
##Use boxplots to represent

#Parallel roles - Success Linear Regression
PR_Succ.model_pcgm <- lm(Avg_Succ_PR_P3 ~ response.diff, data = PR_pcgm.data)
PR_Succ.model_pcgm
summary(PR_Succ.model_pcgm)

#Parallel roles - Success Ordinal Regression
install.packages("effects")
library(splines)
library(MASS)
library(effects)
library(car)
PR_pcgm.data$Avg_Succ_PR_P3_cat <- factor(PR_pcgm.data$Avg_Succ_PR_P3, ordered = TRUE)

PR_succ.cvs.OR_model <- polr(Avg_Succ_PR_P3_cat ~ response.diff, data = PR_pcgm.data, Hess=TRUE)
summary(PR_succ.cvs.OR_model)
PR_Succ.model.table <- coef(summary(PR_succ.cvs.OR_model))
PR_Succ.model.table


plot(Effect(focal.predictors = "response.diff", PR_succ.cvs.OR_model), 
     rug = FALSE, style="stacked",main="CVS effect on PR Success",
     xlab="CVS",
     ylab="Success (Probability)")


#P values and other add to table
p <- pnorm(abs(PR_Succ.model.table[, "t value"]), lower.tail = FALSE) * 2
PR_Succ.model.table <- cbind(PR_Succ.model.table, "p value" = p)
PR_Succ.model.table

#confidence intervals
ci <- confint(PR_succ.cvs.OR_model)
exp(ci)
#odds ratio
exp(coef(PR_succ.cvs.OR_model))
#For every unit increase in parent CVS, the odds of being more successful on the 
#PR task  is multiplied 1.42 times, holding constant all other variables.

#Parallel roles - Latency to success
PR_Lat.model_pcgm <- lm(Avg_Lat_PR_P3 ~ response.diff, data = PR_pcgm.data)
PR_Lat.model_pcgm
summary(PR_Lat.model_pcgm)

#Complementary Roles - Spatial Coordination Linear Regression
FT_SC.model_pcgm <- lm(Avg_SC_FT_P3 ~ response.diff, data = FT_pcgm.data)
FT_SC.model_pcgm
summary(FT_SC.model_pcgm)
confint(FT_SC.model_pcgm, level = .95)

#Complementary Roles - Spatial Coordination Ordinal Regression
library(MASS)
FT_pcgm.data$Avg_SC_FT_P3_cat <- factor(FT_pcgm.data$Avg_SC_FT_P3, ordered = TRUE)

FT_sc.cvs.OR_model <- polr(Avg_SC_FT_P3_cat ~ response.diff, data = FT_pcgm.data, Hess=TRUE)
summary(FT_sc.cvs.OR_model)
FT_SC.model.table <- coef(summary(FT_sc.cvs.OR_model))
FT_SC.model.table

#P values and other add to table
p <- pnorm(abs(FT_SC.model.table[, "t value"]), lower.tail = FALSE) * 2
FT_SC.model.table <- cbind(FT_SC.model.table, "p value" = p)
FT_SC.model.table

#confidence intervals
ci <- confint(FT_sc.cvs.OR_model)
exp(ci)
#odds ratio
exp(coef(FT_sc.cvs.OR_model))

plot(Effect(focal.predictors = "response.diff", FT_sc.cvs.OR_model), 
     rug = FALSE, style="stacked",main="CVS effect on CR Spatial Coordination",
     xlab="CVS",
     ylab="Probability of Spatial Coordination")

#Complementary Roles - Success Linear Regression
FT_Succ.model_pcgm <- lm(Avg_Succ_FT_P3 ~ response.diff, data = FT_pcgm.data)
FT_Succ.model_pcgm
summary(FT_Succ.model_pcgm)
confint(FT_Succ.model_pcgm, level = .95)
autoplot(FT_Succ.model_pcgm)

#Complementary Roles - Success Ordinal Regression
library(MASS)
FT_pcgm.data$Avg_Succ_FT_P3_cat <- factor(FT_pcgm.data$Avg_Succ_FT_P3, ordered = TRUE)

FT_succ.cvs.OR_model <- polr(Avg_Succ_FT_P3_cat ~ response.diff, data = FT_pcgm.data, Hess=TRUE)
summary(FT_succ.cvs.OR_model)
FT_Succ.model.table <- coef(summary(FT_succ.cvs.OR_model))
FT_Succ.model.table

#P values and other add to table
p <- pnorm(abs(FT_Succ.model.table[, "t value"]), lower.tail = FALSE) * 2
FT_Succ.model.table <- cbind(FT_Succ.model.table, "p value" = p)
FT_Succ.model.table

#confidence intervals
ci <- confint(FT_succ.cvs.OR_model, level = 0.95)
exp(ci)
#odds ratio
exp(coef(FT_succ.cvs.OR_model))


library(effects)
plot(Effect(focal.predictors = "response.diff", FT_succ.cvs.OR_model), 
     rug = FALSE, style="stacked",main="CVS effect on CR Success",
     xlab="CVS",
     ylab="Probability of Success")

#Complementary Roles - Latency Action 1 - Linear regression
FT_Lat1.model_pcgm <- lm(Avg_LAT1_FT_P3 ~ response.diff + Age_p3, data = FT_pcgm.data)
FT_Lat1.model_pcgm
summary(FT_Lat1.model_pcgm)
confint(FT_Lat1.model_pcgm, level = .95)
autoplot(FT_Lat1.model_pcgm)

#Complementary Roles - Latency Action 2 - Linear regression
FT_Lat2.model_pcgm <- lm(Avg_LAT2_FT_P3 ~ response.diff, data = FT_pcgm.data)
FT_Lat2.model_pcgm
summary(FT_Lat2.model_pcgm)
confint(FT_Lat2.model_pcgm, level = .95)
##Does parent personality predict CA at P3?

#Parallel Roles Task

#Parallel roles - Spatial Coordination #prosocial personality model
PR_SC.pp.model.pcgm <- lm(Avg_SC_PR_P3 ~ Agree + Open + Gender, data = PR_pcgm.data.p3.p5)
PR_SC.pp.model.pcgm
summary(PR_SC.pp.model.pcgm)
library(car)
vif(PR_SC.pp.model.pcgm)

#Check assumptions
mean(PR_SC.pp.model.pcgm$residuals)
acf(PR_SC.pp.model.pcgm$residuals)
autoplot(PR_SC.pp.model)
var(PR_pcgm.data.p3.p5$Avg_SC_PR_P3, na.rm = TRUE)  
vif(PR_SC.pp.model.pcgm)
library(gvlma)
gvlma::gvlma(PR_SC.pp.model.pcgm)

#ocean full personality model - Linear regression
PR_SC.ocean.model.pcgm <- lm(Avg_SC_PR_P3 ~ Agree + Open + Cons + Extra + Neur + Gender, data = PR_pcgm.data.p3.p5)
PR_SC.ocean.model.pcgm
summary(PR_SC.ocean.model.pcgm)
confint(PR_SC.ocean.model.pcgm, level = .95)
vif(PR_SC.ocean.model.pcgm)

#ocean full personality model - Ordinal regression
PR_pcgm.data.p3.p5$Avg_SC_PR_P3_cat <- factor(PR_pcgm.data.p3.p5$Avg_Succ_PR_P3, ordered = TRUE)

PR_sc.big5.OR_model <- polr(Avg_SC_PR_P3_cat ~ Agree + Open + Cons + Extra + Neur + Gender, data = PR_pcgm.data.p3.p5, Hess=TRUE)
summary(PR_sc.big5.OR_model)
PR_SC_5.model.table <- coef(summary(PR_sc.big5.OR_model))
PR_SC_5.model.table

#P values and other add to table
p <- pnorm(abs(PR_SC_5.model.table[, "t value"]), lower.tail = FALSE) * 2
PR_SC_5.model.table <- cbind(PR_SC_5.model.table, "p value" = p)
PR_SC_5.model.table

#confidence intervals
ci <- confint(PR_sc.big5.OR_model)
exp(ci)
#odds ratio
exp(coef(PR_sc.big5.OR_model))


##Parallel roles - Success 
#prosocial personality model
PR_Succ.pp.model.pcgm <- lm(Avg_Succ_PR_P3 ~ Agree + Open, data = PR_pcgm.data.p3.p5)
PR_Succ.pp.model.pcgm
summary(PR_Succ.pp.model.pcgm)
vif(PR_Succ.pp.model.pcgm)
library(ggfortify)
autoplot(PR_Succ.pp.model.pcgm)

citation(package = "ggfortify")

#full ocean personality model
PR_Succ.ocean.model.pcgm <- lm(Avg_Succ_PR_P3 ~ Agree + Open + Cons + Extra + Neur, data = PR_pcgm.data.p3.p5)
PR_Succ.ocean.model.pcgm
vif(PR_Succ.ocean.model.pcgm)
confint(PR_Succ.ocean.model.pcgm, level = .95)
summary(PR_Succ.ocean.model.pcgm)

#Full OCEAN personality model - Ordinal regression
PR_pcgm.data.p3.p5$Avg_Succ_PR_P3_cat <- factor(PR_pcgm.data.p3.p5$Avg_Succ_PR_P3, ordered = TRUE)

PR_succ.big5.OR_model <- polr(Avg_Succ_PR_P3_cat ~ Agree + Open + Cons + Extra + Neur, data = PR_pcgm.data.p3.p5, Hess=TRUE)
summary(PR_succ.big5.OR_model)
PR_Succ_5.model.table <- coef(summary(PR_succ.big5.OR_model))
PR_Succ_5.model.table

#P values and other add to table
p <- pnorm(abs(PR_Succ_5.model.table[, "t value"]), lower.tail = FALSE) * 2
PR_Succ_5.model.table <- cbind(PR_Succ_5.model.table, "p value" = p)
PR_Succ_5.model.table

#confidence intervals
ci <- confint(PR_succ.big5.OR_model)
exp(ci)
#odds ratio
exp(coef(PR_succ.big5.OR_model))

###Parallel roles -  Latency 
##prosocial personality model
PR_Lat.pp.model.pcgm <- lm(Avg_Lat_PR_P3 ~ Agree + Open, data = PR_pcgm.data.p3.p5)
PR_Lat.pp.model.pcgm
summary(PR_Lat.pp.model.pcgm)
autoplot(PR_Lat.pp.model)

#full ocean personality model
PR_Lat.ocean.model.pcgm <- lm(Avg_Lat_PR_P3 ~ Agree + Open + Cons + Extra + Neur, data = PR_pcgm.data.p3.p5)
PR_Lat.ocean.model.pcgm
confint(PR_Lat.ocean.model.pcgm, level = .95)
summary(PR_Lat.ocean.model.pcgm)

    #Complementary Roles - Froggy Task

#Complementary Roles - Spatial Coordination

FT_SC.ocean.model.pcgm <- lm(Avg_SC_FT_P3 ~ Agree + Open + Cons + Extra + Neur, data = FT_pcgm.data.p3.p5)
FT_SC.ocean.model.pcgm
confint(FT_SC.ocean.model.pcgm, level = .95)
summary(FT_SC.ocean.model.pcgm)
autoplot(FT_SC.ocean.model.pcgm)

#Complementary Roles - Spatial Coordination - Ordinal regression
FT_pcgm.data.p3.p5$Avg_SC_FT_P3_cat <- factor(FT_pcgm.data.p3.p5$Avg_SC_FT_P3, ordered = TRUE)

FT_sc.big5.OR_model <- polr(Avg_SC_FT_P3_cat ~ Agree + Open + Cons + Extra + Neur, data = FT_pcgm.data.p3.p5, Hess=TRUE)
summary(FT_sc.big5.OR_model)
FT_SC_5.model.table <- coef(summary(FT_sc.big5.OR_model))
FT_SC_5.model.table

#P values and other add to table
p <- pnorm(abs(FT_SC_5.model.table[, "t value"]), lower.tail = FALSE) * 2
FT_SC_5.model.table <- cbind(FT_SC_5.model.table, "p value" = p)
FT_SC_5.model.table

#confidence intervals
ci <- confint(FT_sc.big5.OR_model)
exp(ci)
#odds ratio
exp(coef(FT_sc.big5.OR_model))

#Complementary Roles - Success 

FT_Succ.ocean.model.pcgm <- lm(Avg_Succ_FT_P3 ~ Agree + Open + Cons + Extra + Neur, data = FT_pcgm.data.p3.p5)
FT_Succ.ocean.model.pcgm
summary(FT_Succ.ocean.model.pcgm)
confint(FT_Succ.ocean.model.pcgm, level = .95)
autoplot(FT_Succ.ocean.model.pcgm)

#Complementary Roles - Success - Ordinal regression

FT_pcgm.data.p3.p5$Avg_Succ_FT_P3_cat <- factor(FT_pcgm.data.p3.p5$Avg_Succ_FT_P3, ordered = TRUE)

FT_succ.big5.OR_model <- polr(Avg_Succ_FT_P3_cat ~ Agree + Open + Cons + Extra + Neur, data = FT_pcgm.data.p3.p5, Hess=TRUE)
summary(FT_succ.big5.OR_model)
FT_Succ_5.model.table <- coef(summary(FT_succ.big5.OR_model))
FT_Succ_5.model.table

#P values and other add to table
p <- pnorm(abs(FT_Succ_5.model.table[, "t value"]), lower.tail = FALSE) * 2
FT_Succ_5.model.table <- cbind(FT_Succ_5.model.table, "p value" = p)
FT_Succ_5.model.table

#confidence intervals
ci <- confint(FT_succ.big5.OR_model)
ci
#odds ratio
exp(coef(FT_succ.big5.OR_model))

#Complementary Roles - Latency action 1 

FT_LAT1.ocean.model.pcgm <- lm(Avg_LAT1_FT_P3 ~ Agree + Open + Cons + Extra + Neur + Age_p3, data = FT_pcgm.data.p3.p5)
FT_LAT1.ocean.model.pcgm
summary(FT_LAT1.ocean.model.pcgm)
confint(FT_LAT1.ocean.model.pcgm, level = .95)
autoplot(FT_LAT1.ocean.model.pcgm)
gvlma::gvlma(FT_LAT1.ocean.model.pcgm)

FT_LAT1.pp.model.pcgm <- lm(Avg_LAT1_FT_P3 ~ Agree + Open + Age_p3, data = FT_pcgm.data.p3.p5)
FT_LAT1.pp.model.pcgm
summary(FT_LAT1.pp.model.pcgm)

#Complementary Roles - latency action 2 >>no sig<<

FT_LAT2.ocean.model.pcgm <- lm(Avg_LAT2_FT_P3 ~ Agree + Open + Cons + Extra + Neur, data = FT_pcgm.data.p3.p5)
FT_LAT2.ocean.model.pcgm
vif(FT_LAT2.ocean.model.pcgm)
summary(FT_LAT2.ocean.model.pcgm)
confint(FT_LAT2.ocean.model.pcgm, level = .95)
autoplot(FT_LAT2.ocean.model.pcgm)

#Test for moderation effect SC ~ Agree * Social games P1 # sig.

#Centering Data
Open.c   <- with(PR_pcgm.data.p3.p5, c(scale(Open, center=TRUE, scale=FALSE))) #Centering IV; Openness
Agree.c   <- with(PR_pcgm.data.p3.p5, c(scale(Agree,  center=TRUE, scale=FALSE))) #Center IV; Agreeableness
Games.c   <- with(PR_pcgm.data.p3.p5, c(scale(Pi_social_games_freq_p1_fixed,  center=TRUE, scale=FALSE))) #Centering moderator; Social games

##Spatial coordination and Agree/Games +Gender
fitMod <- polr(Avg_SC_PR_P3_cat ~ Gender + Agree.c + Agree.c * Games.c, data = PR_pcgm.data.p3.p5, Hess=TRUE) #Model interacts IV & moderator
summary(fitMod)

fitMod.table <- coef(summary(fitMod))
fitMod.table

p <- pnorm(abs(fitMod.table[, "t value"]), lower.tail = FALSE) * 2
fitMod.table <- cbind(fitMod.table, "p value" = p)
fitMod.table

#confidence intervals
ci <- confint(fitMod)
exp(ci)
#odds ratio
exp(coef(fitMod))
ci

##Graph for LM##
gvlma(fitMod)

library(rockchalk)
ps  <- plotSlopes(fitMod, plotx="Agree.c", modx="Games.c", xlab = "Agreeableness", ylab = "Spatial Coordination", modxVals = "std.dev")

#Test for moderation effect Succ ~ Agree * Social games P1 # sig.

fitMod2 <- polr(Avg_Succ_PR_P3_cat ~ Agree.c + Agree.c * Games.c, data = PR_pcgm.data.p3.p5, Hess=TRUE) #Model interacts IV & moderator
summary(fitMod2)

fitMod2.table <- coef(summary(fitMod2))
fitMod2.table

p <- pnorm(abs(fitMod2.table[, "t value"]), lower.tail = FALSE) * 2
fitMod2.table <- cbind(fitMod2.table, "p value" = p)
fitMod2.table

#confidence intervals
ci <- confint(fitMod2)
exp(ci)
#odds ratio
exp(coef(fitMod2))

library(stargazer)
stargazer(fitMod2,type="text", title = "Agreeableness and Social Games")
confint(fitMod2, level = 0.95)


# STUDY 2 - Parent + JCE --------------------------------------------------

#Parallel roles - Personality
PR_JCE.ocean.model.pcgm <- lm(PR_JCE ~ Agree + Open + Cons + Extra + Neur, data = PR_pcgm.data.p3.p5)
PR_JCE.ocean.model.pcgm
summary(PR_JCE.ocean.model.pcgm)

PR_JCE.pp.model.pcgm <- lm(PR_JCE ~ Agree + Open, data = PR_pcgm.data.p3.p5)
PR_JCE.pp.model.pcgm
summary(PR_JCE.pp.model.pcgm)

#Parallel roles - Parent CVS

PR_JCE.cvs.model.pcgm <- lm(PR_JCE ~ response.diff, data = PR_pcgm.data)
PR_JCE.cvs.model.pcgm
summary(PR_JCE.cvs.model.pcgm)


#Complementary roles - Personality
FT_JCE.ocean.model.pcgm <- lm(CR_JCE ~ Agree + Open + Cons + Extra + Neur, data = FT_pcgm.data.p3.p5)
FT_JCE.ocean.model.pcgm
summary(FT_JCE.ocean.model.pcgm)

FT_JCE.pp.model.pcgm <- lm(CR_JCE ~ Agree + Open, data = FT_pcgm.data.p3.p5)
FT_JCE.pp.model.pcgm
summary(FT_JCE.pp.model.pcgm)

#Complementary roles - Parent CVS

FT_JCE.cvs.model.pcgm <- lm(CR_JCE ~ response.diff, data = FT_pcgm.data)
FT_JCE.cvs.model.pcgm
summary(FT_JCE.cvs.model.pcgm)

citation()

