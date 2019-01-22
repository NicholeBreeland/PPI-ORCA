all.data = read.csv("P1_PPI_Ability_Anticipation_Workingset.csv", stringsAsFactors = FALSE)


options(scipen = 999)

all.data[all.data == 999] <- NA
all.data[all.data == 777] <- NA


# Data subsets by P3 Cooperative ability task -----------------------------

#Difference score for altruistic/egotistic responses

all.data$response.diff = with(all.data, Op_coop_Alt_Avg_p1 - Op_coop_Ego_Avg_p1)

#Response difference score category 

all.data$response.cat[all.data$response.diff == -1] <- "ego"
all.data$response.cat[all.data$response.diff > -1 & all.data$response.diff < 0] <- "mod ego"
all.data$response.cat[all.data$response.diff == 0 ] <- "moderate"
all.data$response.cat[all.data$response.diff > 0 & all.data$response.diff < 1] <- "mod alt"
all.data$response.cat[all.data$response.diff == 1] <- "alt"

#Gender category assign
all.data$Gender[all.data$Gender == 1] <- "Male"
all.data$Gender[all.data$Gender == 2] <- "Female"

#so, 1 = 100% altruistic and -1 = 100% egotistic

#Main data set for ORCA all study in group

P3.data <-all.data[which(all.data$Study_inout == 1),]

#Indep study in data subset

IT.data <- all.data[which(all.data$InOut_IT_P3 == 1 & all.data$Study_inout == 1),]

#Parallel roles study in data subset

PR.data <- all.data[which(all.data$InOut_PR_P3 == 1 & all.data$Study_inout == 1),]

#Froggy task study in data subset

FT.data <- all.data[which(all.data$InOut_FT_P3 == 1 & all.data$Study_inout == 1),]


# Visulizing the data -----------------------------------------------------

#response diff

with(P3.data, hist(response.diff,breaks = 5,
                   xlab = "ego/alt score",
                   main = "hist parent response"))

#IT task - ability data

with(IT.data, hist(Avg_SC_IT_P3,breaks = 5,
                   xlab = "spatial coordination scores",
                   main = "indep spatial coordination"))

with(IT.data, hist(log10(Avg_SC_IT_P3),breaks = 5,
                   xlab = "log spatial coordination scores",
                   main = "indep spatial coordination"))

with(IT.data, hist(Avg_Succ_IT_P3,breaks = 5,
                   xlab = "success scores",
                   main = "indep success"))

with(IT.data, hist(Avg_Lat_IT_P3,breaks = 5,
                   xlab = "latency scores",
                   main = "indep latency"))

with(IT.data, plot(response.diff, log10(Avg_SC_IT_P3)))
with(IT.data, plot(response.diff, Avg_SC_IT_P3))
with(IT.data, plot(response.diff, log10(Avg_Succ_IT_P3)))
with(IT.data, plot(response.diff, log10(Avg_Lat_IT_P3)))

#PR task - Ability data

with(PR.data, hist(Avg_SC_PR_P3,breaks = 5,
                   xlab = "spatial coordination scores",
                   main = "parallel roles spatial coordination"))

with(PR.data, hist(log10(Avg_SC_PR_P3),breaks = 5,
                   xlab = "log spatial coordination scores",
                   main = "parallel roles spatial coordination"))

with(PR.data, hist(Avg_Succ_PR_P3,breaks = 5,
                   xlab = "success scores",
                   main = "parallel roles success"))

with(PR.data, hist(Avg_Lat_PR_P3,breaks = 5,
                   xlab = "latency scores",
                   main = "parallel roles latency"))



mean(PR.data$Op_coop_Alt_Avg_p1, na.rm=TRUE)
mean(PR.data$Op_coop_Ego_Avg_p1, na.rm=TRUE)

with(PR.data, plot(response.diff, Avg_SC_PR_P3))
with(PR.data, plot(log.response.diff, Avg_SC_PR_P3))
with(PR.data, plot(log.response.diff, log10(Avg_SC_PR_P3)))
     
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

#########visulization with categories


with(FT.data, boxplot(Avg_SC_FT_P3 ~ response.cat)) 
with(FT.data, boxplot(Avg_Succ_FT_P3 ~ response.cat)) 
with(FT.data, boxplot(Avg_LAT1_FT_P3 ~ response.cat)) 
with(FT.data, boxplot(Avg_LAT2_FT_P3 ~ response.cat)) 



# Preanalyses -------------------------------------------------------------

#visualize data for PPI responses, groups female vs male children

with(P3.data, boxplot(response.diff ~ Gender)) 

#data for parent responses are non normally distributed

shapiro.test(P3.data$response.diff)

#check for difference bt responses by child gender

wilcox.test(response.diff ~ Gender, data = P3.data)

#visualize data for PPI reasoning and time spent playing social games

trend_line = lm((response.diff) ~ (Pi_social_games_freq_p1_fixed_777), data = all.data)

with(all.data, plot(Pi_social_games_freq_p1_fixed_777, response.diff,
                    main = "social games vs response diff",
                    xlab = "number of social games played per wk",
                    ylab = "response difference score",
                    xlim = c(0, 40), ylim = c(-1,1)))
abline(trend_line)


#spearman correlation for freq social games and response dif#

resp_sg.corr <- cor.test(x = P3.data$Pi_social_games_freq_p1_fixed_777, y = P3.data$response.diff, method = 'spearman')
resp_sg.corr

#ability differences by gender IT

wilcox.test(Avg_SC_IT_P3 ~ Gender, data = IT.data)
wilcox.test(Avg_Succ_IT_P3 ~ Gender, data = IT.data)
wilcox.test(Avg_Lat_IT_P3 ~ Gender, data = IT.data)

#ability differences by gender PR

wilcox.test(Avg_SC_PR_P3 ~ Gender, data = PR.data)
wilcox.test(Avg_Succ_PR_P3 ~ Gender, data = PR.data)
wilcox.test(Avg_Lat_PR_P3 ~ Gender, data = PR.data)

#ability differences by gender FT

wilcox.test(Avg_SC_FT_P3 ~ Gender, data = FT.data)
wilcox.test(Avg_Succ_FT_P3 ~ Gender, data = FT.data)
wilcox.test(Avg_LAT1_FT_P3 ~ Gender, data = FT.data)
wilcox.test(Avg_LAT2_FT_P3 ~ Gender, data = FT.data)

#social games & CA IT
sg.sc.it.corr <- cor.test(x = IT.data$Pi_social_games_freq_p1_fixed_777, y = IT.data$Avg_SC_IT_P3, method = 'spearman')
sg.sc.it.corr

sg.succ.it.corr <- cor.test(x = IT.data$Pi_social_games_freq_p1_fixed_777, y = IT.data$Avg_Succ_IT_P3, method = 'spearman')
sg.succ.it.corr

sg.lat.it.corr <- cor.test(x = IT.data$Pi_social_games_freq_p1_fixed_777, y = IT.data$Avg_Lat_IT_P3, method = 'spearman')
sg.lat.it.corr

#social games & CA PR

sg.sc.pr.corr <- cor.test(x = PR.data$Pi_social_games_freq_p1_fixed_777, y = PR.data$Avg_SC_PR_P3, method = 'spearman')
sg.sc.pr.corr

sg.succ.pr.corr <- cor.test(x = PR.data$Pi_social_games_freq_p1_fixed_777, y = PR.data$Avg_Succ_PR_P3, method = 'spearman')
sg.succ.pr.corr

sg.lat.pr.corr <- cor.test(x = PR.data$Pi_social_games_freq_p1_fixed_777, y = PR.data$Avg_Lat_PR_P3, method = 'spearman')
sg.lat.pr.corr

#social games & CA FT

sg.sc.ft.corr <- cor.test(x = FT.data$Pi_social_games_freq_p1_fixed_777, y = FT.data$Avg_SC_FT_P3, method = 'spearman')
sg.sc.ft.corr

sg.succ.ft.corr <- cor.test(x = FT.data$Pi_social_games_freq_p1_fixed_777, y = FT.data$Avg_Succ_FT_P3, method = 'spearman')
sg.succ.ft.corr

sg.lat1.ft.corr <- cor.test(x = FT.data$Pi_social_games_freq_p1_fixed_777, y = FT.data$Avg_LAT1_FT_P3, method = 'spearman')
sg.lat1.ft.corr

sg.lat2.ft.corr <- cor.test(x = FT.data$Pi_social_games_freq_p1_fixed_777, y = FT.data$Avg_LAT2_FT_P3, method = 'spearman')
sg.lat2.ft.corr


# Simple linear regression models -----------------------------------------

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

#IT Lat Log transform DV

log.IT_Lat.model <- lm(log10(Avg_Lat_IT_P3) ~ response.diff + Gender, data = IT.data)
log.IT_Lat.model
summary(log.IT_Lat.model)


#PR task SC

PR_SC.model <- lm(Avg_SC_PR_P3 ~ response.diff + Gender, data = PR.data)
PR_SC.model
summary(PR_SC.model)

log.PR_SC.model <- lm(log10(Avg_SC_PR_P3) ~ response.diff + Gender, data = PR.data)
log.PR_SC.model
summary(log.PR_SC.model)

#PR task Succ

PR_Succ.model <- lm(Avg_Succ_PR_P3 ~ response.diff, data = PR.data)
PR_Succ.model
summary(PR_Succ.model)

#PR task Lat

PR_Lat.model <- lm(Avg_Lat_PR_P3 ~ response.diff, data = PR.data)
PR_Lat.model
summary(PR_Lat.model)

log.PR_Lat.model <- lm(log10(Avg_Lat_PR_P3) ~ response.diff, data = PR.data)
log.PR_Lat.model
summary(log.PR_Lat.model)

#FT task SC

FT_SC.model <- lm(Avg_SC_FT_P3 ~ response.diff, data = FT.data)
FT_SC.model
summary(FT_SC.model)

#FT task Succ

FT_Succ.model <- lm(Avg_Succ_FT_P3 ~ response.diff, data = FT.data)
FT_Succ.model
summary(FT_Succ.model)

#FT task Lat1

FT_Lat1.model <- lm(Avg_LAT1_FT_P3 ~ response.diff, data = FT.data)
FT_Lat1.model
summary(FT_Lat1.model)

#FT task Lat2

FT_Lat2.model <- lm(Avg_LAT2_FT_P3 ~ response.diff, data = FT.data)
FT_Lat2.model
summary(FT_Lat2.model)


# Sig regression model check ---------------------------------------------

library(ggfortify)

autoplot(FT_Lat2.model)
autoplot(log.PR_Lat.model)


# Categorical analyses ----------------------------------------------------

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

FT.Lat1.aov <- aov(Avg_LAT1_FT_P3 ~ response.cat, data = FT.data)
summary(FT.Lat1.aov)

FT.Lat2.aov <- aov(Avg_LAT2_FT_P3 ~ response.cat, data = FT.data)
summary(FT.Lat2.aov)

#nonparametric FT

FT.data$response.cat <- as.factor(FT.data$response.cat)


kruskal.test(Avg_SC_FT_P3 ~ response.cat, data = FT.data)
kruskal.test(Avg_Succ_FT_P3 ~ response.cat, data = FT.data)
kruskal.test(Avg_LAT1_FT_P3 ~ response.cat, data = FT.data)
kruskal.test(Avg_LAT2_FT_P3 ~ response.cat, data = FT.data)
