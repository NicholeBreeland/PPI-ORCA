all.data = read.csv("P1_PPI_Ability_Anticipation_Workingset.csv", stringsAsFactors = FALSE)


# Data subsets by P3 Cooperative ability task -----------------------------

#Parallel roles study in data subset

PR.data <- all.data[which(all.data$InOut_PR_P3 == 1 & all.data$InOut_P3 == 1),]

#Froggy task study in data subset

FT.data <- all.data[which(all.data$InOut_FT_P3 == 1 & all.data$InOut_P3 == 1),]

#### A question - do we need study in out for P1 as well in this subset or is P3 and task in out sufficient??##




# Visulizing the data -----------------------------------------------------

#PR task

with(PR.data, hist(Op_coop_Alt_Avg_p1, breaks = 20, 
                         xlab = "Altrustic scores",
                         main = "Freq of altruistic responses"))

with(PR.data, hist(Op_coop_Ego_Avg_p1, breaks = 20, 
                   xlab = "Altrustic scores",
                   main = "Freq of egotistical responses"))

mean(PR.data$Op_coop_Alt_Avg_p1, na.rm=TRUE)
mean(PR.data$Op_coop_Ego_Avg_p1, na.rm=TRUE)

with(PR.data, plot(Op_coop_Alt_Avg_p1, Avg_SC_PR_P3))
with(PR.data, plot(Op_coop_Alt_Avg_p1, Avg_Succ_PR_P3))
with(PR.data, plot(Op_coop_Alt_Avg_p1, Avg_Lat_PR_P3))

#Froggy task

with(FT.data, plot(Op_coop_Alt_Avg_p1, Avg_SC_FT_P3))
with(FT.data, plot(Op_coop_Alt_Avg_p1, Avg_Succ_FT_P3))
with(FT.data, plot(Op_coop_Alt_Avg_p1, Avg_LAT1_FT_P3))
with(FT.data, plot(Op_coop_Alt_Avg_p1, Avg_LAT2_FT_P3))
  