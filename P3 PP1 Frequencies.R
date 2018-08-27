##Study in subset with broad category codes##
P3_studyin_BC <- subset(Workingdataset_P3_PPI, P3_invsout == 1, select = c("Op_1_coop_BC_FINAL"))

##Assigning value labels##
P3_studyin_BC$Op_1_coop_BC_FINAL <- factor(P3_studyin_BC$Op_1_coop_BC_FINAL, levels = c(5,6,7,8,9,10,11,999), labels = c("moral", "conventional", "instrumental", "developmental", "survival", "prosocial", "other", "missing"))

table(P3_studyin_BC)
