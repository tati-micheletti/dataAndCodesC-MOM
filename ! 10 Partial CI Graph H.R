##################################################################
#                                                                #
# 10. Creating CI  Graph for  P2T,  TRENDS  and  HETERO         #
#                                                                #
##################################################################

#-----------------------------------------------------------#
#                          H                                #
#-----------------------------------------------------------#

# Load true values and list of Results that contain CI
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/N-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/B-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/Variation.Rdata")

Original.H <- matrix(c(0.99, 0.85, 0.2, NA),ncol=600,nrow=4) # Hetero
rownames(Original.H) <- c("phin","phib","p1","p2")
Original.H[4,] <- Var.tb[1,1:600]
iter <- 600

# For ZPNEc and ZPNEs (as these only have a mean pop per field trip)
ls.H.N.ZPNE <- list()
for (ite in 1:iter){
  ls.H.N.ZPNE[[ite]] <- c(mean(ls.H.N[[ite]][1:3]),mean(ls.H.N[[ite]][4:6]),mean(ls.H.N[[ite]][7:9]))}

#####################  ZPNEc ################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/H-Mark-list-ZPNEc.RData")

ZPNEc.H.N <- ZPNEc.H.phi1 <-  ZPNEc.H.p2 <- list()

for (ite in 1:iter){
  ZPNEc.H.N[[ite]] <- 
    ls.H.N.ZPNE[[ite]]>Mark.list.Hc[[ite]]$results$derived$`N Population Size`$lcl&
    ls.H.N.ZPNE[[ite]]<Mark.list.Hc[[ite]]$results$derived$`N Population Size`$ucl
  
  ZPNEc.H.phi1[[ite]] <- 
    Original.H[2]>Mark.list.Hc[[ite]]$results$real[6,"lcl"]&
    Original.H[2]<Mark.list.Hc[[ite]]$results$real[6,"ucl"]
  
  ZPNEc.H.p2[[ite]] <- 
    Original.H[4]>Mark.list.Hc[[ite]]$results$derived$`Pr(Captured 1 or more times)`$lcl&
    Original.H[4]<Mark.list.Hc[[ite]]$results$derived$`Pr(Captured 1 or more times)`$ucl}


#####################  ZPNEs ################################
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/H-Mark-list-ZPNEs.RData")

ZPNEs.H.N <- ZPNEs.H.phi1 <-  ZPNEs.H.p2 <- list()

for (ite in 1:iter){
  ZPNEs.H.N[[ite]] <- 
    ls.H.N.ZPNE[[ite]]>Mark2.list.Hc[[ite]]$results$derived$`N Population Size`$lcl&
    ls.H.N.ZPNE[[ite]]<Mark2.list.Hc[[ite]]$results$derived$`N Population Size`$ucl
  
  ZPNEs.H.phi1[[ite]] <- 
    Original.H[2]>Mark2.list.Hc[[ite]]$results$real[6,"lcl"]&
    Original.H[2]<Mark2.list.Hc[[ite]]$results$real[6,"ucl"]
  
  ZPNEs.H.p2[[ite]] <- 
    Original.H[4]>Mark2.list.Hc[[ite]]$results$derived$`Pr(Captured 1 or more times)`$lcl&
    Original.H[4]<Mark2.list.Hc[[ite]]$results$derived$`Pr(Captured 1 or more times)`$ucl}

#####################  ITERATIONS HIGH Rhat ################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2H.RData")

#####################  C-MOMd-1H ################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/1H_Backup.RData")

CMOMd1T.H.N <- CMOMd1T.H.B <- CMOMd1T.H.phi1 <- CMOMd1T.H.phi2 <-  CMOMd1T.H.p2 <- list()

for (ite in 1:iter){
  if (ite %in% it.1H$CMOMd) {next}
  CMOMd1T.H.N[[ite]] <- 
    ls.H.N[[ite]]>get(ls.CMOMd.1H[[ite]])[[10]][11:19,"2.5%"]&
    ls.H.N[[ite]]<get(ls.CMOMd.1H[[ite]])[[10]][11:19,"97.5%"]
  
  CMOMd1T.H.B[[ite]] <- 
    ls.H.B[[ite]]>get(ls.CMOMd.1H[[ite]])[[10]][20:21,"2.5%"]&
    ls.H.B[[ite]]<get(ls.CMOMd.1H[[ite]])[[10]][20:21,"97.5%"]
  
  CMOMd1T.H.phi1[[ite]] <- 
    Original.H[2]>get(ls.CMOMd.1H[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.H[2]<get(ls.CMOMd.1H[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMOMd1T.H.phi2[[ite]] <- 
    Original.H[1]>get(ls.CMOMd.1H[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.H[1]<get(ls.CMOMd.1H[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMOMd1T.H.p2[[ite]] <- 
    Original.H[4]>get(ls.CMOMd.1H[[ite]])[[10]][9,"2.5%"]&
    Original.H[4]<get(ls.CMOMd.1H[[ite]])[[10]][9,"97.5%"]}

#####################  C-MOMi-1H ################################

CMOMi1T.H.N <- CMOMi1T.H.B <- CMOMi1T.H.phi1 <- CMOMi1T.H.phi2 <-  CMOMi1T.H.p2 <- list()

for (ite in 1:iter){
  if (ite %in% it.1H$CMOMi) {next}
  CMOMi1T.H.N[[ite]] <- 
    ls.H.N[[ite]]>get(ls.CMOMi.1H[[ite]])[[10]][11:19,"2.5%"]&
    ls.H.N[[ite]]<get(ls.CMOMi.1H[[ite]])[[10]][11:19,"97.5%"]
  
  CMOMi1T.H.B[[ite]] <- 
    ls.H.B[[ite]]>get(ls.CMOMi.1H[[ite]])[[10]][20:21,"2.5%"]&
    ls.H.B[[ite]]<get(ls.CMOMi.1H[[ite]])[[10]][20:21,"97.5%"]
  
  CMOMi1T.H.phi1[[ite]] <- 
    Original.H[2]>get(ls.CMOMi.1H[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.H[2]<get(ls.CMOMi.1H[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMOMi1T.H.phi2[[ite]] <- 
    Original.H[1]>get(ls.CMOMi.1H[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.H[1]<get(ls.CMOMi.1H[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMOMi1T.H.p2[[ite]] <- 
    Original.H[4]>get(ls.CMOMi.1H[[ite]])[[10]][9,"2.5%"]&
    Original.H[4]<get(ls.CMOMi.1H[[ite]])[[10]][9,"97.5%"]}

#####################  CMR-1H ################################

CMR1T.H.N <- CMR1T.H.B <- CMR1T.H.phi1 <- CMR1T.H.phi2 <-  CMR1T.H.p2 <- list()

for (ite in 1:iter){
  if (ite %in% it.1H$CMR) {next}
  CMR1T.H.N[[ite]] <- 
    ls.H.N[[ite]]>get(ls.CMR.1H[[ite]])[[10]][11:19,"2.5%"]&
    ls.H.N[[ite]]<get(ls.CMR.1H[[ite]])[[10]][11:19,"97.5%"]
  
  CMR1T.H.B[[ite]] <- 
    ls.H.B[[ite]]>get(ls.CMR.1H[[ite]])[[10]][20:21,"2.5%"]&
    ls.H.B[[ite]]<get(ls.CMR.1H[[ite]])[[10]][20:21,"97.5%"]
  
  CMR1T.H.phi1[[ite]] <- 
    Original.H[2]>get(ls.CMR.1H[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.H[2]<get(ls.CMR.1H[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMR1T.H.phi2[[ite]] <- 
    Original.H[1]>get(ls.CMR.1H[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.H[1]<get(ls.CMR.1H[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMR1T.H.p2[[ite]] <- 
    Original.H[4]>get(ls.CMR.1H[[ite]])[[10]][9,"2.5%"]&
    Original.H[4]<get(ls.CMR.1H[[ite]])[[10]][9,"97.5%"]}

#***********************************

#    G r a p h      T A B L E      *

#***********************************

ls.CI.1H <- list()

for(PERC in 1:6){
  
  if (PERC == 1) {ini <- 1 
  last <- 100
  FACT <- "10%"}
  if (PERC == 2) {ini <- 101 
  last <- 200
  FACT <- "20%"}
  if (PERC == 3) {ini <- 201 
  last <- 300
  FACT <- "30%"}
  if (PERC == 4) {ini <- 301 
  last <- 400
  FACT <- "40%"}
  if (PERC == 5) {ini <- 401 
  last <- 500
  FACT <- "50%"}
  if (PERC == 6) {ini <- 501 
  last <- 600
  FACT <- "60%"}
  
  CI.1H <- data.frame(
    Parameters=factor(rep(c(
      "Number of Individuals",
      "Recruitment",
      "Survival Prim. Occ.",
      "Survival Second. Occ.",
      "Observation probability",
      "Capture probability"), times=5)),
    Models=factor(rep(c(
      "C-MOMd-1H",
      "C-MOMi-1H",
      "CMR-1H",
      "ZPNEc",
      "ZPNEs"), each=6)),
    Factor=factor(rep(c(
      FACT), times=30)),
    Percentage=c(
      sum(unlist(CMOMd1T.H.N[ini:last]))/length(unlist(CMOMd1T.H.N[ini:last])), 
      sum(unlist(CMOMd1T.H.B[ini:last]))/length(unlist(CMOMd1T.H.B[ini:last])), 
      sum(unlist(CMOMd1T.H.phi1[ini:last]))/length(unlist(CMOMd1T.H.phi1[ini:last])), 
      sum(unlist(CMOMd1T.H.phi2[ini:last]))/length(unlist(CMOMd1T.H.phi2[ini:last])),
      sum(unlist(CMOMd1T.H.p2[ini:last]))/length(unlist(CMOMd1T.H.p2[ini:last])),
      NA,
      sum(unlist(CMOMi1T.H.N[ini:last]))/length(unlist(CMOMi1T.H.N[ini:last])), 
      sum(unlist(CMOMi1T.H.B[ini:last]))/length(unlist(CMOMi1T.H.B[ini:last])), 
      sum(unlist(CMOMi1T.H.phi1[ini:last]))/length(unlist(CMOMi1T.H.phi1[ini:last])), 
      sum(unlist(CMOMi1T.H.phi2[ini:last]))/length(unlist(CMOMi1T.H.phi2[ini:last])),
      sum(unlist(CMOMi1T.H.p2[ini:last]))/length(unlist(CMOMi1T.H.p2[ini:last])),
      NA,
      sum(unlist(CMR1T.H.N[ini:last]))/length(unlist(CMR1T.H.N[ini:last])), 
      sum(unlist(CMR1T.H.B[ini:last]))/length(unlist(CMR1T.H.B[ini:last])), 
      sum(unlist(CMR1T.H.phi1[ini:last]))/length(unlist(CMR1T.H.phi1[ini:last])), 
      sum(unlist(CMR1T.H.phi2[ini:last]))/length(unlist(CMR1T.H.phi2[ini:last])),
      sum(unlist(CMR1T.H.p2[ini:last]))/length(unlist(CMR1T.H.p2[ini:last])),
      NA,
      sum(unlist(ZPNEc.H.N[ini:last]))/length(unlist(ZPNEc.H.N[ini:last])), 
      NA, 
      sum(unlist(ZPNEc.H.phi1[ini:last]))/length(unlist(ZPNEc.H.phi1[ini:last])), 
      NA,
      sum(unlist(ZPNEc.H.p2[ini:last]))/length(unlist(ZPNEc.H.p2[ini:last])),
      NA,
      sum(unlist(ZPNEs.H.N[ini:last]))/length(unlist(ZPNEs.H.N[ini:last])), 
      NA, 
      sum(unlist(ZPNEs.H.phi1[ini:last]))/length(unlist(ZPNEs.H.phi1[ini:last])), 
      NA,
      sum(unlist(ZPNEs.H.p2[ini:last]))/length(unlist(ZPNEs.H.p2[ini:last])),
      NA))
  
  ls.CI.1H[[PERC]] <- assign(paste("CI.1H", PERC, sep=""), CI.1H)
}

CI.1H <- rbind(ls.CI.1H[[1]], ls.CI.1H[[2]], ls.CI.1H[[3]], ls.CI.1H[[4]], ls.CI.1H[[5]], ls.CI.1H[[6]])

save(CI.1H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-1H.RData")

rm(list = ls())




#################################################################
#####################  C-MOMd-2H ################################
#################################################################

# Load true values and list of Results that contain CI
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/N-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/B-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/Variation.Rdata")

Original.H <- matrix(c(0.99, 0.85, 0.2, NA),ncol=600,nrow=4) # Hetero
rownames(Original.H) <- c("phin","phib","p1","p2")
Original.H[4,] <- Var.tb[1,1:600]
iter <- 600


load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/2H_Backup.RData")

CMOMd2T.H.N <- CMOMd2T.H.B <- CMOMd2T.H.phi1 <- CMOMd2T.H.phi2 <-  CMOMd2T.H.p2 <-  CMOMd2T.H.p1 <- list()

for (ite in 1:iter){
  if (ite %in% it.2H$CMOMd) {next}
  CMOMd2T.H.N[[ite]] <- 
    ls.H.N[[ite]]>get(ls.CMOMd.2H[[ite]])[[10]][12:20,"2.5%"]&
    ls.H.N[[ite]]<get(ls.CMOMd.2H[[ite]])[[10]][12:20,"97.5%"]
  
  CMOMd2T.H.B[[ite]] <- 
    ls.H.B[[ite]]>get(ls.CMOMd.2H[[ite]])[[10]][21:22,"2.5%"]&
    ls.H.B[[ite]]<get(ls.CMOMd.2H[[ite]])[[10]][21:22,"97.5%"]
  
  CMOMd2T.H.phi1[[ite]] <- 
    Original.H[2]>get(ls.CMOMd.2H[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.H[2]<get(ls.CMOMd.2H[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMOMd2T.H.phi2[[ite]] <- 
    Original.H[1]>get(ls.CMOMd.2H[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.H[1]<get(ls.CMOMd.2H[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMOMd2T.H.p2[[ite]] <- 
    Original.H[4]>get(ls.CMOMd.2H[[ite]])[[10]][10,"2.5%"]&
    Original.H[4]<get(ls.CMOMd.2H[[ite]])[[10]][10,"97.5%"]
  
  CMOMd2T.H.p1[[ite]] <- 
    Original.H[3]>get(ls.CMOMd.2H[[ite]])[[10]][9,"2.5%"]&
    Original.H[3]<get(ls.CMOMd.2H[[ite]])[[10]][9,"97.5%"]}

#####################  C-MOMi-2H ################################

CMOMi2T.H.N <- CMOMi2T.H.B <- CMOMi2T.H.phi1 <- CMOMi2T.H.phi2 <-  CMOMi2T.H.p2 <-  CMOMi2T.H.p1 <-  list()

for (ite in 1:iter){
  if (ite %in% it.2H$CMOMi) {next}
  CMOMi2T.H.N[[ite]] <- 
    ls.H.N[[ite]]>get(ls.CMOMi.2H[[ite]])[[10]][12:20,"2.5%"]&
    ls.H.N[[ite]]<get(ls.CMOMi.2H[[ite]])[[10]][12:20,"97.5%"]
  
  CMOMi2T.H.B[[ite]] <- 
    ls.H.B[[ite]]>get(ls.CMOMi.2H[[ite]])[[10]][21:22,"2.5%"]&
    ls.H.B[[ite]]<get(ls.CMOMi.2H[[ite]])[[10]][21:22,"97.5%"]
  
  CMOMi2T.H.phi1[[ite]] <- 
    Original.H[2]>get(ls.CMOMi.2H[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.H[2]<get(ls.CMOMi.2H[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMOMi2T.H.phi2[[ite]] <- 
    Original.H[1]>get(ls.CMOMi.2H[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.H[1]<get(ls.CMOMi.2H[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMOMi2T.H.p2[[ite]] <- 
    Original.H[4]>get(ls.CMOMi.2H[[ite]])[[10]][10,"2.5%"]&
    Original.H[4]<get(ls.CMOMi.2H[[ite]])[[10]][10,"97.5%"]
  
  CMOMi2T.H.p1[[ite]] <- 
    Original.H[3]>get(ls.CMOMi.2H[[ite]])[[10]][9,"2.5%"]&
    Original.H[3]<get(ls.CMOMi.2H[[ite]])[[10]][9,"97.5%"]}

#####################  CMR-2H ################################

CMR2T.H.N <- CMR2T.H.B <- CMR2T.H.phi1 <- CMR2T.H.phi2 <-  CMR2T.H.p2 <- CMR2T.H.p1 <- list()

for (ite in 1:iter){
  if (ite %in% it.2H$CMR) {next}
  CMR2T.H.N[[ite]] <- 
    ls.H.N[[ite]]>get(ls.CMR.2H[[ite]])[[10]][12:20,"2.5%"]&
    ls.H.N[[ite]]<get(ls.CMR.2H[[ite]])[[10]][12:20,"97.5%"]
  
  CMR2T.H.B[[ite]] <- 
    ls.H.B[[ite]]>get(ls.CMR.2H[[ite]])[[10]][21:22,"2.5%"]&
    ls.H.B[[ite]]<get(ls.CMR.2H[[ite]])[[10]][21:22,"97.5%"]
  
  CMR2T.H.phi1[[ite]] <- 
    Original.H[2]>get(ls.CMR.2H[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.H[2]<get(ls.CMR.2H[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMR2T.H.phi2[[ite]] <- 
    Original.H[1]>get(ls.CMR.2H[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.H[1]<get(ls.CMR.2H[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMR2T.H.p2[[ite]] <- 
    Original.H[4]>get(ls.CMR.2H[[ite]])[[10]][10,"2.5%"]&
    Original.H[4]<get(ls.CMR.2H[[ite]])[[10]][10,"97.5%"]
  
  CMR2T.H.p1[[ite]] <- 
    Original.H[3]>get(ls.CMR.2H[[ite]])[[10]][9,"2.5%"]&
    Original.H[3]<get(ls.CMR.2H[[ite]])[[10]][9,"97.5%"]}

#***********************************

#    G r a p h      T A B L E      *

#***********************************

ls.CI.2H <- list()

for(PERC in 1:6){
  
  if (PERC == 1) {ini <- 1 
  last <- 100
  FACT <- "10%"}
  if (PERC == 2) {ini <- 101 
  last <- 200
  FACT <- "20%"}
  if (PERC == 3) {ini <- 201 
  last <- 300
  FACT <- "30%"}
  if (PERC == 4) {ini <- 301 
  last <- 400
  FACT <- "40%"}
  if (PERC == 5) {ini <- 401 
  last <- 500
  FACT <- "50%"}
  if (PERC == 6) {ini <- 501 
  last <- 600
  FACT <- "60%"}
  
  CI.2H <- data.frame(
    Parameters=factor(rep(c(
      "Number of Individuals",
      "Recruitment",
      "Survival Prim. Occ.",
      "Survival Second. Occ.",
      "Observation probability",
      "Capture probability"), times=3)),
    Models=factor(rep(c(
      "C-MOMd-2H",
      "C-MOMi-2H",
      "CMR-2H"), each=6)),
    Factor=factor(rep(c(
      FACT), times=18)),
    Percentage=c(
      sum(unlist(CMOMd2T.H.N[ini:last]))/length(unlist(CMOMd2T.H.N[ini:last])), 
      sum(unlist(CMOMd2T.H.B[ini:last]))/length(unlist(CMOMd2T.H.B[ini:last])), 
      sum(unlist(CMOMd2T.H.phi1[ini:last]))/length(unlist(CMOMd2T.H.phi1[ini:last])), 
      sum(unlist(CMOMd2T.H.phi2[ini:last]))/length(unlist(CMOMd2T.H.phi2[ini:last])),
      sum(unlist(CMOMd2T.H.p2[ini:last]))/length(unlist(CMOMd2T.H.p2[ini:last])),
      sum(unlist(CMOMd2T.H.p1[ini:last]))/length(unlist(CMOMd2T.H.p1[ini:last])),
      
      sum(unlist(CMOMi2T.H.N[ini:last]))/length(unlist(CMOMi2T.H.N[ini:last])), 
      sum(unlist(CMOMi2T.H.B[ini:last]))/length(unlist(CMOMi2T.H.B[ini:last])), 
      sum(unlist(CMOMi2T.H.phi1[ini:last]))/length(unlist(CMOMi2T.H.phi1[ini:last])), 
      sum(unlist(CMOMi2T.H.phi2[ini:last]))/length(unlist(CMOMi2T.H.phi2[ini:last])),
      sum(unlist(CMOMi2T.H.p2[ini:last]))/length(unlist(CMOMi2T.H.p2[ini:last])),
      sum(unlist(CMOMi2T.H.p1[ini:last]))/length(unlist(CMOMi2T.H.p1[ini:last])),
      
      sum(unlist(CMR2T.H.N[ini:last]))/length(unlist(CMR2T.H.N[ini:last])), 
      sum(unlist(CMR2T.H.B[ini:last]))/length(unlist(CMR2T.H.B[ini:last])), 
      sum(unlist(CMR2T.H.phi1[ini:last]))/length(unlist(CMR2T.H.phi1[ini:last])), 
      sum(unlist(CMR2T.H.phi2[ini:last]))/length(unlist(CMR2T.H.phi2[ini:last])),
      sum(unlist(CMR2T.H.p2[ini:last]))/length(unlist(CMR2T.H.p2[ini:last])),
      sum(unlist(CMR2T.H.p1[ini:last]))/length(unlist(CMR2T.H.p1[ini:last]))))
  
  ls.CI.2H[[PERC]] <- assign(paste("CI.2H", PERC, sep=""), CI.2H)
}

CI.2H <- rbind(ls.CI.2H[[1]], ls.CI.2H[[2]], ls.CI.2H[[3]], ls.CI.2H[[4]], ls.CI.2H[[5]], ls.CI.2H[[6]])

save(CI.2H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-2H.RData")

rm(list = ls())

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-1H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-2H.RData")

CI.H <- rbind(CI.1H, CI.2H)

save(CI.H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-H.RData")