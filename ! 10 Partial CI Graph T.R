##################################################################
#                                                                #
# 10. Creating CI  Graph for  P2T,  TRENDS  and  HETERO         #
#                                                                #
##################################################################

#-----------------------------------------------------------#
#                          T                                #
#-----------------------------------------------------------#

# Load true values and list of Results that contain CI
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/N-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/B-each.RData")
Original.T <- matrix(c(rep(c(0.85, 0.7, 0.2, 0.6),times=100), rep(c(0.99, 0.85, 0.2, 0.6),times=100)),ncol=200,nrow=4)
rownames(Original.T) <- c("phin","phib","p1","p2")
iter <- 200

# For ZPNEc and ZPNEs (as these only have a mean pop per field trip)
ls.T.N.ZPNE <- list()
for (ite in 1:iter){
  ls.T.N.ZPNE[[ite]] <- c(mean(ls.T.N[[ite]][1:3]),mean(ls.T.N[[ite]][4:6]),mean(ls.T.N[[ite]][7:9]))}

#####################  ZPNEc ################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/T-Mark-list-ZPNEc.RData")

ZPNEc.T.N <- ZPNEc.T.phi1 <-  ZPNEc.T.p2 <- list()

for (ite in 1:iter){
  ZPNEc.T.N[[ite]] <- 
    ls.T.N.ZPNE[[ite]]>Mark.list.Tc[[ite]]$results$derived$`N Population Size`$lcl&
    ls.T.N.ZPNE[[ite]]<Mark.list.Tc[[ite]]$results$derived$`N Population Size`$ucl
  
  ZPNEc.T.phi1[[ite]] <- 
    Original.T[2]>Mark.list.Tc[[ite]]$results$real[6,"lcl"]&
    Original.T[2]<Mark.list.Tc[[ite]]$results$real[6,"ucl"]
  
  ZPNEc.T.p2[[ite]] <- 
    Original.T[4]>Mark.list.Tc[[ite]]$results$derived$`Pr(Captured 1 or more times)`$lcl&
    Original.T[4]<Mark.list.Tc[[ite]]$results$derived$`Pr(Captured 1 or more times)`$ucl}


#####################  ZPNEs ################################
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/T-Mark-list-ZPNEs.RData")

ZPNEs.T.N <- ZPNEs.T.phi1 <-  ZPNEs.T.p2 <- list()

for (ite in 1:iter){
  ZPNEs.T.N[[ite]] <- 
    ls.T.N.ZPNE[[ite]]>Mark2.list.Tc[[ite]]$results$derived$`N Population Size`$lcl&
    ls.T.N.ZPNE[[ite]]<Mark2.list.Tc[[ite]]$results$derived$`N Population Size`$ucl
  
  ZPNEs.T.phi1[[ite]] <- 
    Original.T[2]>Mark2.list.Tc[[ite]]$results$real[6,"lcl"]&
    Original.T[2]<Mark2.list.Tc[[ite]]$results$real[6,"ucl"]
  
  ZPNEs.T.p2[[ite]] <- 
    Original.T[4]>Mark2.list.Tc[[ite]]$results$derived$`Pr(Captured 1 or more times)`$lcl&
    Original.T[4]<Mark2.list.Tc[[ite]]$results$derived$`Pr(Captured 1 or more times)`$ucl}

#####################  ITERATIONS HIGH Rhat ################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1T.RData")

#####################  C-MOMd-1T ################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/1T_Backup.RData")

CMOMd1T.T.N <- CMOMd1T.T.B <- CMOMd1T.T.phi1 <- CMOMd1T.T.phi2 <-  CMOMd1T.T.p2 <- list()

for (ite in 1:iter){
  if (ite %in% it.1T$CMOMd) {next}
  CMOMd1T.T.N[[ite]] <- 
    ls.T.N[[ite]]>get(ls.CMOMd.1T[[ite]])[[10]][11:19,"2.5%"]&
    ls.T.N[[ite]]<get(ls.CMOMd.1T[[ite]])[[10]][11:19,"97.5%"]
  
  CMOMd1T.T.B[[ite]] <- 
    ls.T.B[[ite]]>get(ls.CMOMd.1T[[ite]])[[10]][20:21,"2.5%"]&
    ls.T.B[[ite]]<get(ls.CMOMd.1T[[ite]])[[10]][20:21,"97.5%"]
  
  CMOMd1T.T.phi1[[ite]] <- 
    Original.T[2]>get(ls.CMOMd.1T[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.T[2]<get(ls.CMOMd.1T[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMOMd1T.T.phi2[[ite]] <- 
    Original.T[1]>get(ls.CMOMd.1T[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.T[1]<get(ls.CMOMd.1T[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMOMd1T.T.p2[[ite]] <- 
    Original.T[4]>get(ls.CMOMd.1T[[ite]])[[10]][9,"2.5%"]&
    Original.T[4]<get(ls.CMOMd.1T[[ite]])[[10]][9,"97.5%"]}

#####################  C-MOMi-1T ################################

CMOMi1T.T.N <- CMOMi1T.T.B <- CMOMi1T.T.phi1 <- CMOMi1T.T.phi2 <-  CMOMi1T.T.p2 <- list()

for (ite in 1:iter){
  if (ite %in% it.1T$CMOMi) {next}
  CMOMi1T.T.N[[ite]] <- 
    ls.T.N[[ite]]>get(ls.CMOMi.1T[[ite]])[[10]][11:19,"2.5%"]&
    ls.T.N[[ite]]<get(ls.CMOMi.1T[[ite]])[[10]][11:19,"97.5%"]
  
  CMOMi1T.T.B[[ite]] <- 
    ls.T.B[[ite]]>get(ls.CMOMi.1T[[ite]])[[10]][20:21,"2.5%"]&
    ls.T.B[[ite]]<get(ls.CMOMi.1T[[ite]])[[10]][20:21,"97.5%"]
  
  CMOMi1T.T.phi1[[ite]] <- 
    Original.T[2]>get(ls.CMOMi.1T[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.T[2]<get(ls.CMOMi.1T[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMOMi1T.T.phi2[[ite]] <- 
    Original.T[1]>get(ls.CMOMi.1T[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.T[1]<get(ls.CMOMi.1T[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMOMi1T.T.p2[[ite]] <- 
    Original.T[4]>get(ls.CMOMi.1T[[ite]])[[10]][9,"2.5%"]&
    Original.T[4]<get(ls.CMOMi.1T[[ite]])[[10]][9,"97.5%"]}

#####################  CMR-1T ################################

CMR1T.T.N <- CMR1T.T.B <- CMR1T.T.phi1 <- CMR1T.T.phi2 <-  CMR1T.T.p2 <- list()

for (ite in 1:iter){
  if (ite %in% it.1T$CMR) {next}
  CMR1T.T.N[[ite]] <- 
    ls.T.N[[ite]]>get(ls.CMR.1T[[ite]])[[10]][11:19,"2.5%"]&
    ls.T.N[[ite]]<get(ls.CMR.1T[[ite]])[[10]][11:19,"97.5%"]
  
  CMR1T.T.B[[ite]] <- 
    ls.T.B[[ite]]>get(ls.CMR.1T[[ite]])[[10]][20:21,"2.5%"]&
    ls.T.B[[ite]]<get(ls.CMR.1T[[ite]])[[10]][20:21,"97.5%"]
  
  CMR1T.T.phi1[[ite]] <- 
    Original.T[2]>get(ls.CMR.1T[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.T[2]<get(ls.CMR.1T[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMR1T.T.phi2[[ite]] <- 
    Original.T[1]>get(ls.CMR.1T[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.T[1]<get(ls.CMR.1T[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMR1T.T.p2[[ite]] <- 
    Original.T[4]>get(ls.CMR.1T[[ite]])[[10]][9,"2.5%"]&
    Original.T[4]<get(ls.CMR.1T[[ite]])[[10]][9,"97.5%"]}

#***********************************

#    G r a p h      T A B L E      *

#***********************************

ls.CI.1T <- list()

for(TREN in 1:2){
  
  if (TREN == 1) {ini <- 1 
  last <- 100
  FACT <- "Decreasing"} # Decreasing
  if (TREN == 2) {ini <- 101 
  last <- 200
  FACT <- "Increasing"} # Increasing
  
  CI.1T <- data.frame(
    Parameters=factor(rep(c(
      "Number of Individuals",
      "Recruitment",
      "Survival Prim. Occ.",
      "Survival Second. Occ.",
      "Observation probability",
      "Capture probability"), times=5)),
    Models=factor(rep(c(
      "C-MOMd-1T",
      "C-MOMi-1T",
      "CMR-1T",
      "ZPNEc",
      "ZPNEs"), each=6)),
    Factor=factor(rep(c(
      FACT), times=30)),
    Percentage=c(
      sum(unlist(CMOMd1T.T.N[ini:last]))/length(unlist(CMOMd1T.T.N[ini:last])), 
      sum(unlist(CMOMd1T.T.B[ini:last]))/length(unlist(CMOMd1T.T.B[ini:last])), 
      sum(unlist(CMOMd1T.T.phi1[ini:last]))/length(unlist(CMOMd1T.T.phi1[ini:last])), 
      sum(unlist(CMOMd1T.T.phi2[ini:last]))/length(unlist(CMOMd1T.T.phi2[ini:last])),
      sum(unlist(CMOMd1T.T.p2[ini:last]))/length(unlist(CMOMd1T.T.p2[ini:last])),
      NA,
      sum(unlist(CMOMi1T.T.N[ini:last]))/length(unlist(CMOMi1T.T.N[ini:last])), 
      sum(unlist(CMOMi1T.T.B[ini:last]))/length(unlist(CMOMi1T.T.B[ini:last])), 
      sum(unlist(CMOMi1T.T.phi1[ini:last]))/length(unlist(CMOMi1T.T.phi1[ini:last])), 
      sum(unlist(CMOMi1T.T.phi2[ini:last]))/length(unlist(CMOMi1T.T.phi2[ini:last])),
      sum(unlist(CMOMi1T.T.p2[ini:last]))/length(unlist(CMOMi1T.T.p2[ini:last])),
      NA,
      sum(unlist(CMR1T.T.N[ini:last]))/length(unlist(CMR1T.T.N[ini:last])), 
      sum(unlist(CMR1T.T.B[ini:last]))/length(unlist(CMR1T.T.B[ini:last])), 
      sum(unlist(CMR1T.T.phi1[ini:last]))/length(unlist(CMR1T.T.phi1[ini:last])), 
      sum(unlist(CMR1T.T.phi2[ini:last]))/length(unlist(CMR1T.T.phi2[ini:last])),
      sum(unlist(CMR1T.T.p2[ini:last]))/length(unlist(CMR1T.T.p2[ini:last])),
      NA,
      sum(unlist(ZPNEc.T.N[ini:last]))/length(unlist(ZPNEc.T.N[ini:last])), 
      NA, 
      sum(unlist(ZPNEc.T.phi1[ini:last]))/length(unlist(ZPNEc.T.phi1[ini:last])), 
      NA,
      sum(unlist(ZPNEc.T.p2[ini:last]))/length(unlist(ZPNEc.T.p2[ini:last])),
      NA,
      sum(unlist(ZPNEs.T.N[ini:last]))/length(unlist(ZPNEs.T.N[ini:last])), 
      NA, 
      sum(unlist(ZPNEs.T.phi1[ini:last]))/length(unlist(ZPNEs.T.phi1[ini:last])), 
      NA,
      sum(unlist(ZPNEs.T.p2[ini:last]))/length(unlist(ZPNEs.T.p2[ini:last])),
      NA))
  
  ls.CI.1T[[TREN]] <- assign(paste("CI.1T", TREN, sep=""), CI.1T)
}

CI.1T <- rbind(ls.CI.1T[[1]], ls.CI.1T[[2]])

save(CI.1T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-1T.RData")

rm(list = ls())




#################################################################
#####################  C-MOMd-2T ################################
#################################################################

# Load true values and list of Results that contain CI
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/N-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/B-each.RData")
Original.T <- matrix(c(rep(c(0.85, 0.7, 0.2, 0.6),times=100), rep(c(0.99, 0.85, 0.2, 0.6),times=100)),ncol=200,nrow=4)
rownames(Original.T) <- c("phin","phib","p1","p2")
iter <- 200
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2T.RData")

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/2T_Backup.RData")

CMOMd2T.T.N <- CMOMd2T.T.B <- CMOMd2T.T.phi1 <- CMOMd2T.T.phi2 <-  CMOMd2T.T.p2 <-  CMOMd2T.T.p1 <- list()

for (ite in 1:iter){
  if (ite %in% it.2T$CMOMd) {next}
  CMOMd2T.T.N[[ite]] <- 
    ls.T.N[[ite]]>get(ls.CMOMd.2T[[ite]])[[10]][12:20,"2.5%"]&
    ls.T.N[[ite]]<get(ls.CMOMd.2T[[ite]])[[10]][12:20,"97.5%"]
  
  CMOMd2T.T.B[[ite]] <- 
    ls.T.B[[ite]]>get(ls.CMOMd.2T[[ite]])[[10]][21:22,"2.5%"]&
    ls.T.B[[ite]]<get(ls.CMOMd.2T[[ite]])[[10]][21:22,"97.5%"]
  
  CMOMd2T.T.phi1[[ite]] <- 
    Original.T[2]>get(ls.CMOMd.2T[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.T[2]<get(ls.CMOMd.2T[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMOMd2T.T.phi2[[ite]] <- 
    Original.T[1]>get(ls.CMOMd.2T[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.T[1]<get(ls.CMOMd.2T[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMOMd2T.T.p2[[ite]] <- 
    Original.T[4]>get(ls.CMOMd.2T[[ite]])[[10]][10,"2.5%"]&
    Original.T[4]<get(ls.CMOMd.2T[[ite]])[[10]][10,"97.5%"]
  
  CMOMd2T.T.p1[[ite]] <- 
    Original.T[3]>get(ls.CMOMd.2T[[ite]])[[10]][9,"2.5%"]&
    Original.T[3]<get(ls.CMOMd.2T[[ite]])[[10]][9,"97.5%"]}

#####################  C-MOMi-2T ################################

CMOMi2T.T.N <- CMOMi2T.T.B <- CMOMi2T.T.phi1 <- CMOMi2T.T.phi2 <-  CMOMi2T.T.p2 <-  CMOMi2T.T.p1 <-  list()

for (ite in 1:iter){
  if (ite %in% it.2T$CMOMi) {next}
  CMOMi2T.T.N[[ite]] <- 
    ls.T.N[[ite]]>get(ls.CMOMi.2T[[ite]])[[10]][12:20,"2.5%"]&
    ls.T.N[[ite]]<get(ls.CMOMi.2T[[ite]])[[10]][12:20,"97.5%"]
  
  CMOMi2T.T.B[[ite]] <- 
    ls.T.B[[ite]]>get(ls.CMOMi.2T[[ite]])[[10]][21:22,"2.5%"]&
    ls.T.B[[ite]]<get(ls.CMOMi.2T[[ite]])[[10]][21:22,"97.5%"]
  
  CMOMi2T.T.phi1[[ite]] <- 
    Original.T[2]>get(ls.CMOMi.2T[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.T[2]<get(ls.CMOMi.2T[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMOMi2T.T.phi2[[ite]] <- 
    Original.T[1]>get(ls.CMOMi.2T[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.T[1]<get(ls.CMOMi.2T[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMOMi2T.T.p2[[ite]] <- 
    Original.T[4]>get(ls.CMOMi.2T[[ite]])[[10]][10,"2.5%"]&
    Original.T[4]<get(ls.CMOMi.2T[[ite]])[[10]][10,"97.5%"]
  
  CMOMi2T.T.p1[[ite]] <- 
    Original.T[3]>get(ls.CMOMi.2T[[ite]])[[10]][9,"2.5%"]&
    Original.T[3]<get(ls.CMOMi.2T[[ite]])[[10]][9,"97.5%"]}

#####################  CMR-2T ################################

CMR2T.T.N <- CMR2T.T.B <- CMR2T.T.phi1 <- CMR2T.T.phi2 <-  CMR2T.T.p2 <- CMR2T.T.p1 <- list()

for (ite in 1:iter){
  if (ite %in% it.2T$CMR) {next}
  CMR2T.T.N[[ite]] <- 
    ls.T.N[[ite]]>get(ls.CMR.2T[[ite]])[[10]][12:20,"2.5%"]&
    ls.T.N[[ite]]<get(ls.CMR.2T[[ite]])[[10]][12:20,"97.5%"]
  
  CMR2T.T.B[[ite]] <- 
    ls.T.B[[ite]]>get(ls.CMR.2T[[ite]])[[10]][21:22,"2.5%"]&
    ls.T.B[[ite]]<get(ls.CMR.2T[[ite]])[[10]][21:22,"97.5%"]
  
  CMR2T.T.phi1[[ite]] <- 
    Original.T[2]>get(ls.CMR.2T[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.T[2]<get(ls.CMR.2T[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMR2T.T.phi2[[ite]] <- 
    Original.T[1]>get(ls.CMR.2T[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.T[1]<get(ls.CMR.2T[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMR2T.T.p2[[ite]] <- 
    Original.T[4]>get(ls.CMR.2T[[ite]])[[10]][10,"2.5%"]&
    Original.T[4]<get(ls.CMR.2T[[ite]])[[10]][10,"97.5%"]
  
  CMR2T.T.p1[[ite]] <- 
    Original.T[3]>get(ls.CMR.2T[[ite]])[[10]][9,"2.5%"]&
    Original.T[3]<get(ls.CMR.2T[[ite]])[[10]][9,"97.5%"]}

#***********************************

#    G r a p h      T A B L E      *

#***********************************

ls.CI.2T <- list()

for(TREN in 1:2){
  
  if (TREN == 1) {ini <- 1 
  last <- 100
  FACT <- "Decreasing"} # Decreasing
  if (TREN == 2) {ini <- 101 
  last <- 200
  FACT <- "Increasing"} # Increasing
  
  CI.2T <- data.frame(
    Parameters=factor(rep(c(
      "Number of Individuals",
      "Recruitment",
      "Survival Prim. Occ.",
      "Survival Second. Occ.",
      "Observation probability",
      "Capture probability"), times=3)),
    Models=factor(rep(c(
      "C-MOMd-2T",
      "C-MOMi-2T",
      "CMR-2T"), each=6)),
    Factor=factor(rep(c(
      FACT), times=18)),
    Percentage=c(
      sum(unlist(CMOMd2T.T.N[ini:last]))/length(unlist(CMOMd2T.T.N[ini:last])), 
      sum(unlist(CMOMd2T.T.B[ini:last]))/length(unlist(CMOMd2T.T.B[ini:last])), 
      sum(unlist(CMOMd2T.T.phi1[ini:last]))/length(unlist(CMOMd2T.T.phi1[ini:last])), 
      sum(unlist(CMOMd2T.T.phi2[ini:last]))/length(unlist(CMOMd2T.T.phi2[ini:last])),
      sum(unlist(CMOMd2T.T.p2[ini:last]))/length(unlist(CMOMd2T.T.p2[ini:last])),
      sum(unlist(CMOMd2T.T.p1[ini:last]))/length(unlist(CMOMd2T.T.p1[ini:last])),
      
      sum(unlist(CMOMi2T.T.N[ini:last]))/length(unlist(CMOMi2T.T.N[ini:last])), 
      sum(unlist(CMOMi2T.T.B[ini:last]))/length(unlist(CMOMi2T.T.B[ini:last])), 
      sum(unlist(CMOMi2T.T.phi1[ini:last]))/length(unlist(CMOMi2T.T.phi1[ini:last])), 
      sum(unlist(CMOMi2T.T.phi2[ini:last]))/length(unlist(CMOMi2T.T.phi2[ini:last])),
      sum(unlist(CMOMi2T.T.p2[ini:last]))/length(unlist(CMOMi2T.T.p2[ini:last])),
      sum(unlist(CMOMi2T.T.p1[ini:last]))/length(unlist(CMOMi2T.T.p1[ini:last])),
      
      sum(unlist(CMR2T.T.N[ini:last]))/length(unlist(CMR2T.T.N[ini:last])), 
      sum(unlist(CMR2T.T.B[ini:last]))/length(unlist(CMR2T.T.B[ini:last])), 
      sum(unlist(CMR2T.T.phi1[ini:last]))/length(unlist(CMR2T.T.phi1[ini:last])), 
      sum(unlist(CMR2T.T.phi2[ini:last]))/length(unlist(CMR2T.T.phi2[ini:last])),
      sum(unlist(CMR2T.T.p2[ini:last]))/length(unlist(CMR2T.T.p2[ini:last])),
      sum(unlist(CMR2T.T.p1[ini:last]))/length(unlist(CMR2T.T.p1[ini:last]))))
      
      ls.CI.2T[[TREN]] <- assign(paste("CI.2T", TREN, sep=""), CI.2T)
}

CI.2T <- rbind(ls.CI.2T[[1]], ls.CI.2T[[2]])

save(CI.2T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-2T.RData")

rm(list = ls())

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-1T.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-2T.RData")

CI.T <- rbind(CI.1T, CI.2T)

save(CI.T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-T.RData")