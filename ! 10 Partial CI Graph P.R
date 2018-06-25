##################################################################
#                                                                #
# 10. Creating CI  Graph for  P2P,  TRENDS  and  HETERO         #
#                                                                #
##################################################################

#-----------------------------------------------------------#
#                        1  P                               #
#-----------------------------------------------------------#

# Load true values and list of Results that contain CI
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/N-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/B-each.RData")
Original.P <- matrix(c(0.99, 0.85, 0.2, 0.6),ncol=1,nrow=4)
rownames(Original.P) <- c("phi2","phi1","p1","p2")
iter <- 100

# For ZPNEc and ZPNEs (as these only have a mean pop per field trip)
ls.N.ZPNE <- list()
for (ite in 1:iter){
  ls.N.ZPNE[[ite]] <- c(mean(ls.N[[ite]][1:3]),mean(ls.N[[ite]][4:6]),mean(ls.N[[ite]][7:9]))}

#####################  ZPNEc ################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/Mark-list-ZPNEc.RData")

ZPNEc.P.N <- ZPNEc.P.phi1 <-  ZPNEc.P.p2 <- list()

for (ite in 1:iter){
ZPNEc.P.N[[ite]] <- 
  ls.N.ZPNE[[ite]]>Mark.list[[ite]]$results$derived$`N Population Size`$lcl&
  ls.N.ZPNE[[ite]]<Mark.list[[ite]]$results$derived$`N Population Size`$ucl

ZPNEc.P.phi1[[ite]] <- 
  Original.P[2]>Mark.list[[ite]]$results$real[6,"lcl"]&
  Original.P[2]<Mark.list[[ite]]$results$real[6,"ucl"]

ZPNEc.P.p2[[ite]] <- 
  Original.P[4]>Mark.list[[ite]]$results$derived$`Pr(Captured 1 or more times)`$lcl&
  Original.P[4]<Mark.list[[ite]]$results$derived$`Pr(Captured 1 or more times)`$ucl}


#####################  ZPNEs ################################
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/Mark-list-ZPNEs.RData")

ZPNEs.P.N <- ZPNEs.P.phi1 <-  ZPNEs.P.p2 <- list()

for (ite in 1:iter){
  ZPNEs.P.N[[ite]] <- 
    ls.N.ZPNE[[ite]]>Mark.list[[ite]]$results$derived$`N Population Size`$lcl&
    ls.N.ZPNE[[ite]]<Mark.list[[ite]]$results$derived$`N Population Size`$ucl
  
  ZPNEs.P.phi1[[ite]] <- 
    Original.P[2]>Mark.list[[ite]]$results$real[6,"lcl"]&
    Original.P[2]<Mark.list[[ite]]$results$real[6,"ucl"]
  
  ZPNEs.P.p2[[ite]] <- 
    Original.P[4]>Mark.list[[ite]]$results$derived$`Pr(Captured 1 or more times)`$lcl&
    Original.P[4]<Mark.list[[ite]]$results$derived$`Pr(Captured 1 or more times)`$ucl}

#####################  ITERATIONS HIGH Rhat ################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2P.RData")

#####################  C-MOMd-1P ################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/1P_Backup.RData")

CMOMd1P.P.N <- CMOMd1P.P.B <- CMOMd1P.P.phi1 <- CMOMd1P.P.phi2 <-  CMOMd1P.P.p2 <- list()

for (ite in 1:iter){
  if (ite %in% it.1P$CMOMd) {next}
  CMOMd1P.P.N[[ite]] <- 
    ls.N[[ite]]>get(ls.CMOMd.1P[[ite]])[[10]][11:19,"2.5%"]&
    ls.N[[ite]]<get(ls.CMOMd.1P[[ite]])[[10]][11:19,"97.5%"]

  CMOMd1P.P.B[[ite]] <- 
    ls.B[[ite]]>get(ls.CMOMd.1P[[ite]])[[10]][20:21,"2.5%"]&
    ls.B[[ite]]<get(ls.CMOMd.1P[[ite]])[[10]][20:21,"97.5%"]
  
  CMOMd1P.P.phi1[[ite]] <- 
    Original.P[2]>get(ls.CMOMd.1P[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.P[2]<get(ls.CMOMd.1P[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMOMd1P.P.phi2[[ite]] <- 
    Original.P[1]>get(ls.CMOMd.1P[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.P[1]<get(ls.CMOMd.1P[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]

  CMOMd1P.P.p2[[ite]] <- 
    Original.P[4]>get(ls.CMOMd.1P[[ite]])[[10]][9,"2.5%"]&
    Original.P[4]<get(ls.CMOMd.1P[[ite]])[[10]][9,"97.5%"]}

#####################  C-MOMi-1P ################################

CMOMi1P.P.N <- CMOMi1P.P.B <- CMOMi1P.P.phi1 <- CMOMi1P.P.phi2 <-  CMOMi1P.P.p2 <- list()

for (ite in 1:iter){
  if (ite %in% it.1P$CMOMi) {next}
  CMOMi1P.P.N[[ite]] <- 
    ls.N[[ite]]>get(ls.CMOMi.1P[[ite]])[[10]][11:19,"2.5%"]&
    ls.N[[ite]]<get(ls.CMOMi.1P[[ite]])[[10]][11:19,"97.5%"]
  
  CMOMi1P.P.B[[ite]] <- 
    ls.B[[ite]]>get(ls.CMOMi.1P[[ite]])[[10]][20:21,"2.5%"]&
    ls.B[[ite]]<get(ls.CMOMi.1P[[ite]])[[10]][20:21,"97.5%"]
  
  CMOMi1P.P.phi1[[ite]] <- 
    Original.P[2]>get(ls.CMOMi.1P[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.P[2]<get(ls.CMOMi.1P[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMOMi1P.P.phi2[[ite]] <- 
    Original.P[1]>get(ls.CMOMi.1P[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.P[1]<get(ls.CMOMi.1P[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMOMi1P.P.p2[[ite]] <- 
    Original.P[4]>get(ls.CMOMi.1P[[ite]])[[10]][9,"2.5%"]&
    Original.P[4]<get(ls.CMOMi.1P[[ite]])[[10]][9,"97.5%"]}

#####################  CMR-1P ################################

CMR1P.P.N <- CMR1P.P.B <- CMR1P.P.phi1 <- CMR1P.P.phi2 <-  CMR1P.P.p2 <- list()

for (ite in 1:iter){
  if (ite %in% it.1P$CMR) {next}
  CMR1P.P.N[[ite]] <- 
    ls.N[[ite]]>get(ls.CMR.1P[[ite]])[[10]][11:19,"2.5%"]&
    ls.N[[ite]]<get(ls.CMR.1P[[ite]])[[10]][11:19,"97.5%"]
  
  CMR1P.P.B[[ite]] <- 
    ls.B[[ite]]>get(ls.CMR.1P[[ite]])[[10]][20:21,"2.5%"]&
    ls.B[[ite]]<get(ls.CMR.1P[[ite]])[[10]][20:21,"97.5%"]
  
  CMR1P.P.phi1[[ite]] <- 
    Original.P[2]>get(ls.CMR.1P[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.P[2]<get(ls.CMR.1P[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMR1P.P.phi2[[ite]] <- 
    Original.P[1]>get(ls.CMR.1P[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.P[1]<get(ls.CMR.1P[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMR1P.P.p2[[ite]] <- 
    Original.P[4]>get(ls.CMR.1P[[ite]])[[10]][9,"2.5%"]&
    Original.P[4]<get(ls.CMR.1P[[ite]])[[10]][9,"97.5%"]}

#####################  C-MOMd-2P ################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/2P_Backup.RData")

CMOMd2P.P.N <- CMOMd2P.P.B <- CMOMd2P.P.phi1 <- CMOMd2P.P.phi2 <-  CMOMd2P.P.p2 <-  CMOMd2P.P.p1 <- list()

for (ite in 1:iter){
  if (ite %in% it.2P$CMOMd) {next}
  CMOMd2P.P.N[[ite]] <- 
    ls.N[[ite]]>get(ls.CMOMd.2P[[ite]])[[10]][12:20,"2.5%"]&
    ls.N[[ite]]<get(ls.CMOMd.2P[[ite]])[[10]][12:20,"97.5%"]
  
  CMOMd2P.P.B[[ite]] <- 
    ls.B[[ite]]>get(ls.CMOMd.2P[[ite]])[[10]][21:22,"2.5%"]&
    ls.B[[ite]]<get(ls.CMOMd.2P[[ite]])[[10]][21:22,"97.5%"]
  
  CMOMd2P.P.phi1[[ite]] <- 
    Original.P[2]>get(ls.CMOMd.2P[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.P[2]<get(ls.CMOMd.2P[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMOMd2P.P.phi2[[ite]] <- 
    Original.P[1]>get(ls.CMOMd.2P[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.P[1]<get(ls.CMOMd.2P[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMOMd2P.P.p2[[ite]] <- 
    Original.P[4]>get(ls.CMOMd.2P[[ite]])[[10]][10,"2.5%"]&
    Original.P[4]<get(ls.CMOMd.2P[[ite]])[[10]][10,"97.5%"]
  
  CMOMd2P.P.p1[[ite]] <- 
    Original.P[3]>get(ls.CMOMd.2P[[ite]])[[10]][9,"2.5%"]&
    Original.P[3]<get(ls.CMOMd.2P[[ite]])[[10]][9,"97.5%"]}

#####################  C-MOMi-2P ################################

CMOMi2P.P.N <- CMOMi2P.P.B <- CMOMi2P.P.phi1 <- CMOMi2P.P.phi2 <-  CMOMi2P.P.p2 <-  CMOMi2P.P.p1 <-  list()

for (ite in 1:iter){
  if (ite %in% it.2P$CMOMi) {next}
  CMOMi2P.P.N[[ite]] <- 
    ls.N[[ite]]>get(ls.CMOMi.2P[[ite]])[[10]][12:20,"2.5%"]&
    ls.N[[ite]]<get(ls.CMOMi.2P[[ite]])[[10]][12:20,"97.5%"]
  
  CMOMi2P.P.B[[ite]] <- 
    ls.B[[ite]]>get(ls.CMOMi.2P[[ite]])[[10]][21:22,"2.5%"]&
    ls.B[[ite]]<get(ls.CMOMi.2P[[ite]])[[10]][21:22,"97.5%"]
  
  CMOMi2P.P.phi1[[ite]] <- 
    Original.P[2]>get(ls.CMOMi.2P[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.P[2]<get(ls.CMOMi.2P[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMOMi2P.P.phi2[[ite]] <- 
    Original.P[1]>get(ls.CMOMi.2P[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.P[1]<get(ls.CMOMi.2P[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMOMi2P.P.p2[[ite]] <- 
    Original.P[4]>get(ls.CMOMi.2P[[ite]])[[10]][10,"2.5%"]&
    Original.P[4]<get(ls.CMOMi.2P[[ite]])[[10]][10,"97.5%"]
  
  CMOMi2P.P.p1[[ite]] <- 
    Original.P[3]>get(ls.CMOMi.2P[[ite]])[[10]][9,"2.5%"]&
    Original.P[3]<get(ls.CMOMi.2P[[ite]])[[10]][9,"97.5%"]}

#####################  CMR-2P ################################

CMR2P.P.N <- CMR2P.P.B <- CMR2P.P.phi1 <- CMR2P.P.phi2 <-  CMR2P.P.p2 <- CMR2P.P.p1 <- list()

for (ite in 1:iter){
  if (ite %in% it.2P$CMR) {next}
  CMR2P.P.N[[ite]] <- 
    ls.N[[ite]]>get(ls.CMR.2P[[ite]])[[10]][12:20,"2.5%"]&
    ls.N[[ite]]<get(ls.CMR.2P[[ite]])[[10]][12:20,"97.5%"]
  
  CMR2P.P.B[[ite]] <- 
    ls.B[[ite]]>get(ls.CMR.2P[[ite]])[[10]][21:22,"2.5%"]&
    ls.B[[ite]]<get(ls.CMR.2P[[ite]])[[10]][21:22,"97.5%"]
  
  CMR2P.P.phi1[[ite]] <- 
    Original.P[2]>get(ls.CMR.2P[[ite]])[[10]][c(3,6),"2.5%"]&
    Original.P[2]<get(ls.CMR.2P[[ite]])[[10]][c(3,6),"97.5%"]
  
  CMR2P.P.phi2[[ite]] <- 
    Original.P[1]>get(ls.CMR.2P[[ite]])[[10]][c(1:2,4:5,7:8),"2.5%"]&
    Original.P[1]<get(ls.CMR.2P[[ite]])[[10]][c(1:2,4:5,7:8),"97.5%"]
  
  CMR2P.P.p2[[ite]] <- 
    Original.P[4]>get(ls.CMR.2P[[ite]])[[10]][10,"2.5%"]&
    Original.P[4]<get(ls.CMR.2P[[ite]])[[10]][10,"97.5%"]
  
  CMR2P.P.p1[[ite]] <- 
    Original.P[3]>get(ls.CMR.2P[[ite]])[[10]][9,"2.5%"]&
    Original.P[3]<get(ls.CMR.2P[[ite]])[[10]][9,"97.5%"]}

#***********************************

#    G r a p h      T A B L E      *

#***********************************

CI.P <- data.frame(
  Parameters=factor(rep(c(
           "Number of Individuals",
           "Recruitment",
           "Survival Prim. Occ.",
           "Survival Second. Occ.",
           "Observation probability",
           "Capture probability"), times=8)),
  Models=factor(rep(c(
           "C-MOMd-1P",
           "C-MOMd-2P",
           "C-MOMi-1P",
           "C-MOMi-2P",
           "CMR-1P",
           "CMR-2P",
           "ZPNEc",
           "ZPNEs"), each=6)),
  Factor=factor(rep(c(
           "Stable"), times=48)),
  Percentage=c(
    sum(unlist(CMOMd1P.P.N))/length(unlist(CMOMd1P.P.N)), 
    sum(unlist(CMOMd1P.P.B))/length(unlist(CMOMd1P.P.B)), 
    sum(unlist(CMOMd1P.P.phi1))/length(unlist(CMOMd1P.P.phi1)), 
    sum(unlist(CMOMd1P.P.phi2))/length(unlist(CMOMd1P.P.phi2)),
    sum(unlist(CMOMd1P.P.p2))/length(unlist(CMOMd1P.P.p2)),
    NA,
    sum(unlist(CMOMd2P.P.N))/length(unlist(CMOMd2P.P.N)), 
    sum(unlist(CMOMd2P.P.B))/length(unlist(CMOMd2P.P.B)), 
    sum(unlist(CMOMd2P.P.phi1))/length(unlist(CMOMd2P.P.phi1)), 
    sum(unlist(CMOMd2P.P.phi2))/length(unlist(CMOMd2P.P.phi2)),
    sum(unlist(CMOMd2P.P.p2))/length(unlist(CMOMd2P.P.p2)),
    sum(unlist(CMOMd2P.P.p1))/length(unlist(CMOMd2P.P.p1)),
    
    sum(unlist(CMOMi1P.P.N))/length(unlist(CMOMi1P.P.N)), 
    sum(unlist(CMOMi1P.P.B))/length(unlist(CMOMi1P.P.B)), 
    sum(unlist(CMOMi1P.P.phi1))/length(unlist(CMOMi1P.P.phi1)), 
    sum(unlist(CMOMi1P.P.phi2))/length(unlist(CMOMi1P.P.phi2)),
    sum(unlist(CMOMi1P.P.p2))/length(unlist(CMOMi1P.P.p2)),
    NA,
    sum(unlist(CMOMi2P.P.N))/length(unlist(CMOMi2P.P.N)), 
    sum(unlist(CMOMi2P.P.B))/length(unlist(CMOMi2P.P.B)), 
    sum(unlist(CMOMi2P.P.phi1))/length(unlist(CMOMi2P.P.phi1)), 
    sum(unlist(CMOMi2P.P.phi2))/length(unlist(CMOMi2P.P.phi2)),
    sum(unlist(CMOMi2P.P.p2))/length(unlist(CMOMi2P.P.p2)),
    sum(unlist(CMOMi2P.P.p1))/length(unlist(CMOMi2P.P.p1)),
    
    sum(unlist(CMR1P.P.N))/length(unlist(CMR1P.P.N)), 
    sum(unlist(CMR1P.P.B))/length(unlist(CMR1P.P.B)), 
    sum(unlist(CMR1P.P.phi1))/length(unlist(CMR1P.P.phi1)), 
    sum(unlist(CMR1P.P.phi2))/length(unlist(CMR1P.P.phi2)),
    sum(unlist(CMR1P.P.p2))/length(unlist(CMR1P.P.p2)),
    NA,
    sum(unlist(CMR2P.P.N))/length(unlist(CMR2P.P.N)), 
    sum(unlist(CMR2P.P.B))/length(unlist(CMR2P.P.B)), 
    sum(unlist(CMR2P.P.phi1))/length(unlist(CMR2P.P.phi1)), 
    sum(unlist(CMR2P.P.phi2))/length(unlist(CMR2P.P.phi2)),
    sum(unlist(CMR2P.P.p2))/length(unlist(CMR2P.P.p2)),
    sum(unlist(CMR2P.P.p1))/length(unlist(CMR2P.P.p1)),
    
    sum(unlist(ZPNEc.P.N))/length(unlist(ZPNEc.P.N)), 
    NA, 
    sum(unlist(ZPNEc.P.phi1))/length(unlist(ZPNEc.P.phi1)), 
    NA,
    sum(unlist(ZPNEc.P.p2))/length(unlist(ZPNEc.P.p2)),
    NA,
    
    sum(unlist(ZPNEs.P.N))/length(unlist(ZPNEs.P.N)), 
    NA, 
    sum(unlist(ZPNEs.P.phi1))/length(unlist(ZPNEs.P.phi1)), 
    NA,
    sum(unlist(ZPNEs.P.p2))/length(unlist(ZPNEs.P.p2)),
    NA
   ) 
)

save(CI.P, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-P.RData")
