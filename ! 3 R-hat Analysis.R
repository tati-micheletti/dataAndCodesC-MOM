#########################################
#                                       #
#     3. Checking all models R-hat      #
#                                       #
#########################################

###############################################
#                                             #
#     S T A B L E    P O P U L A T I O N      #
#                                             #
###############################################
#-------------------------- 1P ------------------------------

# 1P C-MOM
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/1P_Backup.RData")

Rhat.CMOMd.1P <- matrix(NA, ncol=1, nrow=length(ls.CMOMd.1P))
for (j in 1:nrow(Rhat.CMOMd.1P)){
  Rhat.CMOMd.1P[j,1] <- max(get(ls.CMOMd.1P[[j]])[[10]][1:(nrow(get(ls.CMOMd.1P[[j]])[[10]])-1),"Rhat"])}
it.CMOMd.1P <- which(Rhat.CMOMd.1P[,1]>1.2)

Rhat.CMOMi.1P <- matrix(NA, ncol=1, nrow=length(ls.CMOMi.1P))
for (j in 1:nrow(Rhat.CMOMi.1P)){
  Rhat.CMOMi.1P[j,1] <- max(get(ls.CMOMi.1P[[j]])[[10]][1:(nrow(get(ls.CMOMi.1P[[j]])[[10]])-1),"Rhat"])}
it.CMOMi.1P <- which(Rhat.CMOMi.1P[,1]>1.2)

# 1P CMR
Rhat.CMR.1P <- matrix(NA, ncol=1, nrow=length(ls.CMR.1P))
for (j in 1:nrow(Rhat.CMR.1P)){
  Rhat.CMR.1P[j,1] <- max(get(ls.CMR.1P[[j]])[[10]][1:(nrow(get(ls.CMR.1P[[j]])[[10]])-1),"Rhat"])}
it.CMR.1P <- which(Rhat.CMR.1P[,1]>1.2)


#-------------------------- 2P ------------------------------

# 2P C-MOM
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/2P_Backup.RData")

Rhat.CMOMd.2P <- matrix(NA, ncol=1, nrow=length(ls.CMOMd.2P))
for (j in 1:nrow(Rhat.CMOMd.2P)){
  Rhat.CMOMd.2P[j,1] <- max(get(ls.CMOMd.2P[[j]])[[10]][1:(nrow(get(ls.CMOMd.2P[[j]])[[10]])-1),"Rhat"])}
it.CMOMd.2P <- which(Rhat.CMOMd.2P[,1]>1.2)

Rhat.CMOMi.2P <- matrix(NA, ncol=1, nrow=length(ls.CMOMi.2P))
for (j in 1:nrow(Rhat.CMOMi.2P)){
  Rhat.CMOMi.2P[j,1] <- max(get(ls.CMOMi.2P[[j]])[[10]][1:(nrow(get(ls.CMOMi.2P[[j]])[[10]])-1),"Rhat"])}
it.CMOMi.2P <- which(Rhat.CMOMi.2P[,1]>1.2)

# 2P CMR
Rhat.CMR.2P <- matrix(NA, ncol=1, nrow=length(ls.CMR.2P))
for (j in 1:nrow(Rhat.CMR.2P)){
  Rhat.CMR.2P[j,1] <- max(get(ls.CMR.2P[[j]])[[10]][1:(nrow(get(ls.CMR.2P[[j]])[[10]])-1),"Rhat"])}
it.CMR.2P <- which(Rhat.CMR.2P[,1]>1.2)

it.1P <- list(CMOMd=it.CMOMd.1P, CMOMi=it.CMOMi.1P, CMR=it.CMR.1P) 
it.2P <- list(CMOMd=it.CMOMd.2P, CMOMi=it.CMOMi.2P, CMR=it.CMR.2P)

save(it.1P, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1P.RData")
save(it.2P, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2P.RData")

rm(list = ls())

###############################################
#                                             #
#     P O P U L A T I O N     T R E N D S     #
#                                             #
###############################################

#-------------------------- 1T ------------------------------

# 1T C-MOM
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/1T_Backup.RData")

Rhat.CMOMd.1T <- matrix(NA, ncol=1, nrow=length(ls.CMOMd.1T))
for (j in 1:nrow(Rhat.CMOMd.1T)){
  Rhat.CMOMd.1T[j,1] <- max(get(ls.CMOMd.1T[[j]])[[10]][1:(nrow(get(ls.CMOMd.1T[[j]])[[10]])-1),"Rhat"])}
it.CMOMd.1T <- which(Rhat.CMOMd.1T[,1]>1.2)

Rhat.CMOMi.1T <- matrix(NA, ncol=1, nrow=length(ls.CMOMi.1T))
for (j in 1:nrow(Rhat.CMOMi.1T)){
  Rhat.CMOMi.1T[j,1] <- max(get(ls.CMOMi.1T[[j]])[[10]][1:(nrow(get(ls.CMOMi.1T[[j]])[[10]])-1),"Rhat"])}
it.CMOMi.1T <- which(Rhat.CMOMi.1T[,1]>1.2)

# 1T CMR
Rhat.CMR.1T <- matrix(NA, ncol=1, nrow=length(ls.CMR.1T))
for (j in 1:nrow(Rhat.CMR.1T)){
  Rhat.CMR.1T[j,1] <- max(get(ls.CMR.1T[[j]])[[10]][1:(nrow(get(ls.CMR.1T[[j]])[[10]])-1),"Rhat"])}
it.CMR.1T <- which(Rhat.CMR.1T[,1]>1.2)

it.1T <- list(CMOMd=it.CMOMd.1T, CMOMi=it.CMOMi.1T, CMR=it.CMR.1T) 
save(it.1T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1T.RData")

rm(list = ls())

#-------------------------- 2T ------------------------------

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/2T_Backup.RData")

Rhat.CMOMd.2T <- matrix(NA, ncol=1, nrow=length(ls.CMOMd.2T))
for (j in 1:nrow(Rhat.CMOMd.2T)){
  Rhat.CMOMd.2T[j,1] <- max(get(ls.CMOMd.2T[[j]])[[10]][1:(nrow(get(ls.CMOMd.2T[[j]])[[10]])-1),"Rhat"])}
it.CMOMd.2T <- which(Rhat.CMOMd.2T[,1]>1.2)

Rhat.CMOMi.2T <- matrix(NA, ncol=1, nrow=length(ls.CMOMi.2T))
for (j in 1:nrow(Rhat.CMOMi.2T)){
  Rhat.CMOMi.2T[j,1] <- max(get(ls.CMOMi.2T[[j]])[[10]][1:(nrow(get(ls.CMOMi.2T[[j]])[[10]])-1),"Rhat"])}
it.CMOMi.2T <- which(Rhat.CMOMi.2T[,1]>1.2)

# 2T CMR
Rhat.CMR.2T <- matrix(NA, ncol=1, nrow=length(ls.CMR.2T))
for (j in 1:nrow(Rhat.CMR.2T)){
  Rhat.CMR.2T[j,1] <- max(get(ls.CMR.2T[[j]])[[10]][1:(nrow(get(ls.CMR.2T[[j]])[[10]])-1),"Rhat"])}
it.CMR.2T <- which(Rhat.CMR.2T[,1]>1.2)


it.2T <- list(CMOMd=it.CMOMd.2T, CMOMi=it.CMOMi.2T, CMR=it.CMR.2T)
save(it.2T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2T.RData")

rm(list = ls())

###############################################
#                                             #
#     P O P U L A T I O N     H E T E R O     #
#                                             #
###############################################
#-------------------------- 1P ------------------------------

# 1H C-MOM
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/1H_Backup.RData")

Rhat.CMOMd.1H <- matrix(NA, ncol=1, nrow=length(ls.CMOMd.1H))
for (j in 1:nrow(Rhat.CMOMd.1H)){
  Rhat.CMOMd.1H[j,1] <- max(get(ls.CMOMd.1H[[j]])[[10]][1:(nrow(get(ls.CMOMd.1H[[j]])[[10]])-1),"Rhat"])}
it.CMOMd.1H <- which(Rhat.CMOMd.1H[,1]>1.2)

Rhat.CMOMi.1H <- matrix(NA, ncol=1, nrow=length(ls.CMOMi.1H))
for (j in 1:nrow(Rhat.CMOMi.1H)){
  Rhat.CMOMi.1H[j,1] <- max(get(ls.CMOMi.1H[[j]])[[10]][1:(nrow(get(ls.CMOMi.1H[[j]])[[10]])-1),"Rhat"])}
it.CMOMi.1H <- which(Rhat.CMOMi.1H[,1]>1.2)

# 1H CMR
Rhat.CMR.1H <- matrix(NA, ncol=1, nrow=length(ls.CMR.1H))
for (j in 1:nrow(Rhat.CMR.1H)){
  Rhat.CMR.1H[j,1] <- max(get(ls.CMR.1H[[j]])[[10]][1:(nrow(get(ls.CMR.1H[[j]])[[10]])-1),"Rhat"])}
it.CMR.1H <- which(Rhat.CMR.1H[,1]>1.2)

it.1H <- list(CMOMd=it.CMOMd.1H, CMOMi=it.CMOMi.1H, CMR=it.CMR.1H) 
save(it.1H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1H.RData")

rm(list = ls())

#-------------------------- 2P ------------------------------

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/2H_Backup.RData")

# 2H C-MOM
Rhat.CMOMd.2H <- matrix(NA, ncol=1, nrow=length(ls.CMOMd.2H))
for (j in 1:nrow(Rhat.CMOMd.2H)){
  Rhat.CMOMd.2H[j,1] <- max(get(ls.CMOMd.2H[[j]])[[10]][1:(nrow(get(ls.CMOMd.2H[[j]])[[10]])-1),"Rhat"])}
it.CMOMd.2H <- which(Rhat.CMOMd.2H[,1]>1.2)

Rhat.CMOMi.2H <- matrix(NA, ncol=1, nrow=length(ls.CMOMi.2H))
for (j in 1:nrow(Rhat.CMOMi.2H)){
  Rhat.CMOMi.2H[j,1] <- max(get(ls.CMOMi.2H[[j]])[[10]][1:(nrow(get(ls.CMOMi.2H[[j]])[[10]])-1),"Rhat"])}
it.CMOMi.2H <- which(Rhat.CMOMi.2H[,1]>1.2)

# 2H CMR
Rhat.CMR.2H <- matrix(NA, ncol=1, nrow=length(ls.CMR.2H))
for (j in 1:nrow(Rhat.CMR.2H)){
  Rhat.CMR.2H[j,1] <- max(get(ls.CMR.2H[[j]])[[10]][1:(nrow(get(ls.CMR.2H[[j]])[[10]])-1),"Rhat"])}
it.CMR.2H <- which(Rhat.CMR.2H[,1]>1.2)

it.2H <- list(CMOMd=it.CMOMd.2H, CMOMi=it.CMOMi.2H, CMR=it.CMR.2H)
save(it.2H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2H.RData")

rm(list = ls())

###############################################
#                                             #
#             S U M M A R Y                   #
#                                             #
###############################################

# At least 62 iterations were run for each analysis

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1T.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2T.RData")

length(it.1P$CMOMd) # 1
length(it.1P$CMOMi) # 3
length(it.1P$CMR) # 24

length(it.2P$CMOMd) # 0
length(it.2P$CMOMi) # 1
length(it.2P$CMR) # 21

# Decreasing
length(it.1T$CMOMd[it.1T$CMOMd<101]) # 1
length(it.1T$CMOMi[it.1T$CMOMi<101]) # 0
length(it.1T$CMR[it.1T$CMR<101]) # 10

# Increasing
length(it.1T$CMOMd[it.1T$CMOMd>100]) # 3
length(it.1T$CMOMi[it.1T$CMOMi>100]) # 3
length(it.1T$CMR[it.1T$CMR>100]) # 38  ====> Highest

# 10%
length(it.1H$CMOMd[it.1H$CMOMd<101]) # 0
length(it.1H$CMOMi[it.1H$CMOMi<101]) # 0
length(it.1H$CMR[it.1H$CMR<101]) # 32

# 20%
length(which(it.1H$CMOMd>100 & it.1H$CMOMd<201)) # 1
length(which(it.1H$CMOMi>100 & it.1H$CMOMi<201)) # 2  
length(which(it.1H$CMR>100 & it.1H$CMR<201)) # 27

# 30%
length(which(it.1H$CMOMd>200 & it.1H$CMOMd<301)) # 1
length(which(it.1H$CMOMi>200 & it.1H$CMOMi<301)) # 0  
length(which(it.1H$CMR>200 & it.1H$CMR<301)) # 30

# 40%
length(which(it.1H$CMOMd>300 & it.1H$CMOMd<401)) # 0
length(which(it.1H$CMOMi>300 & it.1H$CMOMi<401)) # 0  
length(which(it.1H$CMR>300 & it.1H$CMR<401)) # 30

# 50%
length(which(it.1H$CMOMd>400 & it.1H$CMOMd<501)) # 1
length(which(it.1H$CMOMi>400 & it.1H$CMOMi<501)) # 0  
length(which(it.1H$CMR>400 & it.1H$CMR<501)) # 30

# 60%
length(which(it.1H$CMOMd>500 & it.1H$CMOMd<601)) # 0
length(which(it.1H$CMOMi>500 & it.1H$CMOMi<601)) # 0  
length(which(it.1H$CMR>500 & it.1H$CMR<601)) # 26

# Decreasing
length(it.2T$CMOMd[it.2T$CMOMd<101]) # 2
length(it.2T$CMOMi[it.2T$CMOMi<101]) # 0
length(it.2T$CMR[it.2T$CMR<101]) # 8

# Increasing
length(it.2T$CMOMd[it.2T$CMOMd>100]) # 2
length(it.2T$CMOMi[it.2T$CMOMi>100]) # 3
length(it.2T$CMR[it.2T$CMR>100]) # 25

# 10%
length(it.2H$CMOMd[it.2H$CMOMd<101]) # 1
length(it.2H$CMOMi[it.2H$CMOMi<101]) # 0
length(it.2H$CMR[it.2H$CMR<101]) # 28

# 20%
length(which(it.2H$CMOMd>100 & it.2H$CMOMd<201)) # 0
length(which(it.2H$CMOMi>100 & it.2H$CMOMi<201)) #  1 
length(which(it.2H$CMR>100 & it.2H$CMR<201)) # 22

# 30%
length(which(it.2H$CMOMd>200 & it.2H$CMOMd<301)) # 1 
length(which(it.2H$CMOMi>200 & it.2H$CMOMi<301)) #   1 
length(which(it.2H$CMR>200 & it.2H$CMR<301)) # 29

# 40%
length(which(it.2H$CMOMd>300 & it.2H$CMOMd<401)) # 1
length(which(it.2H$CMOMi>300 & it.2H$CMOMi<401)) #  1 
length(which(it.2H$CMR>300 & it.2H$CMR<401)) # 31

# 50%
length(which(it.2H$CMOMd>400 & it.2H$CMOMd<501)) # 2
length(which(it.2H$CMOMi>400 & it.2H$CMOMi<501)) #  1 
length(which(it.2H$CMR>400 & it.2H$CMR<501)) # 20

# 60%
length(which(it.2H$CMOMd>500 & it.2H$CMOMd<601)) # 1
length(which(it.2H$CMOMi>500 & it.2H$CMOMi<601)) #  1 
length(which(it.2H$CMR>500 & it.2H$CMR<601)) # 27
