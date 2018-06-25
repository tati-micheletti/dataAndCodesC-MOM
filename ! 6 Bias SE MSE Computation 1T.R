#####################################################################
#                                                                   #
#         6a.    M S E,     R B    A N D     R S E                  #
#                                                                   #
#                           1  T                                    #
#                                                                   #
#####################################################################

#############################################################################
# Function to compute relative bias, standard error, and Mean squared error #
#                 for N, B, phi.in, phi.btw, p                              #
#############################################################################

# 1 T

# ----- FOR N ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/N-1T.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/N-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/TN-ZPNEc.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/TN-ZPNEs.RData")

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/Ns-1T.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/TNs-ZPNEc.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/TNs-ZPNEs.RData")

# ----- FOR phi and p ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/output1T.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/T-outputZPNEs.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/T-outputZPNEc.RData")

Original.T <- matrix(c(rep(c(0.85, 0.7, 0.2, 0.6),times=100), rep(c(0.99, 0.85, 0.2, 0.6),times=100)),ncol=200,nrow=4)
rownames(Original.T) <- c("phin","phib","p1","p2")

# ----- FOR B ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/B-1T.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/B-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/Bs-1T.RData")

iter <- 200

# ===================== CMOMd, CMOMi, CMR =====================

######################
#         N          #
######################

ls.N.1T.RB <- list()
for (ite in 1:iter){
  ls.N.1T.RB$CMOMd[[ite]] <- (N.1T$CMOMd[[ite]]-ls.T.N[[ite]])/ls.T.N[[ite]]
  ls.N.1T.RB$CMOMi[[ite]] <- (N.1T$CMOMi[[ite]]-ls.T.N[[ite]])/ls.T.N[[ite]]
  ls.N.1T.RB$CMR[[ite]] <- (N.1T$CMR[[ite]]-ls.T.N[[ite]])/ls.T.N[[ite]]}

ls.N.1T.SE <- list()
for (ite in 1:iter){
  ls.N.1T.SE$CMOMd[[ite]] <- Ns.1T$CMOMd[[ite]]/ls.T.N[[ite]]
  ls.N.1T.SE$CMOMi[[ite]] <- Ns.1T$CMOMi[[ite]]/ls.T.N[[ite]]
  ls.N.1T.SE$CMR[[ite]] <- Ns.1T$CMR[[ite]]/ls.T.N[[ite]]}

ls.N.1T.MSE <- list()
for (ite in 1:iter){
  ls.N.1T.MSE$CMOMd[[ite]] <- ls.N.1T.RB$CMOMd[[ite]]^2+ls.N.1T.SE$CMOMd[[ite]]^2
  ls.N.1T.MSE$CMOMi[[ite]] <- ls.N.1T.RB$CMOMi[[ite]]^2+ls.N.1T.SE$CMOMi[[ite]]^2
  ls.N.1T.MSE$CMR[[ite]] <- ls.N.1T.RB$CMR[[ite]]^2+ls.N.1T.SE$CMR[[ite]]^2}

######################
#         B          #
######################

ls.B.1T.RB <- list()
for (ite in 1:iter){
  ls.B.1T.RB$CMOMd[[ite]] <- (B.1T$CMOMd[[ite]]-ls.T.B[[ite]])/ls.T.B[[ite]]
  ls.B.1T.RB$CMOMi[[ite]] <- (B.1T$CMOMi[[ite]]-ls.T.B[[ite]])/ls.T.B[[ite]]
  ls.B.1T.RB$CMR[[ite]] <- (B.1T$CMR[[ite]]-ls.T.B[[ite]])/ls.T.B[[ite]]}

ls.B.1T.SE <- list()
for (ite in 1:iter){
  ls.B.1T.SE$CMOMd[[ite]] <- Bs.1T$CMOMd[[ite]]/ls.T.B[[ite]]
  ls.B.1T.SE$CMOMi[[ite]] <- Bs.1T$CMOMi[[ite]]/ls.T.B[[ite]]
  ls.B.1T.SE$CMR[[ite]] <- Bs.1T$CMR[[ite]]/ls.T.B[[ite]]}

ls.B.1T.MSE <- list()
for (ite in 1:iter){
  ls.B.1T.MSE$CMOMd[[ite]] <- ls.B.1T.RB$CMOMd[[ite]]^2+ls.B.1T.SE$CMOMd[[ite]]^2
  ls.B.1T.MSE$CMOMi[[ite]] <- ls.B.1T.RB$CMOMi[[ite]]^2+ls.B.1T.SE$CMOMi[[ite]]^2
  ls.B.1T.MSE$CMR[[ite]] <- ls.B.1T.RB$CMR[[ite]]^2+ls.B.1T.SE$CMR[[ite]]^2}

##############
#   phin     #
##############

output.1T <- output.1T[c(5:10,15:20,25:30),]

ls.phin.1T.RB <- list()
for (j in 1:iter){
  ls.phin.1T.RB$CMOMd[[j]] <- (output.1T[1,j]-Original.T[1,j])/Original.T[1,j]
  ls.phin.1T.RB$CMOMi[[j]] <- (output.1T[7,j]-Original.T[1,j])/Original.T[1,j]
  ls.phin.1T.RB$CMR[[j]] <- (output.1T[13,j]-Original.T[1,j])/Original.T[1,j]}

ls.phin.1T.SE <- list()
for (j in 1:iter){
  ls.phin.1T.SE$CMOMd[[j]] <- output.1T[2,j]/Original.T[1,j]
  ls.phin.1T.SE$CMOMi[[j]] <- output.1T[8,j]/Original.T[1,j]
  ls.phin.1T.SE$CMR[[j]] <- output.1T[14,j]/Original.T[1,j]}

ls.phin.1T.MSE <- list()
for (j in 1:iter){
  ls.phin.1T.MSE$CMOMd[[j]] <- ls.phin.1T.RB$CMOMd[[j]]^2+ls.phin.1T.SE$CMOMd[[j]]^2
  ls.phin.1T.MSE$CMOMi[[j]] <- ls.phin.1T.RB$CMOMi[[j]]^2+ls.phin.1T.SE$CMOMi[[j]]^2
  ls.phin.1T.MSE$CMR[[j]] <- ls.phin.1T.RB$CMR[[j]]^2+ls.phin.1T.SE$CMR[[j]]^2}

##############
#   phib     #
##############

ls.phib.1T.RB <- list()
for (j in 1:iter){
  ls.phib.1T.RB$CMOMd[[j]] <- (output.1T[3,j]-Original.T[2,j])/Original.T[2,j]
  ls.phib.1T.RB$CMOMi[[j]] <- (output.1T[9,j]-Original.T[2,j])/Original.T[2,j]
  ls.phib.1T.RB$CMR[[j]] <- (output.1T[15,j]-Original.T[2,j])/Original.T[2,j]}

ls.phib.1T.SE <- list()
for (j in 1:iter){
  ls.phib.1T.SE$CMOMd[[j]] <- output.1T[4,j]/Original.T[2,j]
  ls.phib.1T.SE$CMOMi[[j]] <- output.1T[10,j]/Original.T[2,j]
  ls.phib.1T.SE$CMR[[j]] <- output.1T[16,j]/Original.T[2,j]}

ls.phib.1T.MSE <- list()
for (j in 1:iter){
  ls.phib.1T.MSE$CMOMd[[j]] <- ls.phib.1T.RB$CMOMd[[j]]^2+ls.phib.1T.SE$CMOMd[[j]]^2
  ls.phib.1T.MSE$CMOMi[[j]] <- ls.phib.1T.RB$CMOMi[[j]]^2+ls.phib.1T.SE$CMOMi[[j]]^2
  ls.phib.1T.MSE$CMR[[j]] <- ls.phib.1T.RB$CMR[[j]]^2+ls.phib.1T.SE$CMR[[j]]^2}

##############
#     p2     #
##############

ls.p2.1T.RB <- list()
for (j in 1:iter){
  ls.p2.1T.RB$CMOMd[[j]] <- (output.1T[5,j]-Original.T[4,j])/Original.T[4,j]
  ls.p2.1T.RB$CMOMi[[j]] <- (output.1T[11,j]-Original.T[4,j])/Original.T[4,j]
  ls.p2.1T.RB$CMR[[j]] <- (output.1T[17,j]-Original.T[4,j])/Original.T[4,j]}

ls.p2.1T.SE <- list()
for (j in 1:iter){
  ls.p2.1T.SE$CMOMd[[j]] <- output.1T[6,j]/Original.T[4,j]
  ls.p2.1T.SE$CMOMi[[j]] <- output.1T[12,j]/Original.T[4,j]
  ls.p2.1T.SE$CMR[[j]] <- output.1T[18,j]/Original.T[4,j]}

ls.p2.1T.MSE <- list()
for (j in 1:iter){
  ls.p2.1T.MSE$CMOMd[[j]] <- ls.p2.1T.RB$CMOMd[[j]]^2+ls.p2.1T.SE$CMOMd[[j]]^2
  ls.p2.1T.MSE$CMOMi[[j]] <- ls.p2.1T.RB$CMOMi[[j]]^2+ls.p2.1T.SE$CMOMi[[j]]^2
  ls.p2.1T.MSE$CMR[[j]] <- ls.p2.1T.RB$CMR[[j]]^2+ls.p2.1T.SE$CMR[[j]]^2}

# ===================== ZPNEc   &   ZPNEs =====================

output.ZPNEc <- T.output.ZPNEc[c(7:10),]
output.ZPNEs <- T.output.ZPNEs[c(7:10),]

######################
#         N          #
######################

ls.N.ZPNE <- list()
for (ite in 1:iter){
  ls.N.ZPNE[[ite]] <- c(mean(ls.T.N[[ite]][1:3]),mean(ls.T.N[[ite]][4:6]),mean(ls.T.N[[ite]][7:9]))}

for (ite in 1:iter){
  ls.N.1T.RB$ZPNEc[[ite]] <- (TN.ZPNEc[[ite]]-ls.N.ZPNE[[ite]])/ls.N.ZPNE[[ite]]
  ls.N.1T.RB$ZPNEs[[ite]] <- (TN.ZPNEs[[ite]]-ls.N.ZPNE[[ite]])/ls.N.ZPNE[[ite]]}

for (ite in 1:iter){
  ls.N.1T.SE$ZPNEc[[ite]] <- TNs.ZPNEc[[ite]]/ls.N.ZPNE[[ite]]
  ls.N.1T.SE$ZPNEs[[ite]] <- TNs.ZPNEs[[ite]]/ls.N.ZPNE[[ite]]}

for (ite in 1:iter){
  ls.N.1T.MSE$ZPNEc[[ite]] <- ls.N.1T.RB$ZPNEc[[ite]]^2+ls.N.1T.SE$ZPNEc[[ite]]^2
  ls.N.1T.MSE$ZPNEs[[ite]] <- ls.N.1T.RB$ZPNEs[[ite]]^2+ls.N.1T.SE$ZPNEs[[ite]]^2}

##############
#   phib     #
##############

for (j in 1:iter){
  ls.phib.1T.RB$ZPNEc[[j]] <- (output.ZPNEc[1,j]-Original.T[2,j])/Original.T[2,j]
  ls.phib.1T.RB$ZPNEs[[j]] <- (output.ZPNEs[1,j]-Original.T[2,j])/Original.T[2,j]}

for (j in 1:iter){
  ls.phib.1T.SE$ZPNEc[[j]] <- output.ZPNEc[2,j]/Original.T[2,j]
  ls.phib.1T.SE$ZPNEs[[j]] <- output.ZPNEs[2,j]/Original.T[2,j]}

for (j in 1:iter){
  ls.phib.1T.MSE$ZPNEc[[j]] <- ls.phib.1T.RB$ZPNEc[[j]]^2+ls.phib.1T.SE$ZPNEc[[j]]^2
  ls.phib.1T.MSE$ZPNEs[[j]] <- ls.phib.1T.RB$ZPNEs[[j]]^2+ls.phib.1T.SE$ZPNEs[[j]]^2}

##############
#     p2     #
##############

for (j in 1:iter){
  ls.p2.1T.RB$ZPNEc[[j]] <- (output.ZPNEc[3,j]-Original.T[4,j])/Original.T[4,j]
  ls.p2.1T.RB$ZPNEs[[j]] <- (output.ZPNEs[3,j]-Original.T[4,j])/Original.T[4,j]}

for (j in 1:iter){
  ls.p2.1T.SE$ZPNEc[[j]] <- output.ZPNEc[4,j]/Original.T[4,j]
  ls.p2.1T.SE$ZPNEs[[j]] <- output.ZPNEs[4,j]/Original.T[4,j]}

for (j in 1:iter){
  ls.p2.1T.MSE$ZPNEc[[j]] <- ls.p2.1T.RB$ZPNEc[[j]]^2+ls.p2.1T.SE$ZPNEc[[j]]^2
  ls.p2.1T.MSE$ZPNEs[[j]] <- ls.p2.1T.RB$ZPNEs[[j]]^2+ls.p2.1T.SE$ZPNEs[[j]]^2}

save.image("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-1T.Rdata")

########################################################
#                       T A B L E                      #
########################################################

ls.Exc1T <- ls.graph <- list()

for(TREN in 1:2){
  
  if (TREN == 1) {ini <- 1 
  last <- 100} # Decreasing
  if (TREN == 2) {ini <- 101 
  last <- 200} # Increasing

# ====================== N ======================= #

N.RB.G.1T <- list()
for (ite in ini:last){
  N.RB.G.1T$CMOMd[[ite]] <- mean(abs(ls.N.1T.RB$CMOMd[[ite]]))
  N.RB.G.1T$CMOMi[[ite]] <- mean(abs(ls.N.1T.RB$CMOMi[[ite]]))
  N.RB.G.1T$CMR[[ite]] <- mean(abs(ls.N.1T.RB$CMR[[ite]]))
  N.RB.G.1T$ZPNEc[[ite]] <- mean(abs(ls.N.1T.RB$ZPNEc[[ite]]))
  N.RB.G.1T$ZPNEs[[ite]] <- mean(abs(ls.N.1T.RB$ZPNEs[[ite]]))}

for (ite in ini:last){
  N.RB.G.1T$CMOMd <- mean(N.RB.G.1T$CMOMd, na.rm = T)
  N.RB.G.1T$CMOMi <- mean(N.RB.G.1T$CMOMi, na.rm = T)
  N.RB.G.1T$CMR <- mean(N.RB.G.1T$CMR, na.rm = T)
  N.RB.G.1T$ZPNEc <- mean(N.RB.G.1T$ZPNEc, na.rm = T)
  N.RB.G.1T$ZPNEs <- mean(N.RB.G.1T$ZPNEs, na.rm = T)}

N.SE.G.1T <- list()
for (ite in ini:last){
  N.SE.G.1T$CMOMd[[ite]] <- mean(ls.N.1T.SE$CMOMd[[ite]])
  N.SE.G.1T$CMOMi[[ite]] <- mean(ls.N.1T.SE$CMOMi[[ite]])
  N.SE.G.1T$CMR[[ite]] <- mean(ls.N.1T.SE$CMR[[ite]])
  N.SE.G.1T$ZPNEc[[ite]] <- mean(ls.N.1T.SE$ZPNEc[[ite]])
  N.SE.G.1T$ZPNEs[[ite]] <- mean(ls.N.1T.SE$ZPNEs[[ite]])}

for (ite in ini:last){
  N.SE.G.1T$CMOMd <- mean(N.SE.G.1T$CMOMd, na.rm = T)
  N.SE.G.1T$CMOMi <- mean(N.SE.G.1T$CMOMi, na.rm = T)
  N.SE.G.1T$CMR <- mean(N.SE.G.1T$CMR, na.rm = T)
  N.SE.G.1T$ZPNEc <- mean(N.SE.G.1T$ZPNEc, na.rm = T)
  N.SE.G.1T$ZPNEs <- mean(N.SE.G.1T$ZPNEs, na.rm = T)}

N.MSE.G.1T <- list()
for (ite in ini:last){
  N.MSE.G.1T$CMOMd[[ite]] <- mean(ls.N.1T.MSE$CMOMd[[ite]])
  N.MSE.G.1T$CMOMi[[ite]] <- mean(ls.N.1T.MSE$CMOMi[[ite]])
  N.MSE.G.1T$CMR[[ite]] <- mean(ls.N.1T.MSE$CMR[[ite]])
  N.MSE.G.1T$ZPNEc[[ite]] <- mean(ls.N.1T.MSE$ZPNEc[[ite]])
  N.MSE.G.1T$ZPNEs[[ite]] <- mean(ls.N.1T.MSE$ZPNEs[[ite]])}

for (ite in ini:last){
  N.MSE.G.1T$CMOMd <- mean(N.MSE.G.1T$CMOMd, na.rm = T)
  N.MSE.G.1T$CMOMi <- mean(N.MSE.G.1T$CMOMi, na.rm = T)
  N.MSE.G.1T$CMR <- mean(N.MSE.G.1T$CMR, na.rm = T)
  N.MSE.G.1T$ZPNEc <- mean(N.MSE.G.1T$ZPNEc, na.rm = T)
  N.MSE.G.1T$ZPNEs <- mean(N.MSE.G.1T$ZPNEs, na.rm = T)}

N.1T <- list(RB=N.RB.G.1T, RSE=N.SE.G.1T, MSE=N.MSE.G.1T)

# ====================== PHIB ======================= #

phib.RB.G.1T <- list()
for (ite in ini:last){
  phib.RB.G.1T$CMOMd <- mean(abs(ls.phib.1T.RB$CMOMd[ini:last]), na.rm = T)
  phib.RB.G.1T$CMOMi <- mean(abs(ls.phib.1T.RB$CMOMi[ini:last]), na.rm = T)
  phib.RB.G.1T$CMR <- mean(abs(ls.phib.1T.RB$CMR[ini:last]), na.rm = T)
  phib.RB.G.1T$ZPNEc <- mean(abs(ls.phib.1T.RB$ZPNEc[ini:last]), na.rm = T)
  phib.RB.G.1T$ZPNEs <- mean(abs(ls.phib.1T.RB$ZPNEs[ini:last]), na.rm = T)}

phib.SE.G.1T <- list()
for (ite in ini:last){
  phib.SE.G.1T$CMOMd <- mean(abs(ls.phib.1T.SE$CMOMd[ini:last]), na.rm = T)
  phib.SE.G.1T$CMOMi <- mean(abs(ls.phib.1T.SE$CMOMi[ini:last]), na.rm = T)
  phib.SE.G.1T$CMR <- mean(abs(ls.phib.1T.SE$CMR[ini:last]), na.rm = T)
  phib.SE.G.1T$ZPNEc <- mean(abs(ls.phib.1T.SE$ZPNEc[ini:last]), na.rm = T)
  phib.SE.G.1T$ZPNEs <- mean(abs(ls.phib.1T.SE$ZPNEs[ini:last]), na.rm = T)}

phib.MSE.G.1T <- list()
for (ite in ini:last){
  phib.MSE.G.1T$CMOMd <- mean(abs(ls.phib.1T.MSE$CMOMd[ini:last]), na.rm = T)
  phib.MSE.G.1T$CMOMi <- mean(abs(ls.phib.1T.MSE$CMOMi[ini:last]), na.rm = T)
  phib.MSE.G.1T$CMR <- mean(abs(ls.phib.1T.MSE$CMR[ini:last]), na.rm = T)
  phib.MSE.G.1T$ZPNEc <- mean(abs(ls.phib.1T.MSE$ZPNEc[ini:last]), na.rm = T)
  phib.MSE.G.1T$ZPNEs <- mean(abs(ls.phib.1T.MSE$ZPNEs[ini:last]), na.rm = T)}

phib.1T <- list(RB=phib.RB.G.1T, RSE=phib.SE.G.1T, MSE=phib.MSE.G.1T)


# ====================== P2 ======================= #

p2.RB.G.1T <- list()
for (ite in ini:last){
  p2.RB.G.1T$CMOMd <- mean(abs(ls.p2.1T.RB$CMOMd[ini:last]), na.rm = T)
  p2.RB.G.1T$CMOMi <- mean(abs(ls.p2.1T.RB$CMOMi[ini:last]), na.rm = T)
  p2.RB.G.1T$CMR <- mean(abs(ls.p2.1T.RB$CMR[ini:last]), na.rm = T)
  p2.RB.G.1T$ZPNEc <- mean(abs(ls.p2.1T.RB$ZPNEc[ini:last]), na.rm = T)
  p2.RB.G.1T$ZPNEs <- mean(abs(ls.p2.1T.RB$ZPNEs[ini:last]), na.rm = T)}

p2.SE.G.1T <- list()
for (ite in ini:last){
  p2.SE.G.1T$CMOMd <- mean(abs(ls.p2.1T.SE$CMOMd[ini:last]), na.rm = T)
  p2.SE.G.1T$CMOMi <- mean(abs(ls.p2.1T.SE$CMOMi[ini:last]), na.rm = T)
  p2.SE.G.1T$CMR <- mean(abs(ls.p2.1T.SE$CMR[ini:last]), na.rm = T)
  p2.SE.G.1T$ZPNEc <- mean(abs(ls.p2.1T.SE$ZPNEc[ini:last]), na.rm = T)
  p2.SE.G.1T$ZPNEs <- mean(abs(ls.p2.1T.SE$ZPNEs[ini:last]), na.rm = T)}

p2.MSE.G.1T <- list()
for (ite in ini:last){
  p2.MSE.G.1T$CMOMd <- mean(abs(ls.p2.1T.MSE$CMOMd[ini:last]), na.rm = T)
  p2.MSE.G.1T$CMOMi <- mean(abs(ls.p2.1T.MSE$CMOMi[ini:last]), na.rm = T)
  p2.MSE.G.1T$CMR <- mean(abs(ls.p2.1T.MSE$CMR[ini:last]), na.rm = T)
  p2.MSE.G.1T$ZPNEc <- mean(abs(ls.p2.1T.MSE$ZPNEc[ini:last]), na.rm = T)
  p2.MSE.G.1T$ZPNEs <- mean(abs(ls.p2.1T.MSE$ZPNEs[ini:last]), na.rm = T)}

p2.1T <- list(RB=p2.RB.G.1T, RSE=p2.SE.G.1T, MSE=p2.MSE.G.1T)

# ====================== B ======================= #

if (TREN == 1)
{# CLEANING INF VALUES #
ls.B.1T.RB$CMOMd[[16]][2] <- NA
ls.B.1T.RB$CMOMi[[16]][2] <- NA
ls.B.1T.RB$CMR[[16]][2] <- NA

ls.B.1T.RB$CMOMd[[30]][2] <- NA
ls.B.1T.RB$CMOMi[[30]][2] <- NA
ls.B.1T.RB$CMR[[30]][2] <- NA

ls.B.1T.RB$CMOMd[[33]][2] <- NA
ls.B.1T.RB$CMOMi[[33]][2] <- NA
ls.B.1T.RB$CMR[[33]][2] <- NA

ls.B.1T.RB$CMOMd[[67]][2] <- NA
ls.B.1T.RB$CMOMi[[67]][2] <- NA
ls.B.1T.RB$CMR[[67]][2] <- NA

# CLEANING INF VALUES #
ls.B.1T.SE$CMOMd[[16]][2] <- NA
ls.B.1T.SE$CMOMi[[16]][2] <- NA
ls.B.1T.SE$CMR[[16]][2] <- NA

ls.B.1T.SE$CMOMd[[30]][2] <- NA
ls.B.1T.SE$CMOMi[[30]][2] <- NA
ls.B.1T.SE$CMR[[30]][2] <- NA

ls.B.1T.SE$CMOMd[[33]][2] <- NA
ls.B.1T.SE$CMOMi[[33]][2] <- NA
ls.B.1T.SE$CMR[[33]][2] <- NA

ls.B.1T.SE$CMOMd[[67]][2] <- NA
ls.B.1T.SE$CMOMi[[67]][2] <- NA
ls.B.1T.SE$CMR[[67]][2] <- NA

# CLEANING INF VALUES #
ls.B.1T.MSE$CMOMd[[16]][2] <- NA
ls.B.1T.MSE$CMOMi[[16]][2] <- NA
ls.B.1T.MSE$CMR[[16]][2] <- NA

ls.B.1T.MSE$CMOMd[[30]][2] <- NA
ls.B.1T.MSE$CMOMi[[30]][2] <- NA
ls.B.1T.MSE$CMR[[30]][2] <- NA

ls.B.1T.MSE$CMOMd[[33]][2] <- NA
ls.B.1T.MSE$CMOMi[[33]][2] <- NA
ls.B.1T.MSE$CMR[[33]][2] <- NA

ls.B.1T.MSE$CMOMd[[67]][2] <- NA
ls.B.1T.MSE$CMOMi[[67]][2] <- NA
ls.B.1T.MSE$CMR[[67]][2] <- NA}

B.RB.G.1T <- list()
for (ite in ini:last){
  B.RB.G.1T$CMOMd[[ite]] <- mean(abs(ls.B.1T.RB$CMOMd[[ite]]))
  B.RB.G.1T$CMOMi[[ite]] <- mean(abs(ls.B.1T.RB$CMOMi[[ite]]))
  B.RB.G.1T$CMR[[ite]] <- mean(abs(ls.B.1T.RB$CMR[[ite]]))
  B.RB.G.1T$ZPNEc[[ite]] <- NA
  B.RB.G.1T$ZPNEs[[ite]] <- NA}

for (ite in ini:last){
  B.RB.G.1T$CMOMd <- mean(B.RB.G.1T$CMOMd, na.rm = T)
  B.RB.G.1T$CMOMi <- mean(B.RB.G.1T$CMOMi, na.rm = T)
  B.RB.G.1T$CMR <- mean(B.RB.G.1T$CMR, na.rm = T)
  B.RB.G.1T$ZPNEc <- NA
  B.RB.G.1T$ZPNEs <- NA}

B.SE.G.1T <- list()
for (ite in ini:last){
  B.SE.G.1T$CMOMd[[ite]] <- mean(ls.B.1T.SE$CMOMd[[ite]])
  B.SE.G.1T$CMOMi[[ite]] <- mean(ls.B.1T.SE$CMOMi[[ite]])
  B.SE.G.1T$CMR[[ite]] <- mean(ls.B.1T.SE$CMR[[ite]])
  B.SE.G.1T$ZPNEc[[ite]] <- NA
  B.SE.G.1T$ZPNEs[[ite]] <- NA}

for (ite in ini:last){
  B.SE.G.1T$CMOMd <- mean(B.SE.G.1T$CMOMd, na.rm = T)
  B.SE.G.1T$CMOMi <- mean(B.SE.G.1T$CMOMi, na.rm = T)
  B.SE.G.1T$CMR <- mean(B.SE.G.1T$CMR, na.rm = T)
  B.SE.G.1T$ZPNEc <- NA
  B.SE.G.1T$ZPNEs <- NA}

B.MSE.G.1T <- list()
for (ite in ini:last){
  B.MSE.G.1T$CMOMd[[ite]] <- mean(ls.B.1T.MSE$CMOMd[[ite]])
  B.MSE.G.1T$CMOMi[[ite]] <- mean(ls.B.1T.MSE$CMOMi[[ite]])
  B.MSE.G.1T$CMR[[ite]] <- mean(ls.B.1T.MSE$CMR[[ite]])
  B.MSE.G.1T$ZPNEc[[ite]] <- NA
  B.MSE.G.1T$ZPNEs[[ite]] <- NA}

for (ite in ini:last){
  B.MSE.G.1T$CMOMd <- mean(B.MSE.G.1T$CMOMd, na.rm = T)
  B.MSE.G.1T$CMOMi <- mean(B.MSE.G.1T$CMOMi, na.rm = T)
  B.MSE.G.1T$CMR <- mean(B.MSE.G.1T$CMR, na.rm = T)
  B.MSE.G.1T$ZPNEc <- NA
  B.MSE.G.1T$ZPNEs <- NA}

B.1T <- list(RB=B.RB.G.1T, RSE=B.SE.G.1T, MSE=B.MSE.G.1T)

# ====================== PHIN ======================= #

phin.RB.G.1T <- list()
for (ite in ini:last){
  phin.RB.G.1T$CMOMd <- mean(abs(ls.phin.1T.RB$CMOMd[ini:last]), na.rm = T)
  phin.RB.G.1T$CMOMi <- mean(abs(ls.phin.1T.RB$CMOMi[ini:last]), na.rm = T)
  phin.RB.G.1T$CMR <- mean(abs(ls.phin.1T.RB$CMR[ini:last]), na.rm = T)
  phin.RB.G.1T$ZPNEc <- NA
  phin.RB.G.1T$ZPNEs <- NA}

phin.SE.G.1T <- list()
for (ite in ini:last){
  phin.SE.G.1T$CMOMd <- mean(abs(ls.phin.1T.SE$CMOMd[ini:last]), na.rm = T)
  phin.SE.G.1T$CMOMi <- mean(abs(ls.phin.1T.SE$CMOMi[ini:last]), na.rm = T)
  phin.SE.G.1T$CMR <- mean(abs(ls.phin.1T.SE$CMR[ini:last]), na.rm = T)
  phin.SE.G.1T$ZPNEc <- NA
  phin.SE.G.1T$ZPNEs <- NA}

phin.MSE.G.1T <- list()
for (ite in ini:last){
  phin.MSE.G.1T$CMOMd <- mean(abs(ls.phin.1T.MSE$CMOMd[ini:last]), na.rm = T)
  phin.MSE.G.1T$CMOMi <- mean(abs(ls.phin.1T.MSE$CMOMi[ini:last]), na.rm = T)
  phin.MSE.G.1T$CMR <- mean(abs(ls.phin.1T.MSE$CMR[ini:last]), na.rm = T)
  phin.MSE.G.1T$ZPNEc <- NA
  phin.MSE.G.1T$ZPNEs <- NA}

phin.1T <- list(RB=phin.RB.G.1T, RSE=phin.SE.G.1T, MSE=phin.MSE.G.1T)

# ================= DATA FRAME =================

# ~~~~~ N ~~~~~
stack.N.1T <- stack(N.1T)
stack.N.1T$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.N.1T)[1:2] <- c("Values", "Results")
if (TREN == 1) {stack.N.1T$P.factor <- factor("Decreasing")}
if (TREN == 2) {stack.N.1T$P.factor <- factor("Increasing")}
stack.N.1T$Parameters <- factor("Number of Individuals")
stack.N.1T$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ B ~~~~~
stack.B.1T <- stack(B.1T)
stack.B.1T$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.B.1T)[1:2] <- c("Values", "Results")
if (TREN == 1) {stack.B.1T$P.factor <- factor("Decreasing")}
if (TREN == 2) {stack.B.1T$P.factor <- factor("Increasing")}
stack.B.1T$Parameters <- factor("Recruitment")
stack.B.1T$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ phin ~~~~~
stack.phin.1T <- stack(phin.1T)
stack.phin.1T$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.phin.1T)[1:2] <- c("Values", "Results")
if (TREN == 1) {stack.phin.1T$P.factor <- factor("Decreasing")}
if (TREN == 2) {stack.phin.1T$P.factor <- factor("Increasing")}
stack.phin.1T$Parameters <- factor("Survival Second. Occ.")
stack.phin.1T$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ phib ~~~~~
stack.phib.1T <- stack(phib.1T)
stack.phib.1T$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.phib.1T)[1:2] <- c("Values", "Results")
if (TREN == 1) {stack.phib.1T$P.factor <- factor("Decreasing")}
if (TREN == 2) {stack.phib.1T$P.factor <- factor("Increasing")}
stack.phib.1T$Parameters <- factor("Survival Prim. Occ.")
stack.phib.1T$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ p2 ~~~~~
stack.p2.1T <- stack(p2.1T)
stack.p2.1T$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.p2.1T)[1:2] <- c("Values", "Results")
if (TREN == 1) {stack.p2.1T$P.factor <- factor("Decreasing")}
if (TREN == 2) {stack.p2.1T$P.factor <- factor("Increasing")}
stack.p2.1T$Parameters <- factor("Observation probability")
stack.p2.1T$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

graph <- rbind(stack.N.1T, stack.B.1T, stack.phin.1T, stack.phib.1T, stack.p2.1T)

ls.graph[[TREN]] <- assign(paste("graph", TREN, sep=""), graph)
ls.Exc1T[[TREN]] <- subset(graph, !(graph$Value < 2 & graph$Value > -2))}

save(ls.graph,file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-1T.Rdata")
save(ls.Exc1T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Exc-1T.Rdata") 