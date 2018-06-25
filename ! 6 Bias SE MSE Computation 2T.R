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

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/N-2T.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/N-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/TN-ZPNEc.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/TN-ZPNEs.RData")

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/Ns-2T.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/TNs-ZPNEc.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/TNs-ZPNEs.RData")

# ----- FOR phi and p ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/output2T.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/T-outputZPNEs.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/T-outputZPNEc.RData")

Original.T <- matrix(c(rep(c(0.85, 0.7, 0.2, 0.6),times=100), rep(c(0.99, 0.85, 0.2, 0.6),times=100)),ncol=200,nrow=4)
rownames(Original.T) <- c("phin","phib","p1","p2")

# ----- FOR B ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/B-2T.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/B-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/Bs-2T.RData")

iter <- 200

# ===================== CMOMd, CMOMi, CMR =====================

######################
#         N          #
######################

ls.N.2T.RB <- list()
for (ite in 1:iter){
  ls.N.2T.RB$CMOMd[[ite]] <- (N.2T$CMOMd[[ite]]-ls.T.N[[ite]])/ls.T.N[[ite]]
  ls.N.2T.RB$CMOMi[[ite]] <- (N.2T$CMOMi[[ite]]-ls.T.N[[ite]])/ls.T.N[[ite]]
  ls.N.2T.RB$CMR[[ite]] <- (N.2T$CMR[[ite]]-ls.T.N[[ite]])/ls.T.N[[ite]]}

ls.N.2T.SE <- list()
for (ite in 1:iter){
  ls.N.2T.SE$CMOMd[[ite]] <- Ns.2T$CMOMd[[ite]]/ls.T.N[[ite]]
  ls.N.2T.SE$CMOMi[[ite]] <- Ns.2T$CMOMi[[ite]]/ls.T.N[[ite]]
  ls.N.2T.SE$CMR[[ite]] <- Ns.2T$CMR[[ite]]/ls.T.N[[ite]]}

ls.N.2T.MSE <- list()
for (ite in 1:iter){
  ls.N.2T.MSE$CMOMd[[ite]] <- ls.N.2T.RB$CMOMd[[ite]]^2+ls.N.2T.SE$CMOMd[[ite]]^2
  ls.N.2T.MSE$CMOMi[[ite]] <- ls.N.2T.RB$CMOMi[[ite]]^2+ls.N.2T.SE$CMOMi[[ite]]^2
  ls.N.2T.MSE$CMR[[ite]] <- ls.N.2T.RB$CMR[[ite]]^2+ls.N.2T.SE$CMR[[ite]]^2}

######################
#         B          #
######################

ls.B.2T.RB <- list()
for (ite in 1:iter){
  ls.B.2T.RB$CMOMd[[ite]] <- (B.2T$CMOMd[[ite]]-ls.T.B[[ite]])/ls.T.B[[ite]]
  ls.B.2T.RB$CMOMi[[ite]] <- (B.2T$CMOMi[[ite]]-ls.T.B[[ite]])/ls.T.B[[ite]]
  ls.B.2T.RB$CMR[[ite]] <- (B.2T$CMR[[ite]]-ls.T.B[[ite]])/ls.T.B[[ite]]}

ls.B.2T.SE <- list()
for (ite in 1:iter){
  ls.B.2T.SE$CMOMd[[ite]] <- Bs.2T$CMOMd[[ite]]/ls.T.B[[ite]]
  ls.B.2T.SE$CMOMi[[ite]] <- Bs.2T$CMOMi[[ite]]/ls.T.B[[ite]]
  ls.B.2T.SE$CMR[[ite]] <- Bs.2T$CMR[[ite]]/ls.T.B[[ite]]}

ls.B.2T.MSE <- list()
for (ite in 1:iter){
  ls.B.2T.MSE$CMOMd[[ite]] <- ls.B.2T.RB$CMOMd[[ite]]^2+ls.B.2T.SE$CMOMd[[ite]]^2
  ls.B.2T.MSE$CMOMi[[ite]] <- ls.B.2T.RB$CMOMi[[ite]]^2+ls.B.2T.SE$CMOMi[[ite]]^2
  ls.B.2T.MSE$CMR[[ite]] <- ls.B.2T.RB$CMR[[ite]]^2+ls.B.2T.SE$CMR[[ite]]^2}

##############
#   phin     #
##############

output.2T <- output.2T[c(5:12,17:24,29:36),]

ls.phin.2T.RB <- list()
for (j in 1:iter){
  ls.phin.2T.RB$CMOMd[[j]] <- (output.2T[1,j]-Original.T[1,j])/Original.T[1,j]
  ls.phin.2T.RB$CMOMi[[j]] <- (output.2T[9,j]-Original.T[1,j])/Original.T[1,j]
  ls.phin.2T.RB$CMR[[j]] <- (output.2T[17,j]-Original.T[1,j])/Original.T[1,j]}

ls.phin.2T.SE <- list()
for (j in 1:iter){
  ls.phin.2T.SE$CMOMd[[j]] <- output.2T[2,j]/Original.T[1,j]
  ls.phin.2T.SE$CMOMi[[j]] <- output.2T[10,j]/Original.T[1,j]
  ls.phin.2T.SE$CMR[[j]] <- output.2T[18,j]/Original.T[1,j]}

ls.phin.2T.MSE <- list()
for (j in 1:iter){
  ls.phin.2T.MSE$CMOMd[[j]] <- ls.phin.2T.RB$CMOMd[[j]]^2+ls.phin.2T.SE$CMOMd[[j]]^2
  ls.phin.2T.MSE$CMOMi[[j]] <- ls.phin.2T.RB$CMOMi[[j]]^2+ls.phin.2T.SE$CMOMi[[j]]^2
  ls.phin.2T.MSE$CMR[[j]] <- ls.phin.2T.RB$CMR[[j]]^2+ls.phin.2T.SE$CMR[[j]]^2}

##############
#   phib     #
##############

ls.phib.2T.RB <- list()
for (j in 1:iter){
  ls.phib.2T.RB$CMOMd[[j]] <- (output.2T[3,j]-Original.T[2,j])/Original.T[2,j]
  ls.phib.2T.RB$CMOMi[[j]] <- (output.2T[11,j]-Original.T[2,j])/Original.T[2,j]
  ls.phib.2T.RB$CMR[[j]] <- (output.2T[19,j]-Original.T[2,j])/Original.T[2,j]}

ls.phib.2T.SE <- list()
for (j in 1:iter){
  ls.phib.2T.SE$CMOMd[[j]] <- output.2T[4,j]/Original.T[2,j]
  ls.phib.2T.SE$CMOMi[[j]] <- output.2T[12,j]/Original.T[2,j]
  ls.phib.2T.SE$CMR[[j]] <- output.2T[20,j]/Original.T[2,j]}

ls.phib.2T.MSE <- list()
for (j in 1:iter){
  ls.phib.2T.MSE$CMOMd[[j]] <- ls.phib.2T.RB$CMOMd[[j]]^2+ls.phib.2T.SE$CMOMd[[j]]^2
  ls.phib.2T.MSE$CMOMi[[j]] <- ls.phib.2T.RB$CMOMi[[j]]^2+ls.phib.2T.SE$CMOMi[[j]]^2
  ls.phib.2T.MSE$CMR[[j]] <- ls.phib.2T.RB$CMR[[j]]^2+ls.phib.2T.SE$CMR[[j]]^2}

##############
#     p2     #
##############

ls.p2.2T.RB <- list()
for (j in 1:iter){
  ls.p2.2T.RB$CMOMd[[j]] <- (output.2T[5,j]-Original.T[4,j])/Original.T[4,j]
  ls.p2.2T.RB$CMOMi[[j]] <- (output.2T[13,j]-Original.T[4,j])/Original.T[4,j]
  ls.p2.2T.RB$CMR[[j]] <- (output.2T[21,j]-Original.T[4,j])/Original.T[4,j]}

ls.p2.2T.SE <- list()
for (j in 1:iter){
  ls.p2.2T.SE$CMOMd[[j]] <- output.2T[6,j]/Original.T[4,j]
  ls.p2.2T.SE$CMOMi[[j]] <- output.2T[14,j]/Original.T[4,j]
  ls.p2.2T.SE$CMR[[j]] <- output.2T[22,j]/Original.T[4,j]}

ls.p2.2T.MSE <- list()
for (j in 1:iter){
  ls.p2.2T.MSE$CMOMd[[j]] <- ls.p2.2T.RB$CMOMd[[j]]^2+ls.p2.2T.SE$CMOMd[[j]]^2
  ls.p2.2T.MSE$CMOMi[[j]] <- ls.p2.2T.RB$CMOMi[[j]]^2+ls.p2.2T.SE$CMOMi[[j]]^2
  ls.p2.2T.MSE$CMR[[j]] <- ls.p2.2T.RB$CMR[[j]]^2+ls.p2.2T.SE$CMR[[j]]^2}

##############
#     p1     #
##############

ls.p1.2T.RB <- list()
for (j in 1:iter){
  ls.p1.2T.RB$CMOMd[[j]] <- (output.2T[7,j]-Original.T[3,j])/Original.T[3,j]
  ls.p1.2T.RB$CMOMi[[j]] <- (output.2T[15,j]-Original.T[3,j])/Original.T[3,j]
  ls.p1.2T.RB$CMR[[j]] <- (output.2T[23,j]-Original.T[3,j])/Original.T[3,j]}

ls.p1.2T.SE <- list()
for (j in 1:iter){
  ls.p1.2T.SE$CMOMd[[j]] <- output.2T[8,j]/Original.T[3,j]
  ls.p1.2T.SE$CMOMi[[j]] <- output.2T[16,j]/Original.T[3,j]
  ls.p1.2T.SE$CMR[[j]] <- output.2T[24,j]/Original.T[3,j]}

ls.p1.2T.MSE <- list()
for (j in 1:iter){
  ls.p1.2T.MSE$CMOMd[[j]] <- ls.p1.2T.RB$CMOMd[[j]]^2+ls.p1.2T.SE$CMOMd[[j]]^2
  ls.p1.2T.MSE$CMOMi[[j]] <- ls.p1.2T.RB$CMOMi[[j]]^2+ls.p1.2T.SE$CMOMi[[j]]^2
  ls.p1.2T.MSE$CMR[[j]] <- ls.p1.2T.RB$CMR[[j]]^2+ls.p1.2T.SE$CMR[[j]]^2}

save.image("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-2T.Rdata")

########################################################
#                       T A B L E                      #
########################################################

ls.Exc2T <- ls.graph2 <- list()

for(TREN in 1:2){
  
  if (TREN == 1) {ini <- 1 
  last <- 100} # Decreasing
  if (TREN == 2) {ini <- 101 
  last <- 200} # Increasing
  
  # ====================== N ======================= #
  
  N.RB.G.2T <- list()
  for (ite in ini:last){
    N.RB.G.2T$CMOMd[[ite]] <- mean(abs(ls.N.2T.RB$CMOMd[[ite]]))
    N.RB.G.2T$CMOMi[[ite]] <- mean(abs(ls.N.2T.RB$CMOMi[[ite]]))
    N.RB.G.2T$CMR[[ite]] <- mean(abs(ls.N.2T.RB$CMR[[ite]]))
    N.RB.G.2T$ZPNEc[[ite]] <- NA
    N.RB.G.2T$ZPNEs[[ite]] <- NA}
  
  for (ite in ini:last){
    N.RB.G.2T$CMOMd <- mean(N.RB.G.2T$CMOMd, na.rm = T)
    N.RB.G.2T$CMOMi <- mean(N.RB.G.2T$CMOMi, na.rm = T)
    N.RB.G.2T$CMR <- mean(N.RB.G.2T$CMR, na.rm = T)
    N.RB.G.2T$ZPNEc <- NA
    N.RB.G.2T$ZPNEs <- NA}
  
  N.SE.G.2T <- list()
  for (ite in ini:last){
    N.SE.G.2T$CMOMd[[ite]] <- mean(ls.N.2T.SE$CMOMd[[ite]])
    N.SE.G.2T$CMOMi[[ite]] <- mean(ls.N.2T.SE$CMOMi[[ite]])
    N.SE.G.2T$CMR[[ite]] <- mean(ls.N.2T.SE$CMR[[ite]])
    N.SE.G.2T$ZPNEc[[ite]] <- NA
    N.SE.G.2T$ZPNEs[[ite]] <- NA}
  
  for (ite in ini:last){
    N.SE.G.2T$CMOMd <- mean(N.SE.G.2T$CMOMd, na.rm = T)
    N.SE.G.2T$CMOMi <- mean(N.SE.G.2T$CMOMi, na.rm = T)
    N.SE.G.2T$CMR <- mean(N.SE.G.2T$CMR, na.rm = T)
    N.SE.G.2T$ZPNEc <- NA
    N.SE.G.2T$ZPNEs <- NA}
  
  N.MSE.G.2T <- list()
  for (ite in ini:last){
    N.MSE.G.2T$CMOMd[[ite]] <- mean(ls.N.2T.MSE$CMOMd[[ite]])
    N.MSE.G.2T$CMOMi[[ite]] <- mean(ls.N.2T.MSE$CMOMi[[ite]])
    N.MSE.G.2T$CMR[[ite]] <- mean(ls.N.2T.MSE$CMR[[ite]])
    N.MSE.G.2T$ZPNEc[[ite]] <- NA
    N.MSE.G.2T$ZPNEs[[ite]] <- NA}
  
  for (ite in ini:last){
    N.MSE.G.2T$CMOMd <- mean(N.MSE.G.2T$CMOMd, na.rm = T)
    N.MSE.G.2T$CMOMi <- mean(N.MSE.G.2T$CMOMi, na.rm = T)
    N.MSE.G.2T$CMR <- mean(N.MSE.G.2T$CMR, na.rm = T)
    N.MSE.G.2T$ZPNEc <- NA
    N.MSE.G.2T$ZPNEs <- NA}
  
  N.2T <- list(RB=N.RB.G.2T, RSE=N.SE.G.2T, MSE=N.MSE.G.2T)
  
  # ====================== PHIB ======================= #
  
  phib.RB.G.2T <- list()
  for (ite in ini:last){
    phib.RB.G.2T$CMOMd <- mean(abs(ls.phib.2T.RB$CMOMd[ini:last]), na.rm = T)
    phib.RB.G.2T$CMOMi <- mean(abs(ls.phib.2T.RB$CMOMi[ini:last]), na.rm = T)
    phib.RB.G.2T$CMR <- mean(abs(ls.phib.2T.RB$CMR[ini:last]), na.rm = T)
    phib.RB.G.2T$ZPNEc <- NA
    phib.RB.G.2T$ZPNEs <- NA}
  
  phib.SE.G.2T <- list()
  for (ite in ini:last){
    phib.SE.G.2T$CMOMd <- mean(abs(ls.phib.2T.SE$CMOMd[ini:last]), na.rm = T)
    phib.SE.G.2T$CMOMi <- mean(abs(ls.phib.2T.SE$CMOMi[ini:last]), na.rm = T)
    phib.SE.G.2T$CMR <- mean(abs(ls.phib.2T.SE$CMR[ini:last]), na.rm = T)
    phib.SE.G.2T$ZPNEc <- NA
    phib.SE.G.2T$ZPNEs <- NA}
  
  phib.MSE.G.2T <- list()
  for (ite in ini:last){
    phib.MSE.G.2T$CMOMd <- mean(abs(ls.phib.2T.MSE$CMOMd[ini:last]), na.rm = T)
    phib.MSE.G.2T$CMOMi <- mean(abs(ls.phib.2T.MSE$CMOMi[ini:last]), na.rm = T)
    phib.MSE.G.2T$CMR <- mean(abs(ls.phib.2T.MSE$CMR[ini:last]), na.rm = T)
    phib.MSE.G.2T$ZPNEc <- NA
    phib.MSE.G.2T$ZPNEs <- NA}
  
  phib.2T <- list(RB=phib.RB.G.2T, RSE=phib.SE.G.2T, MSE=phib.MSE.G.2T)
  
  # ====================== P2 ======================= #
  
  p2.RB.G.2T <- list()
  for (ite in ini:last){
    p2.RB.G.2T$CMOMd <- mean(abs(ls.p2.2T.RB$CMOMd[ini:last]), na.rm = T)
    p2.RB.G.2T$CMOMi <- mean(abs(ls.p2.2T.RB$CMOMi[ini:last]), na.rm = T)
    p2.RB.G.2T$CMR <- mean(abs(ls.p2.2T.RB$CMR[ini:last]), na.rm = T)
    p2.RB.G.2T$ZPNEc <- NA
    p2.RB.G.2T$ZPNEs <- NA}
  
  p2.SE.G.2T <- list()
  for (ite in ini:last){
    p2.SE.G.2T$CMOMd <- mean(abs(ls.p2.2T.SE$CMOMd[ini:last]), na.rm = T)
    p2.SE.G.2T$CMOMi <- mean(abs(ls.p2.2T.SE$CMOMi[ini:last]), na.rm = T)
    p2.SE.G.2T$CMR <- mean(abs(ls.p2.2T.SE$CMR[ini:last]), na.rm = T)
    p2.SE.G.2T$ZPNEc <- NA
    p2.SE.G.2T$ZPNEs <- NA}
  
  p2.MSE.G.2T <- list()
  for (ite in ini:last){
    p2.MSE.G.2T$CMOMd <- mean(abs(ls.p2.2T.MSE$CMOMd[ini:last]), na.rm = T)
    p2.MSE.G.2T$CMOMi <- mean(abs(ls.p2.2T.MSE$CMOMi[ini:last]), na.rm = T)
    p2.MSE.G.2T$CMR <- mean(abs(ls.p2.2T.MSE$CMR[ini:last]), na.rm = T)
    p2.MSE.G.2T$ZPNEc <- NA
    p2.MSE.G.2T$ZPNEs <- NA}
  
  p2.2T <- list(RB=p2.RB.G.2T, RSE=p2.SE.G.2T, MSE=p2.MSE.G.2T)
  
  # ====================== P1 ======================= #
  
  p1.RB.G.2T <- list()
  for (ite in ini:last){
    p1.RB.G.2T$CMOMd <- mean(abs(ls.p1.2T.RB$CMOMd[ini:last]), na.rm = T)
    p1.RB.G.2T$CMOMi <- mean(abs(ls.p1.2T.RB$CMOMi[ini:last]), na.rm = T)
    p1.RB.G.2T$CMR <- mean(abs(ls.p1.2T.RB$CMR[ini:last]), na.rm = T)
    p1.RB.G.2T$ZPNEc <- NA
    p1.RB.G.2T$ZPNEs <- NA}
  
  p1.SE.G.2T <- list()
  for (ite in ini:last){
    p1.SE.G.2T$CMOMd <- mean(abs(ls.p1.2T.SE$CMOMd[ini:last]), na.rm = T)
    p1.SE.G.2T$CMOMi <- mean(abs(ls.p1.2T.SE$CMOMi[ini:last]), na.rm = T)
    p1.SE.G.2T$CMR <- mean(abs(ls.p1.2T.SE$CMR[ini:last]), na.rm = T)
    p1.SE.G.2T$ZPNEc <- NA
    p1.SE.G.2T$ZPNEs <- NA}
  
  p1.MSE.G.2T <- list()
  for (ite in ini:last){
    p1.MSE.G.2T$CMOMd <- mean(abs(ls.p1.2T.MSE$CMOMd[ini:last]), na.rm = T)
    p1.MSE.G.2T$CMOMi <- mean(abs(ls.p1.2T.MSE$CMOMi[ini:last]), na.rm = T)
    p1.MSE.G.2T$CMR <- mean(abs(ls.p1.2T.MSE$CMR[ini:last]), na.rm = T)
    p1.MSE.G.2T$ZPNEc <- NA
    p1.MSE.G.2T$ZPNEs <- NA}
  
  p1.2T <- list(RB=p1.RB.G.2T, RSE=p1.SE.G.2T, MSE=p1.MSE.G.2T)
  
  # ====================== B ======================= #
  
  if (TREN == 1)
  {# CLEANING INF VALUES #
    ls.B.2T.RB$CMOMd[[16]][2] <- NA
    ls.B.2T.RB$CMOMi[[16]][2] <- NA
    ls.B.2T.RB$CMR[[16]][2] <- NA
    
    ls.B.2T.RB$CMOMd[[30]][2] <- NA
    ls.B.2T.RB$CMOMi[[30]][2] <- NA
    ls.B.2T.RB$CMR[[30]][2] <- NA
    
    ls.B.2T.RB$CMOMd[[33]][2] <- NA
    ls.B.2T.RB$CMOMi[[33]][2] <- NA
    ls.B.2T.RB$CMR[[33]][2] <- NA
    
    ls.B.2T.RB$CMOMd[[67]][2] <- NA
    ls.B.2T.RB$CMOMi[[67]][2] <- NA
    ls.B.2T.RB$CMR[[67]][2] <- NA
    
    # CLEANING INF VALUES #
    ls.B.2T.SE$CMOMd[[16]][2] <- NA
    ls.B.2T.SE$CMOMi[[16]][2] <- NA
    ls.B.2T.SE$CMR[[16]][2] <- NA
    
    ls.B.2T.SE$CMOMd[[30]][2] <- NA
    ls.B.2T.SE$CMOMi[[30]][2] <- NA
    ls.B.2T.SE$CMR[[30]][2] <- NA
    
    ls.B.2T.SE$CMOMd[[33]][2] <- NA
    ls.B.2T.SE$CMOMi[[33]][2] <- NA
    ls.B.2T.SE$CMR[[33]][2] <- NA
    
    ls.B.2T.SE$CMOMd[[67]][2] <- NA
    ls.B.2T.SE$CMOMi[[67]][2] <- NA
    ls.B.2T.SE$CMR[[67]][2] <- NA
    
    # CLEANING INF VALUES #
    ls.B.2T.MSE$CMOMd[[16]][2] <- NA
    ls.B.2T.MSE$CMOMi[[16]][2] <- NA
    ls.B.2T.MSE$CMR[[16]][2] <- NA
    
    ls.B.2T.MSE$CMOMd[[30]][2] <- NA
    ls.B.2T.MSE$CMOMi[[30]][2] <- NA
    ls.B.2T.MSE$CMR[[30]][2] <- NA
    
    ls.B.2T.MSE$CMOMd[[33]][2] <- NA
    ls.B.2T.MSE$CMOMi[[33]][2] <- NA
    ls.B.2T.MSE$CMR[[33]][2] <- NA
    
    ls.B.2T.MSE$CMOMd[[67]][2] <- NA
    ls.B.2T.MSE$CMOMi[[67]][2] <- NA
    ls.B.2T.MSE$CMR[[67]][2] <- NA}
  
  B.RB.G.2T <- list()
  for (ite in ini:last){
    B.RB.G.2T$CMOMd[[ite]] <- mean(abs(ls.B.2T.RB$CMOMd[[ite]]))
    B.RB.G.2T$CMOMi[[ite]] <- mean(abs(ls.B.2T.RB$CMOMi[[ite]]))
    B.RB.G.2T$CMR[[ite]] <- mean(abs(ls.B.2T.RB$CMR[[ite]]))
    B.RB.G.2T$ZPNEc[[ite]] <- NA
    B.RB.G.2T$ZPNEs[[ite]] <- NA}
  
  for (ite in ini:last){
    B.RB.G.2T$CMOMd <- mean(B.RB.G.2T$CMOMd, na.rm = T)
    B.RB.G.2T$CMOMi <- mean(B.RB.G.2T$CMOMi, na.rm = T)
    B.RB.G.2T$CMR <- mean(B.RB.G.2T$CMR, na.rm = T)
    B.RB.G.2T$ZPNEc <- NA
    B.RB.G.2T$ZPNEs <- NA}
  
  B.SE.G.2T <- list()
  for (ite in ini:last){
    B.SE.G.2T$CMOMd[[ite]] <- mean(ls.B.2T.SE$CMOMd[[ite]])
    B.SE.G.2T$CMOMi[[ite]] <- mean(ls.B.2T.SE$CMOMi[[ite]])
    B.SE.G.2T$CMR[[ite]] <- mean(ls.B.2T.SE$CMR[[ite]])
    B.SE.G.2T$ZPNEc[[ite]] <- NA
    B.SE.G.2T$ZPNEs[[ite]] <- NA}
  
  for (ite in ini:last){
    B.SE.G.2T$CMOMd <- mean(B.SE.G.2T$CMOMd, na.rm = T)
    B.SE.G.2T$CMOMi <- mean(B.SE.G.2T$CMOMi, na.rm = T)
    B.SE.G.2T$CMR <- mean(B.SE.G.2T$CMR, na.rm = T)
    B.SE.G.2T$ZPNEc <- NA
    B.SE.G.2T$ZPNEs <- NA}
  
  B.MSE.G.2T <- list()
  for (ite in ini:last){
    B.MSE.G.2T$CMOMd[[ite]] <- mean(ls.B.2T.MSE$CMOMd[[ite]])
    B.MSE.G.2T$CMOMi[[ite]] <- mean(ls.B.2T.MSE$CMOMi[[ite]])
    B.MSE.G.2T$CMR[[ite]] <- mean(ls.B.2T.MSE$CMR[[ite]])
    B.MSE.G.2T$ZPNEc[[ite]] <- NA
    B.MSE.G.2T$ZPNEs[[ite]] <- NA}
  
  for (ite in ini:last){
    B.MSE.G.2T$CMOMd <- mean(B.MSE.G.2T$CMOMd, na.rm = T)
    B.MSE.G.2T$CMOMi <- mean(B.MSE.G.2T$CMOMi, na.rm = T)
    B.MSE.G.2T$CMR <- mean(B.MSE.G.2T$CMR, na.rm = T)
    B.MSE.G.2T$ZPNEc <- NA
    B.MSE.G.2T$ZPNEs <- NA}
  
  B.2T <- list(RB=B.RB.G.2T, RSE=B.SE.G.2T, MSE=B.MSE.G.2T)
  
  # ====================== PHIN ======================= #
  
  phin.RB.G.2T <- list()
  for (ite in ini:last){
    phin.RB.G.2T$CMOMd <- mean(abs(ls.phin.2T.RB$CMOMd[ini:last]), na.rm = T)
    phin.RB.G.2T$CMOMi <- mean(abs(ls.phin.2T.RB$CMOMi[ini:last]), na.rm = T)
    phin.RB.G.2T$CMR <- mean(abs(ls.phin.2T.RB$CMR[ini:last]), na.rm = T)
    phin.RB.G.2T$ZPNEc <- NA
    phin.RB.G.2T$ZPNEs <- NA}
  
  phin.SE.G.2T <- list()
  for (ite in ini:last){
    phin.SE.G.2T$CMOMd <- mean(abs(ls.phin.2T.SE$CMOMd[ini:last]), na.rm = T)
    phin.SE.G.2T$CMOMi <- mean(abs(ls.phin.2T.SE$CMOMi[ini:last]), na.rm = T)
    phin.SE.G.2T$CMR <- mean(abs(ls.phin.2T.SE$CMR[ini:last]), na.rm = T)
    phin.SE.G.2T$ZPNEc <- NA
    phin.SE.G.2T$ZPNEs <- NA}
  
  phin.MSE.G.2T <- list()
  for (ite in ini:last){
    phin.MSE.G.2T$CMOMd <- mean(abs(ls.phin.2T.MSE$CMOMd[ini:last]), na.rm = T)
    phin.MSE.G.2T$CMOMi <- mean(abs(ls.phin.2T.MSE$CMOMi[ini:last]), na.rm = T)
    phin.MSE.G.2T$CMR <- mean(abs(ls.phin.2T.MSE$CMR[ini:last]), na.rm = T)
    phin.MSE.G.2T$ZPNEc <- NA
    phin.MSE.G.2T$ZPNEs <- NA}
  
  phin.2T <- list(RB=phin.RB.G.2T, RSE=phin.SE.G.2T, MSE=phin.MSE.G.2T)
  
  # ================= DATA FRAME =================
  
  # ~~~~~ N ~~~~~
  stack.N.2T <- stack(N.2T)
  stack.N.2T$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
  colnames(stack.N.2T)[1:2] <- c("Values", "Results")
  if (TREN == 1) {stack.N.2T$P.factor <- factor("Decreasing")}
  if (TREN == 2) {stack.N.2T$P.factor <- factor("Increasing")}
  stack.N.2T$Parameters <- factor("Number of Individuals")
  stack.N.2T$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))
  
  # ~~~~~ B ~~~~~
  stack.B.2T <- stack(B.2T)
  stack.B.2T$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
  colnames(stack.B.2T)[1:2] <- c("Values", "Results")
  if (TREN == 1) {stack.B.2T$P.factor <- factor("Decreasing")}
  if (TREN == 2) {stack.B.2T$P.factor <- factor("Increasing")}
  stack.B.2T$Parameters <- factor("Recruitment")
  stack.B.2T$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))
  
  # ~~~~~ phin ~~~~~
  stack.phin.2T <- stack(phin.2T)
  stack.phin.2T$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
  colnames(stack.phin.2T)[1:2] <- c("Values", "Results")
  if (TREN == 1) {stack.phin.2T$P.factor <- factor("Decreasing")}
  if (TREN == 2) {stack.phin.2T$P.factor <- factor("Increasing")}
  stack.phin.2T$Parameters <- factor("Survival Second. Occ.")
  stack.phin.2T$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))
  
  # ~~~~~ phib ~~~~~
  stack.phib.2T <- stack(phib.2T)
  stack.phib.2T$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
  colnames(stack.phib.2T)[1:2] <- c("Values", "Results")
  if (TREN == 1) {stack.phib.2T$P.factor <- factor("Decreasing")}
  if (TREN == 2) {stack.phib.2T$P.factor <- factor("Increasing")}
  stack.phib.2T$Parameters <- factor("Survival Prim. Occ.")
  stack.phib.2T$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))
  
  # ~~~~~ p2 ~~~~~
  stack.p2.2T <- stack(p2.2T)
  stack.p2.2T$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
  colnames(stack.p2.2T)[1:2] <- c("Values", "Results")
  if (TREN == 1) {stack.p2.2T$P.factor <- factor("Decreasing")}
  if (TREN == 2) {stack.p2.2T$P.factor <- factor("Increasing")}
  stack.p2.2T$Parameters <- factor("Observation probability")
  stack.p2.2T$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))
  
  # ~~~~~ p1 ~~~~~
  stack.p1.2T <- stack(p1.2T)
  stack.p1.2T$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
  colnames(stack.p1.2T)[1:2] <- c("Values", "Results")
  if (TREN == 1) {stack.p1.2T$P.factor <- factor("Decreasing")}
  if (TREN == 2) {stack.p1.2T$P.factor <- factor("Increasing")}
  stack.p1.2T$Parameters <- factor("Capture probability")
  stack.p1.2T$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))
  
  graph <- rbind(stack.N.2T, stack.B.2T, stack.phin.2T, stack.phib.2T, stack.p1.2T, stack.p2.2T)
  
  ls.graph2[[TREN]] <- assign(paste("graph", TREN, sep=""), graph)
  ls.Exc2T[[TREN]] <- subset(graph, !(graph$Value < 2 & graph$Value > -2))}

save(ls.graph2,file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-2T.Rdata")
save(ls.Exc2T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Exc-2T.Rdata") 