#####################################################################
#                                                                   #
#         6c.    M S E,     R B    A N D     R S E                  #
#                                                                   #
#                           1  H                                    #
#                                                                   #
#####################################################################

#############################################################################
# Function to compute relative bias, standard error, and Mean squared error #
#                 for N, B, phi.in, phi.btw, p                              #
#############################################################################

# 1 H

# ----- FOR N ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/N-2H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/N-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/HN-ZPNEc.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/HN-ZPNEs.RData")

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/Ns-2H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/HNs-ZPNEc.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/HNs-ZPNEs.RData")

# ----- FOR phi and p ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/output2H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/H-outputZPNEs.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/H-outputZPNEc.RData")

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/Variation.Rdata")

Original.H <- matrix(c(0.99, 0.85, 0.2, NA),ncol=600,nrow=4) # Hetero
rownames(Original.H) <- c("phin","phib","p1","p2")
Original.H[4,] <- Var.tb[1,1:600]

# ----- FOR B ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/B-2H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/B-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/Bs-2H.RData")

iter <- 600

# ===================== CMOMd, CMOMi, CMR =====================

######################
#         N          #
######################

ls.N.2H.RB <- list()
for (ite in 1:iter){
  ls.N.2H.RB$CMOMd[[ite]] <- (N.2H$CMOMd[[ite]]-ls.H.N[[ite]])/ls.H.N[[ite]]
  ls.N.2H.RB$CMOMi[[ite]] <- (N.2H$CMOMi[[ite]]-ls.H.N[[ite]])/ls.H.N[[ite]]
  ls.N.2H.RB$CMR[[ite]] <- (N.2H$CMR[[ite]]-ls.H.N[[ite]])/ls.H.N[[ite]]}

ls.N.2H.SE <- list()
for (ite in 1:iter){
  ls.N.2H.SE$CMOMd[[ite]] <- Ns.2H$CMOMd[[ite]]/ls.H.N[[ite]]
  ls.N.2H.SE$CMOMi[[ite]] <- Ns.2H$CMOMi[[ite]]/ls.H.N[[ite]]
  ls.N.2H.SE$CMR[[ite]] <- Ns.2H$CMR[[ite]]/ls.H.N[[ite]]}

ls.N.2H.MSE <- list()
for (ite in 1:iter){
  ls.N.2H.MSE$CMOMd[[ite]] <- ls.N.2H.RB$CMOMd[[ite]]^2+ls.N.2H.SE$CMOMd[[ite]]^2
  ls.N.2H.MSE$CMOMi[[ite]] <- ls.N.2H.RB$CMOMi[[ite]]^2+ls.N.2H.SE$CMOMi[[ite]]^2
  ls.N.2H.MSE$CMR[[ite]] <- ls.N.2H.RB$CMR[[ite]]^2+ls.N.2H.SE$CMR[[ite]]^2}

######################
#         B          #
######################

ls.B.2H.RB <- list()
for (ite in 1:iter){
  ls.B.2H.RB$CMOMd[[ite]] <- (B.2H$CMOMd[[ite]]-ls.H.B[[ite]])/ls.H.B[[ite]]
  ls.B.2H.RB$CMOMi[[ite]] <- (B.2H$CMOMi[[ite]]-ls.H.B[[ite]])/ls.H.B[[ite]]
  ls.B.2H.RB$CMR[[ite]] <- (B.2H$CMR[[ite]]-ls.H.B[[ite]])/ls.H.B[[ite]]}

ls.B.2H.SE <- list()
for (ite in 1:iter){
  ls.B.2H.SE$CMOMd[[ite]] <- Bs.2H$CMOMd[[ite]]/ls.H.B[[ite]]
  ls.B.2H.SE$CMOMi[[ite]] <- Bs.2H$CMOMi[[ite]]/ls.H.B[[ite]]
  ls.B.2H.SE$CMR[[ite]] <- Bs.2H$CMR[[ite]]/ls.H.B[[ite]]}

ls.B.2H.MSE <- list()
for (ite in 1:iter){
  ls.B.2H.MSE$CMOMd[[ite]] <- ls.B.2H.RB$CMOMd[[ite]]^2+ls.B.2H.SE$CMOMd[[ite]]^2
  ls.B.2H.MSE$CMOMi[[ite]] <- ls.B.2H.RB$CMOMi[[ite]]^2+ls.B.2H.SE$CMOMi[[ite]]^2
  ls.B.2H.MSE$CMR[[ite]] <- ls.B.2H.RB$CMR[[ite]]^2+ls.B.2H.SE$CMR[[ite]]^2}

##############
#   phin     #
##############

output.2H <- output.2H[c(5:12,17:24,29:36),]

ls.phin.2H.RB <- list()
for (j in 1:iter){
  ls.phin.2H.RB$CMOMd[[j]] <- (output.2H[1,j]-Original.H[1,j])/Original.H[1,j]
  ls.phin.2H.RB$CMOMi[[j]] <- (output.2H[9,j]-Original.H[1,j])/Original.H[1,j]
  ls.phin.2H.RB$CMR[[j]] <- (output.2H[17,j]-Original.H[1,j])/Original.H[1,j]}

ls.phin.2H.SE <- list()
for (j in 1:iter){
  ls.phin.2H.SE$CMOMd[[j]] <- output.2H[2,j]/Original.H[1,j]
  ls.phin.2H.SE$CMOMi[[j]] <- output.2H[10,j]/Original.H[1,j]
  ls.phin.2H.SE$CMR[[j]] <- output.2H[18,j]/Original.H[1,j]}

ls.phin.2H.MSE <- list()
for (j in 1:iter){
  ls.phin.2H.MSE$CMOMd[[j]] <- ls.phin.2H.RB$CMOMd[[j]]^2+ls.phin.2H.SE$CMOMd[[j]]^2
  ls.phin.2H.MSE$CMOMi[[j]] <- ls.phin.2H.RB$CMOMi[[j]]^2+ls.phin.2H.SE$CMOMi[[j]]^2
  ls.phin.2H.MSE$CMR[[j]] <- ls.phin.2H.RB$CMR[[j]]^2+ls.phin.2H.SE$CMR[[j]]^2}

##############
#   phib     #
##############

ls.phib.2H.RB <- list()
for (j in 1:iter){
  ls.phib.2H.RB$CMOMd[[j]] <- (output.2H[3,j]-Original.H[2,j])/Original.H[2,j]
  ls.phib.2H.RB$CMOMi[[j]] <- (output.2H[11,j]-Original.H[2,j])/Original.H[2,j]
  ls.phib.2H.RB$CMR[[j]] <- (output.2H[19,j]-Original.H[2,j])/Original.H[2,j]}

ls.phib.2H.SE <- list()
for (j in 1:iter){
  ls.phib.2H.SE$CMOMd[[j]] <- output.2H[4,j]/Original.H[2,j]
  ls.phib.2H.SE$CMOMi[[j]] <- output.2H[12,j]/Original.H[2,j]
  ls.phib.2H.SE$CMR[[j]] <- output.2H[20,j]/Original.H[2,j]}

ls.phib.2H.MSE <- list()
for (j in 1:iter){
  ls.phib.2H.MSE$CMOMd[[j]] <- ls.phib.2H.RB$CMOMd[[j]]^2+ls.phib.2H.SE$CMOMd[[j]]^2
  ls.phib.2H.MSE$CMOMi[[j]] <- ls.phib.2H.RB$CMOMi[[j]]^2+ls.phib.2H.SE$CMOMi[[j]]^2
  ls.phib.2H.MSE$CMR[[j]] <- ls.phib.2H.RB$CMR[[j]]^2+ls.phib.2H.SE$CMR[[j]]^2}

##############
#     p2     #
##############

ls.p2.2H.RB <- list()
for (j in 1:iter){
  ls.p2.2H.RB$CMOMd[[j]] <- (output.2H[5,j]-Original.H[4,j])/Original.H[4,j]
  ls.p2.2H.RB$CMOMi[[j]] <- (output.2H[13,j]-Original.H[4,j])/Original.H[4,j]
  ls.p2.2H.RB$CMR[[j]] <- (output.2H[21,j]-Original.H[4,j])/Original.H[4,j]}

ls.p2.2H.SE <- list()
for (j in 1:iter){
  ls.p2.2H.SE$CMOMd[[j]] <- output.2H[6,j]/Original.H[4,j]
  ls.p2.2H.SE$CMOMi[[j]] <- output.2H[14,j]/Original.H[4,j]
  ls.p2.2H.SE$CMR[[j]] <- output.2H[22,j]/Original.H[4,j]}

ls.p2.2H.MSE <- list()
for (j in 1:iter){
  ls.p2.2H.MSE$CMOMd[[j]] <- ls.p2.2H.RB$CMOMd[[j]]^2+ls.p2.2H.SE$CMOMd[[j]]^2
  ls.p2.2H.MSE$CMOMi[[j]] <- ls.p2.2H.RB$CMOMi[[j]]^2+ls.p2.2H.SE$CMOMi[[j]]^2
  ls.p2.2H.MSE$CMR[[j]] <- ls.p2.2H.RB$CMR[[j]]^2+ls.p2.2H.SE$CMR[[j]]^2}

##############
#     p1     #
##############

ls.p1.2H.RB <- list()
for (j in 1:iter){
  ls.p1.2H.RB$CMOMd[[j]] <- (output.2H[7,j]-Original.H[4,j])/Original.H[4,j]
  ls.p1.2H.RB$CMOMi[[j]] <- (output.2H[15,j]-Original.H[4,j])/Original.H[4,j]
  ls.p1.2H.RB$CMR[[j]] <- (output.2H[23,j]-Original.H[4,j])/Original.H[4,j]}

ls.p1.2H.SE <- list()
for (j in 1:iter){
  ls.p1.2H.SE$CMOMd[[j]] <- output.2H[8,j]/Original.H[4,j]
  ls.p1.2H.SE$CMOMi[[j]] <- output.2H[16,j]/Original.H[4,j]
  ls.p1.2H.SE$CMR[[j]] <- output.2H[24,j]/Original.H[4,j]}

ls.p1.2H.MSE <- list()
for (j in 1:iter){
  ls.p1.2H.MSE$CMOMd[[j]] <- ls.p1.2H.RB$CMOMd[[j]]^2+ls.p1.2H.SE$CMOMd[[j]]^2
  ls.p1.2H.MSE$CMOMi[[j]] <- ls.p1.2H.RB$CMOMi[[j]]^2+ls.p1.2H.SE$CMOMi[[j]]^2
  ls.p1.2H.MSE$CMR[[j]] <- ls.p1.2H.RB$CMR[[j]]^2+ls.p1.2H.SE$CMR[[j]]^2}

save.image("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-2H.Rdata")

########################################################
#                       T A B L E                      #
########################################################

ls.Exc2H <- ls.graph2 <- list()

for(PERC in 1:6){
  
  if (PERC == 1) {ini <- 1 
  last <- 100}
  if (PERC == 2) {ini <- 101 
  last <- 200}
  if (PERC == 3) {ini <- 201 
  last <- 300}
  if (PERC == 4) {ini <- 301 
  last <- 400}
  if (PERC == 5) {ini <- 401 
  last <- 500}
  if (PERC == 6) {ini <- 501 
  last <- 600}
  
  # ====================== N ======================= #
  
  N.RB.G.2H <- list()
  for (ite in ini:last){
    N.RB.G.2H$CMOMd[[ite]] <- mean(abs(ls.N.2H.RB$CMOMd[[ite]]))
    N.RB.G.2H$CMOMi[[ite]] <- mean(abs(ls.N.2H.RB$CMOMi[[ite]]))
    N.RB.G.2H$CMR[[ite]] <- mean(abs(ls.N.2H.RB$CMR[[ite]]))
    N.RB.G.2H$ZPNEc[[ite]] <- NA
    N.RB.G.2H$ZPNEs[[ite]] <- NA}
  
  for (ite in ini:last){
    N.RB.G.2H$CMOMd <- mean(N.RB.G.2H$CMOMd, na.rm = T)
    N.RB.G.2H$CMOMi <- mean(N.RB.G.2H$CMOMi, na.rm = T)
    N.RB.G.2H$CMR <- mean(N.RB.G.2H$CMR, na.rm = T)
    N.RB.G.2H$ZPNEc <- NA
    N.RB.G.2H$ZPNEs <- NA}
  
  N.SE.G.2H <- list()
  for (ite in ini:last){
    N.SE.G.2H$CMOMd[[ite]] <- mean(ls.N.2H.SE$CMOMd[[ite]])
    N.SE.G.2H$CMOMi[[ite]] <- mean(ls.N.2H.SE$CMOMi[[ite]])
    N.SE.G.2H$CMR[[ite]] <- mean(ls.N.2H.SE$CMR[[ite]])
    N.SE.G.2H$ZPNEc[[ite]] <- NA
    N.SE.G.2H$ZPNEs[[ite]] <- NA}
  
  for (ite in ini:last){
    N.SE.G.2H$CMOMd <- mean(N.SE.G.2H$CMOMd, na.rm = T)
    N.SE.G.2H$CMOMi <- mean(N.SE.G.2H$CMOMi, na.rm = T)
    N.SE.G.2H$CMR <- mean(N.SE.G.2H$CMR, na.rm = T)
    N.SE.G.2H$ZPNEc <- NA
    N.SE.G.2H$ZPNEs <- NA}
  
  N.MSE.G.2H <- list()
  for (ite in ini:last){
    N.MSE.G.2H$CMOMd[[ite]] <- mean(ls.N.2H.MSE$CMOMd[[ite]])
    N.MSE.G.2H$CMOMi[[ite]] <- mean(ls.N.2H.MSE$CMOMi[[ite]])
    N.MSE.G.2H$CMR[[ite]] <- mean(ls.N.2H.MSE$CMR[[ite]])
    N.MSE.G.2H$ZPNEc[[ite]] <- NA
    N.MSE.G.2H$ZPNEs[[ite]] <- NA}
  
  for (ite in ini:last){
    N.MSE.G.2H$CMOMd <- mean(N.MSE.G.2H$CMOMd, na.rm = T)
    N.MSE.G.2H$CMOMi <- mean(N.MSE.G.2H$CMOMi, na.rm = T)
    N.MSE.G.2H$CMR <- mean(N.MSE.G.2H$CMR, na.rm = T)
    N.MSE.G.2H$ZPNEc <- NA
    N.MSE.G.2H$ZPNEs <- NA}
  
  N.2H <- list(RB=N.RB.G.2H, RSE=N.SE.G.2H, MSE=N.MSE.G.2H)
  
  # ====================== PHIB ======================= #
  
  phib.RB.G.2H <- list()
  for (ite in ini:last){
    phib.RB.G.2H$CMOMd <- mean(abs(ls.phib.2H.RB$CMOMd[ini:last]), na.rm = T)
    phib.RB.G.2H$CMOMi <- mean(abs(ls.phib.2H.RB$CMOMi[ini:last]), na.rm = T)
    phib.RB.G.2H$CMR <- mean(abs(ls.phib.2H.RB$CMR[ini:last]), na.rm = T)
    phib.RB.G.2H$ZPNEc <- NA
    phib.RB.G.2H$ZPNEs <- NA}
  
  phib.SE.G.2H <- list()
  for (ite in ini:last){
    phib.SE.G.2H$CMOMd <- mean(abs(ls.phib.2H.SE$CMOMd[ini:last]), na.rm = T)
    phib.SE.G.2H$CMOMi <- mean(abs(ls.phib.2H.SE$CMOMi[ini:last]), na.rm = T)
    phib.SE.G.2H$CMR <- mean(abs(ls.phib.2H.SE$CMR[ini:last]), na.rm = T)
    phib.SE.G.2H$ZPNEc <- NA
    phib.SE.G.2H$ZPNEs <- NA}
  
  phib.MSE.G.2H <- list()
  for (ite in ini:last){
    phib.MSE.G.2H$CMOMd <- mean(abs(ls.phib.2H.MSE$CMOMd[ini:last]), na.rm = T)
    phib.MSE.G.2H$CMOMi <- mean(abs(ls.phib.2H.MSE$CMOMi[ini:last]), na.rm = T)
    phib.MSE.G.2H$CMR <- mean(abs(ls.phib.2H.MSE$CMR[ini:last]), na.rm = T)
    phib.MSE.G.2H$ZPNEc <- NA
    phib.MSE.G.2H$ZPNEs <- NA}
  
  phib.2H <- list(RB=phib.RB.G.2H, RSE=phib.SE.G.2H, MSE=phib.MSE.G.2H)
  
  # ====================== P2 ======================= #
  
  p2.RB.G.2H <- list()
  for (ite in ini:last){
    p2.RB.G.2H$CMOMd <- mean(abs(ls.p2.2H.RB$CMOMd[ini:last]), na.rm = T)
    p2.RB.G.2H$CMOMi <- mean(abs(ls.p2.2H.RB$CMOMi[ini:last]), na.rm = T)
    p2.RB.G.2H$CMR <- mean(abs(ls.p2.2H.RB$CMR[ini:last]), na.rm = T)
    p2.RB.G.2H$ZPNEc <- NA
    p2.RB.G.2H$ZPNEs <- NA}
  
  p2.SE.G.2H <- list()
  for (ite in ini:last){
    p2.SE.G.2H$CMOMd <- mean(abs(ls.p2.2H.SE$CMOMd[ini:last]), na.rm = T)
    p2.SE.G.2H$CMOMi <- mean(abs(ls.p2.2H.SE$CMOMi[ini:last]), na.rm = T)
    p2.SE.G.2H$CMR <- mean(abs(ls.p2.2H.SE$CMR[ini:last]), na.rm = T)
    p2.SE.G.2H$ZPNEc <- NA
    p2.SE.G.2H$ZPNEs <- NA}
  
  p2.MSE.G.2H <- list()
  for (ite in ini:last){
    p2.MSE.G.2H$CMOMd <- mean(abs(ls.p2.2H.MSE$CMOMd[ini:last]), na.rm = T)
    p2.MSE.G.2H$CMOMi <- mean(abs(ls.p2.2H.MSE$CMOMi[ini:last]), na.rm = T)
    p2.MSE.G.2H$CMR <- mean(abs(ls.p2.2H.MSE$CMR[ini:last]), na.rm = T)
    p2.MSE.G.2H$ZPNEc <- NA
    p2.MSE.G.2H$ZPNEs <- NA}
  
  p2.2H <- list(RB=p2.RB.G.2H, RSE=p2.SE.G.2H, MSE=p2.MSE.G.2H)
  
  # ====================== P1 ======================= #
  
  p1.RB.G.2H <- list()
  for (ite in ini:last){
    p1.RB.G.2H$CMOMd <- mean(abs(ls.p1.2H.RB$CMOMd[ini:last]), na.rm = T)
    p1.RB.G.2H$CMOMi <- mean(abs(ls.p1.2H.RB$CMOMi[ini:last]), na.rm = T)
    p1.RB.G.2H$CMR <- mean(abs(ls.p1.2H.RB$CMR[ini:last]), na.rm = T)
    p1.RB.G.2H$ZPNEc <- NA
    p1.RB.G.2H$ZPNEs <- NA}
  
  p1.SE.G.2H <- list()
  for (ite in ini:last){
    p1.SE.G.2H$CMOMd <- mean(abs(ls.p1.2H.SE$CMOMd[ini:last]), na.rm = T)
    p1.SE.G.2H$CMOMi <- mean(abs(ls.p1.2H.SE$CMOMi[ini:last]), na.rm = T)
    p1.SE.G.2H$CMR <- mean(abs(ls.p1.2H.SE$CMR[ini:last]), na.rm = T)
    p1.SE.G.2H$ZPNEc <- NA
    p1.SE.G.2H$ZPNEs <- NA}
  
  p1.MSE.G.2H <- list()
  for (ite in ini:last){
    p1.MSE.G.2H$CMOMd <- mean(abs(ls.p1.2H.MSE$CMOMd[ini:last]), na.rm = T)
    p1.MSE.G.2H$CMOMi <- mean(abs(ls.p1.2H.MSE$CMOMi[ini:last]), na.rm = T)
    p1.MSE.G.2H$CMR <- mean(abs(ls.p1.2H.MSE$CMR[ini:last]), na.rm = T)
    p1.MSE.G.2H$ZPNEc <- NA
    p1.MSE.G.2H$ZPNEs <- NA}
  
  p1.2H <- list(RB=p1.RB.G.2H, RSE=p1.SE.G.2H, MSE=p1.MSE.G.2H)
  
  # ====================== B ======================= #
  
  B.RB.G.2H <- list()
  for (ite in ini:last){
    B.RB.G.2H$CMOMd[[ite]] <- mean(abs(ls.B.2H.RB$CMOMd[[ite]]))
    B.RB.G.2H$CMOMi[[ite]] <- mean(abs(ls.B.2H.RB$CMOMi[[ite]]))
    B.RB.G.2H$CMR[[ite]] <- mean(abs(ls.B.2H.RB$CMR[[ite]]))
    B.RB.G.2H$ZPNEc[[ite]] <- NA
    B.RB.G.2H$ZPNEs[[ite]] <- NA}
  
  for (ite in ini:last){
    B.RB.G.2H$CMOMd <- mean(B.RB.G.2H$CMOMd, na.rm = T)
    B.RB.G.2H$CMOMi <- mean(B.RB.G.2H$CMOMi, na.rm = T)
    B.RB.G.2H$CMR <- mean(B.RB.G.2H$CMR, na.rm = T)
    B.RB.G.2H$ZPNEc <- NA
    B.RB.G.2H$ZPNEs <- NA}
  
  B.SE.G.2H <- list()
  for (ite in ini:last){
    B.SE.G.2H$CMOMd[[ite]] <- mean(ls.B.2H.SE$CMOMd[[ite]])
    B.SE.G.2H$CMOMi[[ite]] <- mean(ls.B.2H.SE$CMOMi[[ite]])
    B.SE.G.2H$CMR[[ite]] <- mean(ls.B.2H.SE$CMR[[ite]])
    B.SE.G.2H$ZPNEc[[ite]] <- NA
    B.SE.G.2H$ZPNEs[[ite]] <- NA}
  
  for (ite in ini:last){
    B.SE.G.2H$CMOMd <- mean(B.SE.G.2H$CMOMd, na.rm = T)
    B.SE.G.2H$CMOMi <- mean(B.SE.G.2H$CMOMi, na.rm = T)
    B.SE.G.2H$CMR <- mean(B.SE.G.2H$CMR, na.rm = T)
    B.SE.G.2H$ZPNEc <- NA
    B.SE.G.2H$ZPNEs <- NA}
  
  B.MSE.G.2H <- list()
  for (ite in ini:last){
    B.MSE.G.2H$CMOMd[[ite]] <- mean(ls.B.2H.MSE$CMOMd[[ite]])
    B.MSE.G.2H$CMOMi[[ite]] <- mean(ls.B.2H.MSE$CMOMi[[ite]])
    B.MSE.G.2H$CMR[[ite]] <- mean(ls.B.2H.MSE$CMR[[ite]])
    B.MSE.G.2H$ZPNEc[[ite]] <- NA
    B.MSE.G.2H$ZPNEs[[ite]] <- NA}
  
  for (ite in ini:last){
    B.MSE.G.2H$CMOMd <- mean(B.MSE.G.2H$CMOMd, na.rm = T)
    B.MSE.G.2H$CMOMi <- mean(B.MSE.G.2H$CMOMi, na.rm = T)
    B.MSE.G.2H$CMR <- mean(B.MSE.G.2H$CMR, na.rm = T)
    B.MSE.G.2H$ZPNEc <- NA
    B.MSE.G.2H$ZPNEs <- NA}
  
  B.2H <- list(RB=B.RB.G.2H, RSE=B.SE.G.2H, MSE=B.MSE.G.2H)
  
  # ====================== PHIN ======================= #
  
  phin.RB.G.2H <- list()
  for (ite in ini:last){
    phin.RB.G.2H$CMOMd <- mean(abs(ls.phin.2H.RB$CMOMd[ini:last]), na.rm = T)
    phin.RB.G.2H$CMOMi <- mean(abs(ls.phin.2H.RB$CMOMi[ini:last]), na.rm = T)
    phin.RB.G.2H$CMR <- mean(abs(ls.phin.2H.RB$CMR[ini:last]), na.rm = T)
    phin.RB.G.2H$ZPNEc <- NA
    phin.RB.G.2H$ZPNEs <- NA}
  
  phin.SE.G.2H <- list()
  for (ite in ini:last){
    phin.SE.G.2H$CMOMd <- mean(abs(ls.phin.2H.SE$CMOMd[ini:last]), na.rm = T)
    phin.SE.G.2H$CMOMi <- mean(abs(ls.phin.2H.SE$CMOMi[ini:last]), na.rm = T)
    phin.SE.G.2H$CMR <- mean(abs(ls.phin.2H.SE$CMR[ini:last]), na.rm = T)
    phin.SE.G.2H$ZPNEc <- NA
    phin.SE.G.2H$ZPNEs <- NA}
  
  phin.MSE.G.2H <- list()
  for (ite in ini:last){
    phin.MSE.G.2H$CMOMd <- mean(abs(ls.phin.2H.MSE$CMOMd[ini:last]), na.rm = T)
    phin.MSE.G.2H$CMOMi <- mean(abs(ls.phin.2H.MSE$CMOMi[ini:last]), na.rm = T)
    phin.MSE.G.2H$CMR <- mean(abs(ls.phin.2H.MSE$CMR[ini:last]), na.rm = T)
    phin.MSE.G.2H$ZPNEc <- NA
    phin.MSE.G.2H$ZPNEs <- NA}
  
  phin.2H <- list(RB=phin.RB.G.2H, RSE=phin.SE.G.2H, MSE=phin.MSE.G.2H)
  
  # ================= DATA FRAME =================
  
  # ~~~~~ N ~~~~~
  stack.N.2H <- stack(N.2H)
  stack.N.2H$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
  colnames(stack.N.2H)[1:2] <- c("Values", "Results")
  if (PERC == 1) {stack.N.2H$P.factor <- factor("10%")}
  if (PERC == 2) {stack.N.2H$P.factor <- factor("20%")}
  if (PERC == 3) {stack.N.2H$P.factor <- factor("30%")}
  if (PERC == 4) {stack.N.2H$P.factor <- factor("40%")}
  if (PERC == 5) {stack.N.2H$P.factor <- factor("50%")}
  if (PERC == 6) {stack.N.2H$P.factor <- factor("60%")}
  stack.N.2H$Parameters <- factor("Number of Individuals")
  stack.N.2H$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))
  
  # ~~~~~ B ~~~~~
  stack.B.2H <- stack(B.2H)
  stack.B.2H$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
  colnames(stack.B.2H)[1:2] <- c("Values", "Results")
  if (PERC == 1) {stack.B.2H$P.factor <- factor("10%")}
  if (PERC == 2) {stack.B.2H$P.factor <- factor("20%")}
  if (PERC == 3) {stack.B.2H$P.factor <- factor("30%")}
  if (PERC == 4) {stack.B.2H$P.factor <- factor("40%")}
  if (PERC == 5) {stack.B.2H$P.factor <- factor("50%")}
  if (PERC == 6) {stack.B.2H$P.factor <- factor("60%")}
  stack.B.2H$Parameters <- factor("Recruitment")
  stack.B.2H$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))
  
  # ~~~~~ phin ~~~~~
  stack.phin.2H <- stack(phin.2H)
  stack.phin.2H$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
  colnames(stack.phin.2H)[1:2] <- c("Values", "Results")
  if (PERC == 1) {stack.phin.2H$P.factor <- factor("10%")}
  if (PERC == 2) {stack.phin.2H$P.factor <- factor("20%")}
  if (PERC == 3) {stack.phin.2H$P.factor <- factor("30%")}
  if (PERC == 4) {stack.phin.2H$P.factor <- factor("40%")}
  if (PERC == 5) {stack.phin.2H$P.factor <- factor("50%")}
  if (PERC == 6) {stack.phin.2H$P.factor <- factor("60%")}
  stack.phin.2H$Parameters <- factor("Survival Second. Occ.")
  stack.phin.2H$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))
  
  # ~~~~~ phib ~~~~~
  stack.phib.2H <- stack(phib.2H)
  stack.phib.2H$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
  colnames(stack.phib.2H)[1:2] <- c("Values", "Results")
  if (PERC == 1) {stack.phib.2H$P.factor <- factor("10%")}
  if (PERC == 2) {stack.phib.2H$P.factor <- factor("20%")}
  if (PERC == 3) {stack.phib.2H$P.factor <- factor("30%")}
  if (PERC == 4) {stack.phib.2H$P.factor <- factor("40%")}
  if (PERC == 5) {stack.phib.2H$P.factor <- factor("50%")}
  if (PERC == 6) {stack.phib.2H$P.factor <- factor("60%")}
  stack.phib.2H$Parameters <- factor("Survival Prim. Occ.")
  stack.phib.2H$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))
  
  # ~~~~~ p2 ~~~~~
  stack.p2.2H <- stack(p2.2H)
  stack.p2.2H$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
  colnames(stack.p2.2H)[1:2] <- c("Values", "Results")
  if (PERC == 1) {stack.p2.2H$P.factor <- factor("10%")}
  if (PERC == 2) {stack.p2.2H$P.factor <- factor("20%")}
  if (PERC == 3) {stack.p2.2H$P.factor <- factor("30%")}
  if (PERC == 4) {stack.p2.2H$P.factor <- factor("40%")}
  if (PERC == 5) {stack.p2.2H$P.factor <- factor("50%")}
  if (PERC == 6) {stack.p2.2H$P.factor <- factor("60%")}
  stack.p2.2H$Parameters <- factor("Observation probability")
  stack.p2.2H$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))
  
  # ~~~~~ p1 ~~~~~
  stack.p1.2H <- stack(p1.2H)
  stack.p1.2H$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
  colnames(stack.p1.2H)[1:2] <- c("Values", "Results")
  if (PERC == 1) {stack.p1.2H$P.factor <- factor("10%")}
  if (PERC == 2) {stack.p1.2H$P.factor <- factor("20%")}
  if (PERC == 3) {stack.p1.2H$P.factor <- factor("30%")}
  if (PERC == 4) {stack.p1.2H$P.factor <- factor("40%")}
  if (PERC == 5) {stack.p1.2H$P.factor <- factor("50%")}
  if (PERC == 6) {stack.p1.2H$P.factor <- factor("60%")}
  stack.p1.2H$Parameters <- factor("Capture probability")
  stack.p1.2H$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))
  
  graph <- rbind(stack.N.2H, stack.B.2H, stack.phin.2H, stack.phib.2H, stack.p1.2H, stack.p2.2H)
  
  ls.graph2[[PERC]] <- assign(paste("graph", PERC, sep=""), graph)
  ls.Exc2H[[PERC]] <- subset(graph, !(graph$Value < 2 & graph$Value > -2))}

save(ls.graph2,file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-2H.Rdata")
save(ls.Exc2H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Exc-2H.Rdata") 