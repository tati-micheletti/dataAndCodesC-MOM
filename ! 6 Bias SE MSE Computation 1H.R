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

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/N-1H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/N-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/HN-ZPNEc.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/HN-ZPNEs.RData")

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/Ns-1H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/HNs-ZPNEc.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/HNs-ZPNEs.RData")

# ----- FOR phi and p ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/output1H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/H-outputZPNEs.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/H-outputZPNEc.RData")

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/Variation.Rdata")

Original.H <- matrix(c(0.99, 0.85, 0.2, NA),ncol=600,nrow=4) # Hetero
rownames(Original.H) <- c("phin","phib","p1","p2")
Original.H[4,] <- Var.tb[1,1:600]

# ----- FOR B ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/B-1H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/B-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/Bs-1H.RData")

iter <- 600

# ===================== CMOMd, CMOMi, CMR =====================

######################
#         N          #
######################

ls.N.1H.RB <- list()
for (ite in 1:iter){
  ls.N.1H.RB$CMOMd[[ite]] <- (N.1H$CMOMd[[ite]]-ls.H.N[[ite]])/ls.H.N[[ite]]
  ls.N.1H.RB$CMOMi[[ite]] <- (N.1H$CMOMi[[ite]]-ls.H.N[[ite]])/ls.H.N[[ite]]
  ls.N.1H.RB$CMR[[ite]] <- (N.1H$CMR[[ite]]-ls.H.N[[ite]])/ls.H.N[[ite]]}

ls.N.1H.SE <- list()
for (ite in 1:iter){
  ls.N.1H.SE$CMOMd[[ite]] <- Ns.1H$CMOMd[[ite]]/ls.H.N[[ite]]
  ls.N.1H.SE$CMOMi[[ite]] <- Ns.1H$CMOMi[[ite]]/ls.H.N[[ite]]
  ls.N.1H.SE$CMR[[ite]] <- Ns.1H$CMR[[ite]]/ls.H.N[[ite]]}

ls.N.1H.MSE <- list()
for (ite in 1:iter){
  ls.N.1H.MSE$CMOMd[[ite]] <- ls.N.1H.RB$CMOMd[[ite]]^2+ls.N.1H.SE$CMOMd[[ite]]^2
  ls.N.1H.MSE$CMOMi[[ite]] <- ls.N.1H.RB$CMOMi[[ite]]^2+ls.N.1H.SE$CMOMi[[ite]]^2
  ls.N.1H.MSE$CMR[[ite]] <- ls.N.1H.RB$CMR[[ite]]^2+ls.N.1H.SE$CMR[[ite]]^2}

######################
#         B          #
######################

ls.B.1H.RB <- list()
for (ite in 1:iter){
  ls.B.1H.RB$CMOMd[[ite]] <- (B.1H$CMOMd[[ite]]-ls.H.B[[ite]])/ls.H.B[[ite]]
  ls.B.1H.RB$CMOMi[[ite]] <- (B.1H$CMOMi[[ite]]-ls.H.B[[ite]])/ls.H.B[[ite]]
  ls.B.1H.RB$CMR[[ite]] <- (B.1H$CMR[[ite]]-ls.H.B[[ite]])/ls.H.B[[ite]]}

ls.B.1H.SE <- list()
for (ite in 1:iter){
  ls.B.1H.SE$CMOMd[[ite]] <- Bs.1H$CMOMd[[ite]]/ls.H.B[[ite]]
  ls.B.1H.SE$CMOMi[[ite]] <- Bs.1H$CMOMi[[ite]]/ls.H.B[[ite]]
  ls.B.1H.SE$CMR[[ite]] <- Bs.1H$CMR[[ite]]/ls.H.B[[ite]]}

ls.B.1H.MSE <- list()
for (ite in 1:iter){
  ls.B.1H.MSE$CMOMd[[ite]] <- ls.B.1H.RB$CMOMd[[ite]]^2+ls.B.1H.SE$CMOMd[[ite]]^2
  ls.B.1H.MSE$CMOMi[[ite]] <- ls.B.1H.RB$CMOMi[[ite]]^2+ls.B.1H.SE$CMOMi[[ite]]^2
  ls.B.1H.MSE$CMR[[ite]] <- ls.B.1H.RB$CMR[[ite]]^2+ls.B.1H.SE$CMR[[ite]]^2}

##############
#   phin     #
##############

output.1H <- output.1H[c(5:10,15:20,25:30),]

ls.phin.1H.RB <- list()
for (j in 1:iter){
  ls.phin.1H.RB$CMOMd[[j]] <- (output.1H[1,j]-Original.H[1,j])/Original.H[1,j]
  ls.phin.1H.RB$CMOMi[[j]] <- (output.1H[7,j]-Original.H[1,j])/Original.H[1,j]
  ls.phin.1H.RB$CMR[[j]] <- (output.1H[13,j]-Original.H[1,j])/Original.H[1,j]}

ls.phin.1H.SE <- list()
for (j in 1:iter){
  ls.phin.1H.SE$CMOMd[[j]] <- output.1H[2,j]/Original.H[1,j]
  ls.phin.1H.SE$CMOMi[[j]] <- output.1H[8,j]/Original.H[1,j]
  ls.phin.1H.SE$CMR[[j]] <- output.1H[14,j]/Original.H[1,j]}

ls.phin.1H.MSE <- list()
for (j in 1:iter){
  ls.phin.1H.MSE$CMOMd[[j]] <- ls.phin.1H.RB$CMOMd[[j]]^2+ls.phin.1H.SE$CMOMd[[j]]^2
  ls.phin.1H.MSE$CMOMi[[j]] <- ls.phin.1H.RB$CMOMi[[j]]^2+ls.phin.1H.SE$CMOMi[[j]]^2
  ls.phin.1H.MSE$CMR[[j]] <- ls.phin.1H.RB$CMR[[j]]^2+ls.phin.1H.SE$CMR[[j]]^2}

##############
#   phib     #
##############

ls.phib.1H.RB <- list()
for (j in 1:iter){
  ls.phib.1H.RB$CMOMd[[j]] <- (output.1H[3,j]-Original.H[2,j])/Original.H[2,j]
  ls.phib.1H.RB$CMOMi[[j]] <- (output.1H[9,j]-Original.H[2,j])/Original.H[2,j]
  ls.phib.1H.RB$CMR[[j]] <- (output.1H[15,j]-Original.H[2,j])/Original.H[2,j]}

ls.phib.1H.SE <- list()
for (j in 1:iter){
  ls.phib.1H.SE$CMOMd[[j]] <- output.1H[4,j]/Original.H[2,j]
  ls.phib.1H.SE$CMOMi[[j]] <- output.1H[10,j]/Original.H[2,j]
  ls.phib.1H.SE$CMR[[j]] <- output.1H[16,j]/Original.H[2,j]}

ls.phib.1H.MSE <- list()
for (j in 1:iter){
  ls.phib.1H.MSE$CMOMd[[j]] <- ls.phib.1H.RB$CMOMd[[j]]^2+ls.phib.1H.SE$CMOMd[[j]]^2
  ls.phib.1H.MSE$CMOMi[[j]] <- ls.phib.1H.RB$CMOMi[[j]]^2+ls.phib.1H.SE$CMOMi[[j]]^2
  ls.phib.1H.MSE$CMR[[j]] <- ls.phib.1H.RB$CMR[[j]]^2+ls.phib.1H.SE$CMR[[j]]^2}

##############
#     p2     #
##############

ls.p2.1H.RB <- list()
for (j in 1:iter){
  ls.p2.1H.RB$CMOMd[[j]] <- (output.1H[5,j]-Original.H[4,j])/Original.H[4,j]
  ls.p2.1H.RB$CMOMi[[j]] <- (output.1H[11,j]-Original.H[4,j])/Original.H[4,j]
  ls.p2.1H.RB$CMR[[j]] <- (output.1H[17,j]-Original.H[4,j])/Original.H[4,j]}

ls.p2.1H.SE <- list()
for (j in 1:iter){
  ls.p2.1H.SE$CMOMd[[j]] <- output.1H[6,j]/Original.H[4,j]
  ls.p2.1H.SE$CMOMi[[j]] <- output.1H[12,j]/Original.H[4,j]
  ls.p2.1H.SE$CMR[[j]] <- output.1H[18,j]/Original.H[4,j]}

ls.p2.1H.MSE <- list()
for (j in 1:iter){
  ls.p2.1H.MSE$CMOMd[[j]] <- ls.p2.1H.RB$CMOMd[[j]]^2+ls.p2.1H.SE$CMOMd[[j]]^2
  ls.p2.1H.MSE$CMOMi[[j]] <- ls.p2.1H.RB$CMOMi[[j]]^2+ls.p2.1H.SE$CMOMi[[j]]^2
  ls.p2.1H.MSE$CMR[[j]] <- ls.p2.1H.RB$CMR[[j]]^2+ls.p2.1H.SE$CMR[[j]]^2}

# ===================== ZPNEc   &   ZPNEs =====================

output.ZPNEc <- H.output.ZPNEc[c(7:10),]
output.ZPNEs <- H.output.ZPNEs[c(7:10),]

######################
#         N          #
######################

ls.N.ZPNE <- list()
for (ite in 1:iter){
  ls.N.ZPNE[[ite]] <- c(mean(ls.H.N[[ite]][1:3]),mean(ls.H.N[[ite]][4:6]),mean(ls.H.N[[ite]][7:9]))}

for (ite in 1:iter){
  ls.N.1H.RB$ZPNEc[[ite]] <- (HN.ZPNEc[[ite]]-ls.N.ZPNE[[ite]])/ls.N.ZPNE[[ite]]
  ls.N.1H.RB$ZPNEs[[ite]] <- (HN.ZPNEs[[ite]]-ls.N.ZPNE[[ite]])/ls.N.ZPNE[[ite]]}

for (ite in 1:iter){
  ls.N.1H.SE$ZPNEc[[ite]] <- HNs.ZPNEc[[ite]]/ls.N.ZPNE[[ite]]
  ls.N.1H.SE$ZPNEs[[ite]] <- HNs.ZPNEs[[ite]]/ls.N.ZPNE[[ite]]}

for (ite in 1:iter){
  ls.N.1H.MSE$ZPNEc[[ite]] <- ls.N.1H.RB$ZPNEc[[ite]]^2+ls.N.1H.SE$ZPNEc[[ite]]^2
  ls.N.1H.MSE$ZPNEs[[ite]] <- ls.N.1H.RB$ZPNEs[[ite]]^2+ls.N.1H.SE$ZPNEs[[ite]]^2}

##############
#   phib     #
##############

for (j in 1:iter){
  ls.phib.1H.RB$ZPNEc[[j]] <- (output.ZPNEc[1,j]-Original.H[2,j])/Original.H[2,j]
  ls.phib.1H.RB$ZPNEs[[j]] <- (output.ZPNEs[1,j]-Original.H[2,j])/Original.H[2,j]}

for (j in 1:iter){
  ls.phib.1H.SE$ZPNEc[[j]] <- output.ZPNEc[2,j]/Original.H[2,j]
  ls.phib.1H.SE$ZPNEs[[j]] <- output.ZPNEs[2,j]/Original.H[2,j]}

for (j in 1:iter){
  ls.phib.1H.MSE$ZPNEc[[j]] <- ls.phib.1H.RB$ZPNEc[[j]]^2+ls.phib.1H.SE$ZPNEc[[j]]^2
  ls.phib.1H.MSE$ZPNEs[[j]] <- ls.phib.1H.RB$ZPNEs[[j]]^2+ls.phib.1H.SE$ZPNEs[[j]]^2}

##############
#     p2     #
##############

for (j in 1:iter){
  ls.p2.1H.RB$ZPNEc[[j]] <- (output.ZPNEc[3,j]-Original.H[4,j])/Original.H[4,j]
  ls.p2.1H.RB$ZPNEs[[j]] <- (output.ZPNEs[3,j]-Original.H[4,j])/Original.H[4,j]}

for (j in 1:iter){
  ls.p2.1H.SE$ZPNEc[[j]] <- output.ZPNEc[4,j]/Original.H[4,j]
  ls.p2.1H.SE$ZPNEs[[j]] <- output.ZPNEs[4,j]/Original.H[4,j]}

for (j in 1:iter){
  ls.p2.1H.MSE$ZPNEc[[j]] <- ls.p2.1H.RB$ZPNEc[[j]]^2+ls.p2.1H.SE$ZPNEc[[j]]^2
  ls.p2.1H.MSE$ZPNEs[[j]] <- ls.p2.1H.RB$ZPNEs[[j]]^2+ls.p2.1H.SE$ZPNEs[[j]]^2}

save.image("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-1H.Rdata")

########################################################
#                       T A B L E                      #
########################################################

ls.Exc1H <- ls.graph <- list()

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

N.RB.G.1H <- list()
for (ite in ini:last){
  N.RB.G.1H$CMOMd[[ite]] <- mean(abs(ls.N.1H.RB$CMOMd[[ite]]))
  N.RB.G.1H$CMOMi[[ite]] <- mean(abs(ls.N.1H.RB$CMOMi[[ite]]))
  N.RB.G.1H$CMR[[ite]] <- mean(abs(ls.N.1H.RB$CMR[[ite]]))
  N.RB.G.1H$ZPNEc[[ite]] <- mean(abs(ls.N.1H.RB$ZPNEc[[ite]]))
  N.RB.G.1H$ZPNEs[[ite]] <- mean(abs(ls.N.1H.RB$ZPNEs[[ite]]))}

for (ite in ini:last){
  N.RB.G.1H$CMOMd <- mean(N.RB.G.1H$CMOMd, na.rm = T)
  N.RB.G.1H$CMOMi <- mean(N.RB.G.1H$CMOMi, na.rm = T)
  N.RB.G.1H$CMR <- mean(N.RB.G.1H$CMR, na.rm = T)
  N.RB.G.1H$ZPNEc <- mean(N.RB.G.1H$ZPNEc, na.rm = T)
  N.RB.G.1H$ZPNEs <- mean(N.RB.G.1H$ZPNEs, na.rm = T)}

N.SE.G.1H <- list()
for (ite in ini:last){
  N.SE.G.1H$CMOMd[[ite]] <- mean(ls.N.1H.SE$CMOMd[[ite]])
  N.SE.G.1H$CMOMi[[ite]] <- mean(ls.N.1H.SE$CMOMi[[ite]])
  N.SE.G.1H$CMR[[ite]] <- mean(ls.N.1H.SE$CMR[[ite]])
  N.SE.G.1H$ZPNEc[[ite]] <- mean(ls.N.1H.SE$ZPNEc[[ite]])
  N.SE.G.1H$ZPNEs[[ite]] <- mean(ls.N.1H.SE$ZPNEs[[ite]])}

for (ite in ini:last){
  N.SE.G.1H$CMOMd <- mean(N.SE.G.1H$CMOMd, na.rm = T)
  N.SE.G.1H$CMOMi <- mean(N.SE.G.1H$CMOMi, na.rm = T)
  N.SE.G.1H$CMR <- mean(N.SE.G.1H$CMR, na.rm = T)
  N.SE.G.1H$ZPNEc <- mean(N.SE.G.1H$ZPNEc, na.rm = T)
  N.SE.G.1H$ZPNEs <- mean(N.SE.G.1H$ZPNEs, na.rm = T)}

N.MSE.G.1H <- list()
for (ite in ini:last){
  N.MSE.G.1H$CMOMd[[ite]] <- mean(ls.N.1H.MSE$CMOMd[[ite]])
  N.MSE.G.1H$CMOMi[[ite]] <- mean(ls.N.1H.MSE$CMOMi[[ite]])
  N.MSE.G.1H$CMR[[ite]] <- mean(ls.N.1H.MSE$CMR[[ite]])
  N.MSE.G.1H$ZPNEc[[ite]] <- mean(ls.N.1H.MSE$ZPNEc[[ite]])
  N.MSE.G.1H$ZPNEs[[ite]] <- mean(ls.N.1H.MSE$ZPNEs[[ite]])}

for (ite in ini:last){
  N.MSE.G.1H$CMOMd <- mean(N.MSE.G.1H$CMOMd, na.rm = T)
  N.MSE.G.1H$CMOMi <- mean(N.MSE.G.1H$CMOMi, na.rm = T)
  N.MSE.G.1H$CMR <- mean(N.MSE.G.1H$CMR, na.rm = T)
  N.MSE.G.1H$ZPNEc <- mean(N.MSE.G.1H$ZPNEc, na.rm = T)
  N.MSE.G.1H$ZPNEs <- mean(N.MSE.G.1H$ZPNEs, na.rm = T)}

N.1H <- list(RB=N.RB.G.1H, RSE=N.SE.G.1H, MSE=N.MSE.G.1H)

# ====================== PHIB ======================= #

phib.RB.G.1H <- list()
for (ite in ini:last){
  phib.RB.G.1H$CMOMd <- mean(abs(ls.phib.1H.RB$CMOMd[ini:last]), na.rm = T)
  phib.RB.G.1H$CMOMi <- mean(abs(ls.phib.1H.RB$CMOMi[ini:last]), na.rm = T)
  phib.RB.G.1H$CMR <- mean(abs(ls.phib.1H.RB$CMR[ini:last]), na.rm = T)
  phib.RB.G.1H$ZPNEc <- mean(abs(ls.phib.1H.RB$ZPNEc[ini:last]), na.rm = T)
  phib.RB.G.1H$ZPNEs <- mean(abs(ls.phib.1H.RB$ZPNEs[ini:last]), na.rm = T)}

phib.SE.G.1H <- list()
for (ite in ini:last){
  phib.SE.G.1H$CMOMd <- mean(abs(ls.phib.1H.SE$CMOMd[ini:last]), na.rm = T)
  phib.SE.G.1H$CMOMi <- mean(abs(ls.phib.1H.SE$CMOMi[ini:last]), na.rm = T)
  phib.SE.G.1H$CMR <- mean(abs(ls.phib.1H.SE$CMR[ini:last]), na.rm = T)
  phib.SE.G.1H$ZPNEc <- mean(abs(ls.phib.1H.SE$ZPNEc[ini:last]), na.rm = T)
  phib.SE.G.1H$ZPNEs <- mean(abs(ls.phib.1H.SE$ZPNEs[ini:last]), na.rm = T)}

phib.MSE.G.1H <- list()
for (ite in ini:last){
  phib.MSE.G.1H$CMOMd <- mean(abs(ls.phib.1H.MSE$CMOMd[ini:last]), na.rm = T)
  phib.MSE.G.1H$CMOMi <- mean(abs(ls.phib.1H.MSE$CMOMi[ini:last]), na.rm = T)
  phib.MSE.G.1H$CMR <- mean(abs(ls.phib.1H.MSE$CMR[ini:last]), na.rm = T)
  phib.MSE.G.1H$ZPNEc <- mean(abs(ls.phib.1H.MSE$ZPNEc[ini:last]), na.rm = T)
  phib.MSE.G.1H$ZPNEs <- mean(abs(ls.phib.1H.MSE$ZPNEs[ini:last]), na.rm = T)}

phib.1H <- list(RB=phib.RB.G.1H, RSE=phib.SE.G.1H, MSE=phib.MSE.G.1H)

# ====================== P2 ======================= #

p2.RB.G.1H <- list()
for (ite in ini:last){
  p2.RB.G.1H$CMOMd <- mean(abs(ls.p2.1H.RB$CMOMd[ini:last]), na.rm = T)
  p2.RB.G.1H$CMOMi <- mean(abs(ls.p2.1H.RB$CMOMi[ini:last]), na.rm = T)
  p2.RB.G.1H$CMR <- mean(abs(ls.p2.1H.RB$CMR[ini:last]), na.rm = T)
  p2.RB.G.1H$ZPNEc <- mean(abs(ls.p2.1H.RB$ZPNEc[ini:last]), na.rm = T)
  p2.RB.G.1H$ZPNEs <- mean(abs(ls.p2.1H.RB$ZPNEs[ini:last]), na.rm = T)}

p2.SE.G.1H <- list()
for (ite in ini:last){
  p2.SE.G.1H$CMOMd <- mean(abs(ls.p2.1H.SE$CMOMd[ini:last]), na.rm = T)
  p2.SE.G.1H$CMOMi <- mean(abs(ls.p2.1H.SE$CMOMi[ini:last]), na.rm = T)
  p2.SE.G.1H$CMR <- mean(abs(ls.p2.1H.SE$CMR[ini:last]), na.rm = T)
  p2.SE.G.1H$ZPNEc <- mean(abs(ls.p2.1H.SE$ZPNEc[ini:last]), na.rm = T)
  p2.SE.G.1H$ZPNEs <- mean(abs(ls.p2.1H.SE$ZPNEs[ini:last]), na.rm = T)}

p2.MSE.G.1H <- list()
for (ite in ini:last){
  p2.MSE.G.1H$CMOMd <- mean(abs(ls.p2.1H.MSE$CMOMd[ini:last]), na.rm = T)
  p2.MSE.G.1H$CMOMi <- mean(abs(ls.p2.1H.MSE$CMOMi[ini:last]), na.rm = T)
  p2.MSE.G.1H$CMR <- mean(abs(ls.p2.1H.MSE$CMR[ini:last]), na.rm = T)
  p2.MSE.G.1H$ZPNEc <- mean(abs(ls.p2.1H.MSE$ZPNEc[ini:last]), na.rm = T)
  p2.MSE.G.1H$ZPNEs <- mean(abs(ls.p2.1H.MSE$ZPNEs[ini:last]), na.rm = T)}

p2.1H <- list(RB=p2.RB.G.1H, RSE=p2.SE.G.1H, MSE=p2.MSE.G.1H)

# ====================== B ======================= #

B.RB.G.1H <- list()
for (ite in ini:last){
  B.RB.G.1H$CMOMd[[ite]] <- mean(abs(ls.B.1H.RB$CMOMd[[ite]]))
  B.RB.G.1H$CMOMi[[ite]] <- mean(abs(ls.B.1H.RB$CMOMi[[ite]]))
  B.RB.G.1H$CMR[[ite]] <- mean(abs(ls.B.1H.RB$CMR[[ite]]))
  B.RB.G.1H$ZPNEc[[ite]] <- NA
  B.RB.G.1H$ZPNEs[[ite]] <- NA}

for (ite in ini:last){
  B.RB.G.1H$CMOMd <- mean(B.RB.G.1H$CMOMd, na.rm = T)
  B.RB.G.1H$CMOMi <- mean(B.RB.G.1H$CMOMi, na.rm = T)
  B.RB.G.1H$CMR <- mean(B.RB.G.1H$CMR, na.rm = T)
  B.RB.G.1H$ZPNEc <- NA
  B.RB.G.1H$ZPNEs <- NA}

B.SE.G.1H <- list()
for (ite in ini:last){
  B.SE.G.1H$CMOMd[[ite]] <- mean(ls.B.1H.SE$CMOMd[[ite]])
  B.SE.G.1H$CMOMi[[ite]] <- mean(ls.B.1H.SE$CMOMi[[ite]])
  B.SE.G.1H$CMR[[ite]] <- mean(ls.B.1H.SE$CMR[[ite]])
  B.SE.G.1H$ZPNEc[[ite]] <- NA
  B.SE.G.1H$ZPNEs[[ite]] <- NA}

for (ite in ini:last){
  B.SE.G.1H$CMOMd <- mean(B.SE.G.1H$CMOMd, na.rm = T)
  B.SE.G.1H$CMOMi <- mean(B.SE.G.1H$CMOMi, na.rm = T)
  B.SE.G.1H$CMR <- mean(B.SE.G.1H$CMR, na.rm = T)
  B.SE.G.1H$ZPNEc <- NA
  B.SE.G.1H$ZPNEs <- NA}

B.MSE.G.1H <- list()
for (ite in ini:last){
  B.MSE.G.1H$CMOMd[[ite]] <- mean(ls.B.1H.MSE$CMOMd[[ite]])
  B.MSE.G.1H$CMOMi[[ite]] <- mean(ls.B.1H.MSE$CMOMi[[ite]])
  B.MSE.G.1H$CMR[[ite]] <- mean(ls.B.1H.MSE$CMR[[ite]])
  B.MSE.G.1H$ZPNEc[[ite]] <- NA
  B.MSE.G.1H$ZPNEs[[ite]] <- NA}

for (ite in ini:last){
  B.MSE.G.1H$CMOMd <- mean(B.MSE.G.1H$CMOMd, na.rm = T)
  B.MSE.G.1H$CMOMi <- mean(B.MSE.G.1H$CMOMi, na.rm = T)
  B.MSE.G.1H$CMR <- mean(B.MSE.G.1H$CMR, na.rm = T)
  B.MSE.G.1H$ZPNEc <- NA
  B.MSE.G.1H$ZPNEs <- NA}

B.1H <- list(RB=B.RB.G.1H, RSE=B.SE.G.1H, MSE=B.MSE.G.1H)

# ====================== PHIN ======================= #

phin.RB.G.1H <- list()
for (ite in ini:last){
  phin.RB.G.1H$CMOMd <- mean(abs(ls.phin.1H.RB$CMOMd[ini:last]), na.rm = T)
  phin.RB.G.1H$CMOMi <- mean(abs(ls.phin.1H.RB$CMOMi[ini:last]), na.rm = T)
  phin.RB.G.1H$CMR <- mean(abs(ls.phin.1H.RB$CMR[ini:last]), na.rm = T)
  phin.RB.G.1H$ZPNEc <- NA
  phin.RB.G.1H$ZPNEs <- NA}

phin.SE.G.1H <- list()
for (ite in ini:last){
  phin.SE.G.1H$CMOMd <- mean(abs(ls.phin.1H.SE$CMOMd[ini:last]), na.rm = T)
  phin.SE.G.1H$CMOMi <- mean(abs(ls.phin.1H.SE$CMOMi[ini:last]), na.rm = T)
  phin.SE.G.1H$CMR <- mean(abs(ls.phin.1H.SE$CMR[ini:last]), na.rm = T)
  phin.SE.G.1H$ZPNEc <- NA
  phin.SE.G.1H$ZPNEs <- NA}

phin.MSE.G.1H <- list()
for (ite in ini:last){
  phin.MSE.G.1H$CMOMd <- mean(abs(ls.phin.1H.MSE$CMOMd[ini:last]), na.rm = T)
  phin.MSE.G.1H$CMOMi <- mean(abs(ls.phin.1H.MSE$CMOMi[ini:last]), na.rm = T)
  phin.MSE.G.1H$CMR <- mean(abs(ls.phin.1H.MSE$CMR[ini:last]), na.rm = T)
  phin.MSE.G.1H$ZPNEc <- NA
  phin.MSE.G.1H$ZPNEs <- NA}

phin.1H <- list(RB=phin.RB.G.1H, RSE=phin.SE.G.1H, MSE=phin.MSE.G.1H)

# ================= DATA FRAME =================

# ~~~~~ N ~~~~~
stack.N.1H <- stack(N.1H)
stack.N.1H$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.N.1H)[1:2] <- c("Values", "Results")
if (PERC == 1) {stack.N.1H$P.factor <- factor("10%")}
if (PERC == 2) {stack.N.1H$P.factor <- factor("20%")}
if (PERC == 3) {stack.N.1H$P.factor <- factor("30%")}
if (PERC == 4) {stack.N.1H$P.factor <- factor("40%")}
if (PERC == 5) {stack.N.1H$P.factor <- factor("50%")}
if (PERC == 6) {stack.N.1H$P.factor <- factor("60%")}
stack.N.1H$Parameters <- factor("Number of Individuals")
stack.N.1H$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ B ~~~~~
stack.B.1H <- stack(B.1H)
stack.B.1H$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.B.1H)[1:2] <- c("Values", "Results")
if (PERC == 1) {stack.B.1H$P.factor <- factor("10%")}
if (PERC == 2) {stack.B.1H$P.factor <- factor("20%")}
if (PERC == 3) {stack.B.1H$P.factor <- factor("30%")}
if (PERC == 4) {stack.B.1H$P.factor <- factor("40%")}
if (PERC == 5) {stack.B.1H$P.factor <- factor("50%")}
if (PERC == 6) {stack.B.1H$P.factor <- factor("60%")}
stack.B.1H$Parameters <- factor("Recruitment")
stack.B.1H$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ phin ~~~~~
stack.phin.1H <- stack(phin.1H)
stack.phin.1H$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.phin.1H)[1:2] <- c("Values", "Results")
if (PERC == 1) {stack.phin.1H$P.factor <- factor("10%")}
if (PERC == 2) {stack.phin.1H$P.factor <- factor("20%")}
if (PERC == 3) {stack.phin.1H$P.factor <- factor("30%")}
if (PERC == 4) {stack.phin.1H$P.factor <- factor("40%")}
if (PERC == 5) {stack.phin.1H$P.factor <- factor("50%")}
if (PERC == 6) {stack.phin.1H$P.factor <- factor("60%")}
stack.phin.1H$Parameters <- factor("Survival Second. Occ.")
stack.phin.1H$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ phib ~~~~~
stack.phib.1H <- stack(phib.1H)
stack.phib.1H$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.phib.1H)[1:2] <- c("Values", "Results")
if (PERC == 1) {stack.phib.1H$P.factor <- factor("10%")}
if (PERC == 2) {stack.phib.1H$P.factor <- factor("20%")}
if (PERC == 3) {stack.phib.1H$P.factor <- factor("30%")}
if (PERC == 4) {stack.phib.1H$P.factor <- factor("40%")}
if (PERC == 5) {stack.phib.1H$P.factor <- factor("50%")}
if (PERC == 6) {stack.phib.1H$P.factor <- factor("60%")}
stack.phib.1H$Parameters <- factor("Survival Prim. Occ.")
stack.phib.1H$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ p2 ~~~~~
stack.p2.1H <- stack(p2.1H)
stack.p2.1H$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.p2.1H)[1:2] <- c("Values", "Results")
if (PERC == 1) {stack.p2.1H$P.factor <- factor("10%")}
if (PERC == 2) {stack.p2.1H$P.factor <- factor("20%")}
if (PERC == 3) {stack.p2.1H$P.factor <- factor("30%")}
if (PERC == 4) {stack.p2.1H$P.factor <- factor("40%")}
if (PERC == 5) {stack.p2.1H$P.factor <- factor("50%")}
if (PERC == 6) {stack.p2.1H$P.factor <- factor("60%")}
stack.p2.1H$Parameters <- factor("Observation probability")
stack.p2.1H$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

graph <- rbind(stack.N.1H, stack.B.1H, stack.phin.1H, stack.phib.1H, stack.p2.1H)

ls.graph[[PERC]] <- assign(paste("graph", PERC, sep=""), graph)
ls.Exc1H[[PERC]] <- subset(graph, !(graph$Value < 2 & graph$Value > -2))}

save(ls.graph,file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-1H.Rdata")
save(ls.Exc1H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Exc-1H.Rdata") 