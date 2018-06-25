#####################################################################
#                                                                   #
#         6a.    M S E,     R B    A N D     R S E                  #
#                                                                   #
#                           1  P                                    #
#                                                                   #
#####################################################################

#############################################################################
# Function to compute relative bias, standard error, and Mean squared error #
#                 for N, B, phi.in, phi.btw, p                              #
#############################################################################

# 1 P

# ----- FOR N ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/N-1P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/N-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/N-ZPNEc.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/N-ZPNEs.RData")

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/Ns-1P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/Ns-ZPNEc.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/Ns-ZPNEs.RData")

# ----- FOR phi and p ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/output1P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/outputZPNEs.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/outputZPNEc.RData")

Original.1P2P <- matrix(c(0.99, 0.85, 0.2, 0.6),ncol=100,nrow=4)
rownames(Original.1P2P) <- c("phin","phib","p1","p2")

# ----- FOR B ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/B-1P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/B-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/Bs-1P.RData")

iter <- 100

# ===================== CMOMd, CMOMi, CMR =====================

######################
#         N          #
######################

ls.N.1P.RB <- list()
for (ite in 1:iter){
ls.N.1P.RB$CMOMd[[ite]] <- (N.1P$CMOMd[[ite]]-ls.N[[ite]])/ls.N[[ite]]
ls.N.1P.RB$CMOMi[[ite]] <- (N.1P$CMOMi[[ite]]-ls.N[[ite]])/ls.N[[ite]]
ls.N.1P.RB$CMR[[ite]] <- (N.1P$CMR[[ite]]-ls.N[[ite]])/ls.N[[ite]]}

ls.N.1P.SE <- list()
for (ite in 1:iter){
  ls.N.1P.SE$CMOMd[[ite]] <- Ns.1P$CMOMd[[ite]]/ls.N[[ite]]
  ls.N.1P.SE$CMOMi[[ite]] <- Ns.1P$CMOMi[[ite]]/ls.N[[ite]]
  ls.N.1P.SE$CMR[[ite]] <- Ns.1P$CMR[[ite]]/ls.N[[ite]]}

ls.N.1P.MSE <- list()
for (ite in 1:iter){
  ls.N.1P.MSE$CMOMd[[ite]] <- ls.N.1P.RB$CMOMd[[ite]]^2+ls.N.1P.SE$CMOMd[[ite]]^2
  ls.N.1P.MSE$CMOMi[[ite]] <- ls.N.1P.RB$CMOMi[[ite]]^2+ls.N.1P.SE$CMOMi[[ite]]^2
  ls.N.1P.MSE$CMR[[ite]] <- ls.N.1P.RB$CMR[[ite]]^2+ls.N.1P.SE$CMR[[ite]]^2}

######################
#         B          #
######################

ls.B.1P.RB <- list()
for (ite in 1:iter){
  ls.B.1P.RB$CMOMd[[ite]] <- (B.1P$CMOMd[[ite]]-ls.B[[ite]])/ls.B[[ite]]
  ls.B.1P.RB$CMOMi[[ite]] <- (B.1P$CMOMi[[ite]]-ls.B[[ite]])/ls.B[[ite]]
  ls.B.1P.RB$CMR[[ite]] <- (B.1P$CMR[[ite]]-ls.B[[ite]])/ls.B[[ite]]}

ls.B.1P.SE <- list()
for (ite in 1:iter){
  ls.B.1P.SE$CMOMd[[ite]] <- Bs.1P$CMOMd[[ite]]/ls.B[[ite]]
  ls.B.1P.SE$CMOMi[[ite]] <- Bs.1P$CMOMi[[ite]]/ls.B[[ite]]
  ls.B.1P.SE$CMR[[ite]] <- Bs.1P$CMR[[ite]]/ls.B[[ite]]}

ls.B.1P.MSE <- list()
for (ite in 1:iter){
  ls.B.1P.MSE$CMOMd[[ite]] <- ls.B.1P.RB$CMOMd[[ite]]^2+ls.B.1P.SE$CMOMd[[ite]]^2
  ls.B.1P.MSE$CMOMi[[ite]] <- ls.B.1P.RB$CMOMi[[ite]]^2+ls.B.1P.SE$CMOMi[[ite]]^2
  ls.B.1P.MSE$CMR[[ite]] <- ls.B.1P.RB$CMR[[ite]]^2+ls.B.1P.SE$CMR[[ite]]^2}

##############
#   phin     #
##############

output.1P <- output.1P[c(5:10,15:20,25:30),]

ls.phin.1P.RB <- list()
for (j in 1:iter){
  ls.phin.1P.RB$CMOMd[[j]] <- (output.1P[1,j]-Original.1P2P[1,j])/Original.1P2P[1,j]
  ls.phin.1P.RB$CMOMi[[j]] <- (output.1P[7,j]-Original.1P2P[1,j])/Original.1P2P[1,j]
  ls.phin.1P.RB$CMR[[j]] <- (output.1P[13,j]-Original.1P2P[1,j])/Original.1P2P[1,j]}

ls.phin.1P.SE <- list()
for (j in 1:iter){
  ls.phin.1P.SE$CMOMd[[j]] <- output.1P[2,j]/Original.1P2P[1,j]
  ls.phin.1P.SE$CMOMi[[j]] <- output.1P[8,j]/Original.1P2P[1,j]
  ls.phin.1P.SE$CMR[[j]] <- output.1P[14,j]/Original.1P2P[1,j]}

ls.phin.1P.MSE <- list()
for (j in 1:iter){
  ls.phin.1P.MSE$CMOMd[[j]] <- ls.phin.1P.RB$CMOMd[[j]]^2+ls.phin.1P.SE$CMOMd[[j]]^2
  ls.phin.1P.MSE$CMOMi[[j]] <- ls.phin.1P.RB$CMOMi[[j]]^2+ls.phin.1P.SE$CMOMi[[j]]^2
  ls.phin.1P.MSE$CMR[[j]] <- ls.phin.1P.RB$CMR[[j]]^2+ls.phin.1P.SE$CMR[[j]]^2}

##############
#   phib     #
##############

ls.phib.1P.RB <- list()
for (j in 1:iter){
  ls.phib.1P.RB$CMOMd[[j]] <- (output.1P[3,j]-Original.1P2P[2,j])/Original.1P2P[2,j]
  ls.phib.1P.RB$CMOMi[[j]] <- (output.1P[9,j]-Original.1P2P[2,j])/Original.1P2P[2,j]
  ls.phib.1P.RB$CMR[[j]] <- (output.1P[15,j]-Original.1P2P[2,j])/Original.1P2P[2,j]}

ls.phib.1P.SE <- list()
for (j in 1:iter){
  ls.phib.1P.SE$CMOMd[[j]] <- output.1P[4,j]/Original.1P2P[2,j]
  ls.phib.1P.SE$CMOMi[[j]] <- output.1P[10,j]/Original.1P2P[2,j]
  ls.phib.1P.SE$CMR[[j]] <- output.1P[16,j]/Original.1P2P[2,j]}

ls.phib.1P.MSE <- list()
for (j in 1:iter){
  ls.phib.1P.MSE$CMOMd[[j]] <- ls.phib.1P.RB$CMOMd[[j]]^2+ls.phib.1P.SE$CMOMd[[j]]^2
  ls.phib.1P.MSE$CMOMi[[j]] <- ls.phib.1P.RB$CMOMi[[j]]^2+ls.phib.1P.SE$CMOMi[[j]]^2
  ls.phib.1P.MSE$CMR[[j]] <- ls.phib.1P.RB$CMR[[j]]^2+ls.phib.1P.SE$CMR[[j]]^2}

##############
#     p2     #
##############

ls.p2.1P.RB <- list()
for (j in 1:iter){
  ls.p2.1P.RB$CMOMd[[j]] <- (output.1P[5,j]-Original.1P2P[4,j])/Original.1P2P[4,j]
  ls.p2.1P.RB$CMOMi[[j]] <- (output.1P[11,j]-Original.1P2P[4,j])/Original.1P2P[4,j]
  ls.p2.1P.RB$CMR[[j]] <- (output.1P[17,j]-Original.1P2P[4,j])/Original.1P2P[4,j]}

ls.p2.1P.SE <- list()
for (j in 1:iter){
  ls.p2.1P.SE$CMOMd[[j]] <- output.1P[6,j]/Original.1P2P[4,j]
  ls.p2.1P.SE$CMOMi[[j]] <- output.1P[12,j]/Original.1P2P[4,j]
  ls.p2.1P.SE$CMR[[j]] <- output.1P[18,j]/Original.1P2P[4,j]}

ls.p2.1P.MSE <- list()
for (j in 1:iter){
  ls.p2.1P.MSE$CMOMd[[j]] <- ls.p2.1P.RB$CMOMd[[j]]^2+ls.p2.1P.SE$CMOMd[[j]]^2
  ls.p2.1P.MSE$CMOMi[[j]] <- ls.p2.1P.RB$CMOMi[[j]]^2+ls.p2.1P.SE$CMOMi[[j]]^2
  ls.p2.1P.MSE$CMR[[j]] <- ls.p2.1P.RB$CMR[[j]]^2+ls.p2.1P.SE$CMR[[j]]^2}

# ===================== ZPNEc   &   ZPNEs =====================

output.ZPNEc <- output.ZPNEc[c(7:10),]
output.ZPNEs <- output.ZPNEs[c(7:10),]

######################
#         N          #
######################

ls.N.ZPNE <- list()
for (ite in 1:iter){
ls.N.ZPNE[[ite]] <- c(mean(ls.N[[ite]][1:3]),mean(ls.N[[ite]][4:6]),mean(ls.N[[ite]][7:9]))}

for (ite in 1:iter){
  ls.N.1P.RB$ZPNEc[[ite]] <- (N.ZPNEc[[ite]]-ls.N.ZPNE[[ite]])/ls.N.ZPNE[[ite]]
  ls.N.1P.RB$ZPNEs[[ite]] <- (N.ZPNEs[[ite]]-ls.N.ZPNE[[ite]])/ls.N.ZPNE[[ite]]}

for (ite in 1:iter){
  ls.N.1P.SE$ZPNEc[[ite]] <- Ns.ZPNEc[[ite]]/ls.N.ZPNE[[ite]]
  ls.N.1P.SE$ZPNEs[[ite]] <- Ns.ZPNEs[[ite]]/ls.N.ZPNE[[ite]]}

for (ite in 1:iter){
  ls.N.1P.MSE$ZPNEc[[ite]] <- ls.N.1P.RB$ZPNEc[[ite]]^2+ls.N.1P.SE$ZPNEc[[ite]]^2
  ls.N.1P.MSE$ZPNEs[[ite]] <- ls.N.1P.RB$ZPNEs[[ite]]^2+ls.N.1P.SE$ZPNEs[[ite]]^2}

##############
#   phib     #
##############

for (j in 1:iter){
  ls.phib.1P.RB$ZPNEc[[j]] <- (output.ZPNEc[1,j]-Original.1P2P[2,j])/Original.1P2P[2,j]
  ls.phib.1P.RB$ZPNEs[[j]] <- (output.ZPNEs[1,j]-Original.1P2P[2,j])/Original.1P2P[2,j]}

for (j in 1:iter){
  ls.phib.1P.SE$ZPNEc[[j]] <- output.ZPNEc[2,j]/Original.1P2P[2,j]
  ls.phib.1P.SE$ZPNEs[[j]] <- output.ZPNEs[2,j]/Original.1P2P[2,j]}

for (j in 1:iter){
  ls.phib.1P.MSE$ZPNEc[[j]] <- ls.phib.1P.RB$ZPNEc[[j]]^2+ls.phib.1P.SE$ZPNEc[[j]]^2
  ls.phib.1P.MSE$ZPNEs[[j]] <- ls.phib.1P.RB$ZPNEs[[j]]^2+ls.phib.1P.SE$ZPNEs[[j]]^2}

##############
#     p2     #
##############

for (j in 1:iter){
  ls.p2.1P.RB$ZPNEc[[j]] <- (output.ZPNEc[3,j]-Original.1P2P[4,j])/Original.1P2P[4,j]
  ls.p2.1P.RB$ZPNEs[[j]] <- (output.ZPNEs[3,j]-Original.1P2P[4,j])/Original.1P2P[4,j]}

for (j in 1:iter){
  ls.p2.1P.SE$ZPNEc[[j]] <- output.ZPNEc[4,j]/Original.1P2P[4,j]
  ls.p2.1P.SE$ZPNEs[[j]] <- output.ZPNEs[4,j]/Original.1P2P[4,j]}

for (j in 1:iter){
  ls.p2.1P.MSE$ZPNEc[[j]] <- ls.p2.1P.RB$ZPNEc[[j]]^2+ls.p2.1P.SE$ZPNEc[[j]]^2
  ls.p2.1P.MSE$ZPNEs[[j]] <- ls.p2.1P.RB$ZPNEs[[j]]^2+ls.p2.1P.SE$ZPNEs[[j]]^2}

save.image("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-1P.Rdata")

########################################################
#                       T A B L E                      #
########################################################

# ====================== N ======================= #

N.RB.G.1P <- list()
for (ite in 1:iter){
N.RB.G.1P$CMOMd[[ite]] <- mean(abs(ls.N.1P.RB$CMOMd[[ite]]))
N.RB.G.1P$CMOMi[[ite]] <- mean(abs(ls.N.1P.RB$CMOMi[[ite]]))
N.RB.G.1P$CMR[[ite]] <- mean(abs(ls.N.1P.RB$CMR[[ite]]))
N.RB.G.1P$ZPNEc[[ite]] <- mean(abs(ls.N.1P.RB$ZPNEc[[ite]]))
N.RB.G.1P$ZPNEs[[ite]] <- mean(abs(ls.N.1P.RB$ZPNEs[[ite]]))}

for (ite in 1:iter){
  N.RB.G.1P$CMOMd <- mean(N.RB.G.1P$CMOMd, na.rm = T)
  N.RB.G.1P$CMOMi <- mean(N.RB.G.1P$CMOMi, na.rm = T)
  N.RB.G.1P$CMR <- mean(N.RB.G.1P$CMR, na.rm = T)
  N.RB.G.1P$ZPNEc <- mean(N.RB.G.1P$ZPNEc, na.rm = T)
  N.RB.G.1P$ZPNEs <- mean(N.RB.G.1P$ZPNEs, na.rm = T)}

N.SE.G.1P <- list()
for (ite in 1:iter){
  N.SE.G.1P$CMOMd[[ite]] <- mean(ls.N.1P.SE$CMOMd[[ite]])
  N.SE.G.1P$CMOMi[[ite]] <- mean(ls.N.1P.SE$CMOMi[[ite]])
  N.SE.G.1P$CMR[[ite]] <- mean(ls.N.1P.SE$CMR[[ite]])
  N.SE.G.1P$ZPNEc[[ite]] <- mean(ls.N.1P.SE$ZPNEc[[ite]])
  N.SE.G.1P$ZPNEs[[ite]] <- mean(ls.N.1P.SE$ZPNEs[[ite]])}

for (ite in 1:iter){
  N.SE.G.1P$CMOMd <- mean(N.SE.G.1P$CMOMd, na.rm = T)
  N.SE.G.1P$CMOMi <- mean(N.SE.G.1P$CMOMi, na.rm = T)
  N.SE.G.1P$CMR <- mean(N.SE.G.1P$CMR, na.rm = T)
  N.SE.G.1P$ZPNEc <- mean(N.SE.G.1P$ZPNEc, na.rm = T)
  N.SE.G.1P$ZPNEs <- mean(N.SE.G.1P$ZPNEs, na.rm = T)}

N.MSE.G.1P <- list()
for (ite in 1:iter){
  N.MSE.G.1P$CMOMd[[ite]] <- mean(ls.N.1P.MSE$CMOMd[[ite]])
  N.MSE.G.1P$CMOMi[[ite]] <- mean(ls.N.1P.MSE$CMOMi[[ite]])
  N.MSE.G.1P$CMR[[ite]] <- mean(ls.N.1P.MSE$CMR[[ite]])
  N.MSE.G.1P$ZPNEc[[ite]] <- mean(ls.N.1P.MSE$ZPNEc[[ite]])
  N.MSE.G.1P$ZPNEs[[ite]] <- mean(ls.N.1P.MSE$ZPNEs[[ite]])}

for (ite in 1:iter){
  N.MSE.G.1P$CMOMd <- mean(N.MSE.G.1P$CMOMd, na.rm = T)
  N.MSE.G.1P$CMOMi <- mean(N.MSE.G.1P$CMOMi, na.rm = T)
  N.MSE.G.1P$CMR <- mean(N.MSE.G.1P$CMR, na.rm = T)
  N.MSE.G.1P$ZPNEc <- mean(N.MSE.G.1P$ZPNEc, na.rm = T)
  N.MSE.G.1P$ZPNEs <- mean(N.MSE.G.1P$ZPNEs, na.rm = T)}

N.1P <- list(RB=N.RB.G.1P, RSE=N.SE.G.1P, MSE=N.MSE.G.1P)

# ====================== PHIB ======================= #

phib.RB.G.1P <- list()
  phib.RB.G.1P$CMOMd <- mean(abs(ls.phib.1P.RB$CMOMd), na.rm = T)
  phib.RB.G.1P$CMOMi <- mean(abs(ls.phib.1P.RB$CMOMi), na.rm = T)
  phib.RB.G.1P$CMR <- mean(abs(ls.phib.1P.RB$CMR), na.rm = T)
  phib.RB.G.1P$ZPNEc <- mean(abs(ls.phib.1P.RB$ZPNEc), na.rm = T)
  phib.RB.G.1P$ZPNEs <- mean(abs(ls.phib.1P.RB$ZPNEs), na.rm = T)

phib.SE.G.1P <- list()
  phib.SE.G.1P$CMOMd <- mean(ls.phib.1P.SE$CMOMd, na.rm = T)
  phib.SE.G.1P$CMOMi <- mean(ls.phib.1P.SE$CMOMi, na.rm = T)
  phib.SE.G.1P$CMR <- mean(ls.phib.1P.SE$CMR, na.rm = T)
  phib.SE.G.1P$ZPNEc <- mean(ls.phib.1P.SE$ZPNEc, na.rm = T)
  phib.SE.G.1P$ZPNEs <- mean(ls.phib.1P.SE$ZPNEs, na.rm = T)

phib.MSE.G.1P <- list()
  phib.MSE.G.1P$CMOMd <- mean(ls.phib.1P.MSE$CMOMd, na.rm = T)
  phib.MSE.G.1P$CMOMi <- mean(ls.phib.1P.MSE$CMOMi, na.rm = T)
  phib.MSE.G.1P$CMR <- mean(ls.phib.1P.MSE$CMR, na.rm = T)
  phib.MSE.G.1P$ZPNEc <- mean(ls.phib.1P.MSE$ZPNEc, na.rm = T)
  phib.MSE.G.1P$ZPNEs <- mean(ls.phib.1P.MSE$ZPNEs, na.rm = T)

phib.1P <- list(RB=phib.RB.G.1P, RSE=phib.SE.G.1P, MSE=phib.MSE.G.1P)

# ====================== P2 ======================= #

p2.RB.G.1P <- list()
p2.RB.G.1P$CMOMd <- mean(abs(ls.p2.1P.RB$CMOMd), na.rm = T)
p2.RB.G.1P$CMOMi <- mean(abs(ls.p2.1P.RB$CMOMi), na.rm = T)
p2.RB.G.1P$CMR <- mean(abs(ls.p2.1P.RB$CMR), na.rm = T)
p2.RB.G.1P$ZPNEc <- mean(abs(ls.p2.1P.RB$ZPNEc), na.rm = T)
p2.RB.G.1P$ZPNEs <- mean(abs(ls.p2.1P.RB$ZPNEs), na.rm = T)

p2.SE.G.1P <- list()
p2.SE.G.1P$CMOMd <- mean(ls.p2.1P.SE$CMOMd, na.rm = T)
p2.SE.G.1P$CMOMi <- mean(ls.p2.1P.SE$CMOMi, na.rm = T)
p2.SE.G.1P$CMR <- mean(ls.p2.1P.SE$CMR, na.rm = T)
p2.SE.G.1P$ZPNEc <- mean(ls.p2.1P.SE$ZPNEc, na.rm = T)
p2.SE.G.1P$ZPNEs <- mean(ls.p2.1P.SE$ZPNEs, na.rm = T)

p2.MSE.G.1P <- list()
p2.MSE.G.1P$CMOMd <- mean(ls.p2.1P.MSE$CMOMd, na.rm = T)
p2.MSE.G.1P$CMOMi <- mean(ls.p2.1P.MSE$CMOMi, na.rm = T)
p2.MSE.G.1P$CMR <- mean(ls.p2.1P.MSE$CMR, na.rm = T)
p2.MSE.G.1P$ZPNEc <- mean(ls.p2.1P.MSE$ZPNEc, na.rm = T)
p2.MSE.G.1P$ZPNEs <- mean(ls.p2.1P.MSE$ZPNEs, na.rm = T)

p2.1P <- list(RB=p2.RB.G.1P, RSE=p2.SE.G.1P, MSE=p2.MSE.G.1P)

# ====================== B ======================= #

B.RB.G.1P <- list()
for (ite in 1:iter){
  B.RB.G.1P$CMOMd[[ite]] <- mean(abs(ls.B.1P.RB$CMOMd[[ite]]))
  B.RB.G.1P$CMOMi[[ite]] <- mean(abs(ls.B.1P.RB$CMOMi[[ite]]))
  B.RB.G.1P$CMR[[ite]] <- mean(abs(ls.B.1P.RB$CMR[[ite]]))
  B.RB.G.1P$ZPNEc[[ite]] <- NA
  B.RB.G.1P$ZPNEs[[ite]] <- NA}

for (ite in 1:iter){
  B.RB.G.1P$CMOMd <- mean(B.RB.G.1P$CMOMd, na.rm = T)
  B.RB.G.1P$CMOMi <- mean(B.RB.G.1P$CMOMi, na.rm = T)
  B.RB.G.1P$CMR <- mean(B.RB.G.1P$CMR, na.rm = T)
  B.RB.G.1P$ZPNEc <- NA
  B.RB.G.1P$ZPNEs <- NA}

B.SE.G.1P <- list()
for (ite in 1:iter){
  B.SE.G.1P$CMOMd[[ite]] <- mean(ls.B.1P.SE$CMOMd[[ite]])
  B.SE.G.1P$CMOMi[[ite]] <- mean(ls.B.1P.SE$CMOMi[[ite]])
  B.SE.G.1P$CMR[[ite]] <- mean(ls.B.1P.SE$CMR[[ite]])
  B.SE.G.1P$ZPNEc[[ite]] <- NA
  B.SE.G.1P$ZPNEs[[ite]] <- NA}

for (ite in 1:iter){
  B.SE.G.1P$CMOMd <- mean(B.SE.G.1P$CMOMd, na.rm = T)
  B.SE.G.1P$CMOMi <- mean(B.SE.G.1P$CMOMi, na.rm = T)
  B.SE.G.1P$CMR <- mean(B.SE.G.1P$CMR, na.rm = T)
  B.SE.G.1P$ZPNEc <- NA
  B.SE.G.1P$ZPNEs <- NA}

B.MSE.G.1P <- list()
for (ite in 1:iter){
  B.MSE.G.1P$CMOMd[[ite]] <- mean(ls.B.1P.MSE$CMOMd[[ite]])
  B.MSE.G.1P$CMOMi[[ite]] <- mean(ls.B.1P.MSE$CMOMi[[ite]])
  B.MSE.G.1P$CMR[[ite]] <- mean(ls.B.1P.MSE$CMR[[ite]])
  B.MSE.G.1P$ZPNEc[[ite]] <- NA
  B.MSE.G.1P$ZPNEs[[ite]] <- NA}

for (ite in 1:iter){
  B.MSE.G.1P$CMOMd <- mean(B.MSE.G.1P$CMOMd, na.rm = T)
  B.MSE.G.1P$CMOMi <- mean(B.MSE.G.1P$CMOMi, na.rm = T)
  B.MSE.G.1P$CMR <- mean(B.MSE.G.1P$CMR, na.rm = T)
  B.MSE.G.1P$ZPNEc <- NA
  B.MSE.G.1P$ZPNEs <- NA}

B.1P <- list(RB=B.RB.G.1P, RSE=B.SE.G.1P, MSE=B.MSE.G.1P)

# ====================== PHIN ======================= #

phin.RB.G.1P <- list()
phin.RB.G.1P$CMOMd <- mean(abs(ls.phin.1P.RB$CMOMd), na.rm = T)
phin.RB.G.1P$CMOMi <- mean(abs(ls.phin.1P.RB$CMOMi), na.rm = T)
phin.RB.G.1P$CMR <- mean(abs(ls.phin.1P.RB$CMR), na.rm = T)
phin.RB.G.1P$ZPNEc <- NA
phin.RB.G.1P$ZPNEs <- NA

phin.SE.G.1P <- list()
phin.SE.G.1P$CMOMd <- mean(ls.phin.1P.SE$CMOMd, na.rm = T)
phin.SE.G.1P$CMOMi <- mean(ls.phin.1P.SE$CMOMi, na.rm = T)
phin.SE.G.1P$CMR <- mean(ls.phin.1P.SE$CMR, na.rm = T)
phin.SE.G.1P$ZPNEc <- NA
phin.SE.G.1P$ZPNEs <- NA

phin.MSE.G.1P <- list()
phin.MSE.G.1P$CMOMd <- mean(ls.phin.1P.MSE$CMOMd, na.rm = T)
phin.MSE.G.1P$CMOMi <- mean(ls.phin.1P.MSE$CMOMi, na.rm = T)
phin.MSE.G.1P$CMR <- mean(ls.phin.1P.MSE$CMR, na.rm = T)
phin.MSE.G.1P$ZPNEc <- NA
phin.MSE.G.1P$ZPNEs <- NA

phin.1P <- list(RB=phin.RB.G.1P, RSE=phin.SE.G.1P, MSE=phin.MSE.G.1P)

# ================= DATA FRAME =================

# ~~~~~ N ~~~~~
stack.N.1P <- stack(N.1P)
stack.N.1P$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.N.1P)[1:2] <- c("Values", "Results")
stack.N.1P$P.factor <- factor("1P")
stack.N.1P$Parameters <- factor("Number of Individuals")
stack.N.1P$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ B ~~~~~
stack.B.1P <- stack(B.1P)
stack.B.1P$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.B.1P)[1:2] <- c("Values", "Results")
stack.B.1P$P.factor <- factor("1P")
stack.B.1P$Parameters <- factor("Recruitment")
stack.B.1P$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ phin ~~~~~
stack.phin.1P <- stack(phin.1P)
stack.phin.1P$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.phin.1P)[1:2] <- c("Values", "Results")
stack.phin.1P$P.factor <- factor("1P")
stack.phin.1P$Parameters <- factor("Survival Second. Occ.")
stack.phin.1P$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ phib ~~~~~
stack.phib.1P <- stack(phib.1P)
stack.phib.1P$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.phib.1P)[1:2] <- c("Values", "Results")
stack.phib.1P$P.factor <- factor("1P")
stack.phib.1P$Parameters <- factor("Survival Prim. Occ.")
stack.phib.1P$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ p2 ~~~~~
stack.p2.1P <- stack(p2.1P)
stack.p2.1P$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.p2.1P)[1:2] <- c("Values", "Results")
stack.p2.1P$P.factor <- factor("1P")
stack.p2.1P$Parameters <- factor("Observation probability")
stack.p2.1P$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

graph <- rbind(stack.N.1P, stack.B.1P, stack.phin.1P, stack.phib.1P, stack.p2.1P)

save(graph,file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-1P.Rdata")
Exc1P <- subset(graph, !(graph$Value < 2 & graph$Value > -2))

# save(Exc1P, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Exc-1P.Rdata") None!