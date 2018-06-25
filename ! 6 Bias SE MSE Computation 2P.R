#####################################################################
#                                                                   #
#         6b.    M S E,     R B    A N D     R S E                  #
#                                                                   #
#                           2  P                                    #
#                                                                   #
#####################################################################

#############################################################################
# Function to compute relative bias, standard error, and Mean squared error #
#                 for N, B, phi.in, phi.btw, p                              #
#############################################################################

# 2 P

# ----- FOR N ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/N-2P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/N-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/N-ZPNEc.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/N-ZPNEs.RData")

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/Ns-2P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc/Ns-ZPNEc.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs/Ns-ZPNEs.RData")

# ----- FOR phi and p ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/output2P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/outputZPNEs.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat/outputZPNEc.RData")

Original.1P2P <- matrix(c(0.99, 0.85, 0.2, 0.6),ncol=100,nrow=4)
rownames(Original.1P2P) <- c("phin","phib","p1","p2")

# ----- FOR B ------ #

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/B-2P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/B-each.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/Bs-2P.RData")

iter <- 100

# ===================== CMOMd, CMOMi, CMR =====================

######################
#         N          #
######################

ls.N.2P.RB <- list()
for (ite in 1:iter){
  ls.N.2P.RB$CMOMd[[ite]] <- (N.2P$CMOMd[[ite]]-ls.N[[ite]])/ls.N[[ite]]
  ls.N.2P.RB$CMOMi[[ite]] <- (N.2P$CMOMi[[ite]]-ls.N[[ite]])/ls.N[[ite]]
  ls.N.2P.RB$CMR[[ite]] <- (N.2P$CMR[[ite]]-ls.N[[ite]])/ls.N[[ite]]}

ls.N.2P.SE <- list()
for (ite in 1:iter){
  ls.N.2P.SE$CMOMd[[ite]] <- Ns.2P$CMOMd[[ite]]/ls.N[[ite]]
  ls.N.2P.SE$CMOMi[[ite]] <- Ns.2P$CMOMi[[ite]]/ls.N[[ite]]
  ls.N.2P.SE$CMR[[ite]] <- Ns.2P$CMR[[ite]]/ls.N[[ite]]}

ls.N.2P.MSE <- list()
for (ite in 1:iter){
  ls.N.2P.MSE$CMOMd[[ite]] <- ls.N.2P.RB$CMOMd[[ite]]^2+ls.N.2P.SE$CMOMd[[ite]]^2
  ls.N.2P.MSE$CMOMi[[ite]] <- ls.N.2P.RB$CMOMi[[ite]]^2+ls.N.2P.SE$CMOMi[[ite]]^2
  ls.N.2P.MSE$CMR[[ite]] <- ls.N.2P.RB$CMR[[ite]]^2+ls.N.2P.SE$CMR[[ite]]^2}

######################
#         B          #
######################

ls.B.2P.RB <- list()
for (ite in 1:iter){
  ls.B.2P.RB$CMOMd[[ite]] <- (B.2P$CMOMd[[ite]]-ls.B[[ite]])/ls.B[[ite]]
  ls.B.2P.RB$CMOMi[[ite]] <- (B.2P$CMOMi[[ite]]-ls.B[[ite]])/ls.B[[ite]]
  ls.B.2P.RB$CMR[[ite]] <- (B.2P$CMR[[ite]]-ls.B[[ite]])/ls.B[[ite]]}

ls.B.2P.SE <- list()
for (ite in 1:iter){
  ls.B.2P.SE$CMOMd[[ite]] <- Bs.2P$CMOMd[[ite]]/ls.B[[ite]]
  ls.B.2P.SE$CMOMi[[ite]] <- Bs.2P$CMOMi[[ite]]/ls.B[[ite]]
  ls.B.2P.SE$CMR[[ite]] <- Bs.2P$CMR[[ite]]/ls.B[[ite]]}

ls.B.2P.MSE <- list()
for (ite in 1:iter){
  ls.B.2P.MSE$CMOMd[[ite]] <- ls.B.2P.RB$CMOMd[[ite]]^2+ls.B.2P.SE$CMOMd[[ite]]^2
  ls.B.2P.MSE$CMOMi[[ite]] <- ls.B.2P.RB$CMOMi[[ite]]^2+ls.B.2P.SE$CMOMi[[ite]]^2
  ls.B.2P.MSE$CMR[[ite]] <- ls.B.2P.RB$CMR[[ite]]^2+ls.B.2P.SE$CMR[[ite]]^2}

##############
#   phin     #
##############

output.2P <- output.2P[c(5:12,17:24,29:36),]

ls.phin.2P.RB <- list()
for (j in 1:iter){
  ls.phin.2P.RB$CMOMd[[j]] <- (output.2P[1,j]-Original.1P2P[1,j])/Original.1P2P[1,j]
  ls.phin.2P.RB$CMOMi[[j]] <- (output.2P[9,j]-Original.1P2P[1,j])/Original.1P2P[1,j]
  ls.phin.2P.RB$CMR[[j]] <- (output.2P[17,j]-Original.1P2P[1,j])/Original.1P2P[1,j]}

ls.phin.2P.SE <- list()
for (j in 1:iter){
  ls.phin.2P.SE$CMOMd[[j]] <- output.2P[2,j]/Original.1P2P[1,j]
  ls.phin.2P.SE$CMOMi[[j]] <- output.2P[10,j]/Original.1P2P[1,j]
  ls.phin.2P.SE$CMR[[j]] <- output.2P[18,j]/Original.1P2P[1,j]}

ls.phin.2P.MSE <- list()
for (j in 1:iter){
  ls.phin.2P.MSE$CMOMd[[j]] <- ls.phin.2P.RB$CMOMd[[j]]^2+ls.phin.2P.SE$CMOMd[[j]]^2
  ls.phin.2P.MSE$CMOMi[[j]] <- ls.phin.2P.RB$CMOMi[[j]]^2+ls.phin.2P.SE$CMOMi[[j]]^2
  ls.phin.2P.MSE$CMR[[j]] <- ls.phin.2P.RB$CMR[[j]]^2+ls.phin.2P.SE$CMR[[j]]^2}

##############
#   phib     #
##############

ls.phib.2P.RB <- list()
for (j in 1:iter){
  ls.phib.2P.RB$CMOMd[[j]] <- (output.2P[3,j]-Original.1P2P[2,j])/Original.1P2P[2,j]
  ls.phib.2P.RB$CMOMi[[j]] <- (output.2P[11,j]-Original.1P2P[2,j])/Original.1P2P[2,j]
  ls.phib.2P.RB$CMR[[j]] <- (output.2P[19,j]-Original.1P2P[2,j])/Original.1P2P[2,j]}

ls.phib.2P.SE <- list()
for (j in 1:iter){
  ls.phib.2P.SE$CMOMd[[j]] <- output.2P[4,j]/Original.1P2P[2,j]
  ls.phib.2P.SE$CMOMi[[j]] <- output.2P[12,j]/Original.1P2P[2,j]
  ls.phib.2P.SE$CMR[[j]] <- output.2P[20,j]/Original.1P2P[2,j]}

ls.phib.2P.MSE <- list()
for (j in 1:iter){
  ls.phib.2P.MSE$CMOMd[[j]] <- ls.phib.2P.RB$CMOMd[[j]]^2+ls.phib.2P.SE$CMOMd[[j]]^2
  ls.phib.2P.MSE$CMOMi[[j]] <- ls.phib.2P.RB$CMOMi[[j]]^2+ls.phib.2P.SE$CMOMi[[j]]^2
  ls.phib.2P.MSE$CMR[[j]] <- ls.phib.2P.RB$CMR[[j]]^2+ls.phib.2P.SE$CMR[[j]]^2}

##############
#     p2     #
##############

ls.p2.2P.RB <- list()
for (j in 1:iter){
  ls.p2.2P.RB$CMOMd[[j]] <- (output.2P[5,j]-Original.1P2P[4,j])/Original.1P2P[4,j]
  ls.p2.2P.RB$CMOMi[[j]] <- (output.2P[13,j]-Original.1P2P[4,j])/Original.1P2P[4,j]
  ls.p2.2P.RB$CMR[[j]] <- (output.2P[21,j]-Original.1P2P[4,j])/Original.1P2P[4,j]}

ls.p2.2P.SE <- list()
for (j in 1:iter){
  ls.p2.2P.SE$CMOMd[[j]] <- output.2P[6,j]/Original.1P2P[4,j]
  ls.p2.2P.SE$CMOMi[[j]] <- output.2P[14,j]/Original.1P2P[4,j]
  ls.p2.2P.SE$CMR[[j]] <- output.2P[22,j]/Original.1P2P[4,j]}

ls.p2.2P.MSE <- list()
for (j in 1:iter){
  ls.p2.2P.MSE$CMOMd[[j]] <- ls.p2.2P.RB$CMOMd[[j]]^2+ls.p2.2P.SE$CMOMd[[j]]^2
  ls.p2.2P.MSE$CMOMi[[j]] <- ls.p2.2P.RB$CMOMi[[j]]^2+ls.p2.2P.SE$CMOMi[[j]]^2
  ls.p2.2P.MSE$CMR[[j]] <- ls.p2.2P.RB$CMR[[j]]^2+ls.p2.2P.SE$CMR[[j]]^2}

##############
#     p1     #
##############

ls.p1.2P.RB <- list()
for (j in 1:iter){
  ls.p1.2P.RB$CMOMd[[j]] <- (output.2P[7,j]-Original.1P2P[3,j])/Original.1P2P[3,j]
  ls.p1.2P.RB$CMOMi[[j]] <- (output.2P[15,j]-Original.1P2P[3,j])/Original.1P2P[3,j]
  ls.p1.2P.RB$CMR[[j]] <- (output.2P[23,j]-Original.1P2P[3,j])/Original.1P2P[3,j]}

ls.p1.2P.SE <- list()
for (j in 1:iter){
  ls.p1.2P.SE$CMOMd[[j]] <- output.2P[8,j]/Original.1P2P[3,j]
  ls.p1.2P.SE$CMOMi[[j]] <- output.2P[16,j]/Original.1P2P[3,j]
  ls.p1.2P.SE$CMR[[j]] <- output.2P[24,j]/Original.1P2P[3,j]}

ls.p1.2P.MSE <- list()
for (j in 1:iter){
  ls.p1.2P.MSE$CMOMd[[j]] <- ls.p1.2P.RB$CMOMd[[j]]^2+ls.p1.2P.SE$CMOMd[[j]]^2
  ls.p1.2P.MSE$CMOMi[[j]] <- ls.p1.2P.RB$CMOMi[[j]]^2+ls.p1.2P.SE$CMOMi[[j]]^2
  ls.p1.2P.MSE$CMR[[j]] <- ls.p1.2P.RB$CMR[[j]]^2+ls.p1.2P.SE$CMR[[j]]^2}


save.image("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-2P.Rdata")

########################################################
#                       T A B L E                      #
########################################################

# ====================== N ======================= #

N.RB.G.2P <- list()
for (ite in 1:iter){
  N.RB.G.2P$CMOMd[[ite]] <- mean(abs(ls.N.2P.RB$CMOMd[[ite]]))
  N.RB.G.2P$CMOMi[[ite]] <- mean(abs(ls.N.2P.RB$CMOMi[[ite]]))
  N.RB.G.2P$CMR[[ite]] <- mean(abs(ls.N.2P.RB$CMR[[ite]]))
  N.RB.G.2P$ZPNEc[[ite]] <- NA
  N.RB.G.2P$ZPNEs[[ite]] <- NA}

for (ite in 1:iter){
  N.RB.G.2P$CMOMd <- mean(N.RB.G.2P$CMOMd, na.rm = T)
  N.RB.G.2P$CMOMi <- mean(N.RB.G.2P$CMOMi, na.rm = T)
  N.RB.G.2P$CMR <- mean(N.RB.G.2P$CMR, na.rm = T)
  N.RB.G.2P$ZPNEc <- NA
  N.RB.G.2P$ZPNEs <- NA}

N.SE.G.2P <- list()
for (ite in 1:iter){
  N.SE.G.2P$CMOMd[[ite]] <- mean(ls.N.2P.SE$CMOMd[[ite]])
  N.SE.G.2P$CMOMi[[ite]] <- mean(ls.N.2P.SE$CMOMi[[ite]])
  N.SE.G.2P$CMR[[ite]] <- mean(ls.N.2P.SE$CMR[[ite]])
  N.SE.G.2P$ZPNEc[[ite]] <- NA
  N.SE.G.2P$ZPNEs[[ite]] <- NA}

for (ite in 1:iter){
  N.SE.G.2P$CMOMd <- mean(N.SE.G.2P$CMOMd, na.rm = T)
  N.SE.G.2P$CMOMi <- mean(N.SE.G.2P$CMOMi, na.rm = T)
  N.SE.G.2P$CMR <- mean(N.SE.G.2P$CMR, na.rm = T)
  N.SE.G.2P$ZPNEc <- NA
  N.SE.G.2P$ZPNEs <- NA}

N.MSE.G.2P <- list()
for (ite in 1:iter){
  N.MSE.G.2P$CMOMd[[ite]] <- mean(ls.N.2P.MSE$CMOMd[[ite]])
  N.MSE.G.2P$CMOMi[[ite]] <- mean(ls.N.2P.MSE$CMOMi[[ite]])
  N.MSE.G.2P$CMR[[ite]] <- mean(ls.N.2P.MSE$CMR[[ite]])
  N.MSE.G.2P$ZPNEc[[ite]] <- NA
  N.MSE.G.2P$ZPNEs[[ite]] <- NA}

for (ite in 1:iter){
  N.MSE.G.2P$CMOMd <- mean(N.MSE.G.2P$CMOMd, na.rm = T)
  N.MSE.G.2P$CMOMi <- mean(N.MSE.G.2P$CMOMi, na.rm = T)
  N.MSE.G.2P$CMR <- mean(N.MSE.G.2P$CMR, na.rm = T)
  N.MSE.G.2P$ZPNEc <- NA
  N.MSE.G.2P$ZPNEs <- NA}

N.2P <- list(RB=N.RB.G.2P, RSE=N.SE.G.2P, MSE=N.MSE.G.2P)

# ====================== PHIB ======================= #

phib.RB.G.2P <- list()
phib.RB.G.2P$CMOMd <- mean(abs(ls.phib.2P.RB$CMOMd), na.rm = T)
phib.RB.G.2P$CMOMi <- mean(abs(ls.phib.2P.RB$CMOMi), na.rm = T)
phib.RB.G.2P$CMR <- mean(abs(ls.phib.2P.RB$CMR), na.rm = T)
phib.RB.G.2P$ZPNEc <- NA
phib.RB.G.2P$ZPNEs <- NA

phib.SE.G.2P <- list()
phib.SE.G.2P$CMOMd <- mean(ls.phib.2P.SE$CMOMd, na.rm = T)
phib.SE.G.2P$CMOMi <- mean(ls.phib.2P.SE$CMOMi, na.rm = T)
phib.SE.G.2P$CMR <- mean(ls.phib.2P.SE$CMR, na.rm = T)
phib.SE.G.2P$ZPNEc <- NA
phib.SE.G.2P$ZPNEs <- NA

phib.MSE.G.2P <- list()
phib.MSE.G.2P$CMOMd <- mean(ls.phib.2P.MSE$CMOMd, na.rm = T)
phib.MSE.G.2P$CMOMi <- mean(ls.phib.2P.MSE$CMOMi, na.rm = T)
phib.MSE.G.2P$CMR <- mean(ls.phib.2P.MSE$CMR, na.rm = T)
phib.MSE.G.2P$ZPNEc <- NA
phib.MSE.G.2P$ZPNEs <- NA

phib.2P <- list(RB=phib.RB.G.2P, RSE=phib.SE.G.2P, MSE=phib.MSE.G.2P)

# ====================== P2 ======================= #

p2.RB.G.2P <- list()
p2.RB.G.2P$CMOMd <- mean(abs(ls.p2.2P.RB$CMOMd), na.rm = T)
p2.RB.G.2P$CMOMi <- mean(abs(ls.p2.2P.RB$CMOMi), na.rm = T)
p2.RB.G.2P$CMR <- mean(abs(ls.p2.2P.RB$CMR), na.rm = T)
p2.RB.G.2P$ZPNEc <- NA
p2.RB.G.2P$ZPNEs <- NA

p2.SE.G.2P <- list()
p2.SE.G.2P$CMOMd <- mean(ls.p2.2P.SE$CMOMd, na.rm = T)
p2.SE.G.2P$CMOMi <- mean(ls.p2.2P.SE$CMOMi, na.rm = T)
p2.SE.G.2P$CMR <- mean(ls.p2.2P.SE$CMR, na.rm = T)
p2.SE.G.2P$ZPNEc <- NA
p2.SE.G.2P$ZPNEs <- NA

p2.MSE.G.2P <- list()
p2.MSE.G.2P$CMOMd <- mean(ls.p2.2P.MSE$CMOMd, na.rm = T)
p2.MSE.G.2P$CMOMi <- mean(ls.p2.2P.MSE$CMOMi, na.rm = T)
p2.MSE.G.2P$CMR <- mean(ls.p2.2P.MSE$CMR, na.rm = T)
p2.MSE.G.2P$ZPNEc <- NA
p2.MSE.G.2P$ZPNEs <- NA

p2.2P <- list(RB=p2.RB.G.2P, RSE=p2.SE.G.2P, MSE=p2.MSE.G.2P)

# ====================== P1 ======================= #

p1.RB.G.2P <- list()
p1.RB.G.2P$CMOMd <- mean(abs(ls.p1.2P.RB$CMOMd), na.rm = T)
p1.RB.G.2P$CMOMi <- mean(abs(ls.p1.2P.RB$CMOMi), na.rm = T)
p1.RB.G.2P$CMR <- mean(abs(ls.p1.2P.RB$CMR), na.rm = T)
p1.RB.G.2P$ZPNEc <- NA
p1.RB.G.2P$ZPNEs <- NA

p1.SE.G.2P <- list()
p1.SE.G.2P$CMOMd <- mean(ls.p1.2P.SE$CMOMd, na.rm = T)
p1.SE.G.2P$CMOMi <- mean(ls.p1.2P.SE$CMOMi, na.rm = T)
p1.SE.G.2P$CMR <- mean(ls.p1.2P.SE$CMR, na.rm = T)
p1.SE.G.2P$ZPNEc <- NA
p1.SE.G.2P$ZPNEs <- NA

p1.MSE.G.2P <- list()
p1.MSE.G.2P$CMOMd <- mean(ls.p1.2P.MSE$CMOMd, na.rm = T)
p1.MSE.G.2P$CMOMi <- mean(ls.p1.2P.MSE$CMOMi, na.rm = T)
p1.MSE.G.2P$CMR <- mean(ls.p1.2P.MSE$CMR, na.rm = T)
p1.MSE.G.2P$ZPNEc <- NA
p1.MSE.G.2P$ZPNEs <- NA

p1.2P <- list(RB=p1.RB.G.2P, RSE=p1.SE.G.2P, MSE=p1.MSE.G.2P)

# ====================== B ======================= #

B.RB.G.2P <- list()
for (ite in 1:iter){
  B.RB.G.2P$CMOMd[[ite]] <- mean(abs(ls.B.2P.RB$CMOMd[[ite]]))
  B.RB.G.2P$CMOMi[[ite]] <- mean(abs(ls.B.2P.RB$CMOMi[[ite]]))
  B.RB.G.2P$CMR[[ite]] <- mean(abs(ls.B.2P.RB$CMR[[ite]]))
  B.RB.G.2P$ZPNEc[[ite]] <- NA
  B.RB.G.2P$ZPNEs[[ite]] <- NA}

for (ite in 1:iter){
  B.RB.G.2P$CMOMd <- mean(B.RB.G.2P$CMOMd, na.rm = T)
  B.RB.G.2P$CMOMi <- mean(B.RB.G.2P$CMOMi, na.rm = T)
  B.RB.G.2P$CMR <- mean(B.RB.G.2P$CMR, na.rm = T)
  B.RB.G.2P$ZPNEc <- NA
  B.RB.G.2P$ZPNEs <- NA}

B.SE.G.2P <- list()
for (ite in 1:iter){
  B.SE.G.2P$CMOMd[[ite]] <- mean(ls.B.2P.SE$CMOMd[[ite]])
  B.SE.G.2P$CMOMi[[ite]] <- mean(ls.B.2P.SE$CMOMi[[ite]])
  B.SE.G.2P$CMR[[ite]] <- mean(ls.B.2P.SE$CMR[[ite]])
  B.SE.G.2P$ZPNEc[[ite]] <- NA
  B.SE.G.2P$ZPNEs[[ite]] <- NA}

for (ite in 1:iter){
  B.SE.G.2P$CMOMd <- mean(B.SE.G.2P$CMOMd, na.rm = T)
  B.SE.G.2P$CMOMi <- mean(B.SE.G.2P$CMOMi, na.rm = T)
  B.SE.G.2P$CMR <- mean(B.SE.G.2P$CMR, na.rm = T)
  B.SE.G.2P$ZPNEc <- NA
  B.SE.G.2P$ZPNEs <- NA}

B.MSE.G.2P <- list()
for (ite in 1:iter){
  B.MSE.G.2P$CMOMd[[ite]] <- mean(ls.B.2P.MSE$CMOMd[[ite]])
  B.MSE.G.2P$CMOMi[[ite]] <- mean(ls.B.2P.MSE$CMOMi[[ite]])
  B.MSE.G.2P$CMR[[ite]] <- mean(ls.B.2P.MSE$CMR[[ite]])
  B.MSE.G.2P$ZPNEc[[ite]] <- NA
  B.MSE.G.2P$ZPNEs[[ite]] <- NA}

for (ite in 1:iter){
  B.MSE.G.2P$CMOMd <- mean(B.MSE.G.2P$CMOMd, na.rm = T)
  B.MSE.G.2P$CMOMi <- mean(B.MSE.G.2P$CMOMi, na.rm = T)
  B.MSE.G.2P$CMR <- mean(B.MSE.G.2P$CMR, na.rm = T)
  B.MSE.G.2P$ZPNEc <- NA
  B.MSE.G.2P$ZPNEs <- NA}

B.2P <- list(RB=B.RB.G.2P, RSE=B.SE.G.2P, MSE=B.MSE.G.2P)

# ====================== PHIN ======================= #

phin.RB.G.2P <- list()
phin.RB.G.2P$CMOMd <- mean(abs(ls.phin.2P.RB$CMOMd), na.rm = T)
phin.RB.G.2P$CMOMi <- mean(abs(ls.phin.2P.RB$CMOMi), na.rm = T)
phin.RB.G.2P$CMR <- mean(abs(ls.phin.2P.RB$CMR), na.rm = T)
phin.RB.G.2P$ZPNEc <- NA
phin.RB.G.2P$ZPNEs <- NA

phin.SE.G.2P <- list()
phin.SE.G.2P$CMOMd <- mean(ls.phin.2P.SE$CMOMd, na.rm = T)
phin.SE.G.2P$CMOMi <- mean(ls.phin.2P.SE$CMOMi, na.rm = T)
phin.SE.G.2P$CMR <- mean(ls.phin.2P.SE$CMR, na.rm = T)
phin.SE.G.2P$ZPNEc <- NA
phin.SE.G.2P$ZPNEs <- NA

phin.MSE.G.2P <- list()
phin.MSE.G.2P$CMOMd <- mean(ls.phin.2P.MSE$CMOMd, na.rm = T)
phin.MSE.G.2P$CMOMi <- mean(ls.phin.2P.MSE$CMOMi, na.rm = T)
phin.MSE.G.2P$CMR <- mean(ls.phin.2P.MSE$CMR, na.rm = T)
phin.MSE.G.2P$ZPNEc <- NA
phin.MSE.G.2P$ZPNEs <- NA

phin.2P <- list(RB=phin.RB.G.2P, RSE=phin.SE.G.2P, MSE=phin.MSE.G.2P)
# ================= DATA FRAME =================

# ~~~~~ N ~~~~~
stack.N.2P <- stack(N.2P)
stack.N.2P$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.N.2P)[1:2] <- c("Values", "Results")
stack.N.2P$P.factor <- factor("2P")
stack.N.2P$Parameters <- factor("Number of Individuals")
stack.N.2P$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ B ~~~~~
stack.B.2P <- stack(B.2P)
stack.B.2P$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.B.2P)[1:2] <- c("Values", "Results")
stack.B.2P$P.factor <- factor("2P")
stack.B.2P$Parameters <- factor("Recruitment")
stack.B.2P$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ phin ~~~~~
stack.phin.2P <- stack(phin.2P)
stack.phin.2P$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.phin.2P)[1:2] <- c("Values", "Results")
stack.phin.2P$P.factor <- factor("2P")
stack.phin.2P$Parameters <- factor("Survival Second. Occ.")
stack.phin.2P$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ phib ~~~~~
stack.phib.2P <- stack(phib.2P)
stack.phib.2P$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.phib.2P)[1:2] <- c("Values", "Results")
stack.phib.2P$P.factor <- factor("2P")
stack.phib.2P$Parameters <- factor("Survival Prim. Occ.")
stack.phib.2P$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ p2 ~~~~~
stack.p2.2P <- stack(p2.2P)
stack.p2.2P$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.p2.2P)[1:2] <- c("Values", "Results")
stack.p2.2P$P.factor <- factor("2P")
stack.p2.2P$Parameters <- factor("Observation probability")
stack.p2.2P$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

# ~~~~~ p1 ~~~~~
stack.p1.2P <- stack(p1.2P)
stack.p1.2P$Models <- factor(rep(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"),3))
colnames(stack.p1.2P)[1:2] <- c("Values", "Results")
stack.p1.2P$P.factor <- factor("2P")
stack.p1.2P$Parameters <- factor("Capture probability")
stack.p1.2P$Results <- factor(c(rep("Relative Bias",5),rep("Standard Error",5),rep("Mean-square Error",5)))

graph2 <- rbind(stack.N.2P, stack.B.2P, stack.phin.2P, stack.phib.2P, stack.p1.2P, stack.p2.2P)

save(graph2,file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-2P.Rdata")
Exc2P <- subset(graph2, !(graph2$Value < 2 & graph2$Value > -2))

save(Exc2P, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Exc-2P.Rdata")
