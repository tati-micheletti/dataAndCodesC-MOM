
#######################################################
#                                                     #
# Extract the exact B for each occasion by each model #
#                                                     #
#######################################################

##########################################################

# True B
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/B-fem.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/B-male.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/B-fem.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/B-male.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/B-fem.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/B-male.RData")

ls.T.B <- list()
for (ite in 1:200)
{ls.T.B[[ite]] <- ls.T.Bf[[ite]] + ls.T.Bm[[ite]]
ls.T.B[[ite]] <- ls.T.B[[ite]][c(4,7)]}

save(ls.T.B, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/B-each.RData")

ls.H.B <- list()
for (ite in 1:600)
{ls.H.B[[ite]] <- ls.H.Bf[[ite]] + ls.H.Bm[[ite]]
ls.H.B[[ite]] <- ls.H.B[[ite]][c(4,7)]}

save(ls.H.B, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/B-each.RData")

ls.B <- list()
for (ite in 1:length(ls.Bf)){
  ls.B[[ite]] <- ls.Bf[[ite]] + ls.Bm[[ite]]
  ls.B[[ite]] <- ls.B[[ite]][c(4,7)]
}

save(ls.B, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/B-each.RData")

##########################################################
# 1 P

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/1P_Backup.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1P.RData")

CMOMd <- CMOMi <- CMR <- B.1P <- list()

iter <- 100

for (ite in 1:iter){
  CMOMd[[ite]] <- get(ls.CMOMd.1P[[ite]])[[10]][20:21,"mean"]
  CMOMi[[ite]] <- get(ls.CMOMi.1P[[ite]])[[10]][20:21,"mean"]
  CMR[[ite]] <- get(ls.CMR.1P[[ite]])[[10]][c(23,26),"mean"]}

B.1P <- list(CMOMd=CMOMd, CMOMi=CMOMi, CMR=CMR)

for (ite in 1:length(B.1P$CMOMd)){
  if (ite %in% it.1P$CMOMd) {
    B.1P$CMOMd[[ite]] <- NA  }
  if (ite %in% it.1P$CMOMi) {
    B.1P$CMOMi[[ite]] <- NA }
  if (ite %in% it.1P$CMR) {
    B.1P$CMR[[ite]] <- NA }
}

save(B.1P, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/B-1P.RData")

CMOMds <- CMOMis <- CMRs <- Bs.1P <- list()

for (ite in 1:iter){
  CMOMds[[ite]] <- get(ls.CMOMd.1P[[ite]])[[10]][20:21,"sd"]
  CMOMis[[ite]] <- get(ls.CMOMi.1P[[ite]])[[10]][20:21,"sd"]
  CMRs[[ite]] <- get(ls.CMR.1P[[ite]])[[10]][c(23,26),"sd"]}

Bs.1P <- list(CMOMds=CMOMds, CMOMis=CMOMis, CMRs=CMRs)

for (ite in 1:length(B.1P$CMOMd)){
  if (ite %in% it.1P$CMOMd) {
    Bs.1P$CMOMds[[ite]] <- NA  }
  if (ite %in% it.1P$CMOMi) {
    Bs.1P$CMOMis[[ite]] <- NA }
  if (ite %in% it.1P$CMR) {
    Bs.1P$CMRs[[ite]] <- NA }
}

save(Bs.1P, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/Bs-1P.RData")

rm(list = ls())

###########################################################
# 2 P 

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/2P_Backup.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2P.RData")

CMOMd <- CMOMi <- CMR <- B.2P <- list()

iter <- 100

for (ite in 1:iter){
  CMOMd[[ite]] <- get(ls.CMOMd.2P[[ite]])[[10]][21:22,"mean"]
  CMOMi[[ite]] <- get(ls.CMOMi.2P[[ite]])[[10]][21:22,"mean"]
  CMR[[ite]] <- get(ls.CMR.2P[[ite]])[[10]][c(24,27),"mean"]}

B.2P <- list(CMOMd=CMOMd, CMOMi=CMOMi, CMR=CMR)

for (ite in 1:length(B.2P$CMOMd)){
  if (ite %in% it.2P$CMOMd) {
    B.2P$CMOMd[[ite]] <- NA  }
  if (ite %in% it.2P$CMOMi) {
    B.2P$CMOMi[[ite]] <- NA }
  if (ite %in% it.2P$CMR) {
    B.2P$CMR[[ite]] <- NA }
}

save(B.2P, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/B-2P.RData")

CMOMds <- CMOMis <- CMRs <- Bs.2P <- list()

iter <- 100

for (ite in 1:iter){
  CMOMds[[ite]] <- get(ls.CMOMd.2P[[ite]])[[10]][21:22,"sd"]
  CMOMis[[ite]] <- get(ls.CMOMi.2P[[ite]])[[10]][21:22,"sd"]
  CMRs[[ite]] <- get(ls.CMR.2P[[ite]])[[10]][c(24,27),"sd"]}

Bs.2P <- list(CMOMds=CMOMds, CMOMis=CMOMis, CMRs=CMRs)

for (ite in 1:length(B.2P$CMOMd)){
  if (ite %in% it.2P$CMOMd) {
    Bs.2P$CMOMds[[ite]] <- NA  }
  if (ite %in% it.2P$CMOMi) {
    Bs.2P$CMOMis[[ite]] <- NA }
  if (ite %in% it.2P$CMR) {
    Bs.2P$CMRs[[ite]] <- NA }
}

save(Bs.2P, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/Bs-2P.RData")

rm(list = ls())

###########################################################

#  T R E N D S   1 T

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/1T_Backup.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1T.RData")

CMOMd <- CMOMi <- CMR <- B.1T <- list()

iter <- 200

for (ite in 1:iter){
  CMOMd[[ite]] <- get(ls.CMOMd.1T[[ite]])[[10]][20:21,"mean"]
  CMOMi[[ite]] <- get(ls.CMOMi.1T[[ite]])[[10]][20:21,"mean"]
  CMR[[ite]] <- get(ls.CMR.1T[[ite]])[[10]][c(23,26),"mean"]}

B.1T <- list(CMOMd=CMOMd, CMOMi=CMOMi, CMR=CMR)

for (ite in 1:length(B.1T$CMOMd)){
  if (ite %in% it.1T$CMOMd) {
    B.1T$CMOMd[[ite]] <- NA  }
  if (ite %in% it.1T$CMOMi) {
    B.1T$CMOMi[[ite]] <- NA }
  if (ite %in% it.1T$CMR) {
    B.1T$CMR[[ite]] <- NA }
}

save(B.1T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/B-1T.RData")

CMOMds <- CMOMis <- CMRs <- Bs.1T <- list()

iter <- 200

for (ite in 1:iter){
  CMOMds[[ite]] <- get(ls.CMOMd.1T[[ite]])[[10]][20:21,"sd"]
  CMOMis[[ite]] <- get(ls.CMOMi.1T[[ite]])[[10]][20:21,"sd"]
  CMRs[[ite]] <- get(ls.CMR.1T[[ite]])[[10]][c(23,26),"sd"]}

Bs.1T <- list(CMOMds=CMOMds, CMOMis=CMOMis, CMRs=CMRs)

for (ite in 1:length(B.1T$CMOMd)){
  if (ite %in% it.1T$CMOMd) {
    Bs.1T$CMOMds[[ite]] <- NA  }
  if (ite %in% it.1T$CMOMi) {
    Bs.1T$CMOMis[[ite]] <- NA }
  if (ite %in% it.1T$CMR) {
    Bs.1T$CMRs[[ite]] <- NA }
}

save(Bs.1T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/Bs-1T.RData")


rm(list = ls())

###########################################################

#  T R E N D S   2 T

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/2T_Backup.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2T.RData")

CMOMd <- CMOMi <- CMR <- B.2T <- list()

iter <- 200

for (ite in 1:iter){
  CMOMd[[ite]] <- get(ls.CMOMd.2T[[ite]])[[10]][21:22,"mean"]
  CMOMi[[ite]] <- get(ls.CMOMi.2T[[ite]])[[10]][21:22,"mean"]
  CMR[[ite]] <- get(ls.CMR.2T[[ite]])[[10]][c(24,27),"mean"]}

B.2T <- list(CMOMd=CMOMd, CMOMi=CMOMi, CMR=CMR)

for (ite in 1:length(B.2T$CMOMd)){
  if (ite %in% it.2T$CMOMd) {
    B.2T$CMOMd[[ite]] <- NA  }
  if (ite %in% it.2T$CMOMi) {
    B.2T$CMOMi[[ite]] <- NA }
  if (ite %in% it.2T$CMR) {
    B.2T$CMR[[ite]] <- NA }
}

save(B.2T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/B-2T.RData")

CMOMds <- CMOMis <- CMRs <- Bs.2T <- list()

iter <- 200

for (ite in 1:iter){
  CMOMds[[ite]] <- get(ls.CMOMd.2T[[ite]])[[10]][21:22,"sd"]
  CMOMis[[ite]] <- get(ls.CMOMi.2T[[ite]])[[10]][21:22,"sd"]
  CMRs[[ite]] <- get(ls.CMR.2T[[ite]])[[10]][c(24,27),"sd"]}

Bs.2T <- list(CMOMds=CMOMds, CMOMis=CMOMis, CMRs=CMRs)

for (ite in 1:length(B.2T$CMOMd)){
  if (ite %in% it.2T$CMOMd) {
    Bs.2T$CMOMds[[ite]] <- NA  }
  if (ite %in% it.2T$CMOMi) {
    Bs.2T$CMOMis[[ite]] <- NA }
  if (ite %in% it.2T$CMR) {
    Bs.2T$CMRs[[ite]] <- NA }
}

save(Bs.2T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/Bs-2T.RData")


rm(list = ls())

###########################################################

#  H E T E R O   1 H

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/1H_Backup.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1H.RData")

CMOMd <- CMOMi <- CMR <- B.1H <- list()

iter <- 600

for (ite in 1:iter){
  CMOMd[[ite]] <- get(ls.CMOMd.1H[[ite]])[[10]][20:21,"mean"]
  CMOMi[[ite]] <- get(ls.CMOMi.1H[[ite]])[[10]][20:21,"mean"]
  CMR[[ite]] <- get(ls.CMR.1H[[ite]])[[10]][c(23,26),"mean"]}

B.1H <- list(CMOMd=CMOMd, CMOMi=CMOMi, CMR=CMR)

for (ite in 1:length(B.1H$CMOMd)){
  if (ite %in% it.1H$CMOMd) {
    B.1H$CMOMd[[ite]] <- NA  }
  if (ite %in% it.1H$CMOMi) {
    B.1H$CMOMi[[ite]] <- NA }
  if (ite %in% it.1H$CMR) {
    B.1H$CMR[[ite]] <- NA }
}

save(B.1H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/B-1H.RData")

CMOMds <- CMOMis <- CMRs <- Bs.1H <- list()

iter <- 600

for (ite in 1:iter){
  CMOMds[[ite]] <- get(ls.CMOMd.1H[[ite]])[[10]][20:21,"sd"]
  CMOMis[[ite]] <- get(ls.CMOMi.1H[[ite]])[[10]][20:21,"sd"]
  CMRs[[ite]] <- get(ls.CMR.1H[[ite]])[[10]][c(23,26),"sd"]}

Bs.1H <- list(CMOMds=CMOMds, CMOMis=CMOMis, CMRs=CMRs)

for (ite in 1:length(B.1H$CMOMd)){
  if (ite %in% it.1H$CMOMd) {
    Bs.1H$CMOMd[[ite]] <- NA  }
  if (ite %in% it.1H$CMOMi) {
    Bs.1H$CMOMi[[ite]] <- NA }
  if (ite %in% it.1H$CMR) {
    Bs.1H$CMR[[ite]] <- NA }
}

save(Bs.1H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/Bs-1H.RData")

rm(list = ls())

###########################################################

#  H E T E R O   2 H

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/2H_Backup.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2H.RData")

CMOMd <- CMOMi <- CMR <- B.2H <- list()

iter <- 600

for (ite in 1:iter){
  CMOMd[[ite]] <- get(ls.CMOMd.2H[[ite]])[[10]][21:22,"mean"]
  CMOMi[[ite]] <- get(ls.CMOMi.2H[[ite]])[[10]][21:22,"mean"]
  CMR[[ite]] <- get(ls.CMR.2H[[ite]])[[10]][c(24,27),"mean"]}

B.2H <- list(CMOMd=CMOMd, CMOMi=CMOMi, CMR=CMR)

for (ite in 1:length(B.2H$CMOMd)){
  if (ite %in% it.2H$CMOMd) {
    B.2H$CMOMd[[ite]] <- NA  }
  if (ite %in% it.2H$CMOMi) {
    B.2H$CMOMi[[ite]] <- NA }
  if (ite %in% it.2H$CMR) {
    B.2H$CMR[[ite]] <- NA }
}

save(B.2H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/B-2H.RData")

CMOMds <- CMOMis <- CMRs <- Bs.2H <- list()

iter <- 600

for (ite in 1:iter){
  CMOMds[[ite]] <- get(ls.CMOMd.2H[[ite]])[[10]][21:22,"sd"]
  CMOMis[[ite]] <- get(ls.CMOMi.2H[[ite]])[[10]][21:22,"sd"]
  CMRs[[ite]] <- get(ls.CMR.2H[[ite]])[[10]][c(24,27),"sd"]}

Bs.2H <- list(CMOMds=CMOMds, CMOMis=CMOMis, CMRs=CMRs)

for (ite in 1:length(B.2H$CMOMd)){
  if (ite %in% it.2H$CMOMd) {
    Bs.2H$CMOMd[[ite]] <- NA  }
  if (ite %in% it.2H$CMOMi) {
    Bs.2H$CMOMi[[ite]] <- NA }
  if (ite %in% it.2H$CMR) {
    Bs.2H$CMR[[ite]] <- NA }
}

save(Bs.2H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/Bs-2H.RData")

rm(list = ls())