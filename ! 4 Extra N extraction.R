
#######################################################
#                                                     #
# Extract the exact N for each occasion by each model #
#                                                     #
#######################################################

##########################################################

# True N
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/N-each.RData")
# Already in a list

##########################################################
# 1 P

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/1P_Backup.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1P.RData")

iter <- 100

CMOMd <- CMOMi <- CMR <- N.1P <- list()

for (ite in 1:iter){
CMOMd[[ite]] <- get(ls.CMOMd.1P[[ite]])[[10]][11:19,"mean"]
CMOMi[[ite]] <- get(ls.CMOMi.1P[[ite]])[[10]][11:19,"mean"]
CMR[[ite]] <- get(ls.CMR.1P[[ite]])[[10]][11:19,"mean"]}

N.1P <- list(CMOMd=CMOMd, CMOMi=CMOMi, CMR=CMR)

for (ite in 1:length(N.1P$CMOMd)){
  if (ite %in% it.1P$CMOMd) {
    N.1P$CMOMd[[ite]] <- NA  }
  if (ite %in% it.1P$CMOMi) {
    N.1P$CMOMi[[ite]] <- NA }
  if (ite %in% it.1P$CMR) {
    N.1P$CMR[[ite]] <- NA }
  }

save(N.1P, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/N-1P.RData")

CMOMds <- CMOMis <- CMRs <- Ns.1P <- list()

for (ite in 1:iter){
  CMOMds[[ite]] <- get(ls.CMOMd.1P[[ite]])[[10]][11:19,"sd"]
  CMOMis[[ite]] <- get(ls.CMOMi.1P[[ite]])[[10]][11:19,"sd"]
  CMRs[[ite]] <- get(ls.CMR.1P[[ite]])[[10]][11:19,"sd"]}

Ns.1P <- list(CMOMds=CMOMds, CMOMis=CMOMis, CMRs=CMRs)

for (ite in 1:length(N.1P$CMOMd)){
  if (ite %in% it.1P$CMOMd) {
    Ns.1P$CMOMd[[ite]] <- NA  }
  if (ite %in% it.1P$CMOMi) {
    Ns.1P$CMOMi[[ite]] <- NA }
  if (ite %in% it.1P$CMR) {
    Ns.1P$CMR[[ite]] <- NA }
}

save(Ns.1P, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/Ns-1P.RData")

rm(list = ls())

###########################################################
# 2 P 

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/2P_Backup.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2P.RData")

iter <- 100

CMOMd <- CMOMi <- CMR <- N.2P <- list()

for (ite in 1:iter){
  CMOMd[[ite]] <- get(ls.CMOMd.2P[[ite]])[[10]][12:20,"mean"]
  CMOMi[[ite]] <- get(ls.CMOMi.2P[[ite]])[[10]][12:20,"mean"]
  CMR[[ite]] <- get(ls.CMR.2P[[ite]])[[10]][12:20,"mean"]}

N.2P <- list(CMOMd=CMOMd, CMOMi=CMOMi, CMR=CMR)

for (ite in 1:length(N.2P$CMOMd)){
  if (ite %in% it.2P$CMOMd) {
    N.2P$CMOMd[[ite]] <- NA  }
  if (ite %in% it.2P$CMOMi) {
    N.2P$CMOMi[[ite]] <- NA }
  if (ite %in% it.2P$CMR) {
    N.2P$CMR[[ite]] <- NA }
}

save(N.2P, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/N-2P.RData")

CMOMds <- CMOMis <- CMRs <- Ns.2P <- list()

for (ite in 1:iter){
  CMOMds[[ite]] <- get(ls.CMOMd.2P[[ite]])[[10]][12:20,"sd"]
  CMOMis[[ite]] <- get(ls.CMOMi.2P[[ite]])[[10]][12:20,"sd"]
  CMRs[[ite]] <- get(ls.CMR.2P[[ite]])[[10]][12:20,"sd"]}

Ns.2P <- list(CMOMds=CMOMds, CMOMis=CMOMis, CMRs=CMRs)

for (ite in 1:length(N.2P$CMOMd)){
  if (ite %in% it.2P$CMOMd) {
    Ns.2P$CMOMd[[ite]] <- NA  }
  if (ite %in% it.2P$CMOMi) {
    Ns.2P$CMOMi[[ite]] <- NA }
  if (ite %in% it.2P$CMR) {
    Ns.2P$CMR[[ite]] <- NA }
}

save(Ns.2P, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/Ns-2P.RData")

rm(list = ls())

###########################################################

#  T R E N D S   1T

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/1T_Backup.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1T.RData")

iter <- 200

CMOMd <- CMOMi <- CMR <- N.1T <- list()

for (ite in 1:iter){
  CMOMd[[ite]] <- get(ls.CMOMd.1T[[ite]])[[10]][11:19,"mean"]
  CMOMi[[ite]] <- get(ls.CMOMi.1T[[ite]])[[10]][11:19,"mean"]
  CMR[[ite]] <- get(ls.CMR.1T[[ite]])[[10]][11:19,"mean"]}

N.1T <- list(CMOMd=CMOMd, CMOMi=CMOMi, CMR=CMR)

for (ite in 1:length(N.1T$CMOMd)){
  if (ite %in% it.1T$CMOMd) {
    N.1T$CMOMd[[ite]] <- NA  }
  if (ite %in% it.1T$CMOMi) {
    N.1T$CMOMi[[ite]] <- NA }
  if (ite %in% it.1T$CMR) {
    N.1T$CMR[[ite]] <- NA }
}

save(N.1T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/N-1T.RData")

CMOMds <- CMOMis <- CMRs <- Ns.1T <- list()

for (ite in 1:iter){
  CMOMds[[ite]] <- get(ls.CMOMd.1T[[ite]])[[10]][11:19,"sd"]
  CMOMis[[ite]] <- get(ls.CMOMi.1T[[ite]])[[10]][11:19,"sd"]
  CMRs[[ite]] <- get(ls.CMR.1T[[ite]])[[10]][11:19,"sd"]}

Ns.1T <- list(CMOMds=CMOMds, CMOMis=CMOMis, CMRs=CMRs)

for (ite in 1:length(N.1T$CMOMd)){
  if (ite %in% it.1T$CMOMd) {
    Ns.1T$CMOMd[[ite]] <- NA  }
  if (ite %in% it.1T$CMOMi) {
    Ns.1T$CMOMi[[ite]] <- NA }
  if (ite %in% it.1T$CMR) {
    Ns.1T$CMR[[ite]] <- NA }
}

save(Ns.1T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/Ns-1T.RData")

rm(list = ls())

###########################################################

#  T R E N D S   2T

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/2T_Backup.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2T.RData")

iter <- 200

CMOMd <- CMOMi <- CMR <- N.2T <- list()

for (ite in 1:iter){
  CMOMd[[ite]] <- get(ls.CMOMd.2T[[ite]])[[10]][12:20,"mean"]
  CMOMi[[ite]] <- get(ls.CMOMi.2T[[ite]])[[10]][12:20,"mean"]
  CMR[[ite]] <- get(ls.CMR.2T[[ite]])[[10]][12:20,"mean"]}

N.2T <- list(CMOMd=CMOMd, CMOMi=CMOMi, CMR=CMR)

for (ite in 1:length(N.2T$CMOMd)){
  if (ite %in% it.2T$CMOMd) {
    N.2T$CMOMd[[ite]] <- NA  }
  if (ite %in% it.2T$CMOMi) {
    N.2T$CMOMi[[ite]] <- NA }
  if (ite %in% it.2T$CMR) {
    N.2T$CMR[[ite]] <- NA }
}

save(N.2T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/N-2T.RData")

CMOMds <- CMOMis <- CMRs <- Ns.2T <- list()

for (ite in 1:iter){
  CMOMds[[ite]] <- get(ls.CMOMd.2T[[ite]])[[10]][12:20,"sd"]
  CMOMis[[ite]] <- get(ls.CMOMi.2T[[ite]])[[10]][12:20,"sd"]
  CMRs[[ite]] <- get(ls.CMR.2T[[ite]])[[10]][12:20,"sd"]}

Ns.2T <- list(CMOMds=CMOMds, CMOMis=CMOMis, CMRs=CMRs)

for (ite in 1:length(N.2T$CMOMd)){
  if (ite %in% it.2T$CMOMd) {
    Ns.2T$CMOMd[[ite]] <- NA  }
  if (ite %in% it.2T$CMOMi) {
    Ns.2T$CMOMi[[ite]] <- NA }
  if (ite %in% it.2T$CMR) {
    Ns.2T$CMR[[ite]] <- NA }
}

save(Ns.2T, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/Ns-2T.RData")

rm(list = ls())


###########################################################

#  H E T E R O   1H

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/1H_Backup.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1H.RData")

iter <- 600

CMOMd <- CMOMi <- CMR <- N.1H <- list()

for (ite in 1:iter){
  CMOMd[[ite]] <- get(ls.CMOMd.1H[[ite]])[[10]][11:19,"mean"]
  CMOMi[[ite]] <- get(ls.CMOMi.1H[[ite]])[[10]][11:19,"mean"]
  CMR[[ite]] <- get(ls.CMR.1H[[ite]])[[10]][11:19,"mean"]}

N.1H <- list(CMOMd=CMOMd, CMOMi=CMOMi, CMR=CMR)

for (ite in 1:length(N.1H$CMOMd)){
  if (ite %in% it.1H$CMOMd) {
    N.1H$CMOMd[[ite]] <- NA  }
  if (ite %in% it.1H$CMOMi) {
    N.1H$CMOMi[[ite]] <- NA }
  if (ite %in% it.1H$CMR) {
    N.1H$CMR[[ite]] <- NA }
}

save(N.1H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/N-1H.RData")

CMOMds <- CMOMis <- CMRs <- Ns.1H <- list()

for (ite in 1:iter){
  CMOMds[[ite]] <- get(ls.CMOMd.1H[[ite]])[[10]][11:19,"sd"]
  CMOMis[[ite]] <- get(ls.CMOMi.1H[[ite]])[[10]][11:19,"sd"]
  CMRs[[ite]] <- get(ls.CMR.1H[[ite]])[[10]][11:19,"sd"]}

Ns.1H <- list(CMOMds=CMOMds, CMOMis=CMOMis, CMRs=CMRs)

for (ite in 1:length(N.1H$CMOMd)){
  if (ite %in% it.1H$CMOMd) {
    Ns.1H$CMOMd[[ite]] <- NA  }
  if (ite %in% it.1H$CMOMi) {
    Ns.1H$CMOMi[[ite]] <- NA }
  if (ite %in% it.1H$CMR) {
    Ns.1H$CMR[[ite]] <- NA }
}

save(Ns.1H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/Ns-1H.RData")

rm(list = ls())

###########################################################

#  H E T E R O   2H

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/2H_Backup.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2H.RData")

iter <- 600

CMOMd <- CMOMi <- CMR <- N.2H <- list()

for (ite in 1:iter){
  CMOMd[[ite]] <- get(ls.CMOMd.2H[[ite]])[[10]][12:20,"mean"]
  CMOMi[[ite]] <- get(ls.CMOMi.2H[[ite]])[[10]][12:20,"mean"]
  CMR[[ite]] <- get(ls.CMR.2H[[ite]])[[10]][12:20,"mean"]}

N.2H <- list(CMOMd=CMOMd, CMOMi=CMOMi, CMR=CMR)

for (ite in 1:length(N.2H$CMOMd)){
  if (ite %in% it.2H$CMOMd) {
    N.2H$CMOMd[[ite]] <- NA  }
  if (ite %in% it.2H$CMOMi) {
    N.2H$CMOMi[[ite]] <- NA }
  if (ite %in% it.2H$CMR) {
    N.2H$CMR[[ite]] <- NA }
}

save(N.2H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/N-2H.RData")

CMOMds <- CMOMis <- CMRs <- Ns.2H <- list()

for (ite in 1:iter){
  CMOMds[[ite]] <- get(ls.CMOMd.2H[[ite]])[[10]][12:20,"sd"]
  CMOMis[[ite]] <- get(ls.CMOMi.2H[[ite]])[[10]][12:20,"sd"]
  CMRs[[ite]] <- get(ls.CMR.2H[[ite]])[[10]][12:20,"sd"]}

Ns.2H <- list(CMOMds=CMOMds, CMOMis=CMOMis, CMRs=CMRs)

for (ite in 1:length(N.2H$CMOMd)){
  if (ite %in% it.2H$CMOMd) {
    Ns.2H$CMOMd[[ite]] <- NA  }
  if (ite %in% it.2H$CMOMi) {
    Ns.2H$CMOMi[[ite]] <- NA }
  if (ite %in% it.2H$CMR) {
    Ns.2H$CMR[[ite]] <- NA }
}

save(Ns.2H, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/Ns-2H.RData")

rm(list = ls())