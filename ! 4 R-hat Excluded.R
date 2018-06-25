#####################################################################
#                                                                   #
#         4.    E X C L U D E    B A D    r h a t                   #
#                                                                   #
#####################################################################

# 1P 2P
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/output-1P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/output-1P-CMR.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/output-2P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/output-2P-CMR.RData")

# TRENDS
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/output-1T.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/output-1T-CMR.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/output-2T.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/output-2T-CMR.RData")

# HETERO
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/output-1H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/output-1H-CMR.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/output-2H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/output-2H-CMR.RData")


load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.1T.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Rhat/it.2T.RData")

# Turn iterations present in "it.XX" variables to NA

# FOR 1P
for (ite in 1:ncol(output.1P.CMOM)){
  if (ite %in% it.1P$CMOMd) {
      output.1P.CMOM[1:10,ite] <- NA 
  }
  if (ite %in% it.1P$CMOMi) {
      output.1P.CMOM[11:20,ite] <- NA 
  }}

for (ite in 1:ncol(output.1P.CMR)){
  if (ite %in% it.1P$CMR) {
    output.1P.CMR[,ite] <- NA
  }}

# FOR 2P
for (ite in 1:ncol(output.2P.CMOM)){
  if (ite %in% it.2P$CMOMd) {
    output.2P.CMOM[1:12,ite] <- NA 
  }
  if (ite %in% it.2P$CMOMi) {
    output.2P.CMOM[13:24,ite] <- NA 
  }}

for (ite in 1:ncol(output.2P.CMR)){
  if (ite %in% it.2P$CMR) {
    output.2P.CMR[,ite] <- NA
  }}

# FOR 1T
for (ite in 1:ncol(output.1T.CMOM)){
  if (ite %in% it.1T$CMOMd) {
    output.1T.CMOM[1:10,ite] <- NA 
  }
  if (ite %in% it.1T$CMOMi) {
    output.1T.CMOM[11:20,ite] <- NA 
  }}

for (ite in 1:ncol(output.1T.CMR)){
  if (ite %in% it.1T$CMR) {
    output.1T.CMR[,ite] <- NA
  }}

# FOR 1H
for (ite in 1:ncol(output.1H.CMOM)){
  if (ite %in% it.1H$CMOMd) {
    output.1H.CMOM[1:10,ite] <- NA 
  }
  if (ite %in% it.1H$CMOMi) {
    output.1H.CMOM[11:20,ite] <- NA 
  }}

for (ite in 1:ncol(output.1H.CMR)){
  if (ite %in% it.1H$CMR) {
    output.1H.CMR[,ite] <- NA
  }}

# FOR 2T
for (ite in 1:ncol(output.2T.CMOM)){
  if (ite %in% it.2T$CMOMd) {
    output.2T.CMOM[1:12,ite] <- NA 
  }
  if (ite %in% it.2T$CMOMi) {
    output.2T.CMOM[13:24,ite] <- NA 
  }}

for (ite in 1:ncol(output.2T.CMR)){
  if (ite %in% it.2T$CMR) {
    output.2T.CMR[,ite] <- NA
  }}

output.1P <- rbind(output.1P.CMOM, output.1P.CMR)
output.2P <- rbind(output.2P.CMOM, output.2P.CMR)
output.1T <- rbind(output.1T.CMOM, output.1T.CMR)
output.1H <- rbind(output.1H.CMOM, output.1H.CMR)
output.2T <- rbind(output.2T.CMOM, output.2T.CMR)
output.2H <- rbind(output.2H.CMOM, output.2H.CMR)

setwd("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat")
save(output.1P,file="output1P.RData")
save(output.2P,file="output2P.RData")
save(output.1T,file="output1T.RData")
save(output.1H,file="output1H.RData")
save(output.2T,file="output2T.RData")
save(output.2H,file="output2H.RData")