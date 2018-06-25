#####################################################################
#                                                                   #
#         12a.    C O E F I C I E N T   O F   V A R I A T I O N     #
#                                                                   #
#                           1  P                                    #
#                                                                   #
#####################################################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-1P.Rdata")
  
# ====================== N ======================= #

N.1P.CV <- list()

for (ite in 1:iter){
  N.1P.CV$CMOMd[[ite]] <- log(N.1P$CMOMd[[ite]])-log(ls.N[[ite]])
  N.1P.CV$CMOMi[[ite]] <- log(N.1P$CMOMi[[ite]])-log(ls.N[[ite]])
  N.1P.CV$CMR[[ite]]   <- log(N.1P$CMR[[ite]])-log(ls.N[[ite]])
  N.1P.CV$ZPNEc[[ite]] <- log(N.ZPNEc[[ite]])-log(ls.N.ZPNE[[ite]])
  N.1P.CV$ZPNEs[[ite]] <- log(N.ZPNEs[[ite]])-log(ls.N.ZPNE[[ite]])}

  N.1P.CV$CMOMd <- sd(unlist(N.1P.CV$CMOMd), na.rm = T)
  N.1P.CV$CMOMi <- sd(unlist(N.1P.CV$CMOMi), na.rm = T)  
  N.1P.CV$CMR <- sd(unlist(N.1P.CV$CMR), na.rm = T)  
  N.1P.CV$ZPNEc <- sd(unlist(N.1P.CV$ZPNEc), na.rm = T)  
  N.1P.CV$ZPNEs <- sd(unlist(N.1P.CV$ZPNEs), na.rm = T)

# ====================== B ======================= #
  B.1P.CV <- list()
  
  for (ite in 1:iter){
    B.1P.CV$CMOMd[[ite]] <- log(B.1P$CMOMd[[ite]])-log(ls.B[[ite]])
    B.1P.CV$CMOMi[[ite]] <- log(B.1P$CMOMi[[ite]])-log(ls.B[[ite]])
    B.1P.CV$CMR[[ite]]   <- log(B.1P$CMR[[ite]])-log(ls.B[[ite]])}
  
  B.1P.CV$CMOMd <- sd(unlist(B.1P.CV$CMOMd), na.rm = T)
  B.1P.CV$CMOMi <- sd(unlist(B.1P.CV$CMOMi), na.rm = T)
  B.1P.CV$CMR <- sd(unlist(B.1P.CV$CMR), na.rm = T)

  # ====================== phin ======================= #
  
  phin.1P.CV <- list()
  
  for (ite in 1:iter){
    phin.1P.CV$CMOMd[[ite]] <- output.1P[1,ite]-Original.1P2P[1,ite]
    phin.1P.CV$CMOMi[[ite]] <- output.1P[7,ite]-Original.1P2P[1,ite]
    phin.1P.CV$CMR[[ite]]   <- output.1P[13,ite]-Original.1P2P[1,ite]}
  
  phin.1P.CV$CMOMd <- sd(phin.1P.CV$CMOMd, na.rm = T)
  phin.1P.CV$CMOMi <- sd(phin.1P.CV$CMOMi, na.rm = T)  
  phin.1P.CV$CMR <- sd(phin.1P.CV$CMR, na.rm = T)
  
  # ====================== phib ======================= #
  
  phib.1P.CV <- list()
  
  for (ite in 1:iter){
    phib.1P.CV$CMOMd[[ite]] <- output.1P[3,ite]-Original.1P2P[2,ite]
    phib.1P.CV$CMOMi[[ite]] <- output.1P[9,ite]-Original.1P2P[2,ite]
    phib.1P.CV$CMR[[ite]]   <- output.1P[15,ite]-Original.1P2P[2,ite]
    phib.1P.CV$ZPNEc[[ite]] <- output.ZPNEc[1,ite]-Original.1P2P[2,ite]
    phib.1P.CV$ZPNEs[[ite]]   <- output.ZPNEs[1,ite]-Original.1P2P[2,ite]}
  
  phib.1P.CV$CMOMd <- sd(phib.1P.CV$CMOMd, na.rm = T)
  phib.1P.CV$CMOMi <- sd(phib.1P.CV$CMOMi, na.rm = T)
  phib.1P.CV$CMR <- sd(phib.1P.CV$CMR, na.rm = T)
  phib.1P.CV$ZPNEc <- sd(phib.1P.CV$ZPNEc, na.rm = T)
  phib.1P.CV$ZPNEs <- sd(phib.1P.CV$ZPNEs, na.rm = T)
  
  # ====================== p2 ======================= #
  
  p2.1P.CV <- list()
  
  for (ite in 1:iter){
    p2.1P.CV$CMOMd[[ite]] <- output.1P[5,ite]-Original.1P2P[4,ite]
    p2.1P.CV$CMOMi[[ite]] <- output.1P[11,ite]-Original.1P2P[4,ite]
    p2.1P.CV$CMR[[ite]]   <- output.1P[17,ite]-Original.1P2P[4,ite]
    p2.1P.CV$ZPNEc[[ite]] <- output.ZPNEc[3,ite]-Original.1P2P[4,ite]
    p2.1P.CV$ZPNEs[[ite]]   <- output.ZPNEs[3,ite]-Original.1P2P[4,ite]}
  
  p2.1P.CV$CMOMd <- sd(p2.1P.CV$CMOMd, na.rm = T)
  p2.1P.CV$CMOMi <- sd(p2.1P.CV$CMOMi, na.rm = T)
  p2.1P.CV$CMR <- sd(p2.1P.CV$CMR, na.rm = T)
  p2.1P.CV$ZPNEc <- sd(p2.1P.CV$ZPNEc, na.rm = T)
  p2.1P.CV$ZPNEs <- sd(p2.1P.CV$ZPNEs, na.rm = T)
  
# ================= DATA FRAME =================

# ~~~~~ N ~~~~~
stack.N.1P <- stack(N.1P.CV)
stack.N.1P$Models <- factor(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"))
colnames(stack.N.1P)[1:2] <- c("Values", "Results")
stack.N.1P$P.factor <- factor("1P")
stack.N.1P$Parameters <- factor("Number of Individuals")
stack.N.1P$Results <- paste(stack.N.1P$Models, stack.N.1P$P.factor)
for (i in 1:nrow(stack.N.1P)){
  if (stack.N.1P$Results[i]=="ZPNEc 1P") stack.N.1P$Results[i] <- "ZPNEc"
  if (stack.N.1P$Results[i]=="ZPNEs 1P") stack.N.1P$Results[i] <- "ZPNEs"
}

# ~~~~~ B ~~~~~
stack.B.1P <- stack(B.1P.CV)
stack.B.1P$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.B.1P)[1:2] <- c("Values", "Results")
stack.B.1P$P.factor <- factor("1P")
stack.B.1P$Parameters <- factor("Recruitment")
stack.B.1P$Results <- paste(stack.B.1P$Models, stack.B.1P$P.factor)

# ~~~~~ phin ~~~~~
stack.phin.1P <- stack(phin.1P.CV)
stack.phin.1P$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.phin.1P)[1:2] <- c("Values", "Results")
stack.phin.1P$P.factor <- factor("1P")
stack.phin.1P$Parameters <- factor("Survival Prim. Occ.")
stack.phin.1P$Results <- paste(stack.phin.1P$Models, stack.phin.1P$P.factor)

# ~~~~~ phib ~~~~~
stack.phib.1P <- stack(phib.1P.CV)
stack.phib.1P$Models <- factor(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"))
colnames(stack.phib.1P)[1:2] <- c("Values", "Results")
stack.phib.1P$P.factor <- factor("1P")
stack.phib.1P$Parameters <- factor("Survival Second. Occ.")
stack.phib.1P$Results <- paste(stack.phib.1P$Models, stack.phib.1P$P.factor)
for (i in 1:nrow(stack.phib.1P)){
  if (stack.phib.1P$Results[i]=="ZPNEc 1P") stack.phib.1P$Results[i] <- "ZPNEc"
  if (stack.phib.1P$Results[i]=="ZPNEs 1P") stack.phib.1P$Results[i] <- "ZPNEs"
}

# ~~~~~ p2 ~~~~~
stack.p2.1P <- stack(p2.1P.CV)
stack.p2.1P$Models <- factor(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"))
colnames(stack.p2.1P)[1:2] <- c("Values", "Results")
stack.p2.1P$P.factor <- factor("1P")
stack.p2.1P$Parameters <- factor("Observation Probability")
stack.p2.1P$Results <- paste(stack.p2.1P$Models, stack.p2.1P$P.factor)
for (i in 1:nrow(stack.p2.1P)){
  if (stack.p2.1P$Results[i]=="ZPNEc 1P") stack.p2.1P$Results[i] <- "ZPNEc"
  if (stack.p2.1P$Results[i]=="ZPNEs 1P") stack.p2.1P$Results[i] <- "ZPNEs"
}

CV.1P <- rbind(stack.N.1P, stack.B.1P, stack.phin.1P, stack.phib.1P, stack.p2.1P)

save(CV.1P,file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-1P-Table.Rdata")

