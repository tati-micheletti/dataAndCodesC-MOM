#####################################################################
#                                                                   #
#         12a.    C O E F I C I E N T   O F   V A R I A T I O N     #
#                                                                   #
#                           1  T                                    #
#                                                                   #
#####################################################################

load("C:/Users/Micheletti/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-1T.Rdata")

# Convert NULL into NA
for (ite in 1:iter){
if (is.null(TN.ZPNEc[[ite]]))
  TN.ZPNEc[[ite]] <- rep(NA, 3)}

# ====================== N ======================= #

N.1T.CV <- list()

for (ite in 1:iter){
  N.1T.CV$CMOMd[[ite]] <- abs(log(N.1T$CMOMd[[ite]])-log(ls.T.N[[ite]]))
  N.1T.CV$CMOMi[[ite]] <- abs(log(N.1T$CMOMi[[ite]])-log(ls.T.N[[ite]]))
  N.1T.CV$CMR[[ite]]   <- abs(log(N.1T$CMR[[ite]])-log(ls.T.N[[ite]]))
  N.1T.CV$ZPNEc[[ite]] <- abs(log(TN.ZPNEc[[ite]])-log(ls.N.ZPNE[[ite]]))
  N.1T.CV$ZPNEs[[ite]] <- abs(log(TN.ZPNEs[[ite]])-log(ls.N.ZPNE[[ite]]))}

N.1T.CV$CMOMd <- sd(unlist(N.1T.CV$CMOMd), na.rm = T)
N.1T.CV$CMOMi <- sd(unlist(N.1T.CV$CMOMi), na.rm = T)  
N.1T.CV$CMR <- sd(unlist(N.1T.CV$CMR), na.rm = T)  
N.1T.CV$ZPNEc <- sd(unlist(N.1T.CV$ZPNEc), na.rm = T)  
N.1T.CV$ZPNEs <- sd(unlist(N.1T.CV$ZPNEs), na.rm = T)

# ====================== B ======================= #
B.1T.CV <- list()

for (ite in 1:iter){
  B.1T.CV$CMOMd[[ite]] <- abs(log(B.1T$CMOMd[[ite]])-log(ls.T.B[[ite]]))
  B.1T.CV$CMOMi[[ite]] <- abs(log(B.1T$CMOMi[[ite]])-log(ls.T.B[[ite]]))
  B.1T.CV$CMR[[ite]]   <- abs(log(B.1T$CMR[[ite]])-log(ls.T.B[[ite]]))}

for (ite in 1:iter){
  for (e in 1:2){
  if (is.infinite(B.1T.CV$CMOMd[[ite]][e]))
    B.1T.CV$CMOMd[[ite]][e] <- NA
  if (is.infinite(B.1T.CV$CMOMi[[ite]][e]))
    B.1T.CV$CMOMi[[ite]][e] <- NA
  if (is.infinite(B.1T.CV$CMR[[ite]][e]))
    B.1T.CV$CMR[[ite]][e] <- NA
}}

B.1T.CV$CMOMd <- sd(unlist(B.1T.CV$CMOMd), na.rm = T)
B.1T.CV$CMOMi <- sd(unlist(B.1T.CV$CMOMi), na.rm = T)
B.1T.CV$CMR <- sd(unlist(B.1T.CV$CMR), na.rm = T)

# ====================== phin ======================= #

phin.1T.CV <- list()

for (ite in 1:iter){
  phin.1T.CV$CMOMd[[ite]] <- abs(output.1T[1,ite]-Original.T[1,ite])
  phin.1T.CV$CMOMi[[ite]] <- abs(output.1T[7,ite]-Original.T[1,ite])
  phin.1T.CV$CMR[[ite]]   <- abs(output.1T[13,ite]-Original.T[1,ite])}

phin.1T.CV$CMOMd <- sd(phin.1T.CV$CMOMd, na.rm = T)
phin.1T.CV$CMOMi <- sd(phin.1T.CV$CMOMi, na.rm = T)  
phin.1T.CV$CMR <- sd(phin.1T.CV$CMR, na.rm = T)

# ====================== phib ======================= #

phib.1T.CV <- list()

for (ite in 1:iter){
  phib.1T.CV$CMOMd[[ite]] <- abs(output.1T[3,ite]-Original.T[2,ite])
  phib.1T.CV$CMOMi[[ite]] <- abs(output.1T[9,ite]-Original.T[2,ite])
  phib.1T.CV$CMR[[ite]]   <- abs(output.1T[15,ite]-Original.T[2,ite])
  phib.1T.CV$ZPNEc[[ite]] <- abs(output.ZPNEc[1,ite]-Original.T[2,ite])
  phib.1T.CV$ZPNEs[[ite]]   <- abs(output.ZPNEs[1,ite]-Original.T[2,ite])}

phib.1T.CV$CMOMd <- sd(phib.1T.CV$CMOMd, na.rm = T)
phib.1T.CV$CMOMi <- sd(phib.1T.CV$CMOMi, na.rm = T)
phib.1T.CV$CMR <- sd(phib.1T.CV$CMR, na.rm = T)
phib.1T.CV$ZPNEc <- sd(phib.1T.CV$ZPNEc, na.rm = T)
phib.1T.CV$ZPNEs <- sd(phib.1T.CV$ZPNEs, na.rm = T)

# ====================== p2 ======================= #

p2.1T.CV <- list()

for (ite in 1:iter){
  p2.1T.CV$CMOMd[[ite]] <- abs(output.1T[5,ite]-Original.T[4,ite])
  p2.1T.CV$CMOMi[[ite]] <- abs(output.1T[11,ite]-Original.T[4,ite])
  p2.1T.CV$CMR[[ite]]   <- abs(output.1T[17,ite]-Original.T[4,ite])
  p2.1T.CV$ZPNEc[[ite]] <- abs(output.ZPNEc[3,ite]-Original.T[4,ite])
  p2.1T.CV$ZPNEs[[ite]]   <- abs(output.ZPNEs[3,ite]-Original.T[4,ite])}

for (i in 1:length(p2.1T.CV$ZPNEs)){
  if (p2.1T.CV$ZPNEs[i]>100|p2.1T.CV$ZPNEs[i]<(-100))
    p2.1T.CV$ZPNEs[i] <- NA
}

p2.1T.CV$CMOMd <- sd(p2.1T.CV$CMOMd, na.rm = T)
p2.1T.CV$CMOMi <- sd(p2.1T.CV$CMOMi, na.rm = T)
p2.1T.CV$CMR <- sd(p2.1T.CV$CMR, na.rm = T)
p2.1T.CV$ZPNEc <- sd(p2.1T.CV$ZPNEc, na.rm = T)
p2.1T.CV$ZPNEs <- sd(p2.1T.CV$ZPNEs, na.rm = T)

# ================= DATA FRAME =================

# ~~~~~ N ~~~~~
stack.N.1T <- stack(N.1T.CV)
stack.N.1T$Models <- factor(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"))
colnames(stack.N.1T)[1:2] <- c("Values", "Results")
stack.N.1T$P.factor <- factor("1T")
stack.N.1T$Parameters <- factor("Number of Individuals")
stack.N.1T$Results <- paste(stack.N.1T$Models, stack.N.1T$P.factor)
for (i in 1:nrow(stack.N.1T)){
  if (stack.N.1T$Results[i]=="ZPNEc 1T") stack.N.1T$Results[i] <- "ZPNEc"
  if (stack.N.1T$Results[i]=="ZPNEs 1T") stack.N.1T$Results[i] <- "ZPNEs"
}

# ~~~~~ B ~~~~~
stack.B.1T <- stack(B.1T.CV)
stack.B.1T$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.B.1T)[1:2] <- c("Values", "Results")
stack.B.1T$P.factor <- factor("1T")
stack.B.1T$Parameters <- factor("Recruitment")
stack.B.1T$Results <- paste(stack.B.1T$Models, stack.B.1T$P.factor)

# ~~~~~ phin ~~~~~
stack.phin.1T <- stack(phin.1T.CV)
stack.phin.1T$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.phin.1T)[1:2] <- c("Values", "Results")
stack.phin.1T$P.factor <- factor("1T")
stack.phin.1T$Parameters <- factor("Survival Prim. Occ.")
stack.phin.1T$Results <- paste(stack.phin.1T$Models, stack.phin.1T$P.factor)

# ~~~~~ phib ~~~~~
stack.phib.1T <- stack(phib.1T.CV)
stack.phib.1T$Models <- factor(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"))
colnames(stack.phib.1T)[1:2] <- c("Values", "Results")
stack.phib.1T$P.factor <- factor("1T")
stack.phib.1T$Parameters <- factor("Survival Second. Occ.")
stack.phib.1T$Results <- paste(stack.phib.1T$Models, stack.phib.1T$P.factor)
for (i in 1:nrow(stack.phib.1T)){
  if (stack.phib.1T$Results[i]=="ZPNEc 1T") stack.phib.1T$Results[i] <- "ZPNEc"
  if (stack.phib.1T$Results[i]=="ZPNEs 1T") stack.phib.1T$Results[i] <- "ZPNEs"
}

# ~~~~~ p2 ~~~~~
stack.p2.1T <- stack(p2.1T.CV)
stack.p2.1T$Models <- factor(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"))
colnames(stack.p2.1T)[1:2] <- c("Values", "Results")
stack.p2.1T$P.factor <- factor("1T")
stack.p2.1T$Parameters <- factor("Observation Probability")
stack.p2.1T$Results <- paste(stack.p2.1T$Models, stack.p2.1T$P.factor)
for (i in 1:nrow(stack.p2.1T)){
  if (stack.p2.1T$Results[i]=="ZPNEc 1T") stack.p2.1T$Results[i] <- "ZPNEc"
  if (stack.p2.1T$Results[i]=="ZPNEs 1T") stack.p2.1T$Results[i] <- "ZPNEs"
}

CV.1T <- rbind(stack.N.1T, stack.B.1T, stack.phin.1T, stack.phib.1T, stack.p2.1T)

save(CV.1T,file="C:/Users/Micheletti/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-1T-TableA.Rdata")

