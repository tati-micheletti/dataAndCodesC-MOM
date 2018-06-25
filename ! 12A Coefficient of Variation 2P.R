#####################################################################
#                                                                   #
#         12a.    C O E F I C I E N T   O F   V A R I A T I O N     #
#                                                                   #
#                           1  P                                    #
#                                                                   #
#####################################################################

load("C:/Users/Micheletti/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-2P.Rdata")

# ====================== N ======================= #

N.2P.CV <- list()

for (ite in 1:iter){
  N.2P.CV$CMOMd[[ite]] <- abs(log(N.2P$CMOMd[[ite]])-log(ls.N[[ite]]))
  N.2P.CV$CMOMi[[ite]] <- abs(log(N.2P$CMOMi[[ite]])-log(ls.N[[ite]]))
  N.2P.CV$CMR[[ite]]   <- abs(log(N.2P$CMR[[ite]])-log(ls.N[[ite]]))}

N.2P.CV$CMOMd <- sd(unlist(N.2P.CV$CMOMd), na.rm = T)
N.2P.CV$CMOMi <- sd(unlist(N.2P.CV$CMOMi), na.rm = T)  
N.2P.CV$CMR <- sd(unlist(N.2P.CV$CMR), na.rm = T)  

# ====================== B ======================= #
B.2P.CV <- list()

for (ite in 1:iter){
  B.2P.CV$CMOMd[[ite]] <- abs(log(B.2P$CMOMd[[ite]])-log(ls.B[[ite]]))
  B.2P.CV$CMOMi[[ite]] <- abs(log(B.2P$CMOMi[[ite]])-log(ls.B[[ite]]))
  B.2P.CV$CMR[[ite]]   <- abs(log(B.2P$CMR[[ite]])-log(ls.B[[ite]]))}

B.2P.CV$CMOMd <- sd(unlist(B.2P.CV$CMOMd), na.rm = T)
B.2P.CV$CMOMi <- sd(unlist(B.2P.CV$CMOMi), na.rm = T)
B.2P.CV$CMR <- sd(unlist(B.2P.CV$CMR), na.rm = T)

# ====================== phin ======================= #

phin.2P.CV <- list()

for (ite in 1:iter){
  phin.2P.CV$CMOMd[[ite]] <- abs(output.2P[1,ite]-Original.1P2P[1,ite])
  phin.2P.CV$CMOMi[[ite]] <- abs(output.2P[9,ite]-Original.1P2P[1,ite])
  phin.2P.CV$CMR[[ite]]   <- abs(output.2P[17,ite]-Original.1P2P[1,ite])}

phin.2P.CV$CMOMd <- sd(phin.2P.CV$CMOMd, na.rm = T)
phin.2P.CV$CMOMi <- sd(phin.2P.CV$CMOMi, na.rm = T)  
phin.2P.CV$CMR <- sd(phin.2P.CV$CMR, na.rm = T)

# ====================== phib ======================= #

phib.2P.CV <- list()

for (ite in 1:iter){
  phib.2P.CV$CMOMd[[ite]] <- abs(output.2P[3,ite]-Original.1P2P[2,ite])
  phib.2P.CV$CMOMi[[ite]] <- abs(output.2P[11,ite]-Original.1P2P[2,ite])
  phib.2P.CV$CMR[[ite]]   <- abs(output.2P[19,ite]-Original.1P2P[2,ite])}

phib.2P.CV$CMOMd <- sd(phib.2P.CV$CMOMd, na.rm = T)
phib.2P.CV$CMOMi <- sd(phib.2P.CV$CMOMi, na.rm = T)
phib.2P.CV$CMR <- sd(phib.2P.CV$CMR, na.rm = T)

# ====================== p2 ======================= #

p2.2P.CV <- list()

for (ite in 1:iter){
  p2.2P.CV$CMOMd[[ite]] <- abs(output.2P[7,ite]-Original.1P2P[4,ite])
  p2.2P.CV$CMOMi[[ite]] <- abs(output.2P[15,ite]-Original.1P2P[4,ite])
  p2.2P.CV$CMR[[ite]]   <- abs(output.2P[23,ite]-Original.1P2P[4,ite])}

p2.2P.CV$CMOMd <- sd(p2.2P.CV$CMOMd, na.rm = T)
p2.2P.CV$CMOMi <- sd(p2.2P.CV$CMOMi, na.rm = T)
p2.2P.CV$CMR <- sd(p2.2P.CV$CMR, na.rm = T)

# ====================== p1 ======================= #

p1.2P.CV <- list()

for (ite in 1:iter){
  p1.2P.CV$CMOMd[[ite]] <- abs(output.2P[5,ite]-Original.1P2P[3,ite])
  p1.2P.CV$CMOMi[[ite]] <- abs(output.2P[13,ite]-Original.1P2P[3,ite])
  p1.2P.CV$CMR[[ite]]   <- abs(output.2P[21,ite]-Original.1P2P[3,ite])}

p1.2P.CV$CMOMd <- sd(p1.2P.CV$CMOMd, na.rm = T)
p1.2P.CV$CMOMi <- sd(p1.2P.CV$CMOMi, na.rm = T)
p1.2P.CV$CMR <- sd(p1.2P.CV$CMR, na.rm = T)

# ================= DATA FRAME =================

# ~~~~~ N ~~~~~
stack.N.2P <- stack(N.2P.CV)
stack.N.2P$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.N.2P)[1:2] <- c("Values", "Results")
stack.N.2P$P.factor <- factor("2P")
stack.N.2P$Parameters <- factor("Number of Individuals")
stack.N.2P$Results <- paste(stack.N.2P$Models, stack.N.2P$P.factor)

# ~~~~~ B ~~~~~
stack.B.2P <- stack(B.2P.CV)
stack.B.2P$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.B.2P)[1:2] <- c("Values", "Results")
stack.B.2P$P.factor <- factor("2P")
stack.B.2P$Parameters <- factor("Recruitment")
stack.B.2P$Results <- paste(stack.B.2P$Models, stack.B.2P$P.factor)

# ~~~~~ phin ~~~~~
stack.phin.2P <- stack(phin.2P.CV)
stack.phin.2P$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.phin.2P)[1:2] <- c("Values", "Results")
stack.phin.2P$P.factor <- factor("2P")
stack.phin.2P$Parameters <- factor("Survival Prim. Occ.")
stack.phin.2P$Results <- paste(stack.phin.2P$Models, stack.phin.2P$P.factor)

# ~~~~~ phib ~~~~~
stack.phib.2P <- stack(phib.2P.CV)
stack.phib.2P$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.phib.2P)[1:2] <- c("Values", "Results")
stack.phib.2P$P.factor <- factor("2P")
stack.phib.2P$Parameters <- factor("Survival Second. Occ.")
stack.phib.2P$Results <- paste(stack.phib.2P$Models, stack.phib.2P$P.factor)

# ~~~~~ p2 ~~~~~
stack.p2.2P <- stack(p2.2P.CV)
stack.p2.2P$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.p2.2P)[1:2] <- c("Values", "Results")
stack.p2.2P$P.factor <- factor("2P")
stack.p2.2P$Parameters <- factor("Observation Probability")
stack.p2.2P$Results <- paste(stack.p2.2P$Models, stack.p2.2P$P.factor)

# ~~~~~ p1 ~~~~~
stack.p1.2P <- stack(p1.2P.CV)
stack.p1.2P$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.p1.2P)[1:2] <- c("Values", "Results")
stack.p1.2P$P.factor <- factor("2P")
stack.p1.2P$Parameters <- factor("Observation Probability")
stack.p1.2P$Results <- paste(stack.p1.2P$Models, stack.p1.2P$P.factor)

CV.2P <- rbind(stack.N.2P, stack.B.2P, stack.phin.2P, stack.phib.2P, stack.p2.2P, stack.p1.2P)

save(CV.2P,file="C:/Users/Micheletti/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-2P-TableA.Rdata")

