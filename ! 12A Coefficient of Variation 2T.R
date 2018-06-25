#####################################################################
#                                                                   #
#         12a.    C O E F I C I E N T   O F   V A R I A T I O N     #
#                                                                   #
#                           2  T                                    #
#                                                                   #
#####################################################################

load("C:/Users/Micheletti/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-2T.Rdata")

# Convert NULL into NA
for (ite in 1:iter){
  if (is.null(TN.ZPNEc[[ite]]))
    TN.ZPNEc[[ite]] <- rep(NA, 3)}

# ====================== N ======================= #

N.2T.CV <- list()

for (ite in 1:iter){
  N.2T.CV$CMOMd[[ite]] <- abs(log(N.2T$CMOMd[[ite]])-log(ls.T.N[[ite]]))
  N.2T.CV$CMOMi[[ite]] <- abs(log(N.2T$CMOMi[[ite]])-log(ls.T.N[[ite]]))
  N.2T.CV$CMR[[ite]]   <- abs(log(N.2T$CMR[[ite]])-log(ls.T.N[[ite]]))}

N.2T.CV$CMOMd <- sd(unlist(N.2T.CV$CMOMd), na.rm = T)
N.2T.CV$CMOMi <- sd(unlist(N.2T.CV$CMOMi), na.rm = T)  
N.2T.CV$CMR <- sd(unlist(N.2T.CV$CMR), na.rm = T)

# ====================== B ======================= #
B.2T.CV <- list()

for (ite in 1:iter){
  B.2T.CV$CMOMd[[ite]] <- abs(log(B.2T$CMOMd[[ite]])-log(ls.T.B[[ite]]))
  B.2T.CV$CMOMi[[ite]] <- abs(log(B.2T$CMOMi[[ite]])-log(ls.T.B[[ite]]))
  B.2T.CV$CMR[[ite]]   <- abs(log(B.2T$CMR[[ite]])-log(ls.T.B[[ite]]))}

for (ite in 1:iter){
  for (e in 1:2){
    if (is.infinite(B.2T.CV$CMOMd[[ite]][e]))
      B.2T.CV$CMOMd[[ite]][e] <- NA
    if (is.infinite(B.2T.CV$CMOMi[[ite]][e]))
      B.2T.CV$CMOMi[[ite]][e] <- NA
    if (is.infinite(B.2T.CV$CMR[[ite]][e]))
      B.2T.CV$CMR[[ite]][e] <- NA
  }}

B.2T.CV$CMOMd <- sd(unlist(B.2T.CV$CMOMd), na.rm = T)
B.2T.CV$CMOMi <- sd(unlist(B.2T.CV$CMOMi), na.rm = T)
B.2T.CV$CMR <- sd(unlist(B.2T.CV$CMR), na.rm = T)

# ====================== phin ======================= #

phin.2T.CV <- list()

for (ite in 1:iter){
  phin.2T.CV$CMOMd[[ite]] <- abs(output.2T[1,ite]-Original.T[1,ite])
  phin.2T.CV$CMOMi[[ite]] <- abs(output.2T[9,ite]-Original.T[1,ite])
  phin.2T.CV$CMR[[ite]]   <- abs(output.2T[17,ite]-Original.T[1,ite])}

phin.2T.CV$CMOMd <- sd(phin.2T.CV$CMOMd, na.rm = T)
phin.2T.CV$CMOMi <- sd(phin.2T.CV$CMOMi, na.rm = T)  
phin.2T.CV$CMR <- sd(phin.2T.CV$CMR, na.rm = T)

# ====================== phib ======================= #

phib.2T.CV <- list()

for (ite in 1:iter){
  phib.2T.CV$CMOMd[[ite]] <- abs(output.2T[3,ite]-Original.T[2,ite])
  phib.2T.CV$CMOMi[[ite]] <- abs(output.2T[11,ite]-Original.T[2,ite])
  phib.2T.CV$CMR[[ite]]   <- abs(output.2T[19,ite]-Original.T[2,ite])}

phib.2T.CV$CMOMd <- sd(phib.2T.CV$CMOMd, na.rm = T)
phib.2T.CV$CMOMi <- sd(phib.2T.CV$CMOMi, na.rm = T)
phib.2T.CV$CMR <- sd(phib.2T.CV$CMR, na.rm = T)

# ====================== p2 ======================= #

p2.2T.CV <- list()

for (ite in 1:iter){
  p2.2T.CV$CMOMd[[ite]] <- abs(output.2T[7,ite]-Original.T[4,ite])
  p2.2T.CV$CMOMi[[ite]] <- abs(output.2T[15,ite]-Original.T[4,ite])
  p2.2T.CV$CMR[[ite]]   <- abs(output.2T[23,ite]-Original.T[4,ite])}

p2.2T.CV$CMOMd <- sd(p2.2T.CV$CMOMd, na.rm = T)
p2.2T.CV$CMOMi <- sd(p2.2T.CV$CMOMi, na.rm = T)
p2.2T.CV$CMR <- sd(p2.2T.CV$CMR, na.rm = T)

# ====================== p1 ======================= #

p1.2T.CV <- list()

for (ite in 1:iter){
  p1.2T.CV$CMOMd[[ite]] <- abs(output.2T[5,ite]-Original.T[3,ite])
  p1.2T.CV$CMOMi[[ite]] <- abs(output.2T[13,ite]-Original.T[3,ite])
  p1.2T.CV$CMR[[ite]]   <- abs(output.2T[21,ite]-Original.T[3,ite])}

p1.2T.CV$CMOMd <- sd(p1.2T.CV$CMOMd, na.rm = T)
p1.2T.CV$CMOMi <- sd(p1.2T.CV$CMOMi, na.rm = T)
p1.2T.CV$CMR <- sd(p1.2T.CV$CMR, na.rm = T)
# ================= DATA FRAME =================

# ~~~~~ N ~~~~~
stack.N.2T <- stack(N.2T.CV)
stack.N.2T$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.N.2T)[1:2] <- c("Values", "Results")
stack.N.2T$P.factor <- factor("2T")
stack.N.2T$Parameters <- factor("Number of Individuals")
stack.N.2T$Results <- paste(stack.N.2T$Models, stack.N.2T$P.factor)

# ~~~~~ B ~~~~~
stack.B.2T <- stack(B.2T.CV)
stack.B.2T$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.B.2T)[1:2] <- c("Values", "Results")
stack.B.2T$P.factor <- factor("2T")
stack.B.2T$Parameters <- factor("Recruitment")
stack.B.2T$Results <- paste(stack.B.2T$Models, stack.B.2T$P.factor)

# ~~~~~ phin ~~~~~
stack.phin.2T <- stack(phin.2T.CV)
stack.phin.2T$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.phin.2T)[1:2] <- c("Values", "Results")
stack.phin.2T$P.factor <- factor("2T")
stack.phin.2T$Parameters <- factor("Survival Prim. Occ.")
stack.phin.2T$Results <- paste(stack.phin.2T$Models, stack.phin.2T$P.factor)

# ~~~~~ phib ~~~~~
stack.phib.2T <- stack(phib.2T.CV)
stack.phib.2T$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.phib.2T)[1:2] <- c("Values", "Results")
stack.phib.2T$P.factor <- factor("2T")
stack.phib.2T$Parameters <- factor("Survival Second. Occ.")
stack.phib.2T$Results <- paste(stack.phib.2T$Models, stack.phib.2T$P.factor)

# ~~~~~ p2 ~~~~~
stack.p2.2T <- stack(p2.2T.CV)
stack.p2.2T$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.p2.2T)[1:2] <- c("Values", "Results")
stack.p2.2T$P.factor <- factor("2T")
stack.p2.2T$Parameters <- factor("Observation Probability")
stack.p2.2T$Results <- paste(stack.p2.2T$Models, stack.p2.2T$P.factor)


# ~~~~~ p1 ~~~~~
stack.p1.2T <- stack(p1.2T.CV)
stack.p1.2T$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.p1.2T)[1:2] <- c("Values", "Results")
stack.p1.2T$P.factor <- factor("2T")
stack.p1.2T$Parameters <- factor("Observation Probability")
stack.p1.2T$Results <- paste(stack.p1.2T$Models, stack.p1.2T$P.factor)

CV.2T <- rbind(stack.N.2T, stack.B.2T, stack.phin.2T, stack.phib.2T, stack.p2.2T, stack.p1.2T)

save(CV.2T,file="C:/Users/Micheletti/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-2T-TableA.Rdata")

