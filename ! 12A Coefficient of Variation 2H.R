#####################################################################
#                                                                   #
#         12a.    C O E F I C I E N T   O F   V A R I A T I O N     #
#                                                                   #
#                           2  H                                    #
#                                                                   #
#####################################################################

load("C:/Users/Micheletti/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-2H.Rdata")

# Convert NULL into NA
for (ite in 1:iter){
  if (is.null(HN.ZPNEc[[ite]]))
    HN.ZPNEc[[ite]] <- rep(NA, 3)}

# ====================== N ======================= #

N.2H.CV <- list()

for (ite in 1:iter){
  N.2H.CV$CMOMd[[ite]] <- abs(log(N.2H$CMOMd[[ite]])-log(ls.H.N[[ite]]))
  N.2H.CV$CMOMi[[ite]] <- abs(log(N.2H$CMOMi[[ite]])-log(ls.H.N[[ite]]))
  N.2H.CV$CMR[[ite]]   <- abs(log(N.2H$CMR[[ite]])-log(ls.H.N[[ite]]))}

N.2H.CV$CMOMd <- sd(unlist(N.2H.CV$CMOMd), na.rm = T)
N.2H.CV$CMOMi <- sd(unlist(N.2H.CV$CMOMi), na.rm = T)  
N.2H.CV$CMR <- sd(unlist(N.2H.CV$CMR), na.rm = T)  

# ====================== B ======================= #
B.2H.CV <- list()

for (ite in 1:iter){
  B.2H.CV$CMOMd[[ite]] <- abs(log(B.2H$CMOMd[[ite]])-log(ls.H.B[[ite]]))
  B.2H.CV$CMOMi[[ite]] <- abs(log(B.2H$CMOMi[[ite]])-log(ls.H.B[[ite]]))
  B.2H.CV$CMR[[ite]]   <- abs(log(B.2H$CMR[[ite]])-log(ls.H.B[[ite]]))}

for (ite in 1:iter){
  for (e in 1:2){
    if (is.infinite(B.2H.CV$CMOMd[[ite]][e]))
      B.2H.CV$CMOMd[[ite]][e] <- NA
    if (is.infinite(B.2H.CV$CMOMi[[ite]][e]))
      B.2H.CV$CMOMi[[ite]][e] <- NA
    if (is.infinite(B.2H.CV$CMR[[ite]][e]))
      B.2H.CV$CMR[[ite]][e] <- NA
  }}

B.2H.CV$CMOMd <- sd(unlist(B.2H.CV$CMOMd), na.rm = T)
B.2H.CV$CMOMi <- sd(unlist(B.2H.CV$CMOMi), na.rm = T)
B.2H.CV$CMR <- sd(unlist(B.2H.CV$CMR), na.rm = T)

# ====================== phin ======================= #

phin.2H.CV <- list()

for (ite in 1:iter){
  phin.2H.CV$CMOMd[[ite]] <- abs(output.2H[1,ite]-Original.H[1,ite])
  phin.2H.CV$CMOMi[[ite]] <- abs(output.2H[9,ite]-Original.H[1,ite])
  phin.2H.CV$CMR[[ite]]   <- abs(output.2H[17,ite]-Original.H[1,ite])}

phin.2H.CV$CMOMd <- sd(phin.2H.CV$CMOMd, na.rm = T)
phin.2H.CV$CMOMi <- sd(phin.2H.CV$CMOMi, na.rm = T)  
phin.2H.CV$CMR <- sd(phin.2H.CV$CMR, na.rm = T)

# ====================== phib ======================= #

phib.2H.CV <- list()

for (ite in 1:iter){
  phib.2H.CV$CMOMd[[ite]] <- abs(output.2H[3,ite]-Original.H[2,ite])
  phib.2H.CV$CMOMi[[ite]] <- abs(output.2H[11,ite]-Original.H[2,ite])
  phib.2H.CV$CMR[[ite]]   <- abs(output.2H[19,ite]-Original.H[2,ite])}

phib.2H.CV$CMOMd <- sd(phib.2H.CV$CMOMd, na.rm = T)
phib.2H.CV$CMOMi <- sd(phib.2H.CV$CMOMi, na.rm = T)
phib.2H.CV$CMR <- sd(phib.2H.CV$CMR, na.rm = T)

# ====================== p2 ======================= #

p2.2H.CV <- list()

for (ite in 1:iter){
  p2.2H.CV$CMOMd[[ite]] <- abs(output.2H[7,ite]-Original.H[4,ite])
  p2.2H.CV$CMOMi[[ite]] <- abs(output.2H[15,ite]-Original.H[4,ite])
  p2.2H.CV$CMR[[ite]]   <- abs(output.2H[23,ite]-Original.H[4,ite])}

p2.2H.CV$CMOMd <- sd(p2.2H.CV$CMOMd, na.rm = T)
p2.2H.CV$CMOMi <- sd(p2.2H.CV$CMOMi, na.rm = T)
p2.2H.CV$CMR <- sd(p2.2H.CV$CMR, na.rm = T)


# ====================== p1 ======================= #

p1.2H.CV <- list()

for (ite in 1:iter){
  p1.2H.CV$CMOMd[[ite]] <- abs(output.2H[5,ite]-Original.H[3,ite])
  p1.2H.CV$CMOMi[[ite]] <- abs(output.2H[13,ite]-Original.H[3,ite])
  p1.2H.CV$CMR[[ite]]   <- abs(output.2H[21,ite]-Original.H[3,ite])}

p1.2H.CV$CMOMd <- sd(p1.2H.CV$CMOMd, na.rm = T)
p1.2H.CV$CMOMi <- sd(p1.2H.CV$CMOMi, na.rm = T)
p1.2H.CV$CMR <- sd(p1.2H.CV$CMR, na.rm = T)

# ================= DATA FRAME =================

# ~~~~~ N ~~~~~
stack.N.2H <- stack(N.2H.CV)
stack.N.2H$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.N.2H)[1:2] <- c("Values", "Results")
stack.N.2H$P.factor <- factor("2H")
stack.N.2H$Parameters <- factor("Number of Individuals")
stack.N.2H$Results <- paste(stack.N.2H$Models, stack.N.2H$P.factor)

# ~~~~~ B ~~~~~
stack.B.2H <- stack(B.2H.CV)
stack.B.2H$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.B.2H)[1:2] <- c("Values", "Results")
stack.B.2H$P.factor <- factor("2H")
stack.B.2H$Parameters <- factor("Recruitment")
stack.B.2H$Results <- paste(stack.B.2H$Models, stack.B.2H$P.factor)

# ~~~~~ phin ~~~~~
stack.phin.2H <- stack(phin.2H.CV)
stack.phin.2H$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.phin.2H)[1:2] <- c("Values", "Results")
stack.phin.2H$P.factor <- factor("2H")
stack.phin.2H$Parameters <- factor("Survival Prim. Occ.")
stack.phin.2H$Results <- paste(stack.phin.2H$Models, stack.phin.2H$P.factor)

# ~~~~~ phib ~~~~~
stack.phib.2H <- stack(phib.2H.CV)
stack.phib.2H$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.phib.2H)[1:2] <- c("Values", "Results")
stack.phib.2H$P.factor <- factor("2H")
stack.phib.2H$Parameters <- factor("Survival Second. Occ.")
stack.phib.2H$Results <- paste(stack.phib.2H$Models, stack.phib.2H$P.factor)

# ~~~~~ p2 ~~~~~
stack.p2.2H <- stack(p2.2H.CV)
stack.p2.2H$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.p2.2H)[1:2] <- c("Values", "Results")
stack.p2.2H$P.factor <- factor("2H")
stack.p2.2H$Parameters <- factor("Observation Probability")
stack.p2.2H$Results <- paste(stack.p2.2H$Models, stack.p2.2H$P.factor)

# ~~~~~ p1 ~~~~~
stack.p1.2H <- stack(p1.2H.CV)
stack.p1.2H$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.p1.2H)[1:2] <- c("Values", "Results")
stack.p1.2H$P.factor <- factor("2H")
stack.p1.2H$Parameters <- factor("Observation Probability")
stack.p1.2H$Results <- paste(stack.p1.2H$Models, stack.p1.2H$P.factor)

CV.2H <- rbind(stack.N.2H, stack.B.2H, stack.phin.2H, stack.phib.2H, stack.p2.2H, stack.p1.2H)

save(CV.2H,file="C:/Users/Micheletti/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-2H-TableA.Rdata")

