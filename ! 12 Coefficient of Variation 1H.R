#####################################################################
#                                                                   #
#         12a.    C O E F I C I E N T   O F   V A R I A T I O N     #
#                                                                   #
#                           1  H                                    #
#                                                                   #
#####################################################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-1H.Rdata")

# Convert NULL into NA
for (ite in 1:iter){
  if (is.null(HN.ZPNEc[[ite]]))
    HN.ZPNEc[[ite]] <- rep(NA, 3)}

# ====================== N ======================= #

N.1H.CV <- list()

for (ite in 1:iter){
  N.1H.CV$CMOMd[[ite]] <- log(N.1H$CMOMd[[ite]])-log(ls.H.N[[ite]])
  N.1H.CV$CMOMi[[ite]] <- log(N.1H$CMOMi[[ite]])-log(ls.H.N[[ite]])
  N.1H.CV$CMR[[ite]]   <- log(N.1H$CMR[[ite]])-log(ls.H.N[[ite]])
  N.1H.CV$ZPNEc[[ite]] <- log(HN.ZPNEc[[ite]])-log(ls.N.ZPNE[[ite]])
  N.1H.CV$ZPNEs[[ite]] <- log(HN.ZPNEs[[ite]])-log(ls.N.ZPNE[[ite]])}

for (ite in 1:iter){
  N.1H.CV$ZPNEc[[ite]] <- log(HN.ZPNEc[[ite]])-log(ls.N.ZPNE[[ite]])}


N.1H.CV$CMOMd <- sd(unlist(N.1H.CV$CMOMd), na.rm = T)
N.1H.CV$CMOMi <- sd(unlist(N.1H.CV$CMOMi), na.rm = T)  
N.1H.CV$CMR <- sd(unlist(N.1H.CV$CMR), na.rm = T)  
N.1H.CV$ZPNEc <- sd(unlist(N.1H.CV$ZPNEc), na.rm = T)  
N.1H.CV$ZPNEs <- sd(unlist(N.1H.CV$ZPNEs), na.rm = T)

# ====================== B ======================= #
B.1H.CV <- list()

for (ite in 1:iter){
  B.1H.CV$CMOMd[[ite]] <- log(B.1H$CMOMd[[ite]])-log(ls.H.B[[ite]])
  B.1H.CV$CMOMi[[ite]] <- log(B.1H$CMOMi[[ite]])-log(ls.H.B[[ite]])
  B.1H.CV$CMR[[ite]]   <- log(B.1H$CMR[[ite]])-log(ls.H.B[[ite]])}

for (ite in 1:iter){
  for (e in 1:2){
    if (is.infinite(B.1H.CV$CMOMd[[ite]][e]))
      B.1H.CV$CMOMd[[ite]][e] <- NA
    if (is.infinite(B.1H.CV$CMOMi[[ite]][e]))
      B.1H.CV$CMOMi[[ite]][e] <- NA
    if (is.infinite(B.1H.CV$CMR[[ite]][e]))
      B.1H.CV$CMR[[ite]][e] <- NA
  }}

B.1H.CV$CMOMd <- sd(unlist(B.1H.CV$CMOMd), na.rm = T)
B.1H.CV$CMOMi <- sd(unlist(B.1H.CV$CMOMi), na.rm = T)
B.1H.CV$CMR <- sd(unlist(B.1H.CV$CMR), na.rm = T)

# ====================== phin ======================= #

phin.1H.CV <- list()

for (ite in 1:iter){
  phin.1H.CV$CMOMd[[ite]] <- output.1H[1,ite]-Original.H[1,ite]
  phin.1H.CV$CMOMi[[ite]] <- output.1H[7,ite]-Original.H[1,ite]
  phin.1H.CV$CMR[[ite]]   <- output.1H[13,ite]-Original.H[1,ite]}

phin.1H.CV$CMOMd <- sd(phin.1H.CV$CMOMd, na.rm = T)
phin.1H.CV$CMOMi <- sd(phin.1H.CV$CMOMi, na.rm = T)  
phin.1H.CV$CMR <- sd(phin.1H.CV$CMR, na.rm = T)

# ====================== phib ======================= #

phib.1H.CV <- list()

for (ite in 1:iter){
  phib.1H.CV$CMOMd[[ite]] <- output.1H[3,ite]-Original.H[2,ite]
  phib.1H.CV$CMOMi[[ite]] <- output.1H[9,ite]-Original.H[2,ite]
  phib.1H.CV$CMR[[ite]]   <- output.1H[15,ite]-Original.H[2,ite]
  phib.1H.CV$ZPNEc[[ite]] <- output.ZPNEc[1,ite]-Original.H[2,ite]
  phib.1H.CV$ZPNEs[[ite]]   <- output.ZPNEs[1,ite]-Original.H[2,ite]}

phib.1H.CV$CMOMd <- sd(phib.1H.CV$CMOMd, na.rm = T)
phib.1H.CV$CMOMi <- sd(phib.1H.CV$CMOMi, na.rm = T)
phib.1H.CV$CMR <- sd(phib.1H.CV$CMR, na.rm = T)
phib.1H.CV$ZPNEc <- sd(phib.1H.CV$ZPNEc, na.rm = T)
phib.1H.CV$ZPNEs <- sd(phib.1H.CV$ZPNEs, na.rm = T)

# ====================== p2 ======================= #

p2.1H.CV <- list()

for (ite in 1:iter){
  p2.1H.CV$CMOMd[[ite]] <- output.1H[5,ite]-Original.H[4,ite]
  p2.1H.CV$CMOMi[[ite]] <- output.1H[11,ite]-Original.H[4,ite]
  p2.1H.CV$CMR[[ite]]   <- output.1H[17,ite]-Original.H[4,ite]
  p2.1H.CV$ZPNEc[[ite]] <- output.ZPNEc[3,ite]-Original.H[4,ite]
  p2.1H.CV$ZPNEs[[ite]]   <- output.ZPNEs[3,ite]-Original.H[4,ite]}

for (i in 1:length(p2.1H.CV$ZPNEs)){
  if (p2.1H.CV$ZPNEs[i]>100|p2.1H.CV$ZPNEs[i]<(-100))
    p2.1H.CV$ZPNEs[i] <- NA
}

p2.1H.CV$CMOMd <- sd(p2.1H.CV$CMOMd, na.rm = T)
p2.1H.CV$CMOMi <- sd(p2.1H.CV$CMOMi, na.rm = T)
p2.1H.CV$CMR <- sd(p2.1H.CV$CMR, na.rm = T)
p2.1H.CV$ZPNEc <- sd(p2.1H.CV$ZPNEc, na.rm = T)
p2.1H.CV$ZPNEs <- sd(p2.1H.CV$ZPNEs, na.rm = T)

# ================= DATA FRAME =================

# ~~~~~ N ~~~~~
stack.N.1H <- stack(N.1H.CV)
stack.N.1H$Models <- factor(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"))
colnames(stack.N.1H)[1:2] <- c("Values", "Results")
stack.N.1H$P.factor <- factor("1H")
stack.N.1H$Parameters <- factor("Number of Individuals")
stack.N.1H$Results <- paste(stack.N.1H$Models, stack.N.1H$P.factor)
for (i in 1:nrow(stack.N.1H)){
  if (stack.N.1H$Results[i]=="ZPNEc 1H") stack.N.1H$Results[i] <- "ZPNEc"
  if (stack.N.1H$Results[i]=="ZPNEs 1H") stack.N.1H$Results[i] <- "ZPNEs"
}

# ~~~~~ B ~~~~~
stack.B.1H <- stack(B.1H.CV)
stack.B.1H$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.B.1H)[1:2] <- c("Values", "Results")
stack.B.1H$P.factor <- factor("1H")
stack.B.1H$Parameters <- factor("Recruitment")
stack.B.1H$Results <- paste(stack.B.1H$Models, stack.B.1H$P.factor)

# ~~~~~ phin ~~~~~
stack.phin.1H <- stack(phin.1H.CV)
stack.phin.1H$Models <- factor(c("CMOMd","CMOMi","CMR"))
colnames(stack.phin.1H)[1:2] <- c("Values", "Results")
stack.phin.1H$P.factor <- factor("1H")
stack.phin.1H$Parameters <- factor("Survival Prim. Occ.")
stack.phin.1H$Results <- paste(stack.phin.1H$Models, stack.phin.1H$P.factor)

# ~~~~~ phib ~~~~~
stack.phib.1H <- stack(phib.1H.CV)
stack.phib.1H$Models <- factor(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"))
colnames(stack.phib.1H)[1:2] <- c("Values", "Results")
stack.phib.1H$P.factor <- factor("1H")
stack.phib.1H$Parameters <- factor("Survival Second. Occ.")
stack.phib.1H$Results <- paste(stack.phib.1H$Models, stack.phib.1H$P.factor)
for (i in 1:nrow(stack.phib.1H)){
  if (stack.phib.1H$Results[i]=="ZPNEc 1H") stack.phib.1H$Results[i] <- "ZPNEc"
  if (stack.phib.1H$Results[i]=="ZPNEs 1H") stack.phib.1H$Results[i] <- "ZPNEs"
}

# ~~~~~ p2 ~~~~~
stack.p2.1H <- stack(p2.1H.CV)
stack.p2.1H$Models <- factor(c("CMOMd","CMOMi","CMR","ZPNEc","ZPNEs"))
colnames(stack.p2.1H)[1:2] <- c("Values", "Results")
stack.p2.1H$P.factor <- factor("1H")
stack.p2.1H$Parameters <- factor("Observation Probability")
stack.p2.1H$Results <- paste(stack.p2.1H$Models, stack.p2.1H$P.factor)
for (i in 1:nrow(stack.p2.1H)){
  if (stack.p2.1H$Results[i]=="ZPNEc 1H") stack.p2.1H$Results[i] <- "ZPNEc"
  if (stack.p2.1H$Results[i]=="ZPNEs 1H") stack.p2.1H$Results[i] <- "ZPNEs"
}

CV.1H <- rbind(stack.N.1H, stack.B.1H, stack.phin.1H, stack.phib.1H, stack.p2.1H)

save(CV.1H,file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-1H-Table.Rdata")

