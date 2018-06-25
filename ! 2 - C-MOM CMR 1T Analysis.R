####################################################################
#                                                                  #
# 2a. Creating Population Dynamics and Capture-recapture Histories #
#                                                                  #
#                     C - M O M        1P                          #
#                                                                  #
####################################################################

start.time <- Sys.time()

# Setting working directory
setwd("C:/Users/Tati/Dropbox/MODELS OUTPUT")
bugs.dir <- "C:/Program Files (x86)/WinBUGS14"

# Loading Simulated Capture and Counting Histories 
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/CH.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/CI.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/CD.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/S.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS/N-each.RData")

iter <- length(ls.T.CH) # Setting the number of iterations for analysis

# Creating an output matrix for 1P models for C-MOMd and C-MOMi
output.1T.CMOM <- matrix(NA,ncol=iter,nrow=20)
rownames(output.1T.CMOM) <- c("CMOMd.N","CMOMd.N.sd","CMOMd.B","CMOMd.B.sd","CMOMd.phin","CMOMd.phin.sd",
                              "CMOMd.phib","CMOMd.phib.sd","CMOMd.p2","CMOMd.p2.sd",
                              "CMOMi.N","CMOMi.N.sd","CMOMi.B","CMOMi.B.sd","CMOMi.phin","CMOMi.phin.sd",
                              "CMOMi.phib","CMOMi.phib.sd","CMOMi.p2","CMOMi.p2.sd")

output.TN <- matrix(NA,ncol=iter,nrow=36)
rownames(output.TN) <- c("N1","N2","N3","N4","N5","N6","N7","N8","N9",
                         "CMOMd.N1","CMOMd.N2","CMOMd.N3","CMOMd.N4","CMOMd.N5","CMOMd.N6","CMOMd.N7","CMOMd.N8","CMOMd.N9",
                         "CMOMi.N1","CMOMi.N2","CMOMi.N3","CMOMi.N4","CMOMi.N5","CMOMi.N6","CMOMi.N7","CMOMi.N8","CMOMi.N9",
                         "CMR.N1","CMR.N2","CMR.N3","CMR.N4","CMR.N5","CMR.N6","CMR.N7","CMR.N8","CMR.N9")
# --------------------------- PREPARING VARIABLES FOR ANALYSIS --------------------------------

for(ite in 1:iter)
{
  
  # Augmenting the dataset with pseudoindividuals
  nz <- ls.T.S[[ite]]-nrow(ls.T.CH[[ite]])+3
  t.CH <- ls.T.CH[[ite]]
  CH <- rbind(t.CH, matrix(0, ncol = dim(t.CH)[2], nrow = nz))
  
  COUNT.i <- ls.T.CI[[ite]]
  COUNT.d <- ls.T.CD[[ite]]
  
  ############################################
  #   Preparing information for the models   # 
  ############################################  
  
  # This is done as the model is robust and have different assumptions
  #for primary (field trips) and secondary (days of observation) sample occasions 
  
  n.occ <- 3  # Number of capture occasions (field trips)
  n.days <- 3  # Number of days of work in each occasion
  tot.occ <- n.occ*n.days # Total occasions of capture-recapture
  # Identification of t for first day of the next field trip: reproduction only occur between field trips
  ifelse (n.occ > 1, {
    btw.occ <- c(seq(1:(tot.occ-1)))
    btw.occ <- (btw.occ[seq(n.days, length(btw.occ), n.days)])+1  
  },{
    btw.occ <- 0
  })
  
  #Preparing gamma (entry probability of an available individual)
  gamma.y <- c(rep(NA, tot.occ))
  for (t in 1:tot.occ){
    ifelse (t==1 || t %in% btw.occ,{
      gamma.y[t] <- NA
    },{
      gamma.y[t] <- 0
    })}
  
  # Create IPM init for gamma, where gamma.info is NA, put number of random probability.
  gamma.init <- gamma.y
  
  for (i in 1:length(gamma.y)){
    if (is.na(gamma.init[i])) gamma.init[i] <- runif(1, 0, 1)
    if (gamma.init[i]==0) gamma.init[i] <- NA}
  
  # Create a matrix with information about known latent state z
  CH.z <- CH
  CH.z[(nrow(t.CH)+1):nrow(CH.z), ncol(CH.z)] <- rep(1, (nrow(CH.z)-nrow(t.CH)))
  
  known.state <- function(ch){
    state <- ch
    for (i in 1:nrow(ch)){
      n1 <- min(which(ch[i,]==1))
      n2 <- max(which(ch[i,]==1))
      state[i,n1:n2] <- 1
      if ((min(which(ch[i,]==1)))==ncol(ch)) state[i, ncol(ch)] <- NA 
    }
    state[state==0] <- NA
    
    return(state)
  }
  CH.z <- known.state(CH.z)
  
  # Create an init for z, where CH.z is NA, put "0".
  CH.i <- CH.z
  for (i in 1:nrow(CH.i)){
    for (k in 1:ncol(CH.i)){
      if (is.na(CH.i[i,k])) CH.i[i,k] <- 0
      if (CH.i[i,k]==1) CH.i[i,k] <- NA
    }}
  
  #Preparing B information
  B.info <- c(rep(NA, tot.occ))
  for (t in 1:tot.occ){
    ifelse (t %in% btw.occ,{
      B.info[t] <- NA 
    },{
      B.info[t] <- 0
    })}
  
  # Create IPM init for B, where B.info is NA, put number of random number between 0 & counts.
  B.init <- B.info
  
  for (i in 1:length(B.info)){
    if (is.na(B.init[i])) B.init[i] <- runif(1, 0, COUNT.i[i])
    if (B.init[i]==0) B.init[i] <- NA}
  B.init <- round(B.init)
  
  
  #-------------------------------------------
  
  
  ###########################
  #    DATA     ANALYSIS    #
  ###########################
  
  # Loading required libraries
  library(R2WinBUGS)
  
  # INPUT DATA
  
  # Input data for IPM model
  # CH =  capture-recapture data constant
  # COUNT = number of individuals observed in a given occasion
  
  # OTHER VARIABLES
  # M = Number of individuals + pseudoindividuals (augmented capture history data set)
  # z = Known fate of individuals resighted after periods without observation
  # B = Newborn entree is only possible in primary sampling periods
  # gamma = Entry probability is only possible for primary sample periods
  # tot.occ = Total sampling occasions (field trips x sampling days in each)
  
  # Parameters monitored
  parameters <- c("phi", "p", "S", "N", "B")
  
  ############
  # RUN MCMC #
  ############
  
  # MCMC setings
  ni <- 10000
  nt <- 4
  nb <- 3000
  nc <- 3
  
  #C-MOM
  # Bundle data
  bugs.data.dep <- list(CH = CH, COUNT = COUNT.d, tot.occ = dim(CH)[2], M = dim(CH)[1], gamma = gamma.y, z = CH.z, B = B.info)
  
  bugs.data.ind <- list(CH = CH, COUNT = COUNT.i, tot.occ = dim(CH)[2], M = dim(CH)[1], gamma = gamma.y, z = CH.z, B = B.info)
  
  #Initial values for parameters
  inits <- function(){list(phi = rep(runif(1,0,1), tot.occ-1), p = runif(1,0,1), B = B.init, z = CH.i, gamma = gamma.init)}
  
  CMOMi <- bugs(data = bugs.data.ind, inits = inits, parameters.to.save = parameters, n.iter = ni, model.file="IPM1P.bug", n.chains = nc, n.thin = nt, n.burnin = nb, debug = F, bugs.directory = bugs.dir, working.directory = getwd(), clearWD = T)
  
  CMOMd <- bugs(data = bugs.data.dep, inits = inits, parameters.to.save = parameters, n.iter = ni, model.file="IPM1P.bug", n.chains = nc, n.thin = nt, n.burnin = nb, debug = F, bugs.directory = bugs.dir, working.directory = getwd(), clearWD = T)
  
  #######################
  # SAVE MODEL OUTPUTS  #
  #######################
  
  # Store each output from MCMC
  assign(paste("CMOMd.1T", ite, sep = "_"), CMOMd)
  assign(paste("CMOMi.1T", ite, sep = "_"), CMOMi)
  
  ####################################################################
  #                                                                  #
  # 2b. Creating Population Dynamics and Capture-recapture Histories #
  #                                                                  #
  #                        C M R              1P                     #
  #                                                                  #
  ####################################################################
  
  # Creating an output matrix for 1P models for C-MOMd, C-MOMi and CMR.
  output.1T.CMR <- matrix(NA,ncol=iter,nrow=10)
  rownames(output.1T.CMR) <- c("CMR.N","CMR.N.sd","CMR.B","CMR.B.sd","CMR.phin","CMR.phin.sd","CMR.phib",
                               "CMR.phib.sd","CMR.p2","CMR.p2.sd")
  
  ###########################
  #    DATA     ANALYSIS    #
  ###########################
  
  # CMR
  # Bundle data
  bugs.data <- list(CH = CH, tot.occ = dim(CH)[2], M = dim(CH)[1], gamma = gamma.y, z = CH.z)
  
  #Initial values for parameters
  inits <- function(){list(phi = rep(runif(1,0,1), tot.occ-1), p = runif(1,0,1), z = CH.i, gamma = gamma.init)}
  
  CMR <- bugs(data = bugs.data, inits = inits, parameters.to.save = parameters, n.iter = ni, model.file="CMR1P.bug", n.chains = nc, n.thin = nt, n.burnin = nb, debug = F, bugs.directory = bugs.dir, working.directory = getwd(), clearWD = T)
  
  
  #######################
  # SAVE MODEL OUTPUTS  #
  #######################
  
  # Store each output from MCMC
  assign(paste("CMR.1T", ite, sep = "_"), CMR)
  
  if(ite==iter){
    
    bugsFilter <- function(x) 
      inherits(get(x), 'bugs')
    BugsObj <- Filter(bugsFilter, ls())
    library(gtools)
    ls.CMOMd.1T <- mixedsort(BugsObj[grep("CMOMd.1T_", BugsObj)])
    ls.CMOMi.1T <- mixedsort(BugsObj[grep("CMOMi.1T_", BugsObj)])
    ls.CMR.1T <- mixedsort(BugsObj[grep("CMR.1T_", BugsObj)])
    
    for (j in 1:iter){
      output.1T.CMOM[1,j] <- mean(get(ls.CMOMd.1T[[j]])[[10]][11:19,"mean"])
      output.1T.CMOM[2,j] <- mean(get(ls.CMOMd.1T[[j]])[[10]][11:19,"sd"])
      output.1T.CMOM[3,j] <- mean(get(ls.CMOMd.1T[[j]])[[10]][20:21,"mean"])
      output.1T.CMOM[4,j] <- mean(get(ls.CMOMd.1T[[j]])[[10]][20:21,"sd"])
      output.1T.CMOM[5,j] <- mean(c(get(ls.CMOMd.1T[[j]])[[10]][1:2,"mean"],get(ls.CMOMd.1T[[j]])[[10]][4:5,"mean"],get(ls.CMOMd.1T[[j]])[[10]][7:8,"mean"])) 
      output.1T.CMOM[6,j] <- mean(c(get(ls.CMOMd.1T[[j]])[[10]][1:2,"sd"],get(ls.CMOMd.1T[[j]])[[10]][4:5,"sd"],get(ls.CMOMd.1T[[j]])[[10]][7:8,"sd"]))
      output.1T.CMOM[7,j] <- mean(c(get(ls.CMOMd.1T[[j]])[[10]][3,"mean"],get(ls.CMOMd.1T[[j]])[[10]][6,"mean"])) 
      output.1T.CMOM[8,j] <- mean(c(get(ls.CMOMd.1T[[j]])[[10]][3,"sd"],get(ls.CMOMd.1T[[j]])[[10]][6,"sd"]))
      output.1T.CMOM[9,j] <- get(ls.CMOMd.1T[[j]])[[10]][9,"mean"]
      output.1T.CMOM[10,j] <- get(ls.CMOMd.1T[[j]])[[10]][9,"sd"]
      
      output.1T.CMOM[11,j] <- mean(get(ls.CMOMi.1T[[j]])[[10]][11:19,"mean"])
      output.1T.CMOM[12,j] <- mean(get(ls.CMOMi.1T[[j]])[[10]][11:19,"sd"])
      output.1T.CMOM[13,j] <- mean(get(ls.CMOMi.1T[[j]])[[10]][20:21,"mean"])
      output.1T.CMOM[14,j] <- mean(get(ls.CMOMi.1T[[j]])[[10]][20:21,"sd"])
      output.1T.CMOM[15,j] <- mean(c(get(ls.CMOMi.1T[[j]])[[10]][1:2,"mean"],get(ls.CMOMi.1T[[j]])[[10]][4:5,"mean"],get(ls.CMOMi.1T[[j]])[[10]][7:8,"mean"])) 
      output.1T.CMOM[16,j] <- mean(c(get(ls.CMOMi.1T[[j]])[[10]][1:2,"sd"],get(ls.CMOMi.1T[[j]])[[10]][4:5,"sd"],get(ls.CMOMi.1T[[j]])[[10]][7:8,"sd"]))
      output.1T.CMOM[17,j] <- mean(c(get(ls.CMOMi.1T[[j]])[[10]][3,"mean"],get(ls.CMOMi.1T[[j]])[[10]][6,"mean"]))
      output.1T.CMOM[18,j] <- mean(c(get(ls.CMOMi.1T[[j]])[[10]][3,"sd"],get(ls.CMOMi.1T[[j]])[[10]][6,"sd"]))
      output.1T.CMOM[19,j] <- get(ls.CMOMi.1T[[j]])[[10]][9,"mean"]
      output.1T.CMOM[20,j] <- get(ls.CMOMi.1T[[j]])[[10]][9,"sd"]
      
      output.1T.CMR[1,j] <- mean(get(ls.CMR.1T[[j]])[[10]][11:19,"mean"])
      output.1T.CMR[2,j] <- mean(get(ls.CMR.1T[[j]])[[10]][11:19,"sd"])
      output.1T.CMR[3,j] <- mean(get(ls.CMR.1T[[j]])[[10]][20:21,"mean"])
      output.1T.CMR[4,j] <- mean(get(ls.CMR.1T[[j]])[[10]][20:21,"sd"])
      output.1T.CMR[5,j] <- mean(c(get(ls.CMR.1T[[j]])[[10]][1:2,"mean"],get(ls.CMR.1T[[j]])[[10]][4:5,"mean"],get(ls.CMR.1T[[j]])[[10]][7:8,"mean"])) 
      output.1T.CMR[6,j] <- mean(c(get(ls.CMR.1T[[j]])[[10]][1:2,"sd"],get(ls.CMR.1T[[j]])[[10]][4:5,"sd"],get(ls.CMR.1T[[j]])[[10]][7:8,"sd"]))
      output.1T.CMR[7,j] <- mean(c(get(ls.CMR.1T[[j]])[[10]][3,"mean"],get(ls.CMR.1T[[j]])[[10]][6,"mean"])) 
      output.1T.CMR[8,j] <- mean(c(get(ls.CMR.1T[[j]])[[10]][3,"sd"],get(ls.CMR.1T[[j]])[[10]][6,"sd"]))
      output.1T.CMR[9,j] <- get(ls.CMR.1T[[j]])[[10]][9,"mean"]
      output.1T.CMR[10,j] <- get(ls.CMR.1T[[j]])[[10]][9,"sd"]
      
      output.TN[10,j] <- get(ls.CMOMd.1T[[j]])[[10]][11,"mean"]
      output.TN[11,j] <- get(ls.CMOMd.1T[[j]])[[10]][12,"mean"]
      output.TN[12,j] <- get(ls.CMOMd.1T[[j]])[[10]][13,"mean"]
      output.TN[13,j] <- get(ls.CMOMd.1T[[j]])[[10]][14,"mean"]
      output.TN[14,j] <- get(ls.CMOMd.1T[[j]])[[10]][15,"mean"]
      output.TN[15,j] <- get(ls.CMOMd.1T[[j]])[[10]][16,"mean"]
      output.TN[16,j] <- get(ls.CMOMd.1T[[j]])[[10]][17,"mean"]
      output.TN[17,j] <- get(ls.CMOMd.1T[[j]])[[10]][18,"mean"]
      output.TN[18,j] <- get(ls.CMOMd.1T[[j]])[[10]][19,"mean"]
      output.TN[19,j] <- get(ls.CMOMi.1T[[j]])[[10]][11,"mean"]
      output.TN[20,j] <- get(ls.CMOMi.1T[[j]])[[10]][12,"mean"]
      output.TN[21,j] <- get(ls.CMOMi.1T[[j]])[[10]][13,"mean"]
      output.TN[22,j] <- get(ls.CMOMi.1T[[j]])[[10]][14,"mean"]
      output.TN[23,j] <- get(ls.CMOMi.1T[[j]])[[10]][15,"mean"]
      output.TN[24,j] <- get(ls.CMOMi.1T[[j]])[[10]][16,"mean"]
      output.TN[25,j] <- get(ls.CMOMi.1T[[j]])[[10]][17,"mean"]
      output.TN[26,j] <- get(ls.CMOMi.1T[[j]])[[10]][18,"mean"]
      output.TN[27,j] <- get(ls.CMOMi.1T[[j]])[[10]][19,"mean"]
      output.TN[28,j] <- get(ls.CMR.1T[[j]])[[10]][11,"mean"]
      output.TN[29,j] <- get(ls.CMR.1T[[j]])[[10]][12,"mean"]
      output.TN[30,j] <- get(ls.CMR.1T[[j]])[[10]][13,"mean"]
      output.TN[31,j] <- get(ls.CMR.1T[[j]])[[10]][14,"mean"]
      output.TN[32,j] <- get(ls.CMR.1T[[j]])[[10]][15,"mean"]
      output.TN[33,j] <- get(ls.CMR.1T[[j]])[[10]][16,"mean"]
      output.TN[34,j] <- get(ls.CMR.1T[[j]])[[10]][17,"mean"]
      output.TN[35,j] <- get(ls.CMR.1T[[j]])[[10]][18,"mean"]
      output.TN[36,j] <- get(ls.CMR.1T[[j]])[[10]][19,"mean"]}
    
    save(output.1T.CMOM,file="output-1T.RData")
    save(output.1T.CMR,file="output-1T-CMR.RData")
  }}

# True values N
for (ite in 1:iter){
output.TN[1,ite]<- ls.T.N[[ite]][1]     # Number of Individuals
output.TN[2,ite]<- ls.T.N[[ite]][2]    
output.TN[3,ite]<- ls.T.N[[ite]][3]
output.TN[4,ite]<- ls.T.N[[ite]][4]
output.TN[5,ite]<- ls.T.N[[ite]][5]
output.TN[6,ite]<- ls.T.N[[ite]][6]
output.TN[7,ite]<- ls.T.N[[ite]][7]   
output.TN[8,ite]<- ls.T.N[[ite]][8]
output.TN[9,ite]<- ls.T.N[[ite]][9]}

save(output.TN,file="TN.Rdata")
save.image(file="1T_Backup.RData")

end.time <- Sys.time()
time.it <- end.time - start.time
time.it