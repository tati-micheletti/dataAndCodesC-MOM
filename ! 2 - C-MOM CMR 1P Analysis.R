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
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/CH.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/CI.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/CD.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/S.RData")

iter <- length(ls.CH) # Setting the number of iterations for analysis

# Creating an output matrix for 1P models for C-MOMd and C-MOMi
output.1P.CMOM <- matrix(NA,ncol=iter,nrow=20)
rownames(output.1P.CMOM) <- c("CMOMd.N","CMOMd.N.sd","CMOMd.B","CMOMd.B.sd","CMOMd.phin","CMOMd.phin.sd",
                              "CMOMd.phib","CMOMd.phib.sd","CMOMd.p2","CMOMd.p2.sd",
                              "CMOMi.N","CMOMi.N.sd","CMOMi.B","CMOMi.B.sd","CMOMi.phin","CMOMi.phin.sd",
                              "CMOMi.phib","CMOMi.phib.sd","CMOMi.p2","CMOMi.p2.sd")

# --------------------------- PREPARING VARIABLES FOR ANALYSIS --------------------------------

for(ite in 1:iter)
{
  
  # Augmenting the dataset with pseudoindividuals
  nz <- ls.S[[ite]]-nrow(ls.CH[[ite]])+3
  t.CH <- ls.CH[[ite]]
  CH <- rbind(t.CH, matrix(0, ncol = dim(t.CH)[2], nrow = nz))
  
  COUNT.i <- ls.CI[[ite]]
  COUNT.d <- ls.CD[[ite]]
  
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
  assign(paste("CMOMd.1P", ite, sep = "_"), CMOMd)
  assign(paste("CMOMi.1P", ite, sep = "_"), CMOMi)
  
####################################################################
#                                                                  #
# 2b. Creating Population Dynamics and Capture-recapture Histories #
#                                                                  #
#                        C M R              1P                     #
#                                                                  #
####################################################################

# Creating an output matrix for 1P models for C-MOMd, C-MOMi and CMR.
output.1P.CMR <- matrix(NA,ncol=iter,nrow=10)
rownames(output.1P.CMR) <- c("CMR.N","CMR.N.sd","CMR.B","CMR.B.sd","CMR.phin","CMR.phin.sd","CMR.phib",
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
assign(paste("CMR.1P", ite, sep = "_"), CMR)
  
  if(ite==iter){
    
      bugsFilter <- function(x) 
        inherits(get(x), 'bugs')
      BugsObj <- Filter(bugsFilter, ls())
      library(gtools)
      ls.CMOMd.1P <- mixedsort(BugsObj[grep("CMOMd.1P_", BugsObj)])
      ls.CMOMi.1P <- mixedsort(BugsObj[grep("CMOMi.1P_", BugsObj)])
      ls.CMR.1P <- mixedsort(BugsObj[grep("CMR.1P_", BugsObj)])
      
      for (j in 1:iter){
        output.1P.CMOM[1,j] <- mean(get(ls.CMOMd.1P[[j]])[[10]][11:19,"mean"])
        output.1P.CMOM[2,j] <- mean(get(ls.CMOMd.1P[[j]])[[10]][11:19,"sd"])
        output.1P.CMOM[3,j] <- mean(get(ls.CMOMd.1P[[j]])[[10]][20:21,"mean"])
        output.1P.CMOM[4,j] <- mean(get(ls.CMOMd.1P[[j]])[[10]][20:21,"sd"])
        output.1P.CMOM[5,j] <- mean(c(get(ls.CMOMd.1P[[j]])[[10]][1:2,"mean"],get(ls.CMOMd.1P[[j]])[[10]][4:5,"mean"],get(ls.CMOMd.1P[[j]])[[10]][7:8,"mean"])) 
        output.1P.CMOM[6,j] <- mean(c(get(ls.CMOMd.1P[[j]])[[10]][1:2,"sd"],get(ls.CMOMd.1P[[j]])[[10]][4:5,"sd"],get(ls.CMOMd.1P[[j]])[[10]][7:8,"sd"]))
        output.1P.CMOM[7,j] <- mean(c(get(ls.CMOMd.1P[[j]])[[10]][3,"mean"],get(ls.CMOMd.1P[[j]])[[10]][6,"mean"])) 
        output.1P.CMOM[8,j] <- mean(c(get(ls.CMOMd.1P[[j]])[[10]][3,"sd"],get(ls.CMOMd.1P[[j]])[[10]][6,"sd"]))
        output.1P.CMOM[9,j] <- get(ls.CMOMd.1P[[j]])[[10]][9,"mean"]
        output.1P.CMOM[10,j] <- get(ls.CMOMd.1P[[j]])[[10]][9,"sd"]
        
        output.1P.CMOM[11,j] <- mean(get(ls.CMOMi.1P[[j]])[[10]][11:19,"mean"])
        output.1P.CMOM[12,j] <- mean(get(ls.CMOMi.1P[[j]])[[10]][11:19,"sd"])
        output.1P.CMOM[13,j] <- mean(get(ls.CMOMi.1P[[j]])[[10]][20:21,"mean"])
        output.1P.CMOM[14,j] <- mean(get(ls.CMOMi.1P[[j]])[[10]][20:21,"sd"])
        output.1P.CMOM[15,j] <- mean(c(get(ls.CMOMi.1P[[j]])[[10]][1:2,"mean"],get(ls.CMOMi.1P[[j]])[[10]][4:5,"mean"],get(ls.CMOMi.1P[[j]])[[10]][7:8,"mean"])) 
        output.1P.CMOM[16,j] <- mean(c(get(ls.CMOMi.1P[[j]])[[10]][1:2,"sd"],get(ls.CMOMi.1P[[j]])[[10]][4:5,"sd"],get(ls.CMOMi.1P[[j]])[[10]][7:8,"sd"]))
        output.1P.CMOM[17,j] <- mean(c(get(ls.CMOMi.1P[[j]])[[10]][3,"mean"],get(ls.CMOMi.1P[[j]])[[10]][6,"mean"]))
        output.1P.CMOM[18,j] <- mean(c(get(ls.CMOMi.1P[[j]])[[10]][3,"sd"],get(ls.CMOMi.1P[[j]])[[10]][6,"sd"]))
        output.1P.CMOM[19,j] <- get(ls.CMOMi.1P[[j]])[[10]][9,"mean"]
        output.1P.CMOM[20,j] <- get(ls.CMOMi.1P[[j]])[[10]][9,"sd"]
        
        output.1P.CMR[1,j] <- mean(get(ls.CMR.1P[[j]])[[10]][11:19,"mean"])
        output.1P.CMR[2,j] <- mean(get(ls.CMR.1P[[j]])[[10]][11:19,"sd"])
        output.1P.CMR[3,j] <- mean(get(ls.CMR.1P[[j]])[[10]][20:21,"mean"])
        output.1P.CMR[4,j] <- mean(get(ls.CMR.1P[[j]])[[10]][20:21,"sd"])
        output.1P.CMR[5,j] <- mean(c(get(ls.CMR.1P[[j]])[[10]][1:2,"mean"],get(ls.CMR.1P[[j]])[[10]][4:5,"mean"],get(ls.CMR.1P[[j]])[[10]][7:8,"mean"])) 
        output.1P.CMR[6,j] <- mean(c(get(ls.CMR.1P[[j]])[[10]][1:2,"sd"],get(ls.CMR.1P[[j]])[[10]][4:5,"sd"],get(ls.CMR.1P[[j]])[[10]][7:8,"sd"]))
        output.1P.CMR[7,j] <- mean(c(get(ls.CMR.1P[[j]])[[10]][3,"mean"],get(ls.CMR.1P[[j]])[[10]][6,"mean"])) 
        output.1P.CMR[8,j] <- mean(c(get(ls.CMR.1P[[j]])[[10]][3,"sd"],get(ls.CMR.1P[[j]])[[10]][6,"sd"]))
        output.1P.CMR[9,j] <- get(ls.CMR.1P[[j]])[[10]][9,"mean"]
        output.1P.CMR[10,j] <- get(ls.CMR.1P[[j]])[[10]][9,"sd"]}
      
    save(output.1P.CMOM,file="output-1P.RData")
    save(output.1P.CMR,file="output-1P-CMR.RData")
  }}

save.image(file="1P_Backup.RData")

end.time <- Sys.time()
time.it <- end.time - start.time
time.it