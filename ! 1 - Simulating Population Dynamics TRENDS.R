###################################################################
#                                                                 #
# 1. Creating Population Dynamics and Capture-recapture Histories #
#                                                                 #
#               T R E N D S                                       #
#                                                                 #
###################################################################

# Setting working directory
setwd("C:/Users/Micheletti/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/TRENDS")

# -------------- SETTING NEEDED VARIABLES -----------------------------#

n.occ <- 3  # Number of capture occasions (field trips)
n.days <- 3  # Number of days of work in each occasion

# Initial number of individuals in the colony
N.ini <- 25

tot.occ <- n.occ*n.days # Total occasions of capture-recapture

pm <- rep(0.2, tot.occ)   # Probability of being caught for marking
pr <- rep(0.6, tot.occ)   #Probability of seeing an animal

# Sex-ratio between females and males (OBS. Not necessarily what is born, but the sex ratio for adults) 
sex.ratio <- 0.67

# Cubs probability
pr.cubs <- c(0.95,0.049,0.001) # Probability of the females to have single, twins, triplets, etc.

it <- 100 # Number of iterations for each analysis
anal <- 2 # Decreasing and Increasing Populations

# Total Number of iterations
iter <- it * anal

# To save the outputs
output.Real.T <- matrix(NA,ncol=iter,nrow=5)
rownames(output.Real.T) <- c("N","B","phin","phib","p")

ls.T.CH <- list()
ls.T.CI <- list()
ls.T.CD <- list()
ls.T.p1.CH <- list()
ls.T.Nf <- list()
ls.T.Nm <- list()
ls.T.N <- list()
ls.T.S <- list()
ls.T.Bf <- list()
ls.T.Bm <- list()

# -------------- START OF POPULATION DYNAMICS SIMULATION -----------------------------#

set.seed(1983)
for(ite in 1:iter){
  tryCatch({
    if (ite <= it) { # Decreasing Population Trend
      phi.occ <- 0.7 # Survival from one occasion to the next
      phi.days <- 0.85 # Survival from one day in the same occasion to the next
      rep.fem <- 0.35 # Female reproduction probability
    }   
    
    if (ite > it) {# Increasing Population Trend
      phi.occ <- 0.85 # Survival from one occasion to the next
      phi.days <- 0.99 # Survival from one day in the same occasion to the next
      rep.fem <- 0.9 # Female reproduction probability
    } 
    
    # Identification of t for first day of the next field trip: reproduction only occur between field trips
    ifelse (n.occ > 1, {
      btw.occ <- c(seq(1:(tot.occ-1)))
      btw.occ <- (btw.occ[seq(n.days, length(btw.occ), n.days)])+1  
    },{
      btw.occ <- 0
    })
    
    #Survival probability vector (for all days+occ)
    ifelse (n.occ > 1,
            {ifelse (n.days > 1,
                     {phi <- c(rep(phi.days, tot.occ-1))
                     for (j in 1:tot.occ-1){
                       ifelse (j %in% btw.occ, phi[j-1] <- phi.occ, phi[j-1] <- phi[j-1])}
                     phi[1] <- phi.days}, {
                       phi <- rep(phi.occ, n.occ-1)
                     })},{
                       phi <- rep(phi.days, n.days-1)  
                     })
    
    # To calculate the number of individuals entering the population in each occasion, 
    # we need to make a Bernoulli trial for the females to see if they reproduced or not 
    # in each period of 3 months. 1:3 male/female rate. Cubs probability = 1:95%, 2:4.9%, 3:0.1%). 
    # Individuals that just entered population do not reproduce. Maximum number of broods per field trip = 1.
    
    n.fem <- round(N.ini*sex.ratio) # Total number of females in the population time = 0
    n.male <- round(N.ini-n.fem) # Total number of males in the population at time = 0
    n.cubs <- c(seq(1:length(pr.cubs))) # Total number of possible cubs
    
    # Number of entries based on rock cavy reproduction biology
    fem.ad <- male.ad <- fem.juv <- fem.cubs <- male.cubs <- fem.rep <- fem.all <- male.all <- numeric(tot.occ) 
    fem.ad[1] <- round(n.fem[1]*0.6)
    fem.juv[1] <- n.fem-fem.ad[1]
    male.ad[1] <- n.male
    cubs <- sum.cubs <- fem.cubs <- male.cubs <-  tot.entries <- B.f <- B.m <- numeric(tot.occ)
    
    
    ##################################
    # SURVIVAL AND REPRODUCTION LOOP #
    ##################################
    
    
    for   (t in 1:tot.occ) {
      # Cubs production
      if (t==1 || t %in% btw.occ) {
        fem.rep[t] <- round(fem.ad[t]*rep.fem)   # Adult females reproducing in a given occasion
        cubs <- rowSums(rmultinom(fem.rep[t], n.cubs, pr.cubs)) # Take a random number of cubs for each female in the probability given
        sum.cubs[t] <- sum(cubs*c(seq(1:(length(cubs))))) #Total number of cubs per season
        fem.cubs[t] <- round(sum.cubs[t]*sex.ratio) #Total number of fem cubs per season
        male.cubs[t] <- sum.cubs[t]-fem.cubs[t] #Total number of male cubs per season
      }
      
      fem.all[t] <- fem.ad[t]+fem.juv[t]+fem.cubs[t]
      male.all[t] <- male.ad[t]+male.cubs[t]
      
      # Survival trial
      if (t %in% seq(1:(tot.occ-1))) {
        PHI.fem <- matrix(rep(phi[t], fem.all[t]), ncol = 1, nrow = (fem.all[t]), byrow = T) #Survival probability Matrix for all females (adults and cubs)
        PHI.male <- matrix(rep(phi[t], male.all[t]), ncol = 1, nrow = (male.all[t]), byrow = T) #Survival probability Matrix for all males (adults, juv and cubs)
        
        PHI.fem.ind <- matrix(nrow=(fem.all[t]), ncol=1) #NA Matrix for Bernoulli survival Trial all FEMALES
        PHI.male.ind <- matrix(nrow=(male.all[t]), ncol=1) #NA Matrix for Bernoulli survival Trial all MALES
        
        # Simulating survival for each individual from one occasion to the next
        for (i in 1:(fem.all[t])){
          surv <- rbinom(1, 1, PHI.fem[i]) # Bernoulli trial: has individual survived occasion?
          ifelse (surv==1, PHI.fem.ind[i] <- 1, PHI.fem.ind[i] <- 0)
        } #i
        for (i in 1:(male.all[t])){
          surv <- rbinom(1, 1, PHI.male[i]) # Bernoulli trial: has individual survived occasion?
          ifelse (surv==1, PHI.male.ind[i] <- 1, PHI.male.ind[i] <- 0)
        } #i
        
        # Store each individual's Survival History
        assign(paste("PHI.fem", t, sep = "_"), PHI.fem.ind)
        assign(paste("PHI.male", t, sep = "_"), PHI.male.ind)
      }
      
      #Moment when survival cubs from previous occasion become juvenile, and juveniles mature adults
      
      ifelse (t %in% (btw.occ-1), {   #HAPPENS ONLY IN BETWEEN OCCASIONS (NOT IN OCC 1!)
        # Total female adult+juv that survived from one occasion to the other
        ifelse (fem.ad[t]+fem.juv[t]>=1,
                fem.ad[t+1] <- sum(PHI.fem.ind[1:(fem.ad[t]+fem.juv[t])]),
                fem.ad[t+1] <- 0)
        # Total female cubs that survived from one occasion to the other
        ifelse (nrow(PHI.fem.ind)>=(fem.ad[t]+fem.juv[t]+1),
                fem.juv[t+1] <- sum(PHI.fem.ind[(fem.ad[t]+fem.juv[t]+1):(nrow(PHI.fem.ind))]),
                fem.juv[t+1] <- 0)
        # Total male adults+cubs that survived from one occasion to the other
        ifelse (nrow(PHI.male.ind)>=1,
                male.ad[t+1] <- sum(PHI.male.ind[1:(nrow(PHI.male.ind))]),
                male.ad[t+1] <- 0)
      },{ #HAPPENS IN ALL OCC BUT THE BETWEEN OCCASIONS
        # Total female adult that survived from one occasion to the other
        ifelse (fem.ad[t]>=1,
                fem.ad[t+1] <- sum(PHI.fem.ind[1:(fem.ad[t])]),
                fem.ad[t+1] <- 0)
        # Total female juv that survived from one occasion to the other
        ifelse ((fem.ad[t]+fem.juv[t])>=(fem.ad[t]+1),
                fem.juv[t+1] <- sum(PHI.fem.ind[(fem.ad[t]+1):(fem.ad[t]+fem.juv[t])]),
                fem.juv[t+1] <- 0)
        # Total female cubs that survived from one occasion to the other
        ifelse (nrow(PHI.fem.ind)>=(fem.ad[t]+fem.juv[t]+1),
                fem.cubs[t+1] <- sum(PHI.fem.ind[(fem.ad[t]+fem.juv[t]+1):(nrow(PHI.fem.ind))]),
                fem.cubs[t+1] <- 0)
        # Total male adults that survived from one occasion to the other
        ifelse (male.ad[t]>=1,
                male.ad[t+1] <- sum(PHI.male.ind[1:male.ad[t]]),
                male.ad[t+1] <- 0)
        # Total male cubs that survived from one occasion to the other
        ifelse (nrow(PHI.male.ind)>=(male.ad[t]+1),
                male.cubs[t+1] <- sum(PHI.male.ind[(male.ad[t]+1):(nrow(PHI.male.ind))]),
                male.cubs[t+1] <- 0)
      })
      
      ifelse (t==1 || t %in% btw.occ,{
        tot.entries[t] <- male.cubs[t] + fem.cubs[t] #Total entries.
      },{
        tot.entries[t] <- 0
      })
      
      ifelse (t==1 || t %in% btw.occ,{
        B.f[t] <- fem.cubs[t]
        B.m[t] <- male.cubs[t]
      },{
        B.f[t] <- 0
        B.m[t] <- 0
      })
    }
    
    for (t in 1:(tot.occ+1)) {
      if (is.na(male.cubs[t])){male.cubs[t] <- 0}
      if (is.na(fem.cubs[t])){fem.cubs[t] <- 0}
      if (is.na(male.ad[t])){male.ad[t] <- 0}
      if (is.na(fem.ad[t])){fem.ad[t] <- 0}
      if (is.na(fem.juv[t])){fem.juv[t] <- 0}}
    
    for (t in 1:(tot.occ)) {
      if (is.na(male.all[t])){male.all[t] <- 0}
      if (is.na(fem.all[t])){fem.all[t] <- 0}}
    
    B.f[1] <- B.f[1] + n.fem
    B.m[1] <- B.m[1] + n.male
    B <- tot.entries
    B[1] <- tot.entries[1]+N.ini
    
    #Super population calculation, includes cubs that die in the first year.
    S <- sum(tot.entries, N.ini, na.rm=F)
    S.f <- sum(B.f) 
    S.m <- sum(B.m)
    
    #Variable summary
    
    N <- fem.all+male.all
    N.f <- fem.all
    N.m <- male.all
    ls.T.Nf[[ite]] <- N.f
    ls.T.Bf[[ite]] <- B.f
    ls.T.Nm[[ite]] <- N.m
    ls.T.Bm[[ite]] <- B.m
    ls.T.N[[ite]] <- N
    ls.T.S[[ite]] <- S
    
    #-------------------------------------------------
    
    
    ####################
    # SURVIVAL HISTORY #
    ####################
    
    
    # Organizing the matrix - FEMALES
    phi.matrix <- numeric()
    for (t in 1:(tot.occ-1)){
      phi.matrix[t] <- paste("PHI.fem", t, sep = "_")}
    for (t in 1:(tot.occ-1)){
      phi.matrix[t] <- list(get(phi.matrix[[t]]))}
    for (t in 1:(tot.occ-1)){
      phi.matrix[[t]] <- rbind(phi.matrix[[t]], matrix(rep(NA, abs(S.f-nrow(phi.matrix[[t]]))), ncol=1, byrow=T))}
    phi.matrix.f <- matrix(unlist(phi.matrix), ncol=(tot.occ-1), byrow=F)
    surv.f <- matrix(NA, ncol=ncol(phi.matrix.f), nrow=S.f)
    surv.f[,1] <- phi.matrix.f[,1]
    for (j in 2:(tot.occ-1)){
      for (i in 1:S.f){
        ifelse (surv.f[i,j-1]==2 || surv.f[i,j-1]==0, surv.f[i,j] <- 2, is.na(surv.f[i,j]))
        ifelse (is.na(surv.f[i,j]), surv.f[i,j]<-phi.matrix.f[i-(length(which(surv.f[1:i,j]==2))),j], next)
      }}
    for (j in 2:(tot.occ-1)){
      for (i in 1:S.f){
        ifelse (surv.f[i,j]==2 || surv.f[i,j]==0, surv.f[i,j] <- 0, surv.f[i,j] <- 1)}}
    surv.f <- cbind(matrix(c(rep(1, fem.all[1]), rep(NA, nrow(surv.f)-fem.all[1])), ncol=1, byrow=F), surv.f)
    
    # Adding new cubs to the matrix
    l.btw.occ <- length(btw.occ)
    val.na <- numeric()
    for (t in 1:l.btw.occ){
      ifelse({fem.cubs[btw.occ[t]]==0},
             {next},
             {val.na[t] <- head(min(which(is.na(surv.f[,(btw.occ[t])]))))
             surv.f[(val.na[t]):(val.na[t]+fem.cubs[btw.occ[t]]-1),btw.occ[t]] <- matrix(rep(1,fem.cubs[btw.occ[t]]), ncol=1, byrow=F)})}
    
    # Changing NA for 0
    for (j in 1:tot.occ){
      for (i in 1:S.f){
        if (is.na(surv.f[i,j])) surv.f[i,j] <- 0}}
    
    # Organizing the matrix - MALES
    phi.matrix <- numeric()
    for (t in 1:(tot.occ-1)){
      phi.matrix[t] <- paste("PHI.male", t, sep = "_")}
    for (t in 1:(tot.occ-1)){
      phi.matrix[t] <- list(get(phi.matrix[[t]]))}
    for (t in 1:(tot.occ-1)){
      phi.matrix[[t]] <- rbind(phi.matrix[[t]], matrix(rep(NA, abs(S.m-nrow(phi.matrix[[t]]))), ncol=1, byrow=T))}
    phi.matrix.m <- matrix(unlist(phi.matrix), ncol=(tot.occ-1), byrow=F)
    surv.m <- matrix(NA, ncol=ncol(phi.matrix.m), nrow=S.m)
    surv.m[,1] <- phi.matrix.m[,1]
    for (j in 2:(tot.occ-1)){
      for (i in 1:S.m){
        ifelse (surv.m[i,j-1]==2 || surv.m[i,j-1]==0, surv.m[i,j] <- 2, is.na(surv.m[i,j]))
        ifelse (is.na(surv.m[i,j]), surv.m[i,j]<-phi.matrix.m[i-(length(which(surv.m[1:i,j]==2))),j], next)
      }}
    for (j in 2:(tot.occ-1)){
      for (i in 1:S.m){
        ifelse (surv.m[i,j]==2 || surv.m[i,j]==0, surv.m[i,j] <- 0, surv.m[i,j] <- 1)}}
    surv.m <- cbind(matrix(c(rep(1, male.all[1]), rep(NA, nrow(surv.m)-male.all[1])), ncol=1, byrow=F), surv.m)
    
    # Adding new cubs to the matrix
    l.btw.occ <- length(btw.occ)
    val.na <- numeric()
    for (t in 1:l.btw.occ){
      ifelse({male.cubs[btw.occ[t]]==0},
             {next},
             {val.na[t] <- head(min(which(is.na(surv.m[,(btw.occ[t])]))))
             surv.m[(val.na[t]):(val.na[t]+male.cubs[btw.occ[t]]-1),btw.occ[t]] <- matrix(rep(1,male.cubs[btw.occ[t]]), ncol=1, byrow=F)})}
    
    # Changing NA for 0
    for (j in 1:tot.occ){
      for (i in 1:S.m){
        if (is.na(surv.m[i,j])) surv.m[i,j] <- 0}}
    
    
    # -------------------------------------------
    
    ###################
    # CAPTURE HISTORY #
    ###################
    
    #Create a full matrix for physical and marking capture probability and multiply by survival matrix
    p.1st.f <- matrix(pm, ncol = tot.occ, nrow = sum(B.f)) # Matrix of capture for marking
    p.1st.m <- matrix(pm, ncol = tot.occ, nrow = sum(B.m)) # B.f indicates the number of entries at each occasion.
    p.1st.f <- p.1st.f*surv.f
    p.1st.m <- p.1st.m*surv.m
    
    # Bernoulli trial for first capture (marking occasion)
    for (i in 1:sum(B.f)){
      p.1st.f[i,] <- rbinom(tot.occ, 1, p.1st.f[i,])}
    for (i in 1:sum(B.m)){
      p.1st.m[i,] <- rbinom(tot.occ, 1, p.1st.m[i,])}
    
    # SAVE PHYSICAL CAPTURE ONLY
    ls.T.p1.CH[[ite]] <- rbind(p.1st.f, p.1st.m)
    
    # Identify when each individual was first captured
    p.1st.f.min <- apply(p.1st.f!=0,1,function(x) which(x)[1])
    p.1st.m.min <- apply(p.1st.m!=0,1,function(x) which(x)[1])
    
    # Matrix of already captured and marked (Excludes physical recaptures after marking)
    for (i in 1:length(p.1st.f.min)){
      for (j in 1:tot.occ){
        ifelse (j==p.1st.f.min[i], p.1st.f[i,j] <- 1, p.1st.f[i,j] <- 0)}}
    for (i in 1:length(p.1st.m.min)){
      for (j in 1:tot.occ){
        ifelse (j==p.1st.m.min[i], p.1st.m[i,j] <- 1, p.1st.m[i,j] <- 0)}}
    
    # Matrix of marked and available for recapture
    p.2nd.f <- matrix(0, nrow=length(p.1st.f.min), ncol=tot.occ, byrow=T)
    p.2nd.m <- matrix(0, nrow=length(p.1st.m.min), ncol=tot.occ, byrow=T)
    
    # Allowing for marked individuals to be captured by observation
    for (i in 1:length(p.1st.f.min)){
      for (j in 1:(tot.occ-1)){
        if (p.1st.f[i,j]==1) p.2nd.f[i,(j+1):tot.occ]<-pr[(j+1):tot.occ]
        if (p.1st.f[i,j]==1) next }}
    for (i in 1:length(p.1st.m.min)){
      for (j in 1:(tot.occ-1)){
        if (p.1st.m[i,j]==1) p.2nd.m[i,(j+1):tot.occ]<-pr[(j+1):tot.occ]
        if (p.1st.m[i,j]==1) next }}
    
    # Making sure that dead individuals can't be captured by observation
    p.2nd.f <- p.2nd.f*surv.f
    p.2nd.m <- p.2nd.m*surv.m
    
    # Bernoulli trial for capture (observing occasion)
    for (i in 1:nrow(p.2nd.f)){
      p.2nd.f[i,] <- rbinom(tot.occ, 1, p.2nd.f[i,])}
    for (i in 1:nrow(p.2nd.m)){
      p.2nd.m[i,] <- rbinom(tot.occ, 1, p.2nd.m[i,])}
    
    # Full CH with both physical and observational captures
    pr.f <- p.1st.f + p.2nd.f
    pr.m <- p.1st.m + p.2nd.m
    
    # Remove individuals never captured
    p.sum.f <- rowSums(pr.f)
    never.f <- which(p.sum.f == 0)
    if (any(p.sum.f[]==0))
      pr.f <- pr.f[-never.f,]
    
    p.sum.m <- rowSums(pr.m)
    never.m <- which(p.sum.m == 0)
    if (any(p.sum.m[]==0))
      pr.m <- pr.m[-never.m,]
    
    CH.f <- pr.f
    CH.m <- pr.m
    CH <- rbind(CH.f, CH.m)
    CH
    
    # -------------------------------------------
    
    ##########################################
    # CREATING A POPULATION COUNTING HISTORY #
    ##########################################
    
    # INDEPENDENT
    
    cnt <- list()
    COUNT <- numeric(tot.occ)
    for (i in 1:tot.occ){
      cnt[[i]] <- matrix(nrow=N[i],ncol=1)
      for (j in 1:nrow(cnt[[i]])){
        cnt[[i]][j,1] <- rbinom(1, 1, pr[i])} # Bernoulli trial: was the individual observed in that occasion?
      COUNT[i] <- sum(cnt[[i]])
    } #i
    
    # DEPENDENT
    IndSum <- numeric()
    for (t in 1:tot.occ){
      IndSum[t] <- sum(CH[,t])} #Summed marked individuals that are observed in time t
    Nnm <- N-IndSum # Excluding marked and observed individuals from total individuals available for observation
    cnt.d <- list()
    COUNT.D <- numeric(tot.occ)
    for (i in 1:tot.occ){
      cnt.d[[i]] <- matrix(nrow=Nnm[i],ncol=1)
      for (j in 1:nrow(cnt.d[[i]])){
        cnt.d[[i]][j,1] <- rbinom(1, 1, pr[i])} # Bernoulli trial: was the individual observed in that occasion?
      COUNT.D[i] <- sum(cnt.d[[i]])
    } #i
    COUNT.D <- COUNT.D + IndSum
    
    # Save the CH and Counting History
    Count.i <- COUNT
    Count.d <- COUNT.D
    CapHist <- CH
    
    ls.T.CH[[ite]]  <- assign(paste("CH", ite, sep = "_"), CapHist)
    ls.T.CD[[ite]]  <- assign(paste("CD", ite, sep = "_"), Count.d)
    ls.T.CI[[ite]] <-  assign(paste("CI", ite, sep = "_"), Count.i)
    
    # Population values
    output.Real.T[1,ite]<- round(mean(N))     # Number of Individuals
    output.Real.T[2,ite]<- round(sum(B[2:tot.occ]/(n.occ-1)))     # Number of recruits
    output.Real.T[3,ite]<- phi.days    # Survival probability secondary sample
    output.Real.T[4,ite]<- phi.occ     # Survival probability primary sample
    output.Real.T[5,ite]<- mean(pr)    # Recapture probability
    
  }, error=function(e){cat("ERROR : Oups! No donnuts here! ",conditionMessage(e), "\n")})
}
# End iterations

save(ls.T.CH, file="CH.RData")
save(ls.T.CD, file="CD.RData")
save(ls.T.CI, file="CI.RData")
save(ls.T.p1.CH, file="p1-CH.RData")
save(ls.T.Nf, file="N-fem.RData")
save(ls.T.Nm, file="N-male.RData")
save(ls.T.N, file="N-each.RData")
save(ls.T.S, file="S.RData")
save(ls.T.Bf, file="B-fem.RData")
save(ls.T.Bm, file="B-male.RData")
save(output.Real.T,file="Real.RData")
