########################################
#                                      #
# 5c. Run Mark with simulated datasets #
#                                      #
#         Z P N E c                    #
#                                      #
########################################

# HETERO

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/CH.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/CI.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/HETERO/CD.RData")
setwd("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Mark")

library(RMark)

iter <- length(ls.H.CH)

n.occ <- 3  # Number of capture occasions (field trips)
n.days <- 3  # Number of days of work in each occasion

# Create necessary variables
CH.list <- CH2.list <- Mark.list.Hc <- Mark2.list.Hc <- km.list <- y.list <- km2.list <- y2.list <- Mark.list.summary.Hc <- Mark2.list.summary.Hc <- HN.ZPNEc <- HN.ZPNEs <- HNs.ZPNEc <- HNs.ZPNEs <- list()

H.output.ZPNEc <- matrix(NA,ncol=iter,nrow=10)
rownames(H.output.ZPNEc) <- c("ZPNEc.N","ZPNEc.N.se",
                              "ZPNEc.B","ZPNEc.B.se",
                              "ZPNEc.phin","ZPNEc.phin.se",
                              "ZPNEc.phib","ZPNEc.phib.se",
                              "ZPNEc.p","ZPNEc.p.se")

H.output.ZPNEs <- matrix(NA,ncol=iter,nrow=10)
rownames(H.output.ZPNEs) <- c("ZPNEs.N","ZPNEs.N.se",
                              "ZPNEs.B","ZPNEs.B.se",
                              "ZPNEs.phin","ZPNEs.phin.se",
                              "ZPNEs.phib","ZPNEs.phib.se",
                              "ZPNEs.p","ZPNEs.p.se")
# FOR ZPNEs
ls.H.CH2 <- ls.H.CH

##################
#  FOR ZPNEc     #
##################

for(ite in 1:iter){
  setwd("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Mark")
  
  CH <- data.frame()
  for (i in 1:nrow(ls.H.CH[[ite]])) {
    
    # FOR THE FIRST FIELD TRIP
    CH[i,1] <- ifelse( 
      sum(ls.H.CH[[ite]][i, c(1:3)])>0,
      paste(0, sum(ls.H.CH[[ite]][i, c(1:3)]), sep = ""),
      "..")
    
    # FOR THE SECOND FIELD TRIP
    CH[i,2] <- ifelse( 
      sum(ls.H.CH[[ite]][i, c(4:6)])>0,
      paste(0, sum(ls.H.CH[[ite]][i, c(4:6)]), sep = ""),
      ifelse(CH[i,1]!="..",
             ifelse(sum(ls.H.CH[[ite]][i, c(7:9)])>0,
                    paste("+", 0, sep=""),
                    paste("-", 0, sep="")),
             ".."))
    
    # FOR THE THIRD FIELD TRIP
    CH[i,3] <- ifelse(
      sum(ls.H.CH[[ite]][i, c(7:9)])>0,
      paste(0, sum(ls.H.CH[[ite]][i, c(7:9)]), sep = ""),
      ifelse(CH[i,2]!="..",
             ifelse(sum(ls.H.CH[[ite]][i, c(7:9)])>0,
                    paste("+", 0, sep=""),
                    paste("-", 0, sep="")),
             ".."))
    
  } # END CH LOOP
  
  CH[,4] <- do.call(paste, c(CH[c("V1","V2","V3")], sep = ""))
  CH <- data.frame(CH[,4])
  colnames(CH) <- "ch"
  CH$ch <- as.character(CH$ch)
  CH.list[[ite]]  <- assign(paste("CH", ite, sep = "_"), CH)
  
  # FOR EACH Y (COUNTING)
  # Counting of Unmarked (exclude marked from the ls.CD. Use of ls.CD because it is dependent on marked)
  ls.y <- ls.H.CD[[ite]]
  for (j in 1:length(ls.y)){
    ls.y[j] <- ls.y[j]-sum(ls.H.CH[[ite]][,j])}
  
  y <- numeric()
  y <- c(sum(ls.y[1:3]),sum(ls.y[4:6]),sum(ls.y[7:9]))
  y.list[[ite]]  <- assign(paste("y", ite, sep = "_"), y)
  
  # FOR EACH Known Marks
  
  km <- numeric()
  km <- sum(ls.H.CH[[ite]][,1]==1|ls.H.CH[[ite]][,2]==1|ls.H.CH[[ite]][,3]==1)
  km.list[[ite]]  <- assign(paste("km", ite, sep = "_"), km)
  
  data(PoissonMR)
  pois.proc=process.data(CH.list[[ite]],model="PoissonMR",
                         counts=list("Unmarked Seen"=c(y.list[[ite]]),
                                     "Marked Unidentified"=c(0,0,0),
                                     "Known Marks"=c(km.list[[ite]],0,0)))
  
  mod=mark(pois.proc,ddl=NULL,
           model.parameters=list(Phi=list(formula=~1,link="sin"),
                                 GammaDoublePrime=list(fixed=0),
                                 GammaPrime=list(fixed=0),
                                 alpha=list(formula=~1,link="sin"),
                                 U=list(formula=~-1+time,link="sin"),
                                 sigma=list(formula=~1,link="sin")))
  # SAVE H.output
  
  Mark.list.Hc[[ite]]  <- assign(paste("Mark", ite, sep = "_"), mod)
  Mark.list.summary.Hc[[ite]]  <- assign(paste("Mark", ite, sep = "_"), summary(mod))
  
  N <- mod$results$derived$`N Population Size`$estimate
  N.se <- mod$results$derived$`N Population Size`$se
  p <- mod$results$derived$`Pr(Captured 1 or more times)`$estimate
  p.se <- mod$results$derived$`Pr(Captured 1 or more times)`$se
  phi <- mod$results$real$estimate[6]
  phi.se <- mod$results$real$se[6]
  
  H.output.ZPNEc[1,ite] <- mean(N)
  H.output.ZPNEc[2,ite] <- mean(N.se)
  H.output.ZPNEc[7,ite] <- phi
  H.output.ZPNEc[8,ite] <- phi.se
  H.output.ZPNEc[9,ite] <- mean(p)
  H.output.ZPNEc[10,ite] <- mean(p.se)
  
  HN.ZPNEc[[ite]] <- N
  HNs.ZPNEc[[ite]] <- N.se
  
  if(ite==iter){
    setwd("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat")
    save(H.output.ZPNEc,file="H-outputZPNEc.RData")
    setwd("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc")
    save(Mark.list.Hc,file="H-Mark-list-ZPNEc.RData")
    save(HN.ZPNEc,file="HN-ZPNEc.RData")
    save(HNs.ZPNEc,file="HNs-ZPNEc.RData")
    save(Mark.list.summary.Hc,file="H-Mark-list-summ-ZPNEc.RData")}
  
} # END ITERATION (ite loop)

##########################
#  CONVERGENCE PROBLEMS  #
##########################

# Iterations 
#  
# did not converge even after 10 trials


##################
#  FOR ZPNEs     #
##################


# For the models ZPNEs I decided to do one capture and two recapture days. Even though its a low trapping sp, if I make different is not robust design anymore. Individuals that are not captured in the first 2 days of marking DO NOT COUNT as recapture for the same field trip.

# For rows which the sum of all days in a FT >0, are the ind that entered the pop at the 1st FT
# For these specific ind, check the column when all ind were capt at least once
# Sum all values of these specific ind for all columns until the specific one that the product between these sums is >1.
# This is the "last marking day", na the next the first day of the FT for observation


for(ite in 1:iter)
{
  setwd("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Mark")

  # ORGANIZE DATA for ZPNEs
  
  for(i in 1:nrow(ls.H.CH2[[ite]])) # Loop through individuals so only the ones marked in marking occasions (first day of each field trip) can actually be captured by observation
  {
    ls.H.CH2[[ite]][i,2] <- ifelse(ls.H.CH[[ite]][i,1]==0,0,ls.H.CH[[ite]][i,2])
    ls.H.CH2[[ite]][i,3] <- ifelse(ls.H.CH[[ite]][i,1]==0,0,ls.H.CH[[ite]][i,3])
    ls.H.CH2[[ite]][i,5] <- ifelse(sum(ls.H.CH[[ite]][i,c(1,4)])==0,0,ls.H.CH[[ite]][i,5])
    ls.H.CH2[[ite]][i,6] <- ifelse(sum(ls.H.CH[[ite]][i,c(1,4)])==0,0,ls.H.CH[[ite]][i,6])
    ls.H.CH2[[ite]][i,8] <- ifelse(sum(ls.H.CH[[ite]][i,c(1,4,7)])==0,0,ls.H.CH[[ite]][i,8])
    ls.H.CH2[[ite]][i,9] <- ifelse(sum(ls.H.CH[[ite]][i,c(1,4,7)])==0,0,ls.H.CH[[ite]][i,9])
  }
  
  ls.CH2.y <- ls.H.CH2[[ite]]
  
  ls.H.CH2[[ite]] <- ls.H.CH2[[ite]][,-c(1,4,7)] # Remove marking occasions
  ls.H.CH2[[ite]] <- ls.H.CH2[[ite]][apply(ls.H.CH2[[ite]][,-1], 1, function(x) !all(x==0)),] # Delete individuals with no captures
  
  # For each CH (CAPTURE RECAPTURE)
  
  # CREATE A LOOP TO ONLY RUN DATA IF nrow(CH) after  organization is != NULL
  
  CH2 <- data.frame()
  
  ifelse(is.null(nrow(ls.H.CH2[[ite]])),next,{ # CREATE A LOOP TO ONLY ORGANIZE DATA IF nrow(CH) IS NOT NULL
    
    for (i in 1:nrow(ls.H.CH2[[ite]])) {
      
      # FOR THE FIRST FIELD TRIP
      CH2[i,1] <- ifelse( 
        sum(ls.H.CH2[[ite]][i, c(1:2)])>0,
        paste(0, sum(ls.H.CH2[[ite]][i, c(1:2)]), sep = ""),
        "..")
      
      # FOR THE SECOND FIELD TRIP
      CH2[i,2] <- ifelse(
        sum(ls.H.CH2[[ite]][i, c(3:4)])>0,
        paste(0, sum(ls.H.CH2[[ite]][i, c(3:4)]), sep = ""),
        ifelse(CH2[i,1]!="..",
               ifelse(sum(ls.H.CH2[[ite]][i, c(5:6)])>0,
                      paste("+", 0, sep=""),
                      paste("-", 0, sep="")),
               ".."))
      
      # FOR THE THIRD FIELD TRIP
      CH2[i,3] <- ifelse(
        sum(ls.H.CH2[[ite]][i, c(5:6)])>0,
        paste(0, sum(ls.H.CH2[[ite]][i, c(5:6)]), sep = ""),
        ifelse(CH2[i,2]!="..",
               ifelse(sum(ls.H.CH2[[ite]][i, c(5:6)])>0,
                      paste("+", 0, sep=""),
                      paste("-", 0, sep="")),
               ".."))
    } # END CH LOOP
    
    #Concatenate CH in one column called 'ch'
    CH2[,4] <- do.call(paste, c(CH2[c("V1","V2","V3")], sep = ""))
    CH2 <- data.frame(CH2[,4])
    colnames(CH2) <- "ch"
    CH2$ch <- as.character(CH2$ch)
    CH2.list[[ite]]  <- assign(paste("CH2", ite, sep = "_"), CH2)
    
    # FOR EACH Y (COUNTING)
    # Counting of Unmarked (exclude marked from the ls.CD. Use of ls.CD because it is dependent on marked)
    ls.y2 <- ls.H.CD[[ite]]
    for (j in 1:length(ls.y2)){
      ls.y2[j] <- ls.y2[j]-sum(ls.CH2.y[,j])}
    
    y2 <- numeric()
    y2 <- c(sum(ls.y2[2:3]),sum(ls.y2[5:6]),sum(ls.y2[8:9]))
    y2.list[[ite]]  <- assign(paste("y2", ite, sep = "_"), y2)
    
    # FOR EACH Known Marks
    km2 <- numeric()
    km2 <- colSums(ls.H.CH2[[ite]])[1]+colSums(ls.H.CH2[[ite]])[2]-sum(ls.H.CH2[[ite]][,1]==1&ls.H.CH2[[ite]][,2]==1)
    km2.list[[ite]]  <- assign(paste("km2", ite, sep = "_"), km2)
    
    data(PoissonMR)
    pois.proc=process.data(CH2.list[[ite]],model="PoissonMR",
                           counts=list("Unmarked Seen"=c(y2.list[[ite]]),
                                       "Marked Unidentified"=c(0,0,0),
                                       "Known Marks"=c(km2.list[[ite]],0,0)))
    
    mod=mark(pois.proc,ddl=NULL,
             model.parameters=list(Phi=list(formula=~1,link="sin"),
                                   GammaDoublePrime=list(fixed=0),
                                   GammaPrime=list(fixed=0),
                                   alpha=list(formula=~1,link="sin"),
                                   U=list(formula=~-1+time,link="sin"),
                                   sigma=list(formula=~1,link="sin")))
    # SAVE H.output
    
    Mark2.list.Hc[[ite]]  <- assign(paste("Mark", ite, sep = "_"), mod)
    Mark2.list.summary.Hc[[ite]]  <- assign(paste("Mark", ite, sep = "_"), summary(mod))
    
    N <- mod$results$derived$`N Population Size`$estimate
    N.se <- mod$results$derived$`N Population Size`$se
    p <- mod$results$derived$`Pr(Captured 1 or more times)`$estimate
    p.se <- mod$results$derived$`Pr(Captured 1 or more times)`$se
    phi <- mod$results$real$estimate[6]
    phi.se <- mod$results$real$se[6]
    
    H.output.ZPNEs[1,ite] <- mean(N)
    H.output.ZPNEs[2,ite] <- mean(N.se)
    H.output.ZPNEs[7,ite] <- phi
    H.output.ZPNEs[8,ite] <- phi.se
    H.output.ZPNEs[9,ite] <- mean(p)
    H.output.ZPNEs[10,ite] <- mean(p.se)
    
    HN.ZPNEs[[ite]] <- N
    HNs.ZPNEs[[ite]] <- N.se
    
  })# END OF IFELSE TO AVOID RUNNING NULL CH
  
  
  if(ite==iter){
    setwd("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat")
    save(H.output.ZPNEs,file="H-outputZPNEs.RData")
    setwd("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs")
    save(Mark2.list.Hc,file="H-Mark-list-ZPNEs.RData")
    save(HN.ZPNEs,file="HN-ZPNEs.RData")
    save(HNs.ZPNEs,file="HNs-ZPNEs.RData")
    save(Mark2.list.summary.Hc,file="H-Mark-list-summ-ZPNEs.RData")}
  
} # END ITERATION (ite loop)


##########################
#  CONVERGENCE PROBLEMS  #
##########################

# Iterations 
#  
# did not converge even after 10 trials

save.image(file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/RESULTS-MARK-HETERO.RData")

