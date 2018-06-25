########################################
#                                      #
# 5a. Run Mark with simulated datasets #
#                                      #
#         Z P N E c                    #
#                                      #
########################################

# 1P 2P
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/CH.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/CI.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/1P 2P/CD.RData")


library(RMark)

iter <- length(ls.CH)

n.occ <- 3  # Number of capture occasions (field trips)
n.days <- 3  # Number of days of work in each occasion

# Create necessary variables
CH.list <- CH2.list <- Mark.list <- Mark2.list <- km.list <- y.list <- km2.list <- y2.list <- Mark.list.summary <- Mark2.list.summary <- N.ZPNEc <- N.ZPNEs <- Ns.ZPNEc <- Ns.ZPNEs <-list()
output.ZPNEc <- matrix(NA,ncol=iter,nrow=10)
rownames(output.ZPNEc) <- c("ZPNEc.N","ZPNEc.N.se",
                        "ZPNEc.B","ZPNEc.B.se",
                        "ZPNEc.phin","ZPNEc.phin.se",
                        "ZPNEc.phib","ZPNEc.phib.se",
                        "ZPNEc.p","ZPNEc.p.se")

output.ZPNEs <- matrix(NA,ncol=iter,nrow=10)
rownames(output.ZPNEs) <- c("ZPNEs.N","ZPNEs.N.se",
                            "ZPNEs.B","ZPNEs.B.se",
                            "ZPNEs.phin","ZPNEs.phin.se",
                            "ZPNEs.phib","ZPNEs.phib.se",
                            "ZPNEs.p","ZPNEs.p.se")
# FOR ZPNEs
ls.CH2 <- ls.CH

##################
#  FOR ZPNEc     #
##################

for(ite in 1:iter){
  setwd("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Mark")
  
  CH <- data.frame()
  for (i in 1:nrow(ls.CH[[ite]])) {
    
    # FOR THE FIRST FIELD TRIP
    CH[i,1] <- ifelse( 
      sum(ls.CH[[ite]][i, c(1:3)])>0,
      paste(0, sum(ls.CH[[ite]][i, c(1:3)]), sep = ""),
      "..")
    
    # FOR THE SECOND FIELD TRIP
    CH[i,2] <- ifelse( 
      sum(ls.CH[[ite]][i, c(4:6)])>0,
      paste(0, sum(ls.CH[[ite]][i, c(4:6)]), sep = ""),
      ifelse(CH[i,1]!="..",
             ifelse(sum(ls.CH[[ite]][i, c(7:9)])>0,
                    paste("+", 0, sep=""),
                    paste("-", 0, sep="")),
             ".."))
    
    # FOR THE THIRD FIELD TRIP
    CH[i,3] <- ifelse(
      sum(ls.CH[[ite]][i, c(7:9)])>0,
      paste(0, sum(ls.CH[[ite]][i, c(7:9)]), sep = ""),
      ifelse(CH[i,2]!="..",
             ifelse(sum(ls.CH[[ite]][i, c(7:9)])>0,
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
  ls.y <- ls.CD[[ite]]
  for (j in 1:length(ls.y)){
    ls.y[j] <- ls.y[j]-sum(ls.CH[[ite]][,j])}
    
  y <- numeric()
  y <- c(sum(ls.y[1:3]),sum(ls.y[4:6]),sum(ls.y[7:9]))
  y.list[[ite]]  <- assign(paste("y", ite, sep = "_"), y)
  
  # FOR EACH Known Marks
  
  km <- numeric()
  km <- sum(ls.CH[[ite]][,1]==1|ls.CH[[ite]][,2]==1|ls.CH[[ite]][,3]==1)
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
  # SAVE OUTPUT
  
  Mark.list[[ite]]  <- assign(paste("Mark", ite, sep = "_"), mod)
  Mark.list.summary[[ite]]  <- assign(paste("Mark", ite, sep = "_"), summary(mod))
  
  N <- mod$results$derived$`N Population Size`$estimate
  N.se <- mod$results$derived$`N Population Size`$se
  p <- mod$results$derived$`Pr(Captured 1 or more times)`$estimate
  p.se <- mod$results$derived$`Pr(Captured 1 or more times)`$se
  phi <- mod$results$real$estimate[6]
  phi.se <- mod$results$real$se[6]
  
  output.ZPNEc[1,ite] <- mean(N)
  output.ZPNEc[2,ite] <- mean(N.se)
  output.ZPNEc[7,ite] <- phi
  output.ZPNEc[8,ite] <- phi.se
  output.ZPNEc[9,ite] <- mean(p)
  output.ZPNEc[10,ite] <- mean(p.se)
  
  N.ZPNEc[[ite]] <- N
  Ns.ZPNEc[[ite]] <- N.se
  
  if(ite==iter){
    setwd("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat")
    save(output.ZPNEc,file="outputZPNEc.RData")
    setwd("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEc")
    save(Mark.list,file="Mark-list-ZPNEc.RData")
    save(N.ZPNEc,file="N-ZPNEc.RData")
    save(Ns.ZPNEc,file="Ns-ZPNEc.RData")
    save(Mark.list.summary,file="Mark-list-summ-ZPNEc.RData")}
  
} # END ITERATION (ite loop)

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
  
  for(i in 1:nrow(ls.CH2[[ite]])) # Loop through individuals so only the ones marked in marking occasions (first day of each field trip) can actually be captured by observation
  {
    ls.CH2[[ite]][i,2] <- ifelse(ls.CH[[ite]][i,1]==0,0,ls.CH[[ite]][i,2])
    ls.CH2[[ite]][i,3] <- ifelse(ls.CH[[ite]][i,1]==0,0,ls.CH[[ite]][i,3])
    ls.CH2[[ite]][i,5] <- ifelse(sum(ls.CH[[ite]][i,c(1,4)])==0,0,ls.CH[[ite]][i,5])
    ls.CH2[[ite]][i,6] <- ifelse(sum(ls.CH[[ite]][i,c(1,4)])==0,0,ls.CH[[ite]][i,6])
    ls.CH2[[ite]][i,8] <- ifelse(sum(ls.CH[[ite]][i,c(1,4,7)])==0,0,ls.CH[[ite]][i,8])
    ls.CH2[[ite]][i,9] <- ifelse(sum(ls.CH[[ite]][i,c(1,4,7)])==0,0,ls.CH[[ite]][i,9])
  }

  ls.CH2.y <- ls.CH2[[ite]]
  
  ls.CH2[[ite]] <- ls.CH2[[ite]][,-c(1,4,7)] # Remove marking occasions
  ls.CH2[[ite]] <- ls.CH2[[ite]][apply(ls.CH2[[ite]][,-1], 1, function(x) !all(x==0)),] # Delete individuals with no captures
  
  # For each CH (CAPTURE RECAPTURE)
  
  # CREATE A LOOP TO ONLY RUN DATA IF nrow(CH) after  organization is != NULL
  
  CH2 <- data.frame()
  
  ifelse(is.null(nrow(ls.CH2[[ite]])),next,{ # CREATE A LOOP TO ONLY ORGANIZE DATA IF nrow(CH) IS NOT NULL
    
    for (i in 1:nrow(ls.CH2[[ite]])) {
      
      # FOR THE FIRST FIELD TRIP
      CH2[i,1] <- ifelse( 
        sum(ls.CH2[[ite]][i, c(1:2)])>0,
        paste(0, sum(ls.CH2[[ite]][i, c(1:2)]), sep = ""),
        "..")
      
      # FOR THE SECOND FIELD TRIP
      CH2[i,2] <- ifelse(
        sum(ls.CH2[[ite]][i, c(3:4)])>0,
        paste(0, sum(ls.CH2[[ite]][i, c(3:4)]), sep = ""),
        ifelse(CH2[i,1]!="..",
               ifelse(sum(ls.CH2[[ite]][i, c(5:6)])>0,
                      paste("+", 0, sep=""),
                      paste("-", 0, sep="")),
               ".."))
      
      # FOR THE THIRD FIELD TRIP
      CH2[i,3] <- ifelse(
        sum(ls.CH2[[ite]][i, c(5:6)])>0,
        paste(0, sum(ls.CH2[[ite]][i, c(5:6)]), sep = ""),
        ifelse(CH2[i,2]!="..",
               ifelse(sum(ls.CH2[[ite]][i, c(5:6)])>0,
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
    ls.y2 <- ls.CD[[ite]]
      for (j in 1:length(ls.y2)){
        ls.y2[j] <- ls.y2[j]-sum(ls.CH2.y[,j])}
    
    y2 <- numeric()
    y2 <- c(sum(ls.y2[2:3]),sum(ls.y2[5:6]),sum(ls.y2[8:9]))
    y2.list[[ite]]  <- assign(paste("y2", ite, sep = "_"), y2)
    
    # FOR EACH Known Marks
    km2 <- numeric()
    km2 <- colSums(ls.CH2[[ite]])[1]+colSums(ls.CH2[[ite]])[2]-sum(ls.CH2[[ite]][,1]==1&ls.CH2[[ite]][,2]==1)
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
    # SAVE OUTPUT
    
    Mark2.list[[ite]]  <- assign(paste("Mark", ite, sep = "_"), mod)
    Mark2.list.summary[[ite]]  <- assign(paste("Mark", ite, sep = "_"), summary(mod))
    
    N <- mod$results$derived$`N Population Size`$estimate
    N.se <- mod$results$derived$`N Population Size`$se
    p <- mod$results$derived$`Pr(Captured 1 or more times)`$estimate
    p.se <- mod$results$derived$`Pr(Captured 1 or more times)`$se
    phi <- mod$results$real$estimate[6]
    phi.se <- mod$results$real$se[6]
    
    output.ZPNEs[1,ite] <- mean(N)
    output.ZPNEs[2,ite] <- mean(N.se)
    output.ZPNEs[7,ite] <- phi
    output.ZPNEs[8,ite] <- phi.se
    output.ZPNEs[9,ite] <- mean(p)
    output.ZPNEs[10,ite] <- mean(p.se)
    
    N.ZPNEs[[ite]] <- N
    Ns.ZPNEs[[ite]] <- N.se
    
  })# END OF IFELSE TO AVOID RUNNING NULL CH
  
  
  if(ite==iter){
    setwd("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/Outputs Excluded Rhat")
    save(output.ZPNEs,file="outputZPNEs.RData")
    setwd("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Data/ZPNEs")
    save(Mark2.list,file="Mark-list-ZPNEs.RData")
    save(N.ZPNEs,file="N-ZPNEs.RData")
    save(Ns.ZPNEs,file="Ns-ZPNEs.RData")
    save(Mark2.list.summary,file="Mark-list-summ-ZPNEs.RData")}
  
} # END ITERATION (ite loop)

save.image(file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Backups/RESULTS-MARK.RData")

