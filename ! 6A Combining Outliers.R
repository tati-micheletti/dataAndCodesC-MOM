load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Exc-2P.Rdata")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Exc-2H.Rdata")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Exc-1H.Rdata")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Exc-2T.Rdata")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Exc-1T.Rdata")

Exc2P$Formulation <- factor("2P")
ls.Exc1T <- rbind(ls.Exc1T[[1]], ls.Exc1T[[2]])
ls.Exc1T$Formulation <- factor("1P")
ls.Exc2T <- rbind(ls.Exc2T[[1]], ls.Exc2T[[2]])
ls.Exc2T$Formulation <- factor("2P")
ls.ExcH <- rbind(ls.Exc1H[[1]],ls.Exc1H[[2]],ls.Exc1H[[3]],ls.Exc1H[[4]],ls.Exc1H[[5]],ls.Exc1H[[6]])
ls.ExcH$Formulation <- factor("1P")

Outliers <- rbind(Exc2P,ls.Exc1T,ls.Exc2T,ls.ExcH)

write.csv(Outliers, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Outliers.csv")
