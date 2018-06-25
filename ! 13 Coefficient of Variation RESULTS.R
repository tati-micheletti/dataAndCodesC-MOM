load("C:/Users/Micheletti/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Coefficient of Variation/CV-Table-A.Rdata")

max(unlist(CV.a$CV.1P))
CV <- rbind(CV.a[[1]],
            CV.a[[2]],
            CV.a[[3]],
            CV.a[[4]],
            CV.a[[5]],
            CV.a[[6]])

which.max(CV$Values)
CV[order(CV$Values, decreasing = T),]

