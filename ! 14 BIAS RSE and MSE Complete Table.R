# Saving MSE RSE and BIAS table

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-1P.Rdata")
graphP <- graph
graphP$P.factor <- factor("Stable")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-1T.Rdata")
graph.1PT <- rbind(ls.graph[[1]],
               graphP,
               ls.graph[[2]])


load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-2P.Rdata")
graphP <- graph2
graphP$P.factor <- factor("Stable")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-2T.Rdata")
graph.2PT <- rbind(
  ls.graph2[[1]],
  graphP,
  ls.graph2[[2]])

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-1H.Rdata")
graph.1H <- rbind(ls.graph[[1]], 
               ls.graph[[2]],
               ls.graph[[3]], 
               ls.graph[[4]],
               ls.graph[[5]],
               ls.graph[[6]])

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-2H.Rdata")
graph.2H <- rbind(ls.graph2[[1]], 
               ls.graph2[[2]],
               ls.graph2[[3]], 
               ls.graph2[[4]],
               ls.graph2[[5]],
               ls.graph2[[6]])

graph.1PT$Form <- factor("1P")
graph.2PT$Form <- factor("2P")
graph.1H$Form <- factor("1P")
graph.2H$Form <- factor("2P")

complete.table <- rbind(graph.1PT, graph.1H, graph.2PT, graph.2H) 

write.csv(complete.table, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/complete-table.csv")
          