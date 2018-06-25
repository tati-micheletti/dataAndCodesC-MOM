###################################################################
#                                                                 #
# 9. Creating GRID  Graph for  1P2P,  TRENDS  and  HETERO         #
#                                                                 #
###################################################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-1P.Rdata")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-2P.Rdata")

graph.P <- rbind(graph, graph2)
graph.P$Scenario <- factor("Trends")
graph.P$Form <- graph.P$P.factor
graph.P$P.factor <- factor("Stable")

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-1T.Rdata")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-2T.Rdata")

graph.T1 <- rbind(ls.graph[[1]],
                 ls.graph[[2]])
graph.T1$Form <- factor("1P")

graph.T2 <- rbind(ls.graph2[[1]],
                 ls.graph2[[2]])
graph.T2$Form <- factor("2P")

graph.T <- rbind(graph.T1, graph.T2)
graph.T$Scenario <- factor("Trends")

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-1H.Rdata")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-2H.Rdata")

graph.H1 <- rbind(ls.graph[[1]], 
               ls.graph[[2]],
               ls.graph[[3]], 
               ls.graph[[4]],
               ls.graph[[5]],
               ls.graph[[6]])

graph.H1$Form <- factor("1P")

graph.H2 <- rbind(ls.graph2[[1]], 
                  ls.graph2[[2]],
                  ls.graph2[[3]], 
                  ls.graph2[[4]],
                  ls.graph2[[5]],
                  ls.graph2[[6]])

graph.H2$Form <- factor("2P")
graph.H <- rbind(graph.H1, graph.H2)
graph.H$Scenario <- factor("Heterogenerity")

general.graph <- rbind(graph.P, graph.T, graph.H)
general.graph <- general.graph[general.graph$Results %in% c("Mean-square Error"),]
general.graph <- general.graph[,c(1,3:7)]


for (i in 1:nrow(general.graph)){ # Convert NA to the highest values+1 (10,001)
  if (is.na(general.graph$Values[i]))
    general.graph$Values[i] <- 10000}

# REMOVE ZPNEc and ZPNEs from 2P formulations (these models doesnt exist!)
general.graph <- general.graph[!(general.graph$Form=="2P" & general.graph$Models %in% c("ZPNEc", "ZPNEs")),]

library(plyr)
general.graph$Parameters <- revalue(general.graph$Parameters, c("Number of Individuals"="N"))
general.graph$Parameters <- revalue(general.graph$Parameters, c("Recruitment"="B"))
general.graph$Parameters <- revalue(general.graph$Parameters, c("Survival Second. Occ."="phi2"))
general.graph$Parameters <- revalue(general.graph$Parameters, c("Survival Prim. Occ."="phi1"))
general.graph$Parameters <- revalue(general.graph$Parameters, c("Observation probability"="p2"))
general.graph$Parameters <- revalue(general.graph$Parameters, c("Capture probability"="p1"))
general.graph$Model.Name <- factor(paste(general.graph$Model, general.graph$Form, sep='-'))
general.graph$TypeFactor <-paste(general.graph$Form, general.graph$P.factor, sep=' ')
general.graph$Model.Name <- revalue(general.graph$Model.Name, c("ZPNEc-1P"="ZPNEc"))
general.graph$Model.Name <- revalue(general.graph$Model.Name, c("ZPNEs-1P"="ZPNEs"))

general.graph <- transform(general.graph, Rank.Models = ave(Values, P.factor, Parameters, FUN = function(x) rank(x, ties.method = "average")))

for (i in 1:nrow(general.graph)){
  if (general.graph$Model.Name[i]=="CMOMd-1P") general.graph$Name[i] <- "CMOMd-1P *"
  if (general.graph$Model.Name[i]=="CMOMd-2P") general.graph$Name[i] <- "CMOMd-2P *"
  if (general.graph$Model.Name[i]=="CMOMi-1P") general.graph$Name[i] <- "CMOMi-1P *"
  if (general.graph$Model.Name[i]=="CMOMi-2P") general.graph$Name[i] <- "CMOMi-2P *"
  if (general.graph$Model.Name[i]=="CMR-1P") general.graph$Name[i] <- "CMR-1P **"
  if (general.graph$Model.Name[i]=="CMR-2P") general.graph$Name[i] <- "CMR-2P **"
  if (general.graph$Model.Name[i]=="ZPNEc") general.graph$Name[i] <- "ZPNEc **"
  if (general.graph$Model.Name[i]=="ZPNEs") general.graph$Name[i] <- "ZPNEs ***"
}

general.graph$Model.Name_Stat <- factor(general.graph$Name, 
                                        levels=c("CMOMi-1P *",
                                                 "CMOMd-1P *",
                                                 "CMOMd-2P *",
                                                 "CMOMi-2P *", 
                                               "CMR-2P **", 
                                               "CMR-1P **", 
                                               "ZPNEc **", 
                                               "ZPNEs ***"))

final.grid <- general.graph

library(ggplot2)
grid.plot <- ggplot(data = final.grid, mapping = aes(x=Parameters, y=P.factor)) + 
  geom_tile(mapping = aes(fill=Rank.Models)) +
  scale_fill_gradient(low = "green", high = "red",na.value = "transparent",
                      breaks=c(1,8),labels=c("Best","Worse"),
                      limits=c(1,8)) + 
  theme(axis.text.x = element_text(hjust = 0.5, size=9, vjust=0),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.text.x = element_text(size = 9, colour = "black", face = "bold"),
        strip.text.y = element_text(size = 9, colour = "black", face = "bold")
  ) + 
  facet_grid(Scenario ~ Model.Name_Stat, scales = "free", as.table=T) +
  scale_x_discrete(labels=c("N" = "N",
                            "B" = "B",
                            "phi2" = expression(paste(phi^2)),
                            "phi1" = expression(paste(phi^1)),
                            "p2" = expression(p^2),
                            "p1" = expression(p^1)))


png(filename = "C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/GRAPH GRID.png", width = 36, height = 21,
    units = "cm", pointsize = 10, bg = "white", res = 300,
    restoreConsole = TRUE)

grid.plot

dev.off()

###################
#    R A N K S    #
###################

Ranking <- subset(general.graph, select=c(Model.Name, Rank.Models))
Final.Rank <- aggregate(Rank.Models ~ Model.Name, general.graph, mean)
Final.Rank.S <- aggregate(Rank.Models ~ Model.Name, general.graph, sum)
library(plyr)
attach(general.graph)
Final.Rank.C <- count(Rank.Models, "Model.Name")
Final.Rank <- cbind(Final.Rank.S,Final.Rank.C[,2],Final.Rank[,2])
Final.Rank <-  Final.Rank[order(Final.Rank$Rank.Models),]
colnames(Final.Rank) <- c("Model","Sum","Count","Mean")

Variable.Rank <- aggregate(Rank.Models ~ Model.Name + Parameters, general.graph, mean)
Variable.Rank.S <- aggregate(Rank.Models ~ Model.Name + Parameters, general.graph, sum)
library(plyr)
Variable.Rank.C <- count(Rank.Models, c("Model.Name","Parameters"))
Variable.Rank <- cbind(Variable.Rank.S,Variable.Rank.C[,3],Variable.Rank[,3])
colnames(Variable.Rank) <- c("Model","Parameter","Sum","Count","Mean")
Variable.Rank <-  Variable.Rank[order(Variable.Rank$Models),]
Variable.Rank <- Variable.Rank[with(Variable.Rank, order(Parameter, Mean)), ]

library(pgirmess)
kruskalmc(Rank.Models ~ Model.Name, data = Ranking)
library(PMCMR)
posthoc.kruskal.nemenyi.test(x=Ranking$Rank.Models, g=Ranking$Model.Name, method="Chisq")

# RESULT: 3 Groups (CMOM's, CMR-ZPNEc, ZPNEs)

write.csv(Final.Rank, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Final-Rank.csv")
write.csv(Variable.Rank, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Var-Rank.csv")



# SUPERSCRIPT
# labelsX=parse(text='70^o*N')
# xlab = expression(paste("4"^"th"))
# http://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
