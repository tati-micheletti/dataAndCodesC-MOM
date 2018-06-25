########################################
#                                      #
#   7b. M a k e     G r a p h s        #
#                                      #
#        T R E N D S                   #
#                                      #
########################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-2P.Rdata")

graphP <- graph2
graphP$P.factor <- factor("Stable")

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-2T.Rdata")


graph <- rbind(
  ls.graph2[[1]],
  graphP,
  ls.graph2[[2]])

library(ggplot2)

graph.NB <- subset(graph, Parameters=="Number of Individuals" | Parameters=="Recruitment")
graph.pphi <- subset(graph, !Parameters=="Number of Individuals" & !Parameters=="Recruitment")

new.graph.NB <- subset(graph.NB, graph.NB$Value < 2)

# GRAPH FOR N AND B  
graphics.NB2 <- ggplot(data=new.graph.NB, 
                       aes(x = P.factor, y = Values, shape=Models, size=0.3)) +
  geom_point(position=position_dodge(width=0.8), size=2.7) + 
  scale_shape_manual(values=c(17, 2, 1, 0, 5)) +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(0, 2)) +
  geom_hline(aes(yintercept=0)) +
  geom_vline(xintercept = 1:3, colour="grey", linetype = "longdash") +
  facet_grid(Results ~ Parameters, scales="free_y") +
  labs(x = "Capture and Observation probabilities", y=element_blank()) +
  theme(
    strip.text.x = element_text(size=8, face="bold"),
    strip.text.y = element_text(size=8, face="bold"),
    axis.title.x=element_blank(),
    axis.text.x=element_text(size=7),
    panel.grid.minor=element_blank(),
    panel.grid.major.y=element_blank())

# GRAPH FOR Phi AND Pr
new.graph.pphi <- subset(graph.pphi, graph.pphi$Value < 2)

graphics.pphi2 <- ggplot(data=new.graph.pphi, 
                         aes(x = P.factor, y = Values, shape = Models, size = 0.3)) +
  geom_point(position=position_dodge(width=1), size=2.7) + 
  scale_shape_manual(values=c(17, 2, 1, 0, 5)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme(legend.position="none") +
  geom_hline(aes(yintercept=0)) +
  geom_vline(xintercept = 1:3, colour="grey", linetype = "longdash") +
  facet_grid(Results ~ Parameters, 
             scales="free_y") +
  labs(x = "Capture and Observation probabilities", y=element_blank()) +
  theme(
    strip.text.x = element_text(size=8, face="bold"),
    strip.text.y = element_text(size=8, face="bold"),
    panel.grid.minor=element_blank(),
    axis.text.x=element_text(size=7),
    panel.grid.major.y=element_blank())

save(graphics.pphi2, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/graph-pphi-T2.RData")
save(graphics.NB2, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/graph-NB-T2.RData")

rm(list = ls())

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/graph-pphi-T2.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/graph-NB-T2.RData")

save.image("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/GraphDS-T2.RData")
