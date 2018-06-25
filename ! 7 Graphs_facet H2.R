########################################
#                                      #
#   7c. M a k e     G r a p h s        #
#                                      #
#        H E T E R O                   #
#                                      #
########################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/Graph-2H.Rdata")

graph <- rbind(ls.graph2[[1]], 
               ls.graph2[[2]],
               ls.graph2[[3]], 
               ls.graph2[[4]],
               ls.graph2[[5]],
               ls.graph2[[6]])

library(ggplot2)

graph.NB <- subset(graph, Parameters=="Number of Individuals" | Parameters=="Recruitment")
graph.pphi <- subset(graph, !Parameters=="Number of Individuals" & !Parameters=="Recruitment")

new.graph.NB <- subset(graph.NB, graph.NB$Value < 2)

# GRAPH FOR N AND B  
graphics.NB2 <- ggplot(data=new.graph.NB, 
                      aes(x = P.factor, y = Values, shape=Models, size=0.3)) +
  geom_point(position=position_dodge(width=1), size=2.6) + 
  scale_shape_manual(values=c(17, 2, 1, 0, 5)) +
  scale_y_continuous(limits = c(0, 2)) +
  theme(legend.position="none") +
  geom_hline(aes(yintercept=0)) +
  geom_vline(xintercept = 1:6, colour="grey", linetype = "longdash") +
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
  geom_point(position=position_dodge(width=1), size=2.6) + 
  scale_shape_manual(values=c(17, 2, 1, 0, 5)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme(legend.position="none") +
  geom_hline(aes(yintercept=0)) +
  geom_vline(xintercept = 1:6, colour="grey", linetype = "longdash") +
  facet_grid(Results ~ Parameters, 
             scales="free_y") +
  labs(x = "Capture and Observation probabilities", y=element_blank()) +
  theme(
    strip.text.x = element_text(size=8, face="bold"),
    strip.text.y = element_text(size=8, face="bold"),
    panel.grid.minor=element_blank(),
    axis.text.x=element_text(size=7),
    panel.grid.major.y=element_blank())

save(graphics.pphi2, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/graph-pphi-2H.RData")
save(graphics.NB2, file="C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/graph-NB-2H.RData")

rm(list = ls())

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/graph-pphi-2H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/graph-NB-2H.RData")

save.image("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/GraphDS-2H.RData")