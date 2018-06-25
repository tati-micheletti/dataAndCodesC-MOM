##################################################################
#                                                                #
# 11. Creating CI  Graph for  P2P,  TRENDS  and  HETERO          #
#                                                                #
##################################################################

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Confidence Interval/CI-T.RData")

CI.graph <- rbind(CI.P, CI.T, CI.H)

for (i in 1:nrow(CI.graph)){
  if (CI.graph$Models[i]=="C-MOMd-1P"|
      CI.graph$Models[i]=="C-MOMd-1T"|
      CI.graph$Models[i]=="C-MOMd-1H") CI.graph$Model.Name[i] <- "C-MOMd-1P"
  if (CI.graph$Models[i]=="C-MOMd-2P"|
      CI.graph$Models[i]=="C-MOMd-2T"|
      CI.graph$Models[i]=="C-MOMd-2H") CI.graph$Model.Name[i] <- "C-MOMd-2P"
  
  if (CI.graph$Models[i]=="C-MOMi-1P"|
      CI.graph$Models[i]=="C-MOMi-1T"|
      CI.graph$Models[i]=="C-MOMi-1H") CI.graph$Model.Name[i] <- "C-MOMi-1P"
  if (CI.graph$Models[i]=="C-MOMi-2P"|
      CI.graph$Models[i]=="C-MOMi-2T"|
      CI.graph$Models[i]=="C-MOMi-2H") CI.graph$Model.Name[i] <- "C-MOMi-2P"
  
  if (CI.graph$Models[i]=="CMR-1P"|
      CI.graph$Models[i]=="CMR-1T"|
      CI.graph$Models[i]=="CMR-1H") CI.graph$Model.Name[i] <- "CMR-1P"
  if (CI.graph$Models[i]=="CMR-2P"|
      CI.graph$Models[i]=="CMR-2T"|
      CI.graph$Models[i]=="CMR-2H") CI.graph$Model.Name[i] <- "CMR-2P"
  
  if (CI.graph$Models[i]=="ZPNEc") CI.graph$Model.Name[i] <- "ZPNEc"
  if (CI.graph$Models[i]=="ZPNEs") CI.graph$Model.Name[i] <- "ZPNEs"
}

CI.graph$Model.Name_o <- factor(CI.graph$Model.Name, 
                                        levels=c("C-MOMi-1P",
                                                 "C-MOMd-1P",
                                                 "C-MOMd-2P",
                                                 "C-MOMi-2P", 
                                                 "CMR-2P", 
                                                 "CMR-1P", 
                                                 "ZPNEc", 
                                                 "ZPNEs"))

CI.graph$Parameters_o <- factor(CI.graph$Parameters, 
                                levels=c("Number of Individuals",
                                         "Recruitment",
                                         "Survival Prim. Occ.",
                                         "Survival Second. Occ.", 
                                         "Observation probability", 
                                         "Capture probability"))

graphics <- ggplot(data=CI.graph,
                      aes(x = Model.Name_o, y = Percentage, fill=Model.Name_o)) +
  geom_bar(stat="identity") +
  theme() +
  facet_grid(Factor ~ Parameters_o)+
  labs(y="%") +
  theme(
    legend.position="bottom",
    legend.title = element_blank(),
    strip.text.x = element_text(size=8, face="bold"),
    strip.text.y = element_text(size=8, face="bold"),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x =element_blank(),
    axis.text.y=element_text(size=7),
    panel.grid.minor=element_blank(),
    panel.grid.major.y=element_blank()) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE))

png(filename = "C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/GRAPH CI.png", width = 21, height = 29,
    units = "cm", pointsize = 10, bg = "white", res = 600,
    restoreConsole = TRUE)

graphics

dev.off()


#######################################

# SPECIFIC PERCENTAGES INVESTIGATION

#######################################

# CMOM and CMR

t.perc.CMOM <- CI.graph[CI.graph$Model.Name=="C-MOMd-1P"|CI.graph$Model.Name=="C-MOMd-2P"|CI.graph$Model.Name=="C-MOMi-1P"|CI.graph$Model.Name=="C-MOMi-2P",]

t.perc.CMOM <- t.perc.CMOM[,c(1,4,5)]
t.perc.CMOM <- t.perc.CMOM[!is.na(t.perc.CMOM$Percentage),]

t.perc.CMOM$Form <- NA
t.perc.CMOM$Dep <- NA
for (i in 1:nrow(t.perc.CMOM)){
  ifelse(t.perc.CMOM$Model.Name[i]=="C-MOMd-1P"|t.perc.CMOM$Model.Name[i]=="C-MOMi-1P", t.perc.CMOM$Form[i] <- "1P",t.perc.CMOM$Form[i] <- "2P")
}

for (i in 1:nrow(t.perc.CMOM)){
  ifelse(t.perc.CMOM$Model.Name[i]=="C-MOMd-1P"|t.perc.CMOM$Model.Name[i]=="C-MOMd-2P", t.perc.CMOM$Dep[i] <- "D",t.perc.CMOM$Dep[i] <- "I")
}

t.perc.d1P <- subset(t.perc.CMOM, t.perc.CMOM$Model.Name=="C-MOMd-1P")
t.perc.i1P <- subset(t.perc.CMOM, t.perc.CMOM$Model.Name=="C-MOMi-1P")
t.perc.d2P <- subset(t.perc.CMOM, t.perc.CMOM$Model.Name=="C-MOMd-2P")
t.perc.i2P <- subset(t.perc.CMOM, t.perc.CMOM$Model.Name=="C-MOMi-2P")

t.perc.1P <- subset(t.perc.CMOM, t.perc.CMOM$Form=="1P")
t.perc.2P <- subset(t.perc.CMOM, t.perc.CMOM$Form=="2P")

mean(t.perc.1P$Percentage, na.rm=T)
mean(t.perc.2P$Percentage, na.rm=T)


t.perc.ZPNEc.N.p2 <- subset(CI.graph, c(CI.graph$Models=="ZPNEc"&!CI.graph$Parameters=="Survival Prim. Occ."))
mean(t.perc.ZPNEc.N.p2$Percentage, na.rm = T)

library(pgirmess)
kruskalmc(Percentage ~ Form, data = t.perc)
kruskalmc(Percentage ~ Dep, data = t.perc)

t.perc2 <- t.perc[!is.na(t.perc$Percentage),]
aggregate(t.perc2[,2],list(t.perc2$Form, t.perc2$Dep), mean)
aggregate(t.perc2[,2],list(t.perc2$Form), mean)

kruskalmc(Percentage ~ Form, data = t.perc.CMOM)
kruskalmc(Percentage ~ Dep, data = t.perc.CMOM)

t.perc.CMOM2 <- t.perc.CMOM[!is.na(t.perc.CMOM$Percentage),]
aggregate(t.perc.CMOM2[,2],list(t.perc.CMOM2$Form, t.perc.CMOM2$Dep), mean)
aggregate(t.perc.CMOM2[,2],list(t.perc.CMOM2$Form), mean)

# ZPNEc and ZPNEs


perc.ZPNEc <- CI.graph[CI.graph$Model.Name=="ZPNEc",]
perc.ZPNEc <- perc.ZPNEc[,c(1,4,5)]
perc.ZPNEc <- perc.ZPNEc[!is.na(perc.ZPNEc$Percentage),]
mean(perc.ZPNEc$Percentage)
perc.ZPNEc.Np2 <- perc.ZPNEc[perc.ZPNEc$Parameters=="Number of Individuals"|perc.ZPNEc$Parameters=="Observation probability",]
mean(perc.ZPNEc.Np2$Percentage)

perc.ZPNEs <- CI.graph[CI.graph$Model.Name=="ZPNEs",]
perc.ZPNEs <- perc.ZPNEs[,c(1,4,5)]
perc.ZPNEs <- perc.ZPNEs[!is.na(perc.ZPNEs$Percentage),]
mean(perc.ZPNEs$Percentage)
perc.ZPNEs.Np2 <- perc.ZPNEs[perc.ZPNEs$Parameters=="Number of Individuals"|perc.ZPNEs$Parameters=="Observation probability",]
mean(perc.ZPNEs.Np2$Percentage)

perc.ZPNE <- rbind(perc.ZPNEc, perc.ZPNEs)
kruskalmc(Percentage ~ Model.Name, data = perc.ZPNE)

# CMR

perc.CMR <- CI.graph[CI.graph$Model.Name=="CMR-1P",]
perc.CMR <- perc.CMR[,c(1,4,5)]
perc.CMR <- perc.CMR[!is.na(perc.CMR$Percentage),]
mean(perc.CMR$Percentage)

perc.CMR2 <- CI.graph[CI.graph$Model.Name=="CMR-2P",]
perc.CMR2 <- perc.CMR2[,c(1,4,5)]
perc.CMR2 <- perc.CMR2[!is.na(perc.CMR2$Percentage),]
mean(perc.CMR2$Percentage)

percCMR <- rbind(perc.CMR, perc.CMR2)
kruskalmc(Percentage ~ Model.Name, data = percCMR)

# ALL TOGETHER

perc.all <- rbind(t.perc.CMOM, percCMR, perc.ZPNE)
kruskalmc(Percentage ~ Model.Name, data = perc.all) # DOES NOT WORK WELL.
