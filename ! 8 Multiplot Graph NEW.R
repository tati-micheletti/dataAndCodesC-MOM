
###################################################################
#                                                                 #
# 8. Creating Multiplot Graph for 1P2P, TRENDS and HETERO         #
#                                                                 #
###################################################################

# TRENDS
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/GraphDS-P.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/GraphDS-T2.RData")

library(ggplot2)
library(grid)
library(gridExtra)

png(filename = "C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/GRAPH 1 PART I.png", width = 36, height = 21,
    units = "cm", pointsize = 8, bg = "white", res = 600,
    restoreConsole = TRUE)

grid.arrange(graphics.NB, graphics.NB2, graphics.pphi, graphics.pphi2, ncol = 2)

dev.off()


################################################################

# HETERO

load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/GraphDS-H.RData")
load("C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/GraphDS-2H.RData")

library(ggplot2)
library(grid)
library(gridExtra)

png(filename = "C:/Users/Tati/Dropbox/PhD/Thesis/!! WHOLE DISSERTATION !!/FINAL ANALYSIS/Graphs/GRAPH 2 PART I.png", width = 40, height = 21,
    units = "cm", pointsize = 8, bg = "white", res = 600,
    restoreConsole = TRUE)

grid.arrange(graphics.NB, graphics.NB2, graphics.pphi, graphics.pphi2, ncol = 2)

dev.off()

################################################################