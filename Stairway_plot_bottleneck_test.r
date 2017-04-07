#############################################################################
##Stairway plot statistical test to find difference between two bottlenecks
library(agricolae)
setwd("~/Documents/Results_demography/Demography_paper_plots/Stairway_plot")
file1 <- read.table("Stairway_plot_bottleneck_file.txt", header = T, sep = "\t")
lapply(file1$POP, as.factor) ##convert names to factors
file1.anova <- aov(FIRST_BN ~ POP, data = file1)
summary(file1.anova)
########
par(mfrow = c(2,2))
plot(file1.anova, pch = 16, col = "blue", cex = 0.5)
par(mfrow = c(1,1))
#########
file1.tuckeys <- TukeyHSD(file1.anova, conf.level = 0.95, ordered = F)
summary(file1.tuckeys)
par(cex.axis = 0.5, cex.main = 0.01)
plot(file1.tuckeys, las = 1)
###########################################################
file2.anova <- aov(SEC_BN ~ POP, data = file1)
summary(file2.anova)
######
par(mfrow = c(2,2))
plot(file2.anova, pch = 16, col = "blue", cex = 0.5)
par(mfrow = c(1,1))
#######
file2.tuckeys <- TukeyHSD(file2.anova, conf.level = 0.95, ordered = F)
summary(file2.tuckeys)
par(cex.axis = 0.5, cex.main = 0.01)
plot(file2.tuckeys, las = 1)
######################################################