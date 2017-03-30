setwd("~/Documents/Results_demography/")
library(ggplot2)
file1 <- read.table("plinkFinalcopy_heterozygosity_singlespace.het", sep=" ", header = T)
diff1 <- c()
#calculate difference between total number of sites and number of homozygous sites
for(i in 1:nrow(file1))
{
  diff1[i] <- file1$N.NM.[i] - file1$O.HOM.[i]
}
diff1 <- as.data.frame(diff1)
het <- c()
#calculate the percentage by dividing difference witht the total number of sites
for(i in 1:nrow(file1))
{
  het[i] <- diff1[i,]/file1$N.NM.[i]
}
het <- as.data.frame(het)
file2 <- cbind(file1$IID, het)
write.table(file2, file = "Genome-wide_het_allinds.txt", sep=" ", quote = F, col.names=F, 
            row.names = F, eol = "\n")
#inorder to plot heterozygosity
names(file2) <- c("ID", "Het")
#file with single column, sorted values of heterozygosity from N to S
file_het_10pop <- read.table("genome-wide_het_10pop.txt", header = F)
file_het_10pop <- as.matrix(file_het_10pop)
#read the file by splitting values of inds by population
#vars were named AtoJ because with original names, R assembles them alphabetically
df <- data.frame(values = file_het_10pop[1:391],
                 vars = rep(c("AK","KD","NBC","CBC","CQ","SBC","NUS","GW","OR","Sierras"), 
                            times = c(4,6,13,17,15,120,129,39,17,31)))
#jpeg gives the best picture
pdf("~/Downloads/Results_demography/Genome-wide_hetero.pdf", width=5, height=7)
colors1 = c("darksalmon","gray48","lightskyblue","saddlebrown","limegreen","khaki3","turquoise1","lightpink","goldenrod", "firebrick")
boxplot(values ~ vars, data=df, outline = T, type="n", axes=F, col=colors1, ylab="Genome-wide heterozygosity", ylim=c(0.05,0.11),pch=16, cex = 0.5)
axis(side=1, at=seq(1, 10, 1), labels=F)
axis(side=2, at=seq(0.01, 0.15, 0.01))
mtext(side = 1, at=c(1,2,3,4,5,6,7,8,9,10), las=2, line = .7, cex=0.7, 
      text = c("ANCHORAGE","Kodiak","N.BC","C.BC","CQ","S.BC","NUS","GW","OR","Sierras")) 
dev.off()
###############################################################################
#####non-parametric test for significance between the groups and in all as well
###ON ALL THE POPULATIONS WITH 391 SAMPLES
kruskal.test(values ~ vars, data = df)
## we found that if considered all populations, it very significant
### but if only few populations are considered, it shows that no significant
##differences between AC and KD, or CBC, NBC, CQ and SBC,DP,GW,OR and CA
df_test <- data.frame(values = file_het_10pop[56:391],
                 vars = rep(c("A","B","C","D","E"), 
                            times = c(120,129,39,17,31)))
kruskal.test(values ~ vars, data = df_test) 
#####################################################
###LOOKING AT THE RESIDUALS THE DISTRIBUTION IS NORMAL
###SO ANOVA CAN BE DONE
data1.anova <- aov(values ~ vars, data=df)
summary(data1.anova)
par(mfrow=c(2,2))
plot(data1.anova, pch = 16, col = "blue", cex = 0.5)
par(mfrow = c(1,1))
##########################
tuckeys.anova1 <- TukeyHSD(data1.anova, conf.level = 0.95, ordered = F)
summary(tuckeys.anova1)
par(cex.axis = 0.5, cex.main = 0.01)
plot(tuckeys.anova1, las = 1)
#######Plotting using ggplot2
library(reshape2)
file_het_10pop <- read.table("genome-wide_het_10pop.txt", header = F)
file_het_10pop <- as.matrix(file_het_10pop)
#read the file by splitting values of inds by population
#vars were named AtoJ because with original names, R assembles them alphabetically
df <- data.frame(values = file_het_10pop[1:391],
                 vars = rep(c("Anchorage","Kodiak","NBC","CBC","CQ","SBC","NUS","GW","OR","Sierras"), 
                            times = c(4,6,13,17,15,120,129,39,17,31)))
names(df) <- c("het", "ID")
colors1 = c("darksalmon","gray48","lightskyblue","saddlebrown","limegreen","khaki3","turquoise1",
            "lightpink","goldenrod","firebrick")
###order them in order to prevent arrangement of variables in alphabetical order
df$ID2 <- factor(df$ID, levels = c("Anchorage", "Kodiak", "NBC", "CBC", "CQ", "SBC", "NUS", "GW", "OR", "Sierras"))
q <- ggplot(df, aes(x = ID2, y = het, fill = ID2))
q + geom_boxplot(outlier.shape=NA) +
  geom_point(position=position_jitter(width=.01, height=0)) +
  scale_fill_manual(values = colors1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.background = element_blank(), legend.position="none", 
        axis.line = element_line(colour = "black", size = 0.1, linetype = "solid")) + 
  xlab("") + 
  ylab("Genome-wide heterozygosity")
###

