#######potting population growth change############
library(reshape2)
library(ggplot2)
setwd("~/Downloads/")
file1 <- read.table("Ne_estimation_RB_growth_change.txt", header = T, sep = "\t")
names(file1) <- c("Anchorage", "Kodiak", "NBC", "CBC", "CQ", "SBC", "NUS", "GW", "OR", "Sierras")
df <- melt(file1)
colors1 = c("darksalmon","gray48","lightskyblue","saddlebrown","limegreen","khaki3","turquoise1",
            "lightpink","goldenrod","firebrick")
p <- ggplot(df, aes(x = variable, y = value, fill = variable))
p + geom_bar(stat = "identity") + scale_fill_manual(values = colors1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.background = element_blank(), legend.position="none", 
        axis.line = element_line(colour = "black", size = 0.2, linetype = "solid")) +
  xlab("") +
  ylab("Change in Ne (% per generation)")
#####################################################
###plotting the harmonic mean of Ne
file2 <- read.table("Ne_estimation_RB_harMean_all_and_chr.txt", header = T, sep = "\t")
df2 <- melt(file2)
#####reorder the variables to prevent alphabetical sorting
df2$POP2 <- factor(df2$POP, levels = c("ANCHORAGE", "KODIAK", "NBC", "CBC", "CQ", "SBC", "NUS", "GW", "OR", "SIERRAS"))
q <- ggplot(df2, aes(x = POP2, y = value, fill = POP2))
q + geom_boxplot(outlier.shape=NA) +
  geom_point(position=position_jitter(width=.01, height=0)) +
  scale_fill_manual(values = colors1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.background = element_blank(), legend.position="none", 
        axis.line = element_line(colour = "black", size = 0.1, linetype = "solid")) + 
  xlab("") + 
  ylab("Harmonic Ne Mean")
      
