#####create manhattan plots for FST distribution##########
#### this a sample script on a sample data ###########
setwd("/Users/rajesh13/Documents/vcf/vcftools_0.1.12b/bin/Fst_withall_thresholds_10kb_sw")
file1 <- read.table("AC_vs_CQ.windowed.weir.fst", sep="\t", header=T)
#file_ref <- read.table("/Users/rajesh13/Documents/vcf/vcftools_0.1.12b/bin/Fst_withall_thresholds_noSW/AC_vs_OR.weir.fst", sep="\t", header=T)
#assign the NaN code as NA and remove NA
file1[file1 == NaN] <- NA
file1 <- na.omit(file1)
#library(qqman)
source("~/Documents/Rscripts/manhattan_plot.R")
head(file1)
#change the names as required by the package
#CHR is for chromosome, BP is for position and P is for Fst value
#select those specific values from file1
file2 <- file1[,c(1,2,5)]
names(file2) <- c("CHR", "BP", "P")
#sort the file by the fst value to check the highest value of Fst
file3 <- file2[order(-file2$P),]
#to check number of snps per chromosome
as.data.frame(table(file1$CHR))
###calculate the genomewide mean
#x1 <- file_ref[,3]
#x1[x1 == NaN] <- NA
#x1 <- na.omit(x1)
#mean.x1 <- mean(x1)
#manhattanplot for fst values
#to set the par of 2 rows and 2 columns, for multiple graph plottings
#par(mfrow = c( 1, 1 ))
#specify ylab and xlab if custom in the argument below
manhattan(file2,  col=c("lightskyblue", "lightskyblue1"), ylim = c(0, 0.8), suggestiveline=0.45, genomewideline=0.15, logp=F, ylab="Fst")
####
###PLOTTING AN EPS FILE
dev.print(device=postscript, "OR_SIERRAS_K50kb_sw_noOverlap.eps", onefile=FALSE, horizontal=FALSE)
dev.off()

