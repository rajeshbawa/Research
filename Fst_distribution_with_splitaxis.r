#####create manhattan plots for FST distribution##########
#### this a sample script on a sample data ###########
setwd("/Users/rajesh13/Documents/vcf/vcf_demogP_10/Fst_withmafonly")
file1 <- read.table("allPOP_10demogP.weir.fst", sep="\t", header=T)
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
file2 <- file1
names(file2) <- c("CHR", "BP", "P")
#sort the file by the fst value to check the highest value of Fst
file3 <- file2[order(-file2$P),]
F <- file3$P
#count the range of Fst values
lowest_range1 <- sum(F > 0 & F <= 0.1)
lowest_range2 <- sum(F > 0.1 & F <= 0.2)
lowest_range3 <- sum(F > 0.2 & F <= 0.3)
lowest_range4 <- sum(F > 0.3 & F <= 0.4)
lowest_range5 <- sum(F > 0.4 & F <= 0.5)
lowest_range6 <- sum(F > 0.5 & F <= 0.6)
lowest_range7 <- sum(F > 0.6 & F <= 0.7)
lowest_range8 <- sum(F > 0.7 & F <= 0.8)
lowest_range9 <- sum(F > 0.8 & F <= 0.9)
lowest_range10 <- sum(F > 0.9 & F <= 0.1)
lowest_range11 <- sum(F = 1)
lrange7 <- lowest_range7 + lowest_range8 + lowest_range9 + lowest_range10 + lowest_range11
Fst_range <- c(lowest_range1/1000, lowest_range2/1000, lowest_range3/1000, lowest_range4/1000, lowest_range5/1000, lowest_range6/1000, lrange7/1000)
#plotting these range values
library(plotrix)
par(bty="n") # deleting the box
lables_x = c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6", "0.7-1.0")
gap.barplot(Fst_range, gap=c(100,450), xaxlab= lables_x, ytics=c(0,25,50,75,100,500,600,700), xlab="",
            ylab= "No. of SNPs (x1000)", xaxt="n")
axis(1, at = c(1,2,3,4,5,6,7), labels=lables_x, las=2)
axis(2, at = c(25,50,75,100,500,600,700), labels=c(25,50,75,100,500,600,700), las=1)
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


