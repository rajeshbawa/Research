#####create manhattan plots for FST distribution##########
#### this a sample script on a sample data ###########
setwd("/Users/rajesh13/Documents/vcf/vcftools_0.1.12b/bin/")
file1 <- read.table("AC_vs_CA.weir.fst", sep="\t", header=T)
#assign the NaN code as NA and remove NA
file1[file1 == NaN] <- NA
file1 <- na.omit(file1)
#library(qqman)
source("~/Downloads/manhattan_plot.R")
head(file1)
#change the names as required by the package
#CHR is for chromosome, BP is for position and P is for Fst value
names(file1) <- c("CHR", "BP", "P")
#sort the file by the fst value to check the highest value of Fst
file2 <- file1[order(-file1$P),]
#to check number of snps per chromosome
as.data.frame(table(file1$CHR))
#manhattanplot for fst values
#to set the par of 2 rows and 2 columns, for multiple graph plottings
#par( mfrow = c( 2, 2 ) )
#specify ylab and xlab if custom in the argument below
manhattan(file1, suggestiveline=0.5, genomewideline=0.8, logp=F, ylab="Fst")

