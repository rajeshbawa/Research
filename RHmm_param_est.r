###########################################################################
##############Hidden Markov for Continuous data############################
###Using the RHmm package###############################################
###Using this to launch Baum-Welch 1000 times to generate idea of parameter estimates
###The best model parameters are picked to run the final model using Hidden Markov Package
library(data.table)
library(RHmm)
setwd("~/Documents/vcf/vcf_files_per_chr_2016/fst_files")
######read all the files################
filenames = list.files(pattern = "*.weir.fst")
myfiles = lapply(filenames, fread)
###function to calculate zscore
z.score <- function(chr){
  chr$WEIR_AND_COCKERHAM_FST[chr$WEIR_AND_COCKERHAM_FST<0] <- 0
  chr <- na.omit(chr)
  fst.chr <- chr$WEIR_AND_COCKERHAM_FST
  hist(fst.chr) 
  chr.sd <- sd(fst.chr)
  chr.mean <- mean(fst.chr)
  z.chr <- t(as.data.frame(lapply(fst.chr, function(x) (x - chr.mean)/chr.sd)))
  row.names(z.chr) <- NULL
  return(z.chr)
}
############################################
chr1 <- as.data.frame(myfiles[1])
z.chr1 <- z.score(chr1)
##
chr2 <- as.data.frame(myfiles[12])
z.chr2 <- z.score(chr2)
##
chr3 <- as.data.frame(myfiles[13])
z.chr3 <- z.score(chr3)
##
chr4 <- as.data.frame(myfiles[14])
z.chr4 <- z.score(chr4)
##
chr5 <- as.data.frame(myfiles[15])
z.chr5 <- z.score(chr5)
##
chr6 <- as.data.frame(myfiles[16])
z.chr6 <- z.score(chr6)
##
chr7 <- as.data.frame(myfiles[17])
z.chr7 <- z.score(chr7)
##
chr8 <- as.data.frame(myfiles[18])
z.chr8 <- z.score(chr8)
##
chr9 <- as.data.frame(myfiles[19])
z.chr9 <- z.score(chr9)
##
chr10 <- as.data.frame(myfiles[2])
z.chr10 <- z.score(chr10)
##
chr11 <- as.data.frame(myfiles[3])
z.chr11 <- z.score(chr11)
##
chr12 <- as.data.frame(myfiles[4])
z.chr12 <- z.score(chr12)
##
chr13 <- as.data.frame(myfiles[5])
z.chr13 <- z.score(chr13)
##
chr14 <- as.data.frame(myfiles[6])
z.chr14 <- z.score(chr14)
##
chr15 <- as.data.frame(myfiles[7])
z.chr15 <- z.score(chr15)
##
chr16 <- as.data.frame(myfiles[8])
z.chr16 <- z.score(chr16)
##
chr17 <- as.data.frame(myfiles[9])
z.chr17 <- z.score(chr17)
##
chr18 <- as.data.frame(myfiles[10])
z.chr18 <- z.score(chr18)
##
chr19 <- as.data.frame(myfiles[11])
z.chr19 <- z.score(chr19)
######################################################
################
##setting the probabilities
##according to Hoefer and Excofficier
init.prob <- c(0.0, 0.0, 1)
trans.prob <-  matrix(
  c(0.9, 0.05, 0.05,
    0.03, 0.9, 0.07,
    0.05, 0.07, 0.88),
  nrow = 3, byrow = T)
##The results of these constrained probabilities are similar to the ones seen through
##unconstrained 1000 launches of BW algorithm
##so going with the 1000 launches result
############################################
z.chr1.sort <- as.data.frame(sort(z.chr1))
z.chr1.sort1 <- z.chr1.sort[1:41380, ]
z.chr1.sort2 <- z.chr1.sort[41381:82760, ]
z.chr1.sort3 <- z.chr1.sort[82761:124140, ]
###
mean(z.chr1.sort1)
mean(z.chr1.sort2)
mean(z.chr1.sort3)
sd(z.chr1.sort1)
sd(z.chr1.sort2)
sd(z.chr1.sort3)
###
hmm.chr1 <- dthmm(z.chr1, trans.prob, init.prob, distn = "norm", 
                  pm = list(mean=c(-0.61, -0.36, 0.98), sd=c(0.018, 0.12, 1.23)))
a <- bwcontrol(maxiter=1000, tol = 1e-06)
chr1.EM <- BaumWelch(hmm_chr1, control = a)
print(summary(chr1.EM))
###################################################################
hmmfit.chr1 <- HMMFit(z.chr1, dis="NORMAL", nStates=3, 
                       control=list(iter = 1000, verbose = 1, 
                                    init = "RANDOM", nInit = 1000))
summary(hmmfit.chr1)
#####################################
##setting up the parameters for HMM
#hmmset.chr1 <- HMMSet(init.prob, trans.prob, "NORMAL", mean=c(-0.61, -0.36, 0.98),
#                      var=c(0.0003, 0.014, 1.51))
##USER defined HMM fir
#hmmfit.chr1.1 <- HMMFit(z.chr1, dis="NORMAL", nStates=3, 
#                        control=list(iter = 1000, verbose = 1, 
#                                     init = "USER", initPoint = hmmset.chr19))
####
######################################
hmmfit.chr2 <- HMMFit(z.chr2, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr2)
#######
hmmfit.chr3 <- HMMFit(z.chr3, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr3)
#########
hmmfit.chr4 <- HMMFit(z.chr4, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr4)
##########
hmmfit.chr5 <- HMMFit(z.chr5, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr5)
###########
hmmfit.chr6 <- HMMFit(z.chr6, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr6)
############
hmmfit.chr7 <- HMMFit(z.chr7, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr7)
#############
hmmfit.chr8 <- HMMFit(z.chr8, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr8)
##############
hmmfit.chr9 <- HMMFit(z.chr9, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr9)
###############
hmmfit.chr10 <- HMMFit(z.chr10, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr10)
###############
hmmfit.chr11 <- HMMFit(z.chr11, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr11)
###############
hmmfit.chr12 <- HMMFit(z.chr12, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr12)
#################
hmmfit.chr13 <- HMMFit(z.chr13, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr13)
##################
hmmfit.chr14 <- HMMFit(z.chr14, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr14)
###################
hmmfit.chr15 <- HMMFit(z.chr15, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr15)
####################
hmmfit.chr16 <- HMMFit(z.chr16, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr16)
#####################
hmmfit.chr17 <- HMMFit(z.chr17, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr17)
#####################
hmmfit.chr18 <- HMMFit(z.chr18, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr18)
#####################
hmmfit.chr19 <- HMMFit(z.chr19, dis="NORMAL", nStates=3, 
                      control=list(iter = 1000, verbose = 1, 
                                   init = "RANDOM", nInit = 1000))
summary(hmmfit.chr19)
#########


