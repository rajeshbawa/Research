#########################################################################
###Hidden Markov Model for calculating the Fst hidden####################
library(data.table)
library(HiddenMarkov)
library(PLIS)
setwd("~/Documents/vcf/vcf_files_per_chr_2016/fst_files")
############################################
filenames = list.files(pattern = "*.weir.fst")
myfiles = lapply(filenames, fread)
###function to calculate zscore
z.score <- function(chr){
  chr$WEIR_AND_COCKERHAM_FST[chr$WEIR_AND_COCKERHAM_FST<0] <- 0
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
##setting up the probabilities
##using the parameters estimated from the 1000 run BWA for final model on RHmm
##using initial and transition probabilities
init.prob1 <- c(1.940e-112, 2.329e-94, 1.000e+00)
trans.prob1 <- matrix(
  c(3.402e-01, 4.209e-01, 2.390e-01,
    2.506e-01, 5.011e-01, 2.484e-01,
    2.640e-01, 4.530e-01, 2.830e-01),
  nrow = 3, ncol = 3, byrow = T)
######################
init.prob2 <- c(2.403e-100, 2.676e-76, 1.000e+00)
trans.prob2 <- matrix(
  c(3.648e-01, 4.039e-01, 2.313e-01,
    2.825e-01, 4.768e-01, 2.407e-01,
    3.021e-01, 4.389e-01, 2.590e-01),
  nrow = 3, ncol = 3, byrow = T)
#######################
init.prob3 <- c(0, 4.537e-28, 1)
trans.prob3 <- matrix(
  c(2.895e-01, 2.481e-01, 4.624e-01,
    2.378e-01, 3.517e-01, 4.105e-01,
    2.547e-01, 2.369e-01, 5.085e-01),
  nrow = 3, ncol = 3, byrow = T)
######
init.prob4 <- c(0, 2.585e-179, 1)
trans.prob4 <- matrix(
  c(2.730e-01, 4.526e-01, 2.744e-01,
    2.367e-01, 5.035e-01, 2.598e-01,
    2.372e-01, 4.202e-01, 3.426e-01),
  nrow = 3, ncol = 3, byrow = T)
######
init.prob5 <- c(0, 1.866e-31, 1)
trans.prob5 <- matrix(
  c(2.628e-01, 2.848e-01, 4.525e-01,
    2.420e-01, 3.551e-01, 4.029e-01,
    2.409e-01, 2.607e-01, 4.984e-01),
  nrow = 3, ncol = 3, byrow = T)
#######
init.prob6 <- c(0, 2.622e-27, 1)
trans.prob6 <- matrix(
  c(2.701e-01, 2.764e-01, 4.535e-01,
    2.296e-01, 3.441e-01, 4.263e-01,
    2.454e-01, 2.637e-01, 4.909e-01),
  nrow = 3, ncol = 3, byrow = T)
#######
init.prob7 <- c(0, 0, 1)
trans.prob7 <- matrix(
  c(2.677e-01, 4.648e-01, 2.675e-01,
    2.423e-01, 5.091e-01, 2.486e-01,
    2.354e-01, 4.134e-01, 3.512e-01),
  nrow = 3, ncol = 3, byrow = T)
########
init.prob8 <- c(0, 3.998e-35, 1)
trans.prob8 <- matrix(
  c(2.695e-01, 2.648e-01, 4.657e-01,
    2.350e-01, 3.425e-01, 4.225e-01,
    2.406e-01, 2.514e-01, 5.079e-01),
  nrow = 3, ncol = 3, byrow = T)
#########
init.prob9 <- c(0, 9.478e-220, 1)
trans.prob9 <- matrix(
  c(2.645e-01, 4.750e-01, 2.605e-01,
    2.453e-01, 5.010e-01, 2.537e-01,
    2.346e-01, 4.134e-01, 3.520e-01),
  nrow = 3, ncol = 3, byrow = T)
#########
init.prob10 <- c(0, 8.137e-41, 1)
trans.prob10 <- matrix(
  c(2.671e-01, 2.850e-01, 4.479e-01,
    2.358e-01, 3.596e-01, 4.046e-01,
    2.441e-01, 2.687e-01, 4.872e-01),
  nrow = 3, ncol = 3, byrow = T)
#########
init.prob11 <- c(2.033e-93, 1.180e-69, 1.000e+00)
trans.prob11 <- matrix(
  c(3.734e-01, 3.978e-01, 2.288e-01,
    2.496e-01, 5.037e-01, 2.467e-01,
    2.730e-01, 4.563e-01, 2.707e-01),
  nrow = 3, ncol = 3, byrow = T)
#########
init.prob12 <- c(0, 4.352e-33, 1)
trans.prob12 <- matrix(
  c(2.801e-01, 2.415e-01, 4.784e-01,
    2.367e-01, 3.477e-01, 4.157e-01,
    2.465e-01, 2.315e-01, 5.220e-01),
  nrow = 3, ncol = 3, byrow = T)
#########
init.prob13 <- c(7.108e-112, 7.808e-85, 1.000e+00)
trans.prob13 <- matrix(
  c(3.384e-01, 4.345e-01, 2.271e-01,
    2.553e-01, 5.076e-01, 2.372e-01,
    2.636e-01, 4.777e-01, 2.587e-01),
  nrow = 3, ncol = 3, byrow = T)
#########
init.prob14 <- c(0, 1.224e-97, 1)
trans.prob14 <- matrix(
  c(2.733e-01, 4.358e-01, 2.910e-01,
    2.409e-01, 4.860e-01, 2.731e-01,
    2.367e-01, 4.006e-01, 3.627e-01),
  nrow = 3, ncol = 3, byrow = T)
##########
init.prob15 <- c(0, 0, 1)
trans.prob15 <- matrix(
  c(2.671e-01, 4.654e-01, 2.676e-01,
    2.365e-01, 5.208e-01, 2.427e-01,
    2.177e-01, 4.296e-01, 3.527e-01),
  nrow = 3, ncol = 3, byrow = T)
##########
init.prob16 <- c(0, 5.224e-11, 1)
trans.prob16 <- matrix(
  c(2.616e-01, 4.700e-01, 2.684e-01,
    2.343e-01, 5.193e-01, 2.464e-01,
    2.287e-01, 4.218e-01, 3.495e-01),
  nrow = 3, ncol = 3, byrow = T)
##########
init.prob17 <- c(0, 5.679e-49, 1)
trans.prob17 <- matrix(
  c(2.804e-01, 4.758e-01, 2.438e-01,
    2.446e-01, 5.327e-01, 2.227e-01,
    2.355e-01, 4.273e-01, 3.372e-01),
  nrow = 3, ncol = 3, byrow = T)
###########
init.prob18 <- c(0, 1.445e-32, 1)
trans.prob18 <- matrix(
  c(2.782e-01, 2.497e-01, 4.721e-01,
    2.377e-01, 3.255e-01, 4.368e-01,
    2.454e-01, 2.319e-01, 5.227e-01),
  nrow = 3, ncol = 3, byrow = T)
##########
init.prob19 <- c(1.877e-86, 1.192e-66, 1)
trans.prob19 <- matrix(
  c(3.499e-01, 4.139e-01, 2.362e-01,
    2.205e-01, 5.260e-01, 2.535e-01,
    2.356e-01, 4.756e-01, 2.888e-01),
  nrow = 3, ncol = 3, byrow = T)
#############################################
###calculate the mean and sd for the each chromosome hidden state
##chr1
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
###build the hmm model
HMM_chr1 <- dthmm(z.chr1, trans.prob1, init.prob1, distn = "norm", 
                  pm = list(mean=c(-0.62, -0.36, 0.98), sd=c(0.019, 0.116, 1.23)))
a <- bwcontrol(maxiter=1000, tol = 1e-06)
chr1.EM <- BaumWelch(HMM_chr1, control = a)
print(summary(chr1.EM))
states.chr1 <- Viterbi(HMM_chr1)
states.chr1.1 <- factor(states.chr1, levels=1:3)
###
chr1.1 <- chr1[(which(states.chr1.1==1)), ]
chr1.2 <- chr1[(which(states.chr1.1==2)), ]
chr1.3 <- chr1[(which(states.chr1.1==3)), ]
chr1.z.1 <- z.chr1[(which(states.chr1.1==1)), ]
chr1.z.2 <- z.chr1[(which(states.chr1.1==2)), ]
chr1.z.3 <- z.chr1[(which(states.chr1.1==3)), ]
###
em.chr1.1 <- em.hmm(chr1.z.1, L = 2)
em.chr1.3 <- em.hmm(chr1.z.3, L = 2)
plis.chr1.1 <- plis(em.chr1.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr1.3 <- plis(em.chr1.3$LIS, fdr = 0.001, adjust = TRUE)
######################################################
z.chr2.sort <- as.data.frame(sort(z.chr2))
z.chr2.1 <- z.chr2.sort[1:21185, ]
z.chr2.2 <- z.chr2.sort[21186:42371, ]
z.chr2.3 <- z.chr2.sort[42372:63557, ]
mean(z.chr2.1)
mean(z.chr2.2)
mean(z.chr2.3)
sd(z.chr2.1)
sd(z.chr2.2)
sd(z.chr2.3)
#####build the hmm model
###we assume normal distribution and use EM algorithm with 1000 iterations to find parameters
HMM_chr2 <- dthmm(z.chr2, trans.prob2, init.prob2, distn = "norm", 
                  pm = list(mean=c(-0.62, -0.37, 1.00), sd=c(0.019, 0.106, 1.21)))
chr2.EM <- BaumWelch(HMM_chr2, control = a)
print(summary(chr2.EM))
states.chr2 <- Viterbi(HMM_chr2)
states.chr2.1 <- factor(states.chr2, levels=1:3)
###
chr2.1 <- chr2[(which(states.chr2.1==1)), ]
chr2.2 <- chr2[(which(states.chr2.1==2)), ]
chr2.3 <- chr2[(which(states.chr2.1==3)), ]
chr2.z.1 <- z.chr2[(which(states.chr2.1==1)), ]
chr2.z.2 <- z.chr2[(which(states.chr2.1==2)), ]
chr2.z.3 <- z.chr2[(which(states.chr2.1==3)), ]
###
em.chr2.1 <- em.hmm(chr2.z.1, L = 2)
em.chr2.3 <- em.hmm(chr2.z.3, L = 2)
plis.chr2.1 <- plis(em.chr2.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr2.3 <- plis(em.chr2.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr3.sort <- as.data.frame(sort(z.chr3))
z.chr3.1 <- z.chr3.sort[1:19601, ]
z.chr3.2 <- z.chr3.sort[19602:39203, ]
z.chr3.3 <- z.chr3.sort[39204:58804, ]
mean(z.chr3.1)
mean(z.chr3.2)
mean(z.chr3.3)
sd(z.chr3.1)
sd(z.chr3.2)
sd(z.chr3.3)
HMM_chr3 <- dthmm(z.chr3, trans.prob3, init.prob3, distn = "norm", 
                  pm = list(mean=c(-0.62, -0.36, 0.98), sd=c(0.017, 0.12, 1.23)))
chr3.EM <- BaumWelch(HMM_chr3, control = a)
print(summary(chr3.EM))
states.chr3 <- Viterbi(HMM_chr3)
states.chr3.1 <- factor(states.chr3, levels=1:3)
###
chr3.1 <- chr3[(which(states.chr3.1==1)), ]
chr3.2 <- chr3[(which(states.chr3.1==2)), ]
chr3.3 <- chr3[(which(states.chr3.1==3)), ]
chr3.z.1 <- z.chr3[(which(states.chr3.1==1)), ]
chr3.z.2 <- z.chr3[(which(states.chr3.1==2)), ]
chr3.z.3 <- z.chr3[(which(states.chr3.1==3)), ]
###
em.chr3.1 <- em.hmm(chr3.z.1, L = 2)
em.chr3.3 <- em.hmm(chr3.z.3, L = 2)
plis.chr3.1 <- plis(em.chr3.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr3.3 <- plis(em.chr3.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr4.sort <- as.data.frame(sort(z.chr4))
z.chr4.1 <- z.chr4.sort[1:20133, ]
z.chr4.2 <- z.chr4.sort[20134:40266, ]
z.chr4.3 <- z.chr4.sort[40267:60399, ]
mean(z.chr4.1)
mean(z.chr4.2)
mean(z.chr4.3)
sd(z.chr4.1)
sd(z.chr4.2)
sd(z.chr4.3)
HMM_chr4 <- dthmm(z.chr4, trans.prob4, init.prob4, distn = "norm", 
                  pm = list(mean=c(-0.64, -0.36, 1.0), sd=c(0.021, 0.118, 1.19)))
chr4.EM <- BaumWelch(HMM_chr4, control = a)
print(summary(chr4.EM))
states.chr4 <- Viterbi(HMM_chr4)
states.chr4.1 <- factor(states.chr4, levels=1:3)
############
chr4.1 <- chr4[(which(states.chr4.1==1)), ]
chr4.2 <- chr4[(which(states.chr4.1==2)), ]
chr4.3 <- chr4[(which(states.chr4.1==3)), ]
chr4.z.1 <- z.chr4[(which(states.chr4.1==1)), ]
chr4.z.2 <- z.chr4[(which(states.chr4.1==2)), ]
chr4.z.3 <- z.chr4[(which(states.chr4.1==3)), ]
####
em.chr4.1 <- em.hmm(chr4.z.1, L = 2)
em.chr4.3 <- em.hmm(chr4.z.3, L = 2)
plis.chr4.1 <- plis(em.chr4.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr4.3 <- plis(em.chr4.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr5.sort <- as.data.frame(sort(z.chr5))
z.chr5.1 <- z.chr5.sort[1:22598, ]
z.chr5.2 <- z.chr5.sort[22599:45196, ]
z.chr5.3 <- z.chr5.sort[45197:67794, ]
mean(z.chr5.1)
mean(z.chr5.2)
mean(z.chr5.3)
sd(z.chr5.1)
sd(z.chr5.2)
sd(z.chr5.3)
HMM_chr5 <- dthmm(z.chr5, trans.prob5, init.prob5, distn = "norm", 
                  pm = list(mean=c(-0.63, -0.36, 0.998), sd=c(0.021, 0.121, 1.204)))
chr5.EM <- BaumWelch(HMM_chr5, control = a)
print(summary(chr5.EM))
states.chr5 <- Viterbi(HMM_chr5)
states.chr5.1 <- factor(states.chr5, levels=1:3)
###
chr5.1 <- chr5[(which(states.chr5.1==1)), ]
chr5.2 <- chr5[(which(states.chr5.1==2)), ]
chr5.3 <- chr5[(which(states.chr5.1==3)), ]
chr5.z.1 <- z.chr5[(which(states.chr5.1==1)), ]
chr5.z.2 <- z.chr5[(which(states.chr5.1==2)), ]
chr5.z.3 <- z.chr5[(which(states.chr5.1==3)), ]
###
em.chr5.1 <- em.hmm(chr5.z.1, L = 2)
em.chr5.3 <- em.hmm(chr5.z.3, L = 2)
plis.chr5.1 <- plis(em.chr5.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr5.3 <- plis(em.chr5.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr6.sort <- as.data.frame(sort(z.chr6))
z.chr6.1 <- z.chr6.sort[1:25057, ]
z.chr6.2 <- z.chr6.sort[25058:50114, ]
z.chr6.3 <- z.chr6.sort[50115:75171, ]
mean(z.chr6.1)
mean(z.chr6.2)
mean(z.chr6.3)
sd(z.chr6.1)
sd(z.chr6.2)
sd(z.chr6.3)
HMM_chr6 <- dthmm(z.chr6, trans.prob6, init.prob6, distn = "norm", 
                  pm = list(mean=c(-0.59, -0.36, 0.95), sd=c(0.017, 0.099, 1.267)))
chr6.EM <- BaumWelch(HMM_chr6, control = a)
print(summary(chr6.EM))
states.chr6 <- Viterbi(HMM_chr6)
states.chr6.1 <- factor(states.chr6, levels=1:3)
###
chr6.1 <- chr6[(which(states.chr6.1==1)), ]
chr6.2 <- chr6[(which(states.chr6.1==2)), ]
chr6.3 <- chr6[(which(states.chr6.1==3)), ]
chr6.z.1 <- z.chr6[(which(states.chr6.1==1)), ]
chr6.z.2 <- z.chr6[(which(states.chr6.1==2)), ]
chr6.z.3 <- z.chr6[(which(states.chr6.1==3)), ]
###
em.chr6.1 <- em.hmm(chr6.z.1, L = 2)
em.chr6.3 <- em.hmm(chr6.z.3, L = 2)
plis.chr6.1 <- plis(em.chr6.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr6.3 <- plis(em.chr6.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr7.sort <- as.data.frame(sort(z.chr7))
z.chr7.1 <- z.chr7.sort[1:14041, ]
z.chr7.2 <- z.chr7.sort[14042:28082, ]
z.chr7.3 <- z.chr7.sort[28083:42124, ]
mean(z.chr7.1)
mean(z.chr7.2)
mean(z.chr7.3)
sd(z.chr7.1)
sd(z.chr7.2)
sd(z.chr7.3)
HMM_chr7 <- dthmm(z.chr7, trans.prob7, init.prob7, distn = "norm", 
                  pm = list(mean=c(-0.624, -0.368, 0.993), sd=c(0.020, 0.113, 1.214)))
chr7.EM <- BaumWelch(HMM_chr7, control = a)
print(summary(chr7.EM))
states.chr7 <- Viterbi(HMM_chr7)
states.chr7.1 <- factor(states.chr7, levels=1:3)
####
chr7.1 <- chr7[(which(states.chr7.1==1)), ]
chr7.2 <- chr7[(which(states.chr7.1==2)), ]
chr7.3 <- chr7[(which(states.chr7.1==3)), ]
chr7.z.1 <- z.chr7[(which(states.chr7.1==1)), ]
chr7.z.2 <- z.chr7[(which(states.chr7.1==2)), ]
chr7.z.3 <- z.chr7[(which(states.chr7.1==3)), ]
####
em.chr7.1 <- em.hmm(chr7.z.1, L = 2)
em.chr7.3 <- em.hmm(chr7.z.3, L = 2)
plis.chr7.1 <- plis(em.chr7.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr7.3 <- plis(em.chr7.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr8.sort <- as.data.frame(sort(z.chr8))
z.chr8.1 <- z.chr8.sort[1:21095, ]
z.chr8.2 <- z.chr8.sort[21096:42190, ]
z.chr8.3 <- z.chr8.sort[42191:63286, ]
mean(z.chr8.1)
mean(z.chr8.2)
mean(z.chr8.3)
sd(z.chr8.1)
sd(z.chr8.2)
sd(z.chr8.3)
HMM_chr8 <- dthmm(z.chr8, trans.prob8, init.prob8, distn = "norm", 
                  pm = list(mean=c(-0.64, -0.36, 1.0), sd=c(0.021, 0.119, 1.202)))
chr8.EM <- BaumWelch(HMM_chr8, control = a)
print(summary(chr8.EM))
states.chr8 <- Viterbi(HMM_chr8)
states.chr8.1 <- factor(states.chr8, levels=1:3)
###
chr8.1 <- chr8[(which(states.chr8.1==1)), ]
chr8.2 <- chr8[(which(states.chr8.1==2)), ]
chr8.3 <- chr8[(which(states.chr8.1==3)), ]
chr8.z.1 <- z.chr8[(which(states.chr8.1==1)), ]
chr8.z.2 <- z.chr8[(which(states.chr8.1==2)), ]
chr8.z.3 <- z.chr8[(which(states.chr8.1==3)), ]
######
em.chr8.1 <- em.hmm(chr8.z.1, L = 2)
em.chr8.3 <- em.hmm(chr8.z.3, L = 2)
plis.chr8.1 <- plis(em.chr8.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr8.3 <- plis(em.chr8.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr9.sort <- as.data.frame(sort(z.chr9))
z.chr9.1 <- z.chr9.sort[1:15030, ]
z.chr9.2 <- z.chr9.sort[15031:30060, ]
z.chr9.3 <- z.chr9.sort[30061:45091, ]
mean(z.chr9.1)
mean(z.chr9.2)
mean(z.chr9.3)
sd(z.chr9.1)
sd(z.chr9.2)
sd(z.chr9.3)
HMM_chr9 <- dthmm(z.chr9, trans.prob9, init.prob9, distn = "norm", 
                  pm = list(mean=c(-0.623, -0.366, 0.99), sd=c(0.020, 0.111, 1.219)))
chr9.EM <- BaumWelch(HMM_chr9, control = a)
print(summary(chr9.EM))
states.chr9 <- Viterbi(HMM_chr9)
states.chr9.1 <- factor(states.chr9, levels=1:3)
######
chr9.1 <- chr9[(which(states.chr9.1==1)), ]
chr9.2 <- chr9[(which(states.chr9.1==2)), ]
chr9.3 <- chr9[(which(states.chr9.1==3)), ]
chr9.z.1 <- z.chr9[(which(states.chr9.1==1)), ]
chr9.z.2 <- z.chr9[(which(states.chr9.1==2)), ]
chr9.z.3 <- z.chr9[(which(states.chr9.1==3)), ]
###
em.chr9.1 <- em.hmm(chr9.z.1, L = 2)
em.chr9.3 <- em.hmm(chr9.z.3, L = 2)
plis.chr9.1 <- plis(em.chr9.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr9.3 <- plis(em.chr9.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr10.sort <- as.data.frame(sort(z.chr10))
z.chr10.1 <- z.chr10.sort[1:22952, ]
z.chr10.2 <- z.chr10.sort[22953:45904, ]
z.chr10.3 <- z.chr10.sort[45905:68857, ]
mean(z.chr10.1)
mean(z.chr10.2)
mean(z.chr10.3)
sd(z.chr10.1)
sd(z.chr10.2)
sd(z.chr10.3)
HMM_chr10 <- dthmm(z.chr10, trans.prob10, init.prob10, distn = "norm", 
                  pm = list(mean=c(-0.625, -0.37, 0.995), sd=c(0.0196, 0.111, 1.213)))
chr10.EM <- BaumWelch(HMM_chr10, control = a)
print(summary(chr10.EM))
states.chr10 <- Viterbi(HMM_chr10)
states.chr10.1 <- factor(states.chr10, levels=1:3)
###
chr10.1 <- chr10[(which(states.chr10.1==1)), ]
chr10.2 <- chr10[(which(states.chr10.1==2)), ]
chr10.3 <- chr10[(which(states.chr10.1==3)), ]
chr10.z.1 <- z.chr10[(which(states.chr10.1==1)), ]
chr10.z.2 <- z.chr10[(which(states.chr10.1==2)), ]
chr10.z.3 <- z.chr10[(which(states.chr10.1==3)), ]
###
em.chr10.1 <- em.hmm(chr10.z.1, L = 2)
em.chr10.3 <- em.hmm(chr10.z.3, L = 2)
plis.chr10.1 <- plis(em.chr10.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr10.3 <- plis(em.chr10.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr11.sort <- as.data.frame(sort(z.chr11))
z.chr11.1 <- z.chr11.sort[1:14323, ]
z.chr11.2 <- z.chr11.sort[14324:28646, ]
z.chr11.3 <- z.chr11.sort[28647:42971, ]
mean(z.chr11.1)
mean(z.chr11.2)
mean(z.chr11.3)
sd(z.chr11.1)
sd(z.chr11.2)
sd(z.chr11.3)
HMM_chr11 <- dthmm(z.chr11, trans.prob11, init.prob11, distn = "norm", 
                   pm = list(mean=c(-0.64, -0.36, 1.00), sd=c(0.0217, 0.125, 1.202)))
chr11.EM <- BaumWelch(HMM_chr11, control = a)
print(summary(chr11.EM))
states.chr11 <- Viterbi(HMM_chr11)
states.chr11.1 <- factor(states.chr11, levels=1:3)
###
chr11.1 <- chr11[(which(states.chr11.1==1)), ]
chr11.2 <- chr11[(which(states.chr11.1==2)), ]
chr11.3 <- chr11[(which(states.chr11.1==3)), ]
chr11.z.1 <- z.chr11[(which(states.chr11.1==1)), ]
chr11.z.2 <- z.chr11[(which(states.chr11.1==2)), ]
chr11.z.3 <- z.chr11[(which(states.chr11.1==3)), ]
###
em.chr11.1 <- em.hmm(chr11.z.1, L = 2)
em.chr11.3 <- em.hmm(chr11.z.3, L = 2)
plis.chr11.1 <- plis(em.chr11.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr11.3 <- plis(em.chr11.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr12.sort <- as.data.frame(sort(z.chr12))
z.chr12.1 <- z.chr12.sort[1:10568, ]
z.chr12.2 <- z.chr12.sort[10569:21136, ]
z.chr12.3 <- z.chr12.sort[21137:31704, ]
mean(z.chr12.1)
mean(z.chr12.2)
mean(z.chr12.3)
sd(z.chr12.1)
sd(z.chr12.2)
sd(z.chr12.3)
HMM_chr12 <- dthmm(z.chr12, trans.prob12, init.prob12, distn = "norm", 
                   pm = list(mean=c(-0.613, -0.357, 0.97), sd=c(0.0186, 0.112, 1.242)))
chr12.EM <- BaumWelch(HMM_chr12, control = a)
print(summary(chr12.EM))
states.chr12 <- Viterbi(HMM_chr12)
states.chr12.1 <- factor(states.chr12, levels=1:3)
###
chr12.1 <- chr12[(which(states.chr12.1==1)), ]
chr12.2 <- chr12[(which(states.chr12.1==2)), ]
chr12.3 <- chr12[(which(states.chr12.1==3)), ]
chr12.z.1 <- z.chr12[(which(states.chr12.1==1)), ]
chr12.z.2 <- z.chr12[(which(states.chr12.1==2)), ]
chr12.z.3 <- z.chr12[(which(states.chr12.1==3)), ]
###
em.chr12.1 <- em.hmm(chr12.z.1, L = 2)
em.chr12.3 <- em.hmm(chr12.z.3, L = 2)
plis.chr12.1 <- plis(em.chr12.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr12.3 <- plis(em.chr12.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr13.sort <- as.data.frame(sort(z.chr13))
z.chr13.1 <- z.chr13.sort[1:13672, ]
z.chr13.2 <- z.chr13.sort[13673:27344, ]
z.chr13.3 <- z.chr13.sort[27345:41018, ]
mean(z.chr13.1)
mean(z.chr13.2)
mean(z.chr13.3)
sd(z.chr13.1)
sd(z.chr13.2)
sd(z.chr13.3)
HMM_chr13 <- dthmm(z.chr13, trans.prob13, init.prob13, distn = "norm", 
                   pm = list(mean=c(-0.606, -0.365, 0.97), sd=c(0.0197, 0.105, 1.243)))
chr13.EM <- BaumWelch(HMM_chr13, control = a)
print(summary(chr13.EM))
states.chr13 <- Viterbi(HMM_chr13)
states.chr13.1 <- factor(states.chr13, levels=1:3)
###
chr13.1 <- chr13[(which(states.chr13.1==1)), ]
chr13.2 <- chr13[(which(states.chr13.1==2)), ]
chr13.3 <- chr13[(which(states.chr13.1==3)), ]
chr13.z.1 <- z.chr13[(which(states.chr13.1==1)), ]
chr13.z.2 <- z.chr13[(which(states.chr13.1==2)), ]
chr13.z.3 <- z.chr13[(which(states.chr13.1==3)), ]
###
em.chr13.1 <- em.hmm(chr13.z.1, L = 2)
em.chr13.3 <- em.hmm(chr13.z.3, L = 2)
plis.chr13.1 <- plis(em.chr13.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr13.3 <- plis(em.chr13.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr14.sort <- as.data.frame(sort(z.chr14))
z.chr14.1 <- z.chr14.sort[1:16610, ]
z.chr14.2 <- z.chr14.sort[16611:33220, ]
z.chr14.3 <- z.chr14.sort[33221:49830, ]
mean(z.chr14.1)
mean(z.chr14.2)
mean(z.chr14.3)
sd(z.chr14.1)
sd(z.chr14.2)
sd(z.chr14.3)
HMM_chr14 <- dthmm(z.chr14, trans.prob14, init.prob14, distn = "norm", 
                   pm = list(mean=c(-0.622, -0.371, 0.992), sd=c(0.019, 0.111, 1.215)))
chr14.EM <- BaumWelch(HMM_chr14, control = a)
print(summary(chr14.EM))
states.chr14 <- Viterbi(HMM_chr14)
states.chr14.1 <- factor(states.chr14, levels=1:3)
###
chr14.1 <- chr14[(which(states.chr14.1==1)), ]
chr14.2 <- chr14[(which(states.chr14.1==2)), ]
chr14.3 <- chr14[(which(states.chr14.1==3)), ]
chr14.z.1 <- z.chr14[(which(states.chr14.1==1)), ]
chr14.z.2 <- z.chr14[(which(states.chr14.1==2)), ]
chr14.z.3 <- z.chr14[(which(states.chr14.1==3)), ]
###
em.chr14.1 <- em.hmm(chr14.z.1, L = 2)
em.chr14.3 <- em.hmm(chr14.z.3, L = 2)
plis.chr14.1 <- plis(em.chr14.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr14.3 <- plis(em.chr14.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr15.sort <- as.data.frame(sort(z.chr15))
z.chr15.1 <- z.chr15.sort[1:13092, ]
z.chr15.2 <- z.chr15.sort[13093:26184, ]
z.chr15.3 <- z.chr15.sort[26185:39277, ]
mean(z.chr15.1)
mean(z.chr15.2)
mean(z.chr15.3)
sd(z.chr15.1)
sd(z.chr15.2)
sd(z.chr15.3)
HMM_chr15 <- dthmm(z.chr15, trans.prob15, init.prob15, distn = "norm", 
                   pm = list(mean=c(-0.624, -0.36, 0.983), sd=c(0.022, 0.111, 1.226)))
chr15.EM <- BaumWelch(HMM_chr15, control = a)
print(summary(chr15.EM))
states.chr15 <- Viterbi(HMM_chr15)
states.chr15.1 <- factor(states.chr15, levels=1:3)
###
chr15.1 <- chr15[(which(states.chr15.1==1)), ]
chr15.2 <- chr15[(which(states.chr15.1==2)), ]
chr15.3 <- chr15[(which(states.chr15.1==3)), ]
chr15.z.1 <- z.chr15[(which(states.chr15.1==1)), ]
chr15.z.2 <- z.chr15[(which(states.chr15.1==2)), ]
chr15.z.3 <- z.chr15[(which(states.chr15.1==3)), ]
####
em.chr15.1 <- em.hmm(chr15.z.1, L = 2)
em.chr15.3 <- em.hmm(chr15.z.3, L = 2)
plis.chr15.1 <- plis(em.chr15.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr15.3 <- plis(em.chr15.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr16.sort <- as.data.frame(sort(z.chr16))
z.chr16.1 <- z.chr16.sort[1:12397, ]
z.chr16.2 <- z.chr16.sort[12398:24794, ]
z.chr16.3 <- z.chr16.sort[24795:37191, ]
mean(z.chr16.1)
mean(z.chr16.2)
mean(z.chr16.3)
sd(z.chr16.1)
sd(z.chr16.2)
sd(z.chr16.3)
HMM_chr16 <- dthmm(z.chr16, trans.prob16, init.prob16, distn = "norm", 
                   pm = list(mean=c(-0.615, -0.361, 0.976), sd=c(0.020, 0.111, 1.235)))
chr16.EM <- BaumWelch(HMM_chr16, control = a)
print(summary(chr16.EM))
states.chr16 <- Viterbi(HMM_chr16)
states.chr16.1 <- factor(states.chr16, levels=1:3)
###
chr16.1 <- chr16[(which(states.chr16.1==1)), ]
chr16.2 <- chr16[(which(states.chr16.1==2)), ]
chr16.3 <- chr16[(which(states.chr16.1==3)), ]
chr16.z.1 <- z.chr16[(which(states.chr16.1==1)), ]
chr16.z.2 <- z.chr16[(which(states.chr16.1==2)), ]
chr16.z.3 <- z.chr16[(which(states.chr16.1==3)), ]
###
em.chr16.1 <- em.hmm(chr16.z.1, L = 2)
em.chr16.3 <- em.hmm(chr16.z.3, L = 2)
plis.chr16.1 <- plis(em.chr16.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr16.3 <- plis(em.chr16.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr17.sort <- as.data.frame(sort(z.chr17))
z.chr17.1 <- z.chr17.sort[1:12229, ]
z.chr17.2 <- z.chr17.sort[12230:24458, ]
z.chr17.3 <- z.chr17.sort[24459:36688, ]
mean(z.chr17.1)
mean(z.chr17.2)
mean(z.chr17.3)
sd(z.chr17.1)
sd(z.chr17.2)
sd(z.chr17.3)
HMM_chr17 <- dthmm(z.chr17, trans.prob17, init.prob17, distn = "norm", 
                   pm = list(mean=c(-0.593, -0.35, 0.944), sd=c(0.0184, 0.108, 1.273)))
chr17.EM <- BaumWelch(HMM_chr17, control = a)
print(summary(chr17.EM))
states.chr17 <- Viterbi(HMM_chr17)
states.chr17.1 <- factor(states.chr17, levels=1:3)
####
chr17.1 <- chr17[(which(states.chr17.1==1)), ]
chr17.2 <- chr17[(which(states.chr17.1==2)), ]
chr17.3 <- chr17[(which(states.chr17.1==3)), ]
chr17.z.1 <- z.chr17[(which(states.chr17.1==1)), ]
chr17.z.2 <- z.chr17[(which(states.chr17.1==2)), ]
chr17.z.3 <- z.chr17[(which(states.chr17.1==3)), ]
####
em.chr17.1 <- em.hmm(chr17.z.1, L = 2)
em.chr17.3 <- em.hmm(chr17.z.3, L = 2)
plis.chr17.1 <- plis(em.chr17.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr17.3 <- plis(em.chr17.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr18.sort <- as.data.frame(sort(z.chr18))
z.chr18.1 <- z.chr18.sort[1:12817, ]
z.chr18.2 <- z.chr18.sort[12818:25634, ]
z.chr18.3 <- z.chr18.sort[25635:38452, ]
mean(z.chr18.1)
mean(z.chr18.2)
mean(z.chr18.3)
sd(z.chr18.1)
sd(z.chr18.2)
sd(z.chr18.3)
HMM_chr18 <- dthmm(z.chr18, trans.prob18, init.prob18, distn = "norm", 
                   pm = list(mean=c(-0.588, -0.36, 0.95), sd=c(0.0171, 0.104, 1.27)))
chr18.EM <- BaumWelch(HMM_chr18, control = a)
print(summary(chr18.EM))
states.chr18 <- Viterbi(HMM_chr18)
states.chr18.1 <- factor(states.chr18, levels=1:3)
###
chr18.1 <- chr18[(which(states.chr18.1==1)), ]
chr18.2 <- chr18[(which(states.chr18.1==2)), ]
chr18.3 <- chr18[(which(states.chr18.1==3)), ]
chr18.z.1 <- z.chr18[(which(states.chr18.1==1)), ]
chr18.z.2 <- z.chr18[(which(states.chr18.1==2)), ]
chr18.z.3 <- z.chr18[(which(states.chr18.1==3)), ]
####LIS
em.chr18.1 <- em.hmm(chr18.z.1, L = 2)
em.chr18.3 <- em.hmm(chr18.z.3, L = 2)
plis.chr18.1 <- plis(em.chr18.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr18.3 <- plis(em.chr18.3$LIS, fdr = 0.001, adjust = TRUE)
#######################################################
z.chr19.sort <- as.data.frame(sort(z.chr19))
z.chr19.1 <- z.chr19.sort[1:9465, ]
z.chr19.2 <- z.chr19.sort[9466:18930, ]
z.chr19.3 <- z.chr19.sort[18931:28397, ]
mean(z.chr19.1)
mean(z.chr19.2)
mean(z.chr19.3)
sd(z.chr19.1)
sd(z.chr19.2)
sd(z.chr19.3)
HMM_chr19 <- dthmm(z.chr19, trans.prob19, init.prob19, distn = "norm", 
                   pm = list(mean=c(-0.626, -0.356, 0.982), sd=c(0.0197, 0.13, 1.225)))
chr19.EM <- BaumWelch(HMM_chr19, control = a)
print(summary(chr19.EM))
states.chr19 <- Viterbi(HMM_chr19)
states.chr19.1 <- factor(states.chr19, levels=1:3)
###
chr19.1 <- chr19[(which(states.chr19.1==1)), ]
chr19.2 <- chr19[(which(states.chr19.1==2)), ]
chr19.3 <- chr19[(which(states.chr19.1==3)), ]
chr19.z.1 <- z.chr19[(which(states.chr19.1==1)), ]
chr19.z.2 <- z.chr19[(which(states.chr19.1==2)), ]
chr19.z.3 <- z.chr19[(which(states.chr19.1==3)), ]
###########################################
##calculate the LIS score for each snp
em.chr19.1 <- em.hmm(chr19.z.1, L = 2)
em.chr19.3 <- em.hmm(chr19.z.3, L = 2)
plis.chr19.1 <- plis(em.chr19.1$LIS, fdr = 0.001, adjust = TRUE)
plis.chr19.3 <- plis(em.chr19.3$LIS, fdr = 0.001, adjust = TRUE)
###having 1 in the states is the correct model per say! 
###################################################################################
