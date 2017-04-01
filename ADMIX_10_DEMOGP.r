CV_error = c(0.194, 0.18302, 0.18151, 0.17855, 0.17763, 0.17799, 0.17920, 0.17753, 0.18087, 0.18187, 0.18176)
K = c(1:11)
setwd("/Users/rajesh13/Documents/Results_demography/Admixture_results/")
pdf("admix_noOut_cverror", width=5, height=7)
plot(K, CV_error, type="n", axes = F, xlim=c(1,11), ylim=c(0.177,0.195), ylab="Cross-validation error")
axis(side=1, at=seq(1, 11, 1))
axis(side=2, at=seq(0.165, 0.195, 0.005))
lines(K, CV_error, lwd=1, col="black")
dev.off()
######################
setwd("/Users/rajesh13/Documents/Results_demography/Admixture_results/Admixture_files/ADMIXTURE_200/")
tbl=read.table("admix1.8.Q")
Geofile1 <- read.table("/Users/rajesh13/Documents/Results_demography/Admixture_results/Admixture_files/sample_act.txt", header=T)
file1.admix <- cbind(Geofile1, tbl)
file2.demopop <- read.table("/Users/rajesh13/Documents/Results_demography/Admixture_results/Admixture_files/demography_pop_10.txt", header=F)
new_demo.pop1 <- file1.admix[match(file2.demopop$V1, file1.admix$POS), ]
row.names(new_demo.pop1) <- NULL
NSCODES <- read.table("/Users/rajesh13/Documents/Results_demography/Admixture_results/Admixture_files/NScodes_demogP", header = T)
file3.demopop <- cbind(NSCODES, new_demo.pop1)
new_demo.pop2 <- file3.demopop[order(file3.demopop$NS_codes, na.last = NA, decreasing = F),]
row.names(new_demo.pop2) <- NULL
###########################################
data <- new_demo.pop2[,c(3:10)]
setwd("/Users/rajesh13/Downloads/Results_demography/Admixture_results/")
pdf("admix_noOut_10.pdf", width=14, height=4)
par(mar=c(2,2,6,2) + 0.1)
barplot(t(as.matrix(data)),col=rainbow(8),cex.names=.75, las=2,lwd = 0.2, space=rep(c(.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1),c(6,1,3,1,12,1,16,1,14,1,119,1,128,1,38,1,16,1,30)))
mtext(side = 3, at=c(4,10,19,38,60,120,260,360,400,430), las=2, line = .5, cex=0.7, 
      text = c("KODIAK","ANCHORAGE","N.BC","C.BC","CQ","S.BC","NUS","GW","OR","Sierras")) 
dev.off()
###################
#for admixture with outgroup data####################
setwd("/Users/rajesh13/Downloads/ADMIXTURE/ADMIXTURE_OUT_200/")
CV_error_out = c(0.20286, 0.18911, 0.18142, 0.18144, 0.17853, 0.17760, 0.17750, 0.17697, 0.17909, 0.17822, 0.17841, 0.17889, 0.17861)
K_out = c(1:13)
setwd("/Users/rajesh13/Downloads/Results_demography/Admixture_results/")
pdf("admix_withOut_cverror.pdf", width=5, height=7)
plot(K_out, CV_error_out, type="n", axes = F, xlim=c(1,13), ylim=c(0.177,0.203), ylab="Cross-validation error")
axis(side=1, at=seq(1, 13, 1))
axis(side=2, at=seq(0.165, 0.205, 0.005))
lines(K_out, CV_error_out, lwd=1, col="black")
dev.off()
######################
setwd("/Users/rajesh13/Documents/Results_demography/Admixture_results/Admixture_files/ADMIXTURE_OUT_200/")
tbl_out=read.table("admixture_out.7.Q")
Geofile1_out <- read.table("/Users/rajesh13/Documents/Results_demography/Admixture_results/Admixture_files/sample_out_act.txt", header=T)
file1.admix_out <- cbind(Geofile1_out, tbl_out)
file2.demopop_out <- read.table("/Users/rajesh13/Documents/Results_demography/Admixture_results/Admixture_files/demography_pop_out_10.txt", header=F)
new_demo.pop1_out <- file1.admix_out[match(file2.demopop_out$V1, file1.admix_out$POS), ]
row.names(new_demo.pop1_out) <- NULL
NSCODES_out <- read.table("/Users/rajesh13/Documents/Results_demography/Admixture_results/Admixture_files/NScodes_demogP_out", header = T)
file3.demopop_out <- cbind(NSCODES_out, new_demo.pop1_out)
new_demo.pop2_out <- file3.demopop_out[order(file3.demopop_out$NS_codes, na.last = NA, decreasing = F),]
row.names(new_demo.pop2_out) <- NULL
##########################
data <- new_demo.pop2_out[,c(3:9)]
setwd("/Users/rajesh13/Downloads/Results_demography/Admixture_results/")
pdf("admix_withOut_12.pdf", width=14, height=4)
par(mar=c(2,2,6,2) + 0.1)
barplot(t(as.matrix(data)),col=rainbow(3),cex.names=.75,las=2,ylab="Ancestry",space=rep(c(.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1),c(2,1,5,1,3,1,12,1,16,1,14,1,119,1,128,1,38,1,16,1,30,1,1,1,1,1,1,1,1)))
mtext(side = 3, at=c(1,7,14,21,41,62,122,262,362,402,432,447,451,455,459), las=2, line = .5, cex=0.7, 
      text = c("Balsam","Kodiak","Anchorage","N.BC","C.BC","CQ","S.BC","N.US","GW","OR","Sierras","delt","fermontii","tremula","tremuloides")) 
dev.off()

#######################to generate the figure with two K=3 and 8 in the same plot window
setwd("/Users/rajesh13/Documents/Results_demography/Admixture_results/Admixture_files/ADMIXTURE_OUT_200/")
k3.file=read.table("admixture_out.3.Q")
Geofile1_out <- read.table("/Users/rajesh13/Documents/Results_demography/Admixture_results/Admixture_files/sample_out_act.txt", header=T)
k3.admix_out <- cbind(Geofile1_out, k3.file)
file2.demopop_out <- read.table("/Users/rajesh13/Documents/Results_demography/Admixture_results/Admixture_files/demography_pop_out_10.txt", header=F)
k3.pop1_out <- k3.admix_out[match(file2.demopop_out$V1, k3.admix_out$POS), ]
row.names(k3.pop1_out) <- NULL
NSCODES_out <- read.table("/Users/rajesh13/Documents/Results_demography/Admixture_results/Admixture_files/NScodes_demogP_out", header = T)
k3.demopop_out <- cbind(NSCODES_out, k3.pop1_out)
k3_new.pop2_out <- k3.demopop_out[order(k3.demopop_out$NS_codes, na.last = NA, decreasing = F),]
row.names(k3_new.pop2_out) <- NULL
########################
k8.file=read.table("admixture_out.8.Q")
k8.admix_out <- cbind(Geofile1_out, k8.file)
k8.pop1_out <- k8.admix_out[match(file2.demopop_out$V1, k8.admix_out$POS), ]
row.names(k8.pop1_out) <- NULL
k8.demopop_out <- cbind(NSCODES_out, k8.pop1_out)
k8_new.pop2_out <- k8.demopop_out[order(k8.demopop_out$NS_codes, na.last = NA, decreasing = F),]
row.names(k8_new.pop2_out) <- NULL
########################
k3.data <- k3_new.pop2_out[,c(3:5)]
k8.data <- k8_new.pop2_out[,c(3:10)]
setwd("/Users/rajesh13/Downloads/Results_demography/Admixture_results/")
pdf("admix_k3-8_wOut.pdf", width=15, height=6)
par(mar=c(0.1,2,6,2) + 0.1, mfrow = c(2,1), oma = c(15, 2, 2, 2))
barplot(t(as.matrix(k3.data)),col=rainbow(3), axes = F, ylim = c(0,1),cex.names=.75,las=2,ylab="Ancestry",space=rep(c(.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1),c(2,1,5,1,3,1,12,1,16,1,14,1,119,1,128,1,38,1,16,1,30,1,1,1,1,1,1,1,1)))
axis(side=2, at = c(0, 0.5, 1))
mtext(side = 3, at=c(1,7,14,21,41,62,122,262,362,402,432,447,451,455,459), las=2, line = .5, cex=0.7, 
      text = c("Balsam","KODIAK","ANCHORAGE","N.BC","C.BC","CQ","S.BC","DP","GW","OR","CA","delt","fermontii","tremula","tremuloides")) 
par(mar=c(6,2,0.5,2))
barplot(t(as.matrix(k8.data)),col=rainbow(8), axes = F, ylim = c(0,1), cex.names=.75,las=2,ylab="Ancestry",space=rep(c(.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1,1.5,.1),c(2,1,5,1,3,1,12,1,16,1,14,1,119,1,128,1,38,1,16,1,30,1,1,1,1,1,1,1,1)))
axis(side = 2, at = c(0, 0.5, 1))
dev.off()
########nospace between the bars instructions on plotting
par(mar=c(0.1,2,6,2) + 0.1, mfrow = c(2,1), oma = c(10, 2, 2, 2))
barplot(t(as.matrix(k3.data)),col=rainbow(3),axes = F,ylim = c(0,1),cex.names=.75,las=2,ylab="Ancestry",border = NA, space=rep(c(0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0),c(2,1,5,1,3,1,12,1,16,1,14,1,119,1,128,1,38,1,16,1,30,1,1,1,1,1,1,1,1)))
axis(side = 2, at = c(0, 0.5, 1))
mtext(side = 3, at=c(1,7,14,21,41,62,122,262,325,365,391,410,414,417,421), las=2, line = .5, cex=0.7, 
      text = c("Balsam","KODIAK","ANCHORAGE","N.BC","C.BC","CQ","S.BC","N.US","GW","OR","Sierras","delt","fermontii","tremula","tremuloides")) 
par(mar=c(6,2,0.5,2))
barplot(t(as.matrix(k8.data)),col=rainbow(8),axes = F,ylim = c(0,1),cex.names=.75,las=2,ylab="Ancestry",border = NA, space=rep(c(0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0,1.5,0),c(2,1,5,1,3,1,12,1,16,1,14,1,119,1,128,1,38,1,16,1,30,1,1,1,1,1,1,1,1)))
axis(side = 2, at = c(0, 0.5, 1))
###############################################################