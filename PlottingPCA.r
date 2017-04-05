#######plotting script for PCA results#######################
######for PCA without outgroups#############################
setwd("/Users/rajesh13/Documents/Results_demography/smart-PCA/")
file1 <- read.table("PCA_10_demogP_noOUT.txt", sep="\t", header=T)
#jpeg("pcaClump.jpg", width=11, height=10, units="in",res=72,quality = 100)
pdf("final_PCA_no_outgroups.pdf", width=11, height=15)
plot(file1$EV2, file1$EV1,
col= ifelse((file1$pop == "Anchorage"),"darksalmon", 
            ifelse((file1$pop == "Kodiak"),"gray48",
                   ifelse((file1$pop == "NBC"),"lightskyblue",
                          ifelse((file1$pop == "CQ"),"limegreen",
                                 ifelse((file1$pop == "CBC"),"saddlebrown",
                                        ifelse((file1$pop == "SBC"),"khaki3",
                                               ifelse((file1$pop == "WA"),"turquoise1",
                                                      ifelse((file1$pop == "GW"),"lightpink",
                                                             ifelse((file1$pop == "OR"),"goldenrod",
                                                                    ifelse((file1$pop == "CA"),"firebrick",
                                                                           ifelse((file1$pop == "BALSAM"),"blue",
                                                                                  ifelse((file1$pop == "DELT"),"cadetblue",
                                                                                         ifelse((file1$pop == "FERMONTII"),"darkviolet",
                                                                                                ifelse((file1$pop == "TREMULA"), "midnightblue", "royalblue"
                                                                                                )))))))))))))),
pch = 16, cex=0.8, xlab="PC2", ylab="PC1")
colors1 = c("darksalmon","gray48","lightskyblue","limegreen","saddlebrown","khaki3","turquoise1",
            "lightpink","goldenrod","firebrick")
samp.loc1 = c("Anchorage", "Kodiak", "NBC", "CQ", "CBC", "SBC",
              "NUS", "GW", "OR", "Sierras")
###############
par(mar=c(0, 1, 0, 1))
legend("bottomleft", legend=samp.loc1, col=colors1, pch=20, cex=0.6, xjust=0.5, text.col=colors1, bty = "n")
dev.off()
######################################################
####plotting pca with outgroups########
file2 <- read.table("PCA_10_demogP_withOUT.txt", sep="\t", header=T)
jpeg("final_PCA_with-Out.jpg", width=11, height=10, units="in",res=72,quality = 100)
#svg("pcaclump.SVG", width=11, height=10)
plot(file2$EV2, file2$EV1,
     col= ifelse((file2$pop == "Anchorage"),"darksalmon", 
                 ifelse((file2$pop == "Kodiak"),"gray48",
                        ifelse((file2$pop == "NBC"),"lightskyblue",
                               ifelse((file2$pop == "CQ"),"limegreen",
                                      ifelse((file2$pop == "CBC"),"saddlebrown",
                                             ifelse((file2$pop == "SBC"),"khaki3",
                                                    ifelse((file2$pop == "WA"),"turquoise1",
                                                           ifelse((file2$pop == "GW"),"lightpink",
                                                                  ifelse((file2$pop == "OR"),"goldenrod",
                                                                         ifelse((file2$pop == "CA"),"firebrick",
                                                                                ifelse((file2$pop == "BALSAM"),"blue",
                                                                                       ifelse((file2$pop == "DELT"),"cadetblue",
                                                                                              ifelse((file2$pop == "FERMONTII"),"darkviolet",
                                                                                                     ifelse((file2$pop == "TREMULA"), "midnightblue", "royalblue"
                                                                                                     )))))))))))))),
     pch = 16, cex=0.8, xlab="PC2", ylab="PC1")
#####################################
colors1 = c("darksalmon","gray48","lightskyblue","limegreen","saddlebrown","khaki3", "turquoise1", "lightpink", "goldenrod",
            "firebrick", "blue", "cadetblue", "darkviolet", "midnightblue", "royalblue")
samp.loc1 = c("Anchorage", "Kodiak", "NBC", "CQ", "CBC", "SBC",
              "NUS", "GW", "OR", "Sierras", "Balsam", "Deltoides", "Fermontii", "Tremula", "Tremuloides")
legend("bottomleft", legend=samp.loc1, col=colors1, pch=20, cex=0.7, xjust=1, text.col=colors1,bty = "n")
dev.off()
##################################
####plotting the samples with the color gradient with North being blue and South being red#######
####COLOR USING ggplot's Qplot to plot lat's sort data
setwd("/Users/rajesh13/Documents/Results_demography/SNP_relate/")
Geo_locations <-  read.table("sample_loc_poplar.txt", header=T, stringsAsFactors=FALSE)
Geofile1 <- read.table("sample_act.txt", header=T)
new_loc <- Geo_locations[match(Geofile1$POS, Geo_locations$New_ID), ]
#dummy <- as.data.frame(matrix(, nrow = 10, ncol = 3))
#names(dummy) <- c("New_ID", "Latitude",	"Longitude")
#newloc <- rbind(new_loc, dummy)
data1 <- cbind(new_loc$Latitude, file1)
names(data1)[1] <- c("Lats")
data_latsort <- na.omit(data1[order(data1$Lats, na.last = NA),])

library(ggplot2)
col <- sort(rnorm(383))
setwd("~/Documents/Results_demography/SNP_relate/")
pdf("final_PCA_noOut_NS.pdf", width=7, height = 5)
myplot<-qplot(EV2, EV1, data=data_latsort, colour=col) + scale_colour_gradient(low="red", high="blue")+xlab("PC2 (3.3%)")+ylab("PC1 (5.2%)")+scale_size(range = c(0.5,1))
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()
#myplot

##################################
#BARPLOT FOR EIGENVALUES for without outgroups
X1 <- c(1,2,3,4,5)
X2 <- c(3.5130810, 2.9584095, 1.4162972, 1.1470340, 1.1208425)
pdf("PCA_10_demogP_noOUT_eigenvalues.pdf", width=5, height=7)
#plot(X1, X2, type="h", xlab="PCs", cex = 5)
par(mar = c(10, 5, 10, 10))
barplot(height= X2, beside=T, xlim=c(1,5), col="green", width=0.3, space=0.5, axes=F, names.arg=c(1:5))
dev.off()
##################
#bar plot for eigenvalues with outgroups
X1 <- c(1,2,3,4,5)
X2 <- c(5.214155, 3.329129, 2.756815, 1.576433, 1.3429611)
pdf("PCA_10_demogP_withOUT_eigenvalues.pdf", width=5, height=7)
#plot(X1, X2, type="h", xlab="PCs", cex = 5)
par(mar = c(10, 5, 10, 10))
barplot(height= X2, beside=T, xlim=c(1,5), col="green", width=0.3, space=0.5, axes=F, names.arg=c(1:5))
dev.off()
##################


