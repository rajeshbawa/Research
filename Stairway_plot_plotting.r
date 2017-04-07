#################################################################
#########Stairway Plot##########################################
setwd("~/Documents/Stairway_plot/summary_stairwayPlot")
####read all the files#####
file_anchorage <- read.table("Anchorage_allFiles_summary.txt", header = T, sep="\t")
file_kodiak <- read.table("Kodiak_summary_allFiles.txt", header = T, sep="\t")
file_NBC <- read.table("NBC_allFiles_summary.txt", header = T, sep="\t")
file_CBC <- read.table("CBC_allFiles_summary.txt", header = T, sep="\t")
file_CQ <- read.table("CQ_allFiles_summary.txt", header = T, sep="\t")
file_SBC <- read.table("SBC_allFiles_summary.txt", header = T, sep="\t")
file_NUS <- read.table("NUS_allFiles_summary.txt", header = T, sep="\t")
file_Gwood <- read.table("Gwood_allFiles_summary.txt", header=T, sep = "\t")
file_oregon <- read.table("Oregon_allFiles_summary.txt", header = T, sep="\t")
file_sierras <- read.table("Sierras_delt_summary.txt", sep="\t", header = T)
#####change the scale to 1000th######
data_anchorage <- data.frame(lapply(file_anchorage, function(X) X/1000), scipen = 999)
data_kodiak <- data.frame(lapply(file_kodiak, function(X) X/1000), scipen = 999)
data_NBC <- data.frame(lapply(file_NBC, function(X) X/1000), scipen = 999)
data_CBC <- data.frame(lapply(file_CBC, function(X) X/1000), scipen = 999)
data_CQ <- data.frame(lapply(file_CQ, function(X) X/1000), scipen = 999)
data_SBC <- data.frame(lapply(file_SBC, function(X) X/1000), scipen = 999)
data_NUS <- data.frame(lapply(file_NUS, function(X) X/1000), scipen = 999)
data_Gwood <- data.frame(lapply(file_Gwood, function(X) X/1000), scipen = 999)
data_oregon <- data.frame(lapply(file_oregon, function(X) X/1000), scipen = 999)
data_sierras <- data.frame(lapply(file_sierras, function(X) X/1000), scipen = 999)
################################################
plot(y=data_kodiak$Ne_median, x=data_kodiak$year, log="x", type = "n", bty="n",axes=F, 
     ylim=c(0,5000), xlim=c(10000,100000), xlab="Time (1K year)", ylab="Ne (1K individual)", 
     col="firebrick", lwd=2)
####xlim=c(10,100000)
####at=c(10,100,1000,10000,100000)
axis(1, at=c(10000,25000,50000,100000))
axis(2)
lines(data_anchorage$year, data_anchorage$Ne_median, type = "l", lwd = 5, col="darksalmon")
lines(data_kodiak$year, data_kodiak$Ne_median, type = "l", lwd = 5, col="gray48")
lines(data_CBC$year, data_CBC$Ne_median, type = "l", lwd = 5, col="saddlebrown")
lines(data_NBC$year, data_NBC$Ne_median, type = "l", lwd = 5, col="lightskyblue")
lines(data_CQ$year, data_CQ$Ne_median, type = "l", lwd = 5, col="limegreen")
lines(data_SBC$year, data_SBC$Ne_median, type = "l", lwd = 5, col="khaki3")
lines(data_NUS$year, data_NUS$Ne_median, type = "l", lwd = 5, col="turquoise1")
lines(data_Gwood$year, data_Gwood$Ne_median, type = "l", lwd = 5, col="lightpink")
lines(data_oregon$year, data_oregon$Ne_median, type = "l", lwd = 5, col="goldenrod")
lines(data_sierras$year, data_sierras$Ne_median, type = "l", lwd = 5, col="firebrick")
#####
colors1 = c("darksalmon","dimgray","lightskyblue","burlywood4","chartreuse","khaki3","cyan",
            "deeppink","goldenrod","firebrick")
samp.loc1 = c("Anchorage", "Kodiak", "NBC", "CBC", "CQ", "SBC",
              "NUS", "GWood", "Oregon", "Sierras")
legend("topright", legend=samp.loc1, col=colors1, pch=NA_integer_, cex=0.8, lwd=2, xjust=1, bty="n", text.col=colors1)
##################################################################
plot(y=data_kodiak$Ne_median, x=data_kodiak$year, log="x", type = "n", bty="n",axes=F, 
     ylim=c(0,5000), xlim=c(10,100000), xlab="Time (1K year)", ylab="Ne (1K individual)", 
     col="firebrick", lwd=2)
axis(1, at=c(10,100,1000,10000, 100000))
axis(2)
lines(data_anchorage$year, data_anchorage$Ne_2.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_anchorage$year, data_anchorage$Ne_97.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_anchorage$year, data_anchorage$Ne_median, type = "l", lwd = 3, col="black")
################
plot(y=data_kodiak$Ne_median, x=data_kodiak$year, log="x", type = "n", bty="n",axes=F, 
     ylim=c(0,5000), xlim=c(10,100000), xlab="Time (1K year)", ylab="Ne (1K individual)", 
     col="firebrick", lwd=2)
axis(1, at=c(10,100,1000,10000, 100000))
axis(2)
lines(data_kodiak$year, data_kodiak$Ne_2.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_kodiak$year, data_kodiak$Ne_97.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_kodiak$year, data_kodiak$Ne_median, type = "l", lwd = 3, col="black")
####################
plot(y=data_kodiak$Ne_median, x=data_kodiak$year, log="x", type = "n", bty="n",axes=F, 
     ylim=c(0,5000), xlim=c(10,100000), xlab="Time (1K year)", ylab="Ne (1K individual)", 
     col="firebrick", lwd=2)
axis(1, at=c(10,100,1000,10000, 100000))
axis(2)
lines(data_CBC$year, data_CBC$Ne_2.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_CBC$year, data_CBC$Ne_97.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_CBC$year, data_CBC$Ne_median, type = "l", lwd = 3, col="black")
####################
plot(y=data_kodiak$Ne_median, x=data_kodiak$year, log="x", type = "n", bty="n",axes=F, 
     ylim=c(0,5000), xlim=c(10,100000), xlab="Time (1K year)", ylab="Ne (1K individual)", 
     col="firebrick", lwd=2)
axis(1, at=c(10,100,1000,10000, 100000))
axis(2)
lines(data_NBC$year, data_NBC$Ne_2.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_NBC$year, data_NBC$Ne_97.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_NBC$year, data_NBC$Ne_median, type = "l", lwd = 3, col="black")
#####################
plot(y=data_kodiak$Ne_median, x=data_kodiak$year, log="x", type = "n", bty="n",axes=F, 
     ylim=c(0,5000), xlim=c(10,100000), xlab="Time (1K year)", ylab="Ne (1K individual)", 
     col="firebrick", lwd=2)
axis(1, at=c(10,100,1000,10000, 100000))
axis(2)
lines(data_SBC$year, data_SBC$Ne_2.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_SBC$year, data_SBC$Ne_97.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_SBC$year, data_SBC$Ne_median, type = "l", lwd = 3, col="black")
#####################
plot(y=data_kodiak$Ne_median, x=data_kodiak$year, log="x", type = "n", bty="n",axes=F, 
     ylim=c(0,5000), xlim=c(10,100000), xlab="Time (1K year)", ylab="Ne (1K individual)", 
     col="firebrick", lwd=2)
axis(1, at=c(10,100,1000,10000, 100000))
axis(2)
lines(data_CQ$year, data_CQ$Ne_2.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_CQ$year, data_CQ$Ne_97.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_CQ$year, data_CQ$Ne_median, type = "l", lwd = 3, col="black")
######################
plot(y=data_kodiak$Ne_median, x=data_kodiak$year, log="x", type = "n", bty="n",axes=F, 
     ylim=c(0,5000), xlim=c(10,100000), xlab="Time (1K year)", ylab="Ne (1K individual)", 
     col="firebrick", lwd=2)
axis(1, at=c(10,100,1000,10000, 100000))
axis(2)
lines(data_NUS$year, data_NUS$Ne_2.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_NUS$year, data_NUS$Ne_97.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_NUS$year, data_NUS$Ne_median, type = "l", lwd = 3, col="black")
########################
plot(y=data_kodiak$Ne_median, x=data_kodiak$year, log="x", type = "n", bty="n",axes=F, 
     ylim=c(0,5000), xlim=c(10,100000), xlab="Time (1K year)", ylab="Ne (1K individual)", 
     col="firebrick", lwd=2)
axis(1, at=c(10,100,1000,10000, 100000))
axis(2)
lines(data_Gwood$year, data_Gwood$Ne_2.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_Gwood$year, data_Gwood$Ne_97.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_Gwood$year, data_Gwood$Ne_median, type = "l", lwd = 3, col="black")
###########################
plot(y=data_kodiak$Ne_median, x=data_kodiak$year, log="x", type = "n", bty="n",axes=F, 
     ylim=c(0,5000), xlim=c(10,100000), xlab="Time (1K year)", ylab="Ne (1K individual)", 
     col="firebrick", lwd=2)
axis(1, at=c(10,100,1000,10000, 100000))
axis(2)
lines(data_oregon$year, data_oregon$Ne_2.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_oregon$year, data_oregon$Ne_97.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_oregon$year, data_oregon$Ne_median, type = "l", lwd = 3, col="black")
############################
plot(y=data_kodiak$Ne_median, x=data_kodiak$year, log="x", type = "n", bty="n",axes=F, 
     ylim=c(0,5000), xlim=c(10,100000), xlab="Time (1K year)", ylab="Ne (1K individual)", 
     col="firebrick", lwd=2)
axis(1, at=c(10,100,1000,10000, 100000))
axis(2)
lines(data_sierras$year, data_sierras$Ne_2.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_sierras$year, data_sierras$Ne_97.5., type = "l", lty = 5, lwd = 2, col="gray48")
lines(data_sierras$year, data_sierras$Ne_median, type = "l", lwd = 3, col="black")
#################################################################


