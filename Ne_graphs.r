setwd("/Users/rajesh13/Documents/Results_demography/LD_populations/")
file1 <- read.table("Ne_mcevoy1.txt", header = T, sep="\t")
#plot(file1$Time, file1$Ne1, type="n", axes=F, xlim=c(100, 20), ylim=c(0,950), ylab="", xlab="Thousands of Years ago (KYA)")
plot(file1$Time, file1$Ne1, type="n", axes=F, xlim=c(18, 3), ylim=c(0,950), ylab="", xlab="Thousands of Years ago (KYA)")
#axis(side=1, at=trunc(seq(20, 100 , 20), digits = 0))
axis(side=1, at=trunc(seq(3, 18 , 2), digits = 0))
axis(side=2, at=trunc(seq(0, 950 , 50), digits = 0))
lines(file1$Time, file1$Ne1, type="l", lwd=2, col="darksalmon")
lines(file1$Time, file1$Ne2, type="l", lwd=2, col="dimgray")
lines(file1$Time, file1$Mean.Ne3, type="l", lwd=2, col="lightskyblue")
lines(file1$Time, file1$Mean.Ne4, type="l", lwd=2, col="burlywood4")
lines(file1$Time, file1$Ne5, type="l", lwd=2, col="chartreuse")
lines(file1$Time, file1$Mean.Ne6, type="l", lwd=2, col="khaki3")
lines(file1$Time, file1$Mean.Ne7, type="l", lwd=2, col="cyan")
lines(file1$Time, file1$Mean.Ne8, type="l", lwd=2, col="deeppink")
lines(file1$Time, file1$Mean.Ne9, type="l", lwd=2, col="goldenrod")
lines(file1$Time,file1$Mean.Ne10, type="l", lwd=2, col="firebrick")

colors1 = c("darksalmon","dimgray","lightskyblue","burlywood4","chartreuse","khaki3","cyan",
            "deeppink","goldenrod","firebrick")
samp.loc1 = c("AC", "KD", "BC1", "BC2", "BC3", "BC4",
              "WA1", "GW", "OR", "CA")

par(xpd=F)
legend("topright", inset=c(-0.02,0), legend=samp.loc1, col=colors1, lwd=1, pch=NA_integer_, cex=0.8, xjust=1, text.col=colors1, bty='n')

