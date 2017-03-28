#This makes the graph including the initial file, so 21 plots in total
#if one tends to see the graphs individually and not in one jpeg file, one should comment out line 21, 23
##set working directory
setwd("C:\\Users\\Rajesh\\Desktop\\Spring 2013\\Perl-CS5046")

#reading one file to get the time data, which remains same among all the files 
file1 <- read.csv('FinalExam_dataFile.csv', header = TRUE)

# get a list of files having a csv extension
my.file.list <- list.files(pattern = "csv$")
# for each file, run read.table and select only the cln3 column
my.list <- lapply(X = my.file.list, FUN = function(x) {
  read.table(x, header = TRUE, colClasses = c("NULL", "NULL", "NULL", "numeric"), sep = ",")[,1]
})

# merge columns that are in a list into one data.frame
my.df <- do.call("cbind", my.list)
#bind time column with rest of the data frame
my.final.file <- cbind(my.df, file1$Time)
#activate jpeg file, with dimensions of file and quality of plot
jpeg('timeSeries.jpeg', width = 2000, height = 1500, quality = 100)
#par is dividing plot window into 25 frames for having all plots as single file
par(mfcol=c(5,5))
#loop through file to plot
for ( i in seq(1,ncol(my.final.file)-1,1) ) plot(x<- my.final.file[,22], y<- my.final.file[,i], type="l",lwd=1, xlab = "Time", ylab = "Cln3", main = i)
dev.off()

#Written by Rajesh K Bawa and Sandesh Shreshta
