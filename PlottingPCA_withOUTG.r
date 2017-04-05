#########################################
##plotting pca with outgroups
setwd("~/Documents/Results_demography/smart-PCA/")
file2 <- read.table("PCA_10_demogP_withOUT.txt", sep="\t", header=T)
jpeg("final_PCA_with-Out.jpg", width=11, height=10, units="in",res=72,quality = 100)
#svg("pcaclump.SVG", width=11, height=10)
plot(file2$EV2, file2$EV1,
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
colors1 = c("darksalmon","gray48","lightskyblue","limegreen","saddlebrown","khaki3", "turquoise1", "lightpink", "goldenrod",
            "firebrick", "blue", "cadetblue", "darkviolet", "midnightblue", "royalblue")
samp.loc1 = c("Anchorage", "Kodiak", "NBC", "CQ", "CBC", "SBC",
              "NUS", "GW", "OR", "Sierras", "Balsam", "Deltoides", "Fermontii", "Tremula", "Tremuloides")
legend("bottomleft", legend=samp.loc1, col=colors1, pch=20, cex=0.7, xjust=1, text.col=colors1,bty = "n")
dev.off()
###############
##CN populations
colors1 = c("darkorange","blue","lightpink","deeppink","red",
            "goldenrod4","orchid","bisque4")
samp.loc1 = c("Rob_Develice", "Don_Pigot", "BC_MoF", "Paul_Hennon", "Greenwood",
              "Rebecca_Best", "MyrtleCreek_OR", "Will_Haag", "Juneau")
#################
##CS populations
colors1 = c("blue","lightpink","deeppink","red","khaki2","mediumspringgreen",
            "goldenrod4","cornflowerblue","seagreen4","yellow")
samp.loc1 = c("Don_Pigot", "BC_MoF", "Paul_Hennon", "Greenwood", "Dennis_Ringnes",
              "Rebecca_Best", "MyrtleCreek_OR", "RogueRiver_OR", "Sierras", "Yreka_CA")
###################
##NS populations
colors1 = c("darkorange","deeppink","khaki2","mediumspringgreen",
            "cornflowerblue","seagreen4","orchid","yellow","bisque4")
samp.loc1 = c("Rob_Develice", "Paul_Hennon", "Dennis_Ringnes",
              "Rebecca_Best", "RogueRiver_OR", "Sierras", "Will_Haag", "Yreka_CA", "Juneau")
####################
##for all outgroups
colors1 = c("darkorange","blue","lightpink","deeppink","red","khaki2","mediumspringgreen",
            "goldenrod4","cornflowerblue","seagreen4","orchid","yellow","bisque4","dimgray","olivedrab","cyan","midnightblue","deeppink4")
samp.loc1 = c("Rob_Develice", "Don_Pigot", "BC_MoF", "Paul_Hennon", "Greenwood", "Dennis_Ringnes",
              "Rebecca_Best", "MyrtleCreek_OR", "RogueRiver_OR", "Sierras", "Will_Haag", "Yreka_CA", "Juneau", "BALSAM", "DELT", "FERMONTII", "TREMULA", "TREMULOIDES")
legend("bottomleft", legend=samp.loc1, col=colors1, pch=20, cex=0.6, xjust=1, text.col=colors1, bty="n")
######################
#text(file1$EV2, file1$EV1, file1$sample.id, cex=0.6, pos=5, col="black")
#dev.off()

################################################
####PCA for PC3 and 4
lot(file1$EV4, file1$EV3,main="PCA of all populations with outgroups",
    col= ifelse((file1$pop == "Rob_Develice"),"darkorange", 
                ifelse((file1$pop == "Don_Pigot"),"blue",
                       ifelse((file1$pop == "BC_MoF"),"lightpink",
                              ifelse((file1$pop == "Paul_Hennon"),"deeppink",
                                     ifelse((file1$pop == "Greenwood"),"red",
                                            ifelse((file1$pop == "Dennis_Ringnes"),"khaki2",
                                                   ifelse((file1$pop == "Rebecca_Best"),"mediumspringgreen",
                                                          ifelse((file1$pop == "MyrtleCreek"),"goldenrod4",
                                                                 ifelse((file1$pop == "RogueRiver"),"cornflowerblue",
                                                                        ifelse((file1$pop == "Sierras"),"seagreen4",
                                                                               ifelse((file1$pop == "Will_Haag"),"orchid",
                                                                                      ifelse((file1$pop == "Yreka_CA"),"yellow",
                                                                                             ifelse((file1$pop == "Juneau"),"bisque4",
                                                                                                    ifelse((file1$pop == "BALSAM"),"dimgray",
                                                                                                           ifelse((file1$pop == "DELT"),"olivedrab",
                                                                                                                  ifelse((file1$pop == "FERMONTII"),"cyan",
                                                                                                                         ifelse((file1$pop == "TREMULA"),"midnightblue",
                                                                                                                                ifelse((file1$pop == "TREMULOIDES"),"deeppink4","green"
                                                                                                                                )))))))))))))))))),
pch = 20, cex=0.8, xlab="PC4", ylab="PC3")
################################################################