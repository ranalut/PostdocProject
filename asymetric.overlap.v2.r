# Figure for % overlap among pairs

# setwd('//cfr.washington.edu/main/space/lawler/shared/hybridzone/')
setwd("C:/Users/cwilsey/Dropbox/Inter-postdoctoral Interactions (1)/Figures 4 Revision")
# source('perc.overlap.fig.v2.r') # Builds table needed for these figures.
source('plot.fxns.v2.r')

groups <- c('birds','mammals','amphibians')
the.data <- read.csv(paste(getwd(),'/ranges.and.overlap.table.0.8.v11.csv',sep=''),header=TRUE)
# print(colnames(the.data))

all.pred <- read.csv(paste(getwd(),'/CongenersDataSummaryTable.0.8.v11.csv',sep=''),header=TRUE,row.names=1)

sp1.indices <- grep('p1',colnames(all.pred))
sp2.indices <- grep('p2',colnames(all.pred))
# print(colnames(all.pred)[sp1.indices])
# print(colnames(all.pred)[sp1.indices])
# stop('cbw')

all.pred[,c(sp1.indices,sp2.indices)][is.na(all.pred[,c(sp1.indices,sp2.indices)])==TRUE] <- 0
the.data$fut.p1 <- apply(all.pred[,sp1.indices],1,mean)
the.data$fut.p2 <- apply(all.pred[,sp2.indices],1,mean)

the.data <- the.data[the.data$overlap.cells==0,] # Only non-overlapping pairs
birds <- the.data[the.data$species=='birds',]
mammals <- the.data[the.data$species=='mammals',]
amphibians <- the.data[the.data$species=='amphibians',]
sink(paste(getwd(),'/asymetric.overlap.v2.txt',sep=''))
png(paste(getwd(),'/asymetric.overlap.v2.png',sep=''))
par(mfrow=c(2,2), mar=c(5,5,4,1))
	plot.fxn2(birds, main='Birds')
	plot.fxn2(mammals, main='Mammals')
	plot.fxn2(amphibians, main='Amphibians')
	plot.fxn2(the.data, main='All')
dev.off()
sink()

sink(paste(getwd(),'/asymetric.overlap.v3.txt',sep=''))
png(paste(getwd(),'/asymetric.overlap.v3.png',sep=''))
par(mfrow=c(2,2), mar=c(5,5,4,1))
	plot.fxn3(birds, main='Birds')
	plot.fxn3(mammals, main='Mammals')
	plot.fxn3(amphibians, main='Amphibians')
	plot.fxn3(the.data, main='All')
dev.off()
sink()
