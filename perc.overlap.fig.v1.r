# Figure for % overlap among pairs

setwd('//cfr.washington.edu/main/space/lawler/shared/hybridzone/')

birds <- read.csv(paste(getwd(),'/wh_obs/analysis.congenerics.birds.0.8.v3.csv',sep=''),header=TRUE,row.names=1)
mammals <- read.csv(paste(getwd(),'/wh_obs/analysis.congenerics.mammals.0.8.v3.csv',sep=''),header=TRUE,row.names=1)
amphibians <- read.csv(paste(getwd(),'/wh_obs/analysis.congenerics.amphibians.0.8.v3.csv',sep=''),header=TRUE,row.names=1)
all.pairs <- as.data.frame(rbind(birds,mammals,amphibians))

the.fun <- function(x) { return(c(min(x),max(x))) }
plot.fxn <- function(the.mat,main)
{
	plot.table <- apply(X=the.mat[,c('percent.1','percent.2')],MAR=1,FUN=the.fun)
	plot.table <- t(plot.table)
	plot(plot.table[,2] ~ plot.table[,1], xlim=c(0,100), ylim=c(0,100),pch='.',xlab='% overlap, larger range',ylab='% overlap, smaller range', main=main)
	abline(a=0,b=1,lwd=0.5)
}

png(paste(getwd(),'/current.percent.overlap.v1.png',sep=''))
	par(mfrow=c(2,2))
	plot.fxn(birds, main='Birds')
	plot.fxn(mammals, main='Mammals')
	plot.fxn(amphibians, main='Amphibians')
	plot.fxn(all.pairs, main='All')
dev.off()

# For GCMs
for (i in c('cccma_cgcm3.1_t47_run1','cnrm-cm3_run1','gfdl-cm2.0_run1','gfdl-cm2.1_run1','giss-er_run1','inm-cm3.0_run1','miroc3.2_medres_run1','mri_cgcm2.3.2a_run1','ncar_ccsm3.0_run1','ukmo-hadcm3_run1'))
{
	birds <- read.csv(paste(getwd(),'/wh_predictions/',i,'/congeners.analysis.birds.0.8.v5.csv',sep=''),header=TRUE,row.names=1)
	mammals <- read.csv(paste(getwd(),'/wh_predictions/',i,'/congeners.analysis.mammals.0.8.v5.csv',sep=''),header=TRUE,row.names=1)
	amphibians <- read.csv(paste(getwd(),'/wh_predictions/',i,'/congeners.analysis.amphibians.0.8.v5.csv',sep=''),header=TRUE,row.names=1)
	all.pairs <- as.data.frame(rbind(birds,mammals,amphibians))
	# stop('cbw')
	png(paste(getwd(),'/',i,'percent.overlap.v1.png',sep=''))
		par(mfrow=c(2,2))
		plot.fxn(birds, main='Birds')
		plot.fxn(mammals, main='Mammals')
		plot.fxn(amphibians, main='Amphibians')
		plot.fxn(all.pairs, main='All')
	dev.off()
}

