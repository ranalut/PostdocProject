# Figure for % overlap among pairs

setwd('//cfr.washington.edu/main/space/lawler/shared/hybridzone/')

all.pairs <- read.csv(paste(getwd(),'/CongenersDataSummaryAnalysis.0.8.v11.csv',sep=''),header=TRUE,stringsAsFactors=FALSE)
overlap <- read.csv(paste(getwd(),'/CongenersDataSummaryTable.0.8.v11.csv',sep=''),header=TRUE,stringsAsFactors=FALSE)
groups <- c('birds','mammals','amphibians')

stack.fxn <- function(x,row.field,start.fields.1,start.fields.2,end.fields)
{
	temp1 <- x[,c(row.field,start.fields.1)]
	temp2 <- x[,c(row.field,start.fields.2)]
	temp2[,row.field] <- temp2[,row.field] + dim(temp1)[1]
	colnames(temp1) <- end.fields
	colnames(temp2) <- end.fields
	temp <- rbind(temp1,temp2)
	cat('dimensions of stacked table',dim(temp),'\n')
	return(temp)
}

range.size <- function(x,group)
{
	# Read in presence/absence data
	the.range <- as.numeric(scan(file = paste(getwd(),'/WH_obs/',group,'/',x['code'],'.txt',sep=''), what = "numeric", quiet=TRUE))
	
	# Ranges
	range.cells <- length(the.range[the.range==1])
	# range.area <- range.cells * 2500
	output.vector <- c(range.cells) # ,range.area)
	return(output.vector)
}

png(paste(getwd(),'/hist.ranges.v1.png',sep=''),width=1440)
par(mfrow=c(1,3))
for (i in 1:3)
{
	# Histogram of ranges for species currently non-overlapping with at least one other species.
	temp <- stack.fxn(
				x=overlap[overlap$species==groups[i],],
				row.field='X',
				start.fields.1=c('Species1.code','overlap.cells'),
				start.fields.2=c('Species2.code','overlap.cells'),
				end.fields=c('row','code','overlap')
				)
	
	temp <- temp[temp$overlap==0,]
	cat('dimensions of reduced table',dim(temp),'\n')
	test <- duplicated(temp$code)
	non.overlap.ranges <- temp[test==FALSE,]
	print(dim(non.overlap.ranges))
	# print(head(non.overlap.ranges)); stop('cbw')
	# Add current range sizes...
	non.overlap.ranges$cells <- apply(non.overlap.ranges,1,range.size, group=groups[i])
	non.overlap.ranges$area <- non.overlap.ranges$cells * 2500
	
	hist(non.overlap.ranges$area,freq=FALSE)
}
dev.off()

# the.fun <- function(x) { return(c(min(x),max(x))) }
# plot.fxn <- function(the.mat,main)
# {
	# plot.table <- apply(X=the.mat[,c('percent.1','percent.2')],MAR=1,FUN=the.fun)
	# plot.table <- t(plot.table)
	# plot(plot.table[,2] ~ plot.table[,1], xlim=c(0,100), ylim=c(0,100),pch='.',xlab='% overlap, larger range',ylab='% overlap, smaller range', main=main)
	# abline(a=0,b=1,lwd=0.5)
# }
