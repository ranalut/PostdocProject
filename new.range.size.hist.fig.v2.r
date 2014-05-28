# Figure for % overlap among pairs

setwd('//cfr.washington.edu/main/space/lawler/shared/hybridzone/')
# source('perc.overlap.fig.v2.r') # Builds new table for these figures.

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

groups <- c('birds','mammals','amphibians')
the.data <- read.csv(paste(getwd(),'/ranges.and.overlap.table.0.8.v11.csv',sep=''),header=TRUE)
print(colnames(the.data))

the.data <- the.data[the.data$overlap.cells==0,] # Only non-overlapping pairs

png(paste(getwd(),'/hist.ranges.v1.png',sep=''))
par(mfrow=c(2,2))
for (i in 1:4)
{
	if (i==4) { i <- c(1:3) }
	# Histogram of ranges for species currently non-overlapping with at least one other species.
	temp <- stack.fxn(
				x=the.data[the.data$species %in% groups[i],],
				row.field='X',
				start.fields.1=c('Species1.code','range.cells.sp1','cells.num.gcm.overlap'),
				start.fields.2=c('Species2.code','range.cells.sp2','cells.num.gcm.overlap'),
				end.fields=c('row','code','range.cells','num.gcm')
				)
	
	# print(temp[1:10,])
	fig.table <- aggregate(num.gcm ~ code + range.cells, data=temp, FUN=max)
	# print(dim(fig.table))
	# print(fig.table[1:10,]); stop('cbw')
	fig.table$area <- fig.table$range.cells * 2500
	
	hist(fig.table$area,freq=FALSE)
	
	if (length(i)>1) { write.csv(fig.table, paste(getwd(),'/hist.fig.table.all.csv',sep='')) }
	else { write.csv(fig.table, paste(getwd(),'/hist.fig.table.',groups[i],'.csv',sep='')) }
}
dev.off()

stop('cbw')

all.pairs <- read.csv(paste(getwd(),'/CongenersDataSummaryAnalysis.0.8.v11.csv',sep=''),header=TRUE,stringsAsFactors=FALSE)
overlap <- read.csv(paste(getwd(),'/CongenersDataSummaryTable.0.8.v11.csv',sep=''),header=TRUE,stringsAsFactors=FALSE)


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

# the.fun <- function(x) { return(c(min(x),max(x))) }
# plot.fxn <- function(the.mat,main)
# {
	# plot.table <- apply(X=the.mat[,c('percent.1','percent.2')],MAR=1,FUN=the.fun)
	# plot.table <- t(plot.table)
	# plot(plot.table[,2] ~ plot.table[,1], xlim=c(0,100), ylim=c(0,100),pch='.',xlab='% overlap, larger range',ylab='% overlap, smaller range', main=main)
	# abline(a=0,b=1,lwd=0.5)
# }
