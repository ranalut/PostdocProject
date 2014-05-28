
range.size <- function(x,group.name,col.name)
{
	# Read in presence/absence data
	if (x[group.name]=='amphibians') { x[group.name] <- 'amphibs' }
	the.range <- as.numeric(scan(file = paste(getwd(),'/WH_obs/',x[group.name],'/',x[col.name],'.txt',sep=''), what = "numeric", quiet=TRUE))
	
	# Ranges
	range.cells <- length(the.range[the.range==1])
	# range.area <- range.cells * 2500
	output.vector <- c(range.cells) # ,range.area)
	return(output.vector)
}

the.fun <- function(x) { return(c(min(x),max(x))) }

the.fun2 <- function(x) { return(as.numeric(x[c('fut.p1','fut.p2')][order(x[c('range.cells.sp1','range.cells.sp2')])])) }

the.fun3 <- function(x) { return(as.numeric(x[c('fut.p1','fut.p2')][order(x[c('fut.p1','fut.p2')])])) }

plot.fxn2 <- function(the.mat,main)
{
  cat('################\n',main,'\n##############\n')
  plot.table <- apply(X=the.mat,MAR=1,FUN=the.fun2)
  plot.table <- t(plot.table)
  # print(head(plot.table)); stop('cbw')
  plot(plot.table[,2] ~ plot.table[,1], xlim=c(0,100), ylim=c(0,100),pch=20,xlab='mean % future overlap,\nsmaller current range',ylab='mean % future overlap,\nlarger current range', main=main)
  model <- lm(plot.table[,2] ~ plot.table[,1])
  abline(a=0,b=1,lwd=0.5)
  # print(coef(model)); dev.off(); stop('cbw')
  print(summary(model))
  abline(a=as.numeric(coef(model)[2]), b=as.numeric(coef(model)[1]), lty=2)
}

plot.fxn3 <- function(the.mat,main)
{
  cat('################\n',main,'\n##############\n')
  plot.table <- apply(X=the.mat,MAR=1,FUN=the.fun3)
  plot.table <- t(plot.table)
  # print(head(plot.table)); stop('cbw')
  plot(plot.table[,2] ~ plot.table[,1], xlim=c(0,100), ylim=c(0,100),pch=20,xlab='mean % future overlap,\nsmaller range',ylab='mean % future overlap,\nlarger range', main=main)
  model <- lm(plot.table[,2] ~ plot.table[,1])
  abline(a=0,b=1,lwd=0.5)
  # print(coef(model)); dev.off(); stop('cbw')
  print(summary(model))
  # abline(a=as.numeric(coef(model)[2]), b=as.numeric(coef(model)[1]), lty=2)
}
