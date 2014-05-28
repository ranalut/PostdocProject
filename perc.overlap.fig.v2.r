# Figure for % overlap among pairs

# setwd('//cfr.washington.edu/main/space/lawler/shared/hybridzone/')
setwd("C:/Users/cwilsey/Dropbox/Inter-postdoctoral Interactions (1)/Figures 4 Revision")
source('plot.fxns.v2.r')

data.table <- read.csv(paste(getwd(),'/CongenersDataSummaryTable.0.8.v11.csv',sep=''),header=TRUE,row.names=1)
data.table <- data.table[,c('species','Species1.code','Species2.code','overlap.cells')]
print(dim(data.table))
# print(colnames(data.table))
data.table$range.cells.sp1 <- apply(data.table,1,range.size,group.name='species',col.name='Species1.code')
data.table$range.cells.sp2 <- apply(data.table,1,range.size,group.name='species',col.name='Species2.code')

other.table <- read.csv(paste(getwd(),'/CongenersDataSummaryAnalysis.0.8.v11.csv',sep=''),header=TRUE,row.names=1)

data.table <- data.frame(data.table, other.table[,c('cells.num.gcm.overlap','cells.new.overlap','cells.mean.overlap')])

write.csv(data.table, paste(getwd(),'/ranges.and.overlap.table.0.8.v11.csv',sep=''))
stop('cbw')
