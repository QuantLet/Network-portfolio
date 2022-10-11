
##### SETTINGS #####

# Clean the environment 
graphics.off()
rm(list = ls(all = TRUE))

setwd("~/Documents/Code/Network structure based portfolio/Efficient frontier")

# Load Functions and other Files
source('./PackagesRobustMV.R')
source('./FunctionsRobustMV.R')


#Choose dataset to analyse
prices<-read_excel("SP500 securities.xlsx")
ZOO <- zoo(prices[,-1], order.by=as.Date(as.character(prices$Dates), format='%Y-%m-%d'))
#return
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)

# set label
node.label=colnames(returnstd)
node.label<-gsub("Equity","",node.label)
node.label<-gsub("UN","",node.label)
node.label<-gsub("UW","",node.label)

### Efficient frontier ###
er = colMeans(returnstd)
names(er) = node.label
evar = colSds(as.matrix(returnstd[sapply(returnstd, is.numeric)]))
names(evar) = node.label
covmat = cov(returnstd)
dimnames(covmat) = list(node.label, node.label)
r.free = 0.00005
# tangency portfolio
tan.port = tangency.portfolio(er, covmat, r.free)
# compute global minimum variance portfolio
gmin.port = globalMin.portfolio(er, covmat)
# compute portfolio frontier
ef  = efficient.frontier(er, covmat, alpha.min=-2,
                         alpha.max=1.5, nport=500)
# plot efficient frontier
pngname =  paste0("Efficient_frontiers.png")
png(file = pngname, width=1000, height=800, bg = "transparent")
plot(ef)
points(evar,  er, pch = 16)
dev.off()

