
##### SETTINGS #####

# Clean the environment 
graphics.off()
rm(list = ls(all = TRUE))

setwd("~/Documents/Code/Network_structure_based_portfolio/Network_Efficient_frontier_1120")

# Load Functions and other Files
source('./PackagesNetworkPortfolio.R')
source('./FunctionsNetworkPortfolio.R')

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
names(returnstd) = node.label

# network of portfolio based on correlation matrix
network_port = network.portfolio(returnstd)
# MST of portfolio
mst_port = MST.portfolio(returnstd)

# eigenvalue
centralization <- centr_eigen(mst_port)$centralization 
ec_port <- centr_eigen(mst_port)$vector
centralization1 <- eigen_centrality(mst_port,directed = FALSE, scale = TRUE)$value
ec_port1 <- eigen_centrality(mst_port,directed = FALSE, scale = TRUE)$vector
centralization2 <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$value
ec_port2 <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector

# setting
er = colMeans(returnstd)
names(er) = node.label
estd = colSds(as.matrix(returnstd[sapply(returnstd, is.numeric)]))
names(estd) = node.label
sr = er/estd
names(sr) = node.label
covmat = cov(returnstd)
dimnames(covmat) = list(node.label, node.label)
rm = rowMeans(returnstd)
sigma_m = sd(rm)
sigma_im = cov(returnstd,rm)
beta_port = sigma_im/sigma_m^2
corr_im=cor(returnstd,rm)
r.free = 0

# plot eigenvalue centrality 
pngname =  paste0("Eigenvalue centrality vs Mean.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(ec_port2,er,xlab="Eigenvalue centrality",ylab="Expected return")
line.model<-lm(er~ec_port2)
summary(line.model)
abline(line.model, lwd=2, col="red")
dev.off()
pngname =  paste0("Eigenvalue centrality vs Std.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(ec_port2,estd,xlab="Eigenvalue centrality",ylab="Standard deviation")
line.model<-lm(estd~ec_port2)
summary(line.model)
abline(line.model, lwd=2, col="red")
dev.off()
pngname =  paste0("Eigenvalue centrality vs Sharp ratio.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(ec_port2,sr,xlab="Eigenvalue centrality",ylab="Sharp ratio")
line.model<-lm(sr~ec_port2)
summary(line.model)
abline(line.model, lwd=2, col="red")
dev.off()
pngname =  paste0("Eigenvalue centrality vs Beta.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(ec_port2,beta_port,xlab="Eigenvalue centrality",ylab="Beta")
line.model<-lm(beta_port~ec_port2)
summary(line.model)
abline(line.model, lwd=2, col="red")
dev.off()
pngname =  paste0("Mean vs Std.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(estd,er,xlab="Standard deviation",ylab="Expected return")
line.model<-lm(er~estd)
abline(line.model, lwd=2, col="red")
dev.off()
pngname =  paste0("Std. vs Beta.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(estd,beta_port,xlab="Std.",ylab="Beta")
line.model<-lm(beta_port~estd)
summary(line.model)
abline(line.model, lwd=2, col="red")
dev.off()
pngname =  paste0("Std. vs Corr.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(estd,corr_im,xlab="Std.",ylab="Correlation")
line.model<-lm(corr_im~estd)
summary(line.model)
abline(line.model, lwd=2, col="red")
dev.off()

##### Efficient frontier of Markowitz #####
# tangency portfolio
tan.port = tangency.portfolio(er, covmat, r.free)
# compute global minimum variance portfolio
gmin.port = globalMin.portfolio(er, covmat,TRUE)
# compute portfolio frontier
ef  = efficient.frontier(er, covmat, alpha.min=-2,
                         alpha.max=1.5, nport=5000)
# QP
B<- rbind(matrix(1,1,dim(covmat)[1]), er, diag(dim(covmat)[1]))
f<- c(1, 0, rep(-0.001,dim(covmat)[1]))
sol<- solve.QP(Dmat=covmat, dvec = matrix(0,dim(covmat)[1]), Amat=t(B), bvec=f, meq=1)
w<-matrix(round(sol$solution,6))
mu_w4= t(w)%*%er
std_w4 = sqrt(t(w)%*%covmat%*%w)

# plot efficient frontier of mean-variance
pngname =  paste0("Efficient_frontiers.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(ef$sd,ef$er, plot.assets = F, pch = 16, xlab = "Standard Deviation", ylab="Mean")
points(estd,  er, pch = 16)
points(gmin.port$sd,  gmin.port$er, pch = 16,col="red")
dev.off()

##### Network efficient frontier #####
# compute global minimum variance portfolio
net.gmin.port = network.globalMin.portfolio(ec_port2, covmat,TRUE)
# compute portfolio frontier of network
net.ef  = network.efficient.frontier(ec_port2, covmat, alpha.min=-0.1,
                         alpha.max=2, nport=5000)
# plot efficient frontier of eigenvalue centrality-variance
pngname =  paste0("Efficient_frontiers_eigenvalue_centrality.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(net.ef$sd,net.ef$nc,xlab="Standard deviation",ylab="Portfolio centrality")
points(estd,  ec_port2, pch = 16)
points(net.gmin.port$sd,  net.gmin.port$nc, pch = 16,col="red")
dev.off()
# animation of efficient frontier
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in seq(1,-0.1,-0.02) ) {
  plot(net.ef$sd,net.ef$nc,xlab="Standard deviation",ylab="Portfolio centrality")
  #points(estd,  ec_port2, pch = 16)
  net.ef1<-network.efficient.frontier.moving(ec_port2, covmat, alpha=i)
  points(net.ef1$sd,  net.ef1$nc, pch = 16,col="red")
}
dev.off()
animation <- image_animate(fig, fps = 5)
image_write(animation, paste0(getwd(), "/Efficient_frontiers_eigenvector_centrality_movie.gif"))
# animation of efficient frontier 1
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in seq(1.1,net.gmin.port$nc,-0.02)) {
  plot(net.ef$sd,net.ef$nc,xlab="Standard deviation",ylab="Portfolio centrality")
  lines(seq(0.0044,0.0067,0.0001),matrix(i,length(seq(0.0044,0.0067,0.0001)),1),col="red",lwd = 0.5)
  points(net.gmin.port$sd,  net.gmin.port$nc, pch = 16,col="red")
}
for (i in seq(1,-0.1,-0.02) ) {
  plot(net.ef$sd,net.ef$nc,xlab="Standard deviation",ylab="Portfolio centrality")
  #points(estd,  ec_port2, pch = 16)
  net.ef1<-network.efficient.frontier.moving(ec_port2, covmat, alpha=i)
  points(net.ef1$sd,  net.ef1$nc, pch = 16,col="red")
  lines(seq(0.0044,0.0067,0.0001),matrix(net.ef1$nc,length(seq(0.0044,0.0067,0.0001)),1),col="red",lwd = 0.5)
}
dev.off()
animation <- image_animate(fig, fps = 5)
image_write(animation, paste0(getwd(), "/Efficient_frontiers_eigenvector_centrality_movie1.gif"))

# portfolio frontier containing more assets 
net.ef.more  = network.efficient.frontier(ec_port2, covmat, alpha.min=-10,
                                     alpha.max=10, nport=500)
# plot efficient frontier of eigenvalue centrality-variance
pngname =  paste0("Efficient_frontiers_eigenvalue_centrality_more.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(net.ef.more$sd,net.ef.more$nc,xlab="Standard deviation",ylab="Portfolio centrality")
points(estd,  ec_port2, pch = 16)
points(net.gmin.port$sd,  net.gmin.port$nc, pch = 16,col="red")
dev.off()

# plot eigenvalue centrality and weights
pngname =  paste0("Eigenvalue_centrality vs weights.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(ec_port2,net.gmin.port$weights,xlab="Eigenvalue centrality",ylab="Weight")
line.model<-lm(net.gmin.port$weights~ec_port2)
summary(line.model)
abline(line.model, lwd=2, col="red")
#points(estd,  ec_port, pch = 16)
#points(net.gmin.port$sd,  net.gmin.port$nc, pch = 16,col="red")
dev.off()

# boxplot
pngname =  paste0("boxplot.png")
df=data.frame(ec_port2,sr)
boxplot(df)

# plot centrality 


