# network_portfolio version 1.1.2

# Comparing version 1.0.2
#
# In this file,  we modify network constructions, adjacency matrix by correlation
#                   matrix - diag(1,p)

rm(list = ls())

setwd("~/Documents/Code/Network structure based portfolio/Dantzig-selector estimation covariance/Dantzig-selector_estimation_0925")

# Load Functions and other Files
source('./PackagesNetworkPortfolio.R')
source('./FunctionsNetworkPortfolio.R')

# load data
prices<-read_excel("SP500 securities.xlsx")
ZOO <- zoo(prices[,-1], order.by=as.Date(as.character(prices$Dates), format='%Y-%m-%d'))

#return
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)
p=dim(return)[2]

# set label
node.label=colnames(returnstd)
node.label<-gsub("Equity","",node.label)
node.label<-gsub("UN","",node.label)
node.label<-gsub("UW","",node.label)
names(returnstd) = node.label

# rolling window
W<-list()
for(t in 0: 166){
  W[[(t+1)]]=returnstd[(1+t*7):(500+t*7),]
}
W_in<-list()
W_out<-list()
for(t in 0: 166){
  W_in[[(t+1)]]=W[[t+1]][c(1:493),]
  W_out[[(t+1)]]=W[[t+1]][c(494:500),]
}
T.windows<-length(W)
# correlation matrix, Expected return, covariance matrix
C_in <- list()     
ER_in <- list()     
COV_in <- list()     
EC_in <- list()  
EC.posi_in <- list()  
EC.nega_in <- list()  
BetweennessCentrality<-list()
ClosenessCentrality<-list()
DegreeCentrality<-list()
for(t in 1: length(W_in)){
  C_in[[(t)]] =cor(W_in[[(t)]])
  ER_in[[(t)]] = colMeans(W_in[[(t)]])
  COV_in[[(t)]] = cov(W_in[[(t)]])
  # network_port = network.portfolio(W_in[[(t)]])
  network_port = network.correlation(W_in[[(t)]])
  # EC_in[[(t)]] <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector
  # EC_in[[t]]<-eigen(C_in[[(t)]])$vector[,1]
  # network_port<-network.abs(W_in[[(t)]])
  EC_in[[(t)]] <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector
  BetweennessCentrality[[(t)]] <- centr_degree(network_port)
  ClosenessCentrality[[(t)]] <- eigen_centrality(network_port)
  DegreeCentrality[[(t)]] <- eigen_centrality(network_port)
  network_in<-network.posi.naga(W_in[[(t)]])
  EC.posi_in[[(t)]] <- eigen_centrality(network_in$positive,directed = FALSE, scale = TRUE)$vector
  EC.nega_in[[(t)]] <- eigen_centrality(network_in$negative,directed = FALSE, scale = TRUE)$vector
}

# generate timeline "cri" for later portfolio
date<-prices$Dates
date<-as.Date.factor(date[-c(1:501)], format="%Y-%m-%d")
meanlayer1<-list()
for(t in 1: 167){
  meanlayer1[[t]]<-date[[t*7]]
}
meanlayer1<-as.Date(unlist(meanlayer1))
psi<-data.frame(meanlayer1)
psi<-t(psi)
gloxi<-t(data.frame(psi))  # can not combine
gloxia<-data.frame(gloxi)            # can not combine
glox<-as.data.frame(gloxia$meanlayer1) # can not combine
glox<-data.frame(glox)                 # can not combine
glox<-as.matrix(glox$gloxia.meanlayer1)
glox<-glox[-168]
cri<-as.Date(as.character(glox)) # from "2017-12-13" to "2022-05-27"

#### Dantzig estimation ####

##### CV for tuning lambda #####

######CV for tuning lambda in estimation Sigma^-1 1, using the whole sample ######
# n<-dim(returnstd)[1]
# B=250
# n.block=floor(n/B)+1
# block.start=1+(0:(n.block-1))*B
# test.block=sort(sample(1:n.block,floor(n.block/4)))
# test.ind=NULL
# for(k in test.block)
# {
#   test.ind=c(test.ind,block.start[k]:(min(block.start[k]+B-1,n)))
# }
# n.test=length(test.ind)
# train.ind=setdiff(1:n,test.ind)
# n.train=length(train.ind)
# returnstd.train=returnstd[train.ind,]
# returnstd.test=returnstd[test.ind,]
# mu.train=rep(1,p)
# mu.test=rep(1,p)
# cov.train=cov(returnstd.train)
# cov.test=cov(returnstd.test)
# lambda.grid=seq(max(abs(mu.train))/100,max(abs(mu.train)),length=101)[2:101]
# l.lambda=length(lambda.grid)
# cv.l.error=NULL
# for(i in 1:l.lambda){
#   lmd=lambda.grid[i]
#   print(i)
#   lin.train=linfun1(cov.train,mu.train,lmd)
#   error=sum((cov.test%*%lin.train-mu.test)^2)
#   cv.l.error=c(cv.l.error,error)
# }
# plot(lambda.grid,cv.l.error)
# lmd1=min(lambda.grid[which(cv.l.error==min(cv.l.error))])
# lmd1

###### CV for tuning lambda in estimation Sigma^-1 1 using the first 500 data ######
# n<-dim(returnstd[1:500,])[1]
# B=100
# n.block=floor(n/B)+1
# block.start=1+(0:(n.block-1))*B
# test.block=sort(sample(1:n.block,floor(n.block/4)))
# test.ind=NULL
# for(k in test.block){
#   test.ind=c(test.ind,block.start[k]:(min(block.start[k]+B-1,n)))
# }
# n.test=length(test.ind)
# train.ind=setdiff(1:n,test.ind)
# n.train=length(train.ind)
# returnstd.train=returnstd[1:500,][train.ind,]
# returnstd.test=returnstd[1:500,][test.ind,]
# mu.train=rep(1,p)
# mu.test=rep(1,p)
# cov.train=cov(returnstd.train)
# cov.test=cov(returnstd.test)
# lambda.grid=seq(0.25,max(abs(mu.train)),length=101)[2:101]
# l.lambda=length(lambda.grid)
# cv.l.error=NULL
# for(i in 1:l.lambda){
#   lmd=lambda.grid[i]
#   print(i)
#   lin.train=linfun1(cov.train,mu.train,lmd)
#   error=sum((cov.test%*%lin.train-mu.test)^2)
#   cv.l.error=c(cv.l.error,error)
# }
# plot(lambda.grid,cv.l.error)
# lmd1=min(lambda.grid[which(cv.l.error==min(cv.l.error))])
# lmd1 
lmd1 <- 0.3025

###### CV for tuning lambda in estimation Sigma^-1 mu, using the first 500 data ######
# n<-dim(returnstd[1:500,])[1]
# B=100
# n.block=floor(n/B)+1
# block.start=1+(0:(n.block-1))*B
# test.block=sort(sample(1:n.block,floor(n.block/4)))
# test.ind=NULL
# for(k in test.block){
#   test.ind=c(test.ind,block.start[k]:(min(block.start[k]+B-1,n)))
# }
# n.test=length(test.ind)
# train.ind=setdiff(1:n,test.ind)
# n.train=length(train.ind)
# returnstd.train=returnstd[1:500,][train.ind,]
# returnstd.test=returnstd[1:500,][test.ind,]
# mu.train=colMeans(returnstd.train)
# mu.test=colMeans(returnstd.test)
# cov.train=cov(returnstd.train)
# cov.test=cov(returnstd.test)
# lambda.grid=seq(max(abs(mu.train))/100,max(abs(mu.train)),length=101)[2:101]
# l.lambda=length(lambda.grid)
# cv.l.error=NULL
# for(i in 1:l.lambda){
#   lmd=lambda.grid[i]
#   print(i)
#   print(lmd)
#   lin.train=linfun1(cov.train,mu.train,lmd)
#   #	lin.test=linfun(cov.test,mu.test,lmd)
#   error=sum((cov.test%*%lin.train-mu.test)^2)
#   cv.l.error=c(cv.l.error,error)
#   #print(i)
# }
# plot(lambda.grid,cv.l.error)
# lmd2=min(lambda.grid[which(cv.l.error==min(cv.l.error))])
# lmd2 
lmd2 <- 0.001653768

##### CV for tuning parameter in glasso #####
# n<-dim(returnstd[1:500,])[1]
# B=100
# n.block=floor(n/B)+1
# block.start=1+(0:(n.block-1))*B
# test.block=sort(sample(1:n.block,floor(n.block/4)))
# test.ind=NULL
# for(k in test.block){
#   test.ind=c(test.ind,block.start[k]:(min(block.start[k]+B-1,n)))
# }
# n.test=length(test.ind)
# train.ind=setdiff(1:n,test.ind)
# n.train=length(train.ind)
# returnstd.train=returnstd[1:500,][train.ind,]
# returnstd.test=returnstd[1:500,][test.ind,]
# mu.train=colMeans(returnstd.train)
# mu.test=colMeans(returnstd.test)
# cov.train=cov(returnstd.train)
# cov.test=cov(returnstd.test)
# rho.grid=seq(0,0.8,length=101)[2:101]
# l.rho=length(rho.grid)
# cv.rho.error=NULL
# for (i in 1:l.rho){
#   rho=rho.grid[i]
#   prec.glasso=glasso(cov.train,rho=rho)$wi
#   error=sum((prec.glasso%*%cov.test-diag(rep(1,p)))^2)
#   cv.rho.error=c(cv.rho.error,error)
#   print(i)
# }
# plot(rho.grid,cv.rho.error)
# rho=rho.grid[which(cv.rho.error==min(cv.rho.error))]
# rho 
rho<-0.032

###### CV for tuning lambda in estimation Sigma^-1 phi, using the first 500 data ######
# n<-dim(returnstd[1:500,])[1]
# B=100
# n.block=floor(n/B)+1
# block.start=1+(0:(n.block-1))*B
# test.block=sort(sample(1:n.block,floor(n.block/4)))
# test.ind=NULL
# for(k in test.block){
#   test.ind=c(test.ind,block.start[k]:(min(block.start[k]+B-1,n)))
# }
# n.test=length(test.ind)
# train.ind=setdiff(1:n,test.ind)
# n.train=length(train.ind)
# returnstd.train=returnstd[1:500,][train.ind,]
# returnstd.test=returnstd[1:500,][test.ind,]
# # train centrality
# network.train = network.portfolio(returnstd.train)
# mu.train <- eigen_centrality(network.train,directed = FALSE, scale = TRUE)$vector
# # test centrality
# network.test = network.portfolio(returnstd.test)
# mu.test <- eigen_centrality(network.test,directed = FALSE, scale = TRUE)$vector
# cov.train=cov(returnstd.train)
# cov.test=cov(returnstd.test)
# lambda.grid=seq(0.07,max(abs(mu.train)),length=101)[2:101]
# l.lambda=length(lambda.grid)
# cv.l.error=NULL
# for(i in 1:l.lambda){
#   lmd=lambda.grid[i]
#   print(i)
#   print(lmd)
#   lin.train=linfun1(cov.train,mu.train,lmd)
#   #	lin.test=linfun(cov.test,mu.test,lmd)
#   error=sum((cov.test%*%lin.train-mu.test)^2)
#   cv.l.error=c(cv.l.error,error)
#   #print(i)
# }
# plot(lambda.grid,cv.l.error)
# lmd3=min(lambda.grid[which(cv.l.error==min(cv.l.error))])
# lmd3 
lmd3 <- 0.1816

##### Dantzig selector estimation for theta1, theta2, theta3 #####
# #      theta1 <- Sigma^-1 1
# #      theta2 <- Sigma^-1 mu
# #      theta3 <- Sigma^-1 phi
# theta1<-list()
# theta2<-list()
# theta3<-list()
# # for(t in 6: length(W_in)){
# for(t in 1: 5){
#   print(t)
#   ptm<-proc.time()
#   ## compute global minimum variance portfolio ##
#   theta1[[t]] =linfun1(COV_in[[t]],rep(1,p),lambda=lmd1) # lambda <= 0.1 will lead to be infeasible
#   print('theta1')
#   theta2[[t]] =linfun1(COV_in[[t]],ER_in[[t]],lambda=lmd2) # lambda <= 0.1 will lead to be infeasible
#   print('theta2')
#   theta3[[t]] =linfun1(COV_in[[t]],EC_in[[t]],lambda=lmd3) # lambda <= 0.1 will lead to be infeasible
#   ptm<-proc.time()-ptm
#   print(ptm)
# }
# theta<-list("theta1"=theta1,
#             "theta2"=theta2,
#             "theta3"=theta3)
# save(theta,file="theta_Dantzig.RData")
load("theta_Dantzig.RData")
theta1=theta$theta1
theta2=theta$theta2
theta3=theta$theta3

#### exercise for glasso comparison ####
# Op.return=NULL
# Plugin.return=NULL
# Glasso.return=NULL
# Op.return1=NULL
# Plugin.return1=NULL
# Glasso.return1=NULL
# Op.risk=NULL
# Plugin.risk=NULL
# Glasso.risk=NULL
# Op.cost=NULL
# Glasso.cost=NULL
# Plugin.cost=NULL
# m=1
# for (t in 1:(length(W_in)-1)) {
#   print(t)
#   cov.past<-COV_in[[t]]
#   mu.past<-ER_in[[t]]
#   newdata=W_in[[t+1]]
#   newS=cov(newdata)
#   newmu=colMeans(newdata)
#   # lin.past=linfun1(cov.past,mu.past,lmd2)
#   lin.past=theta$theta2
#   Delta=sum(mu.past*lin.past)
#   omega=m/Delta*lin.past # weight
#   cost=sum(omega)       # sum of weight
#   Op.cost=c(Op.cost,cost)
#   oprisk=t(omega)%*%newS%*%omega    # variance of new time window
#   Op.risk=c(Op.risk,oprisk)      
#   monthrate<-col_sums(W_out[[t]])   # out of sample return
#   opreturn=sum(omega*monthrate)     # out of sample portfolio performance
#   opreturn1=sum(omega*newmu)        # next window portfolio performance
#   Op.return=c(Op.return,opreturn)
#   Op.return1=c(Op.return1,opreturn1)
#   
#   # plug in
#   plugin.theta=tryCatch(ginv(cov.past)%*%mu.past, error=function(e) NA)
#   Delta=sum(mu.past*plugin.theta)
#   plugin.omega=plugin.theta*m/Delta # weight
#   pluginreturn=sum(t(plugin.omega)*monthrate) # out of sample portfolio performance
#   pluginreturn1=sum(plugin.omega*newmu)       # next window portfolio performance
#   Plugin.return=c(Plugin.return,pluginreturn)
#   Plugin.return1=c(Plugin.return1,pluginreturn1)
#   pluginrisk=tryCatch(t(plugin.omega)%*%newS%*%plugin.omega,error=function(e) NA) # variance of new time window
#   Plugin.risk=c(Plugin.risk,pluginrisk)
#   cost=sum(plugin.omega)            # sum of weight
#   Plugin.cost=c(Plugin.cost,cost)
#   
#   #  glasso
#   glasso.icov=glasso(cov.past,rho=rho)$wi
#   glasso.theta=glasso.icov%*%mu.past
#   Delta=sum(mu.past*glasso.theta)
#   glasso.omega=glasso.theta*m/Delta  # weight of glasso
#   glassoreturn=sum(glasso.omega*monthrate) # out of sample portfolio performance
#   glassoreturn1=sum(glasso.omega*newmu)    # next window portfolio performance
#   Glasso.return=c(Glasso.return,glassoreturn)
#   Glasso.return1=c(Glasso.return1,glassoreturn1)
#   glassorisk=t(glasso.omega)%*%newS%*%glasso.omega  # variance of new time window
#   Glasso.risk=c(Glasso.risk,glassorisk)
#   cost=sum(glasso.omega)
#   Glasso.cost=c(Glasso.cost,cost)#eqreturn=mean(monthrate)
#   
# }

# result=matrix(0,3,4)
# colnames(result)=c("mean.return","sd.return","risk","cost")
# rownames(result)=c("functional","plugin","glasso")
# result[1,1]=mean(Op.return,na.rm=T)
# result[1,2]=sd(Op.return,na.rm=T)
# result[1,3]=mean(Op.risk,na.rm=T)
# result[2,1]=mean(Plugin.return,na.rm=T)
# result[2,2]=sd(Plugin.return,na.rm=T)
# result[2,3]=mean(Plugin.risk,na.rm=T)
# result[3,1]=mean(Glasso.return)
# result[3,2]=sd(Glasso.return)
# result[3,3]=mean(Glasso.risk,na.rm=T)
# result[3,3]=mean(Glasso.risk,na.rm=T)
# result[1,4]=mean(Op.cost,na.rm=T)
# result[2,4]=mean(Plugin.cost,na.rm=T)
# result[3,4]=mean(Glasso.cost,na.rm=T)
# jpeg("risk.jpeg")
# par(mfrow=c(2,1))
# plot(Op.risk,type="l",lwd=2,ylim=range(c(Op.risk,Plugin.risk,Glasso.risk)))
# lines(Plugin.risk,lwd=2,col=2,lty=2)
# lines(Glasso.risk,lwd=2,col=3,lty=3)
# plot(Op.return,type="l",lwd=2,ylim=range(c(Op.return,Plugin.return,Glasso.return)))
# lines(Plugin.return,lwd=2,col=2,lty=2)
# lines(Glasso.return,lwd=2,col=3,lty=3)
# dev.off()

#### Weights of different portfolios in each rolling window ####
##### minimum variance portfolio  #####

###### minimum variance portfolio with Dantzig estimation  ######
w<-list()
cumureturn_minVar_Dantzig<-list()
for(t in 1: length(W_in)){
  w[[t]]=theta1[[t]]/sum(theta1[[t]])
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_minVar_Dantzig[[t]]<-rowSums(aus)
}
return_minVar_Dantzig<-as.matrix(cbind(unlist(cumureturn_minVar_Dantzig)))
cumureturn_minVar_Dantzig<-cumsum(return_minVar_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_minVar_Dantzig<-w

###### minimum variance portfolio with plug in  ######
w<-list()
cumureturn_minVar<-list()
for(t in 1: length(W_in)){
  portf_minVar =globalMin.portfolio(ER_in[[(t)]],COV_in[[(t)]])
  w[[(t)]] =portf_minVar$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_minVar[[t]]<-rowSums(aus)
}
return_minVar<-as.matrix(cbind(unlist(cumureturn_minVar)))
cumureturn_minVar<-cumsum(return_minVar)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_minVar<-w

##### minimum variance portfolio with glasso #####
w<-list()
cumureturn<-list()
for(t in 1: length(W_in)){
  glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
  w[[t]]=row_sums(glasso.icov)/sum(glasso.icov)
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn[[t]]<-rowSums(aus)
}
return_minVar_glasso<-as.matrix(cbind(unlist(cumureturn)))
cumureturn_minVar_glasso<-cumsum(return_minVar_glasso)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_minVar_glasso<-w

# ##### minimum variance portfolio with Dantzig estimation no short #####
# w<-list()
# cumureturn_minVar_Dantzig_noshort<-list()
# for(t in 1: length(W_in)){
#   portf_minVar =globalMin.portfolio(ER_in[[(t)]],COV_in[[(t)]],FALSE)
#   w[[(t)]] =portf_minVar$weights
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_minVar_Dantzig_noshort[[t]]<-rowSums(aus)
# }
# return_minVar_Dantzig_noshort<-as.matrix(cbind(unlist(cumureturn_minVar_Dantzig_noshort)))
# cumureturn_minVar_Dantzig_noshort<-cumsum(return_minVar_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_minVar_Dantzig_noshort<-w

###### minimum variance portfolio with plug in no short ######
w<-list()
cumureturn_minVar_noshort<-list()
for(t in 1: length(W_in)){
  portf_minVar =globalMin.portfolio(ER_in[[(t)]],COV_in[[(t)]],FALSE)
  w[[(t)]] =portf_minVar$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_minVar_noshort[[t]]<-rowSums(aus)
}
return_minVar_noshort<-as.matrix(cbind(unlist(cumureturn_minVar_noshort)))
cumureturn_minVar_noshort<-cumsum(return_minVar_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_minVar_noshort<-w

##### minimum variance portfolio with glasso no short #####
# w<-list()
# cumureturn<-list()
# for(t in 1: length(W_in)){
#   glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
#   w[[t]]=row_sums(glasso.icov)/sum(glasso.icov)
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn[[t]]<-rowSums(aus)
# }
# return_minVar_glasso_noshort<-as.matrix(cbind(unlist(cumureturn)))
# cumureturn_minVar_glasso_noshort<-cumsum(return_minVar_glasso_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_minVar_glasso_noshort<-w

##### mean variance portfolio  #####
###### mean variance portfolio with plug in  ######
w<-list()
cumureturn_meanVar<-list()
for(t in 1: length(W_in)){
  portf_meanVar =efficient.portfolio(ER_in[[(t)]],COV_in[[(t)]],mean(ER_in[[t]]))
  # alpha=(sum(theta2[[t]])*sum(theta1[[t]])*mean(ER_in[[t]])-(sum(theta2[[t]]))^2)/(ER_in[[(t)]]%*%theta2[[t]]*sum(theta1[[t]])-(sum(theta2[[t]]))^2)
  w[[(t)]] =portf_meanVar$weights
  # w[[(t)]] = alpha*theta2[[t]]/sum(theta2[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_meanVar[[t]]<-rowSums(aus)
}
return_meanVar<-as.matrix(cbind(unlist(cumureturn_meanVar)))
cumureturn_meanVar<-cumsum(return_meanVar)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_meanVar<-w

###### mean variance portfolio with Dantzig estimation  ######
w<-list()
cumureturn_meanVar_Dantzig<-list()
for(t in 1: length(W_in)){
  # portf_meanVar =efficient.portfolio(ER_in[[(t)]],COV_in[[(t)]],mean(ER_in[[t]]))
  alpha=(sum(theta2[[t]])*sum(theta1[[t]])*mean(ER_in[[t]])-(sum(theta2[[t]]))^2)/(ER_in[[(t)]]%*%theta2[[t]]*sum(theta1[[t]])-(sum(theta2[[t]]))^2)
  w[[(t)]] = alpha*theta2[[t]]/sum(theta2[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_meanVar_Dantzig[[t]]<-rowSums(aus)
}
return_meanVar_Dantzig<-as.matrix(cbind(unlist(cumureturn_meanVar_Dantzig)))
cumureturn_meanVar_Dantzig<-cumsum(return_meanVar_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_meanVar_Dantzig<-w

###### mean varriance portfolio with glasso ######
w<-list()
cumureturn<-list()
for(t in 1: length(W_in)){
  glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
  alpha=(sum(glasso.icov%*%ER_in[[t]])*sum(glasso.icov)*mean(ER_in[[t]])-(sum(glasso.icov%*%ER_in[[t]]))^2)/(ER_in[[(t)]]%*%glasso.icov%*%ER_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%ER_in[[t]]))^2)
  w[[(t)]] = c(alpha[1]*glasso.icov%*%ER_in[[t]]/sum(glasso.icov%*%ER_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn[[t]]<-rowSums(aus)
}
return_meanVar_glasso<-as.matrix(cbind(unlist(cumureturn)))
cumureturn_meanVar_glasso<-cumsum(return_meanVar_glasso)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_meanVar_glasso<-w

###### mean variance portfolio with plug in no short ######
w<-list()
cumureturn_meanVar_noshort<-list()
for(t in 1: length(W_in)){
  portf_meanVar =efficient.portfolio(ER_in[[(t)]],COV_in[[(t)]],mean(ER_in[[t]]),shorts = FALSE)
  w[[(t)]] =portf_meanVar$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_meanVar_noshort[[t]]<-rowSums(aus)
}
return_meanVar_noshort<-as.matrix(cbind(unlist(cumureturn_meanVar_noshort)))
cumureturn_meanVar_noshort<-cumsum(return_meanVar_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_meanVar_noshort<-w

###### mean variance portfolio with Dantzig no short ######
# w<-list()
# cumureturn_meanVar_noshort<-list()
# for(t in 1: length(W_in)){
#   portf_meanVar =efficient.portfolio(ER_in[[(t)]],COV_in[[(t)]],mean(ER_in[[t]]),shorts = FALSE)
#   w[[(t)]] =portf_meanVar$weights
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_meanVar_noshort[[t]]<-rowSums(aus)
# }
# return_meanVar_Dantzig_noshort<-as.matrix(cbind(unlist(cumureturn_meanVar_Dantzig_noshort)))
# cumureturn_meanVar_Dantzig_noshort<-cumsum(return_meanVar_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_meanVar_Dantzig_noshort<-w

###### mean varriance portfolio with glasso no short ######
# w<-list()
# cumureturn<-list()
# for(t in 1: length(W_in)){
#   glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
#   alpha=(sum(glasso.icov%*%ER_in[[t]])*sum(glasso.icov)*mean(ER_in[[t]])-(sum(glasso.icov%*%ER_in[[t]]))^2)/(ER_in[[(t)]]%*%glasso.icov%*%ER_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%ER_in[[t]]))^2)
#   w[[(t)]] = c(alpha[1]*glasso.icov%*%ER_in[[t]]/sum(glasso.icov%*%ER_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn[[t]]<-rowSums(aus)
# }
# return_meanVar_glasso_noshort<-as.matrix(cbind(unlist(cumureturn)))
# cumureturn_meanVar_glasso_noshort<-cumsum(return_meanVar_glasso_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_meanVar_glasso_noshort<-w

##### equally weighted portfolio #####
w<-list()
cumureturn_temporal<-list()
centrality_equal_portfolio<-list()
for(t in 1: length(W_in)){
  w[[(t)]] =matrix(1/465,1,465)
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
  centrality_equal_portfolio[[t]]<-as.double(w[[(t)]]%*%EC_in[[(t)]])
}
return_equal<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_equal<-cumsum(return_equal)
centrality_equal_portfolio<-as.matrix(cbind(unlist(centrality_equal_portfolio)))
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_equal<-w

##### network portfolio 1 constraint #####
### default setting centrality constraint as mean centrality ###
###### network portfolio fixed constraint as mean centrality with plug in ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],mean(EC_in[[(t)]]))
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_1constraint<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_1constraint<-cumsum(return_network_1constraint)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_1constraint<-w

###### network portfolio fixed constraint as mean centrality with Dantzig ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_in[[(t)]])>mean(EC_in[[t]])){
    alpha=(sum(theta3[[t]])*sum(theta1[[t]])*mean(EC_in[[t]])-(sum(theta3[[t]]))^2)/(EC_in[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2)
    w[[(t)]] = c(alpha)*theta3[[t]]/sum(theta3[[t]])+(1-c(alpha))*theta1[[t]]/sum(theta1[[t]])
    }
  else{
    w[[(t)]] = theta1[[t]]/sum(theta1[[t]])
  }
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_1constraint_Dantzig<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_1constraint_Dantzig<-cumsum(return_network_1constraint_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_1constraint_Dantzig<-w

###### network portfolio fixed constraint as mean centrality with glasso ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
  if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(EC_in[[t]])>mean(EC_in[[t]])){
    alpha=c((sum(glasso.icov%*%EC_in[[t]])*sum(glasso.icov)*mean(EC_in[[t]])-(sum(glasso.icov%*%EC_in[[t]]))^2)/(EC_in[[(t)]]%*%glasso.icov%*%EC_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_in[[t]]))^2))
    w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_in[[t]]/sum(glasso.icov%*%EC_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
  }
  else{
    w[[(t)]] = row_sums(glasso.icov)/sum(glasso.icov)
  }
  # alpha=(sum(glasso.icov%*%EC_in[[t]])*sum(glasso.icov)*mean(EC_in[[t]])-(sum(glasso.icov%*%EC_in[[t]]))^2)/(EC_in[[(t)]]%*%glasso.icov%*%EC_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_in[[t]]))^2)
  # w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_in[[t]]/sum(glasso.icov%*%EC_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_1constraint_glasso<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_1constraint_glasso<-cumsum(return_network_1constraint_glasso)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_1constraint_glasso<-w

###### network portfolio fixed constraint as mean centrality with plug in no short ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],mean(EC_in[[(t)]]),FALSE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
  # print(t)
}
return_network_1constraint_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_1constraint_noshort<-cumsum(return_network_1constraint_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_1constraint_noshort<-w

# ###### network portfolio fixed constraint as mean centrality with Dantzig no short ######
# w<-list()
# cumureturn_temporal<-list()
# for(t in 1: length(W_in)){
#   ## compute global minimum variance portfolio without short ##
#   net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],mean(EC_in[[(t)]]),FALSE)
#   w[[(t)]] =net.gmin.port$weights
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_temporal[[t]]<-rowSums(aus)
#   # print(t)
# }
# return_network_Dantzig_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
# cumureturn_network_Dantzig_noshort<-cumsum(return_network_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_network_1constraint_Dantzig_noshort<-w

# ###### network portfolio fixed constraint as mean centrality with glasso no short ######
# w<-list()
# cumureturn_temporal<-list()
# for(t in 1: length(W_in)){
#   ## compute global minimum variance portfolio without short ##
#   glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
#   alpha=(sum(glasso.icov%*%EC_in[[t]])*sum(glasso.icov)*mean(EC_in[[t]])-(sum(glasso.icov%*%EC_in[[t]]))^2)/(EC_in[[(t)]]%*%glasso.icov%*%EC_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_in[[t]]))^2)
#   w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_in[[t]]/sum(glasso.icov%*%EC_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_temporal[[t]]<-rowSums(aus)
# }
# return_network_1constraint_glasso_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
# cumureturn_network_1constraint_glasso_noshort<-cumsum(return_network_1constraint_glasso_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_network_1constraint_glasso_noshort<-w


###### network portfolio varying constraint with plug in ######
cumureturn_network_vary_with_phi<-list()
return_network_vary_with_phi<-list()
quantl<-seq(0.1,0.9,0.1)
w_network_vary_with_phi<-list()
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute 1-constraint network portfolio ##
    net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],quantile(EC_in[[(t)]],quantl[i]),TRUE)
    w[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi[[i]]<-cumsum(return_network_vary_with_phi[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi[[i]]<-w
}

###### network portfolio varying constraint with Dantzig ######
cumureturn_network_vary_with_phi_Dantzig<-list()
return_network_vary_with_phi_Dantzig<-list()
quantl<-seq(0.1,0.9,0.1)
w_network_vary_with_phi_Dantzig<-list()
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    # c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_in[[(t)]])>mean(EC_in[[t]])
    if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_in[[(t)]])>quantile(EC_in[[(t)]],quantl[i])){
      alpha=c((sum(theta3[[t]])*sum(theta1[[t]])*quantile(EC_in[[(t)]],quantl[i])-(sum(theta3[[t]]))^2)/(EC_in[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2))
      w[[(t)]] = alpha*theta3[[t]]/sum(theta3[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
    }
    else{
      w[[(t)]] = theta1[[t]]/sum(theta1[[t]])
    }
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_Dantzig[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_Dantzig[[i]]<-cumsum(return_network_vary_with_phi_Dantzig[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_Dantzig[[i]]<-w
}

###### network portfolio varying constraint with glasso ######
cumureturn_network_vary_with_phi_glasso<-list()
return_network_vary_with_phi_glasso<-list()
quantl<-seq(0.1,0.9,0.1)
w_network_vary_with_phi_glasso<-list()
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
    glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
    if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(EC_in[[t]])>quantile(EC_in[[(t)]],quantl[i])){
      alpha=c((sum(glasso.icov%*%EC_in[[t]])*sum(glasso.icov)*quantile(EC_in[[(t)]],quantl[i])-(sum(glasso.icov%*%EC_in[[t]]))^2)/(EC_in[[(t)]]%*%glasso.icov%*%EC_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_in[[t]]))^2))
      w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_in[[t]]/sum(glasso.icov%*%EC_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
    }
    else{
      w[[(t)]] = row_sums(glasso.icov)/sum(glasso.icov)
    }
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_glasso[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_glasso[[i]]<-cumsum(return_network_vary_with_phi_glasso[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_glasso[[i]]<-w
}

###### network portfolio varying constraint with plug in no short ######
cumureturn_network_vary_with_phi_noshort<-list()
return_network_vary_with_phi_noshort<-list()
w_network_vary_with_phi_noshort<-list()
quantl<-seq(0.1,0.9,0.1)
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],quantile(EC_in[[(t)]],quantl[i]),FALSE)
    w[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_noshort[[i]]<-cumsum(return_network_vary_with_phi_noshort[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_noshort[[i]]<-w
}

# ###### network portfolio varying constraint with Dantzig no short ######
# cumureturn_network_vary_with_phi_Dantzig_noshort<-list()
# return_network_vary_with_phi_Dantzig_noshort<-list()
# w_network_vary_with_phi_Dantzig_noshort<-list()
# quantl<-seq(0.1,0.9,0.1)
# for (i in 1:length(quantl)) {
#   w<-list()
#   cumureturn_temporal<-list()
#   for(t in 1: length(W_in)){
#     ## compute global minimum variance portfolio ##
#     net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],quantile(EC_in[[(t)]],quantl[i]),FALSE)
#     w[[(t)]] =net.gmin.port$weights
#     aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#     cumureturn_temporal[[t]]<-rowSums(aus)
#   }
#   return_network_vary_with_phi_Dantzig_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
#   cumureturn_network_vary_with_phi_Dantzig_noshort[[i]]<-cumsum(return_network_vary_with_phi_noshort[[i]])
#   w<-t(matrix(unlist(w),p,T.windows))
#   colnames(w) = node.label
#   w_network_vary_with_phi_Dantzig_noshort[[i]]<-w
# }

###### network portfolio varying constraint with glasso no short######
# cumureturn_network_vary_with_phi_glasso_noshort<-list()
# return_network_vary_with_phi_glasso_noshort<-list()
# quantl<-seq(0.1,0.9,0.1)
# w_network_vary_with_phi_glasso_noshort<-list()
# for (i in 1:length(quantl)) {
#   w<-list()
#   cumureturn_temporal<-list()
#   for(t in 1: length(W_in)){
#     ## compute global minimum variance portfolio ##glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
#     alpha=(sum(glasso.icov%*%EC_in[[t]])*sum(glasso.icov)*quantile(EC_in[[(t)]],quantl[i])-(sum(glasso.icov%*%EC_in[[t]]))^2)/(EC_in[[(t)]]%*%glasso.icov%*%EC_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_in[[t]]))^2)
#     w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_in[[t]]/sum(glasso.icov%*%EC_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
#     aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#     cumureturn_temporal[[t]]<-rowSums(aus)
#   }
#   return_network_vary_with_phi_glasso_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
#   cumureturn_network_vary_with_phi_glasso_noshort[[i]]<-cumsum(return_network_vary_with_phi_glasso_noshort[[i]])
#   w<-t(matrix(unlist(w),p,T.windows))
#   colnames(w) = node.label
#   w_network_vary_with_phi_glasso_noshort[[i]]<-w
# }


###### network portfolio data-driven constraint with plug in ######
cumureturn_network_datadriven_phistar<-list()
return_network_datadriven_phistar<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],phi_star[t],TRUE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar<-cumsum(return_network_datadriven_phistar)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar<-w

###### network portfolio data-driven constraint with Dantzig ######
cumureturn_network_datadriven_phistar_Dantzig<-list()
return_network_datadriven_phistar_Dantzig<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_in[[(t)]])>phi_star[t]){
    alpha=c((sum(theta3[[t]])*sum(theta1[[t]])*phi_star[t]-(sum(theta3[[t]]))^2)/(EC_in[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2))
    w[[(t)]] = alpha*theta3[[t]]/sum(theta3[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
  }
  else{
    w[[(t)]] = theta1[[t]]/sum(theta1[[t]])
  }
  aus <- as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]] <- rowSums(aus)
}
return_network_datadriven_phistar_Dantzig<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_Dantzig<-cumsum(return_network_datadriven_phistar_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_Dantzig<-w

###### network portfolio data-driven constraint with glasso ######
cumureturn_network_datadriven_phistar_glasso<-list()
return_network_datadriven_phistar_glasso<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
  if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(EC_in[[t]])>phi_star[t]){
    alpha=c((sum(glasso.icov%*%EC_in[[t]])*sum(glasso.icov)*phi_star[t]-(sum(glasso.icov%*%EC_in[[t]]))^2)/(EC_in[[(t)]]%*%glasso.icov%*%EC_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_in[[t]]))^2))
    w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_in[[t]]/sum(glasso.icov%*%EC_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
  }
  else{
    w[[(t)]] = row_sums(glasso.icov)/sum(glasso.icov)
  }
  aus <- as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]] <- rowSums(aus)
}
return_network_datadriven_phistar_glasso<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_glasso<-cumsum(return_network_datadriven_phistar_glasso)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_glasso<-w


###### network portfolio data-driven constraint with plug in no short ######
cumureturn_network_datadriven_phistar_noshort<-list()
return_network_datadriven_phistar_noshort<-list()
cumureturn_temporal<-list()
phi_star<-centrality_equal_portfolio
w<-list()
for(t in 1: length(W_in)){
  ## compute network portfolio data-driven constraint no short ##
  net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],phi_star[t],FALSE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_noshort<-cumsum(return_network_datadriven_phistar_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_noshort<-w

##### network portfolio 2 constraint #####
### default setting centrality constraint as mean centrality ###

###### network portfolio fixed constraint with plug in ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute 2 constraint network portfolio ##
  net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[(t)]], COV_in[[(t)]],mean(EC_in[[(t)]]),mean(ER_in[[(t)]]))
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_2constraint<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_2constraint<-cumsum(return_network_2constraint)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_2constraint<-w

###### network portfolio fixed constraint with Dantzig ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute 2 constraint network portfolio ##
  if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_in[[(t)]])>mean(EC_in[[t]])){
    if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(ER_in[[(t)]])<mean(ER_in[[t]])){
      # 2 constraint case
      M1 <- cbind(rbind(1,mean(EC_in[[(t)]]),mean(ER_in[[(t)]])),
                  rbind(sum(theta3[[(t)]]),EC_in[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
                  rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
      M2 <- cbind(rbind(sum(theta1[[(t)]]),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                  rbind(1,mean(EC_in[[(t)]]),mean(ER_in[[(t)]])),
                  rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
      M <- cbind(rbind(sum(theta1[[(t)]]),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                 rbind(sum(theta3[[(t)]]),EC_in[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
                 rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
      gamma1 <- det(M1)/det(M)
      gamma2 <- det(M2)/det(M)
      alpha1 <- c(gamma1*sum(theta3[[t]]))
      alpha2 <- c(gamma2*sum(theta2[[t]]))
      w1 <- (1-alpha1-alpha2)*theta1[[t]]/sum(theta1[[t]])
      w2 <- alpha1*theta3[[t]]/sum(theta3[[t]])
      w3 <- alpha2*theta2[[t]]/sum(theta2[[t]])
      w[[(t)]] = w1 +  w2 + w3
    }
    else{
      # 1 centrality constraint case
      alpha=(sum(theta3[[t]])*sum(theta1[[t]])*mean(EC_in[[t]])-(sum(theta3[[t]]))^2)/(EC_in[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2)
      w[[(t)]] = alpha*theta3[[t]]/sum(theta3[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
    }}
  else{
    if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(ER_in[[(t)]])<mean(ER_in[[t]])){
      # 1 expected return constraint case 
      alpha=c((sum(theta2[[t]])*sum(theta1[[t]])*mean(ER_in[[t]])-(sum(theta2[[t]]))^2)/(ER_in[[(t)]]%*%theta2[[t]]*sum(theta1[[t]])-(sum(theta2[[t]]))^2))
      w[[(t)]] = alpha*theta2[[t]]/sum(theta2[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
    }
    else{
      # global minimum variance case 
      w[[(t)]] = theta1[[t]]/sum(theta1[[t]])
    }
  }
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_2constraint_Dantzig<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_2constraint_Dantzig<-cumsum(return_network_2constraint_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_2constraint_Dantzig<-w

###### network portfolio fixed constraint with glasso ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute 2 constraint network portfolio ##
  glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
  if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(EC_in[[(t)]])>mean(EC_in[[t]])){
    if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(ER_in[[(t)]])<mean(ER_in[[t]])){
      # 2 constraint case
      M1 <- cbind(rbind(1,mean(EC_in[[(t)]]),mean(ER_in[[(t)]])),
                  rbind(sum(glasso.icov%*%EC_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]]),
                  rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
      M2 <- cbind(rbind(sum(glasso.icov),EC_in[[(t)]]%*%row_sums(glasso.icov),ER_in[[(t)]]%*%row_sums(glasso.icov)),
                  rbind(1,mean(EC_in[[(t)]]),mean(ER_in[[(t)]])),
                  rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
      M <- cbind(rbind(sum(glasso.icov),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                 rbind(sum(glasso.icov%*%EC_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]]),
                 rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
      gamma1 <- det(M1)/det(M)
      gamma2 <- det(M2)/det(M)
      alpha1 <- c(gamma1*sum(glasso.icov%*%EC_in[[(t)]]))
      alpha2 <- c(gamma2*sum(glasso.icov%*%ER_in[[(t)]]))
      w1 <- (1-alpha1-alpha2)*row_sums(glasso.icov)/sum(glasso.icov)
      w2 <- alpha1*glasso.icov%*%EC_in[[(t)]]/sum(glasso.icov%*%EC_in[[(t)]])
      w3 <- alpha2*glasso.icov%*%ER_in[[(t)]]/sum(glasso.icov%*%ER_in[[(t)]])
      w[[(t)]] = c(w1 +  w2 + w3)
    }
    else{
      # 1 centrality constraint case
      alpha=c((sum(glasso.icov%*%EC_in[[t]])*sum(glasso.icov)*mean(EC_in[[t]])-(sum(glasso.icov%*%EC_in[[t]]))^2)/(EC_in[[(t)]]%*%glasso.icov%*%EC_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_in[[t]]))^2))
      w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_in[[t]]/sum(glasso.icov%*%EC_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
    }}
  else{
    if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(ER_in[[(t)]])<mean(ER_in[[t]])){
      # 1 expected return constraint case 
      alpha=c((sum(glasso.icov%*%ER_in[[t]])*sum(glasso.icov)*mean(ER_in[[t]])-(sum(glasso.icov%*%ER_in[[t]]))^2)/(ER_in[[(t)]]%*%glasso.icov%*%ER_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%ER_in[[t]]))^2))
      w[[(t)]] = c(alpha[1]*glasso.icov%*%ER_in[[t]]/sum(glasso.icov%*%ER_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
    }
    else{
      # global minimum variance case 
      w[[t]]=row_sums(glasso.icov)/sum(glasso.icov)
    }
  }
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_2constraint_glasso<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_2constraint_glasso<-cumsum(return_network_2constraint_glasso)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_2constraint_glasso<-w

###### network portfolio fixed constraint with plug in no short ######
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],mean(EC_in[[(t)]]),mean(ER_in[[t]]),FALSE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_2constraint_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_2constraint_noshort<-cumsum(return_network_2constraint_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_2constraint_noshort<-w

# ###### network portfolio fixed constraint with Dantzig no short ######
# w<-list()
# cumureturn_temporal<-list()
# for(t in 1: length(W_in)){
#   ## compute global minimum variance portfolio without short ##
#   net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],mean(EC_in[[(t)]]),mean(ER_in[[t]]),FALSE)
#   w[[(t)]] =net.gmin.port$weights
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_temporal[[t]]<-rowSums(aus)
# }
# return_network_2constraint_Dantzig_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
# cumureturn_network_2constraint_Dantzig_noshort<-cumsum(return_network_2constraint_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_network_2constraint_Dantzig_noshort<-w


# ###### network portfolio fixed constraint with glasso no short ######
# w<-list()
# cumureturn_temporal<-list()
# for(t in 1: length(W_in)){
#   ## compute 2 constraint network portfolio ##
#   glasso.icov=glasso(COV_in[[(t)]],rho=rho)$wi
#   
#   M1 <- cbind(rbind(1,mean(EC_in[[(t)]]),mean(ER_in[[(t)]])),
#               rbind(sum(glasso.icov%*%EC_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]]),
#               rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
#   M2 <- cbind(rbind(sum(glasso.icov),EC_in[[(t)]]%*%row_sums(glasso.icov),ER_in[[(t)]]%*%row_sums(glasso.icov)),
#               rbind(1,mean(EC_in[[(t)]]),mean(ER_in[[(t)]])),
#               rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
#   M <- cbind(rbind(sum(glasso.icov),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
#              rbind(sum(glasso.icov%*%EC_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]]),
#              rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
#   gamma1 <- det(M1)/det(M)
#   gamma2 <- det(M2)/det(M)
#   alpha1 <- gamma1*sum(glasso.icov%*%EC_in[[(t)]])
#   alpha2 <- gamma2*sum(glasso.icov%*%ER_in[[(t)]])
#   w1 <- (1-alpha1-alpha2)*row_sums(glasso.icov)/sum(glasso.icov)
#   w2 <- alpha1*glasso.icov%*%EC_in[[(t)]]/sum(glasso.icov%*%EC_in[[(t)]])
#   w3 <- alpha2*glasso.icov%*%ER_in[[(t)]]/sum(glasso.icov%*%ER_in[[(t)]])
#   w[[(t)]] = c(w1 +  w2 + w3)
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_temporal[[t]]<-rowSums(aus)
# }
# return_network_2constraint_glasso_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
# cumureturn_network_2constraint_glasso_noshort<-cumsum(return_network_2constraint_glasso_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_network_2constraint_glasso_noshort<-w


###### network portfolio varying constraint with plug in ######
cumureturn_network_vary_with_phi_2constraint<-list()
return_network_vary_with_phi_2constraint<-list()
w_network_vary_with_phi_2constraint<-list()
quantl<-seq(0.1,0.9,0.1)
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],quantile(EC_in[[(t)]],quantl[i]),mean(ER_in[[t]]),TRUE)
    # net.gmin.port = network.efficient.portfolio(EC.posi_in[[(t)]], COV_in[[(t)]],phi_star[i],TRUE)
    w[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_2constraint[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_2constraint[[i]]<-cumsum(return_network_vary_with_phi_2constraint[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_2constraint[[i]]<-w
}

###### network portfolio varying constraint with Dantzig ######
cumureturn_network_vary_with_phi_2constraint_Dantzig<-list()
return_network_vary_with_phi_2constraint_Dantzig<-list()
w_network_vary_with_phi_2constraint_Dantzig<-list()
quantl<-seq(0.1,0.9,0.1)
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## network portfolio varying constraint with Dantzig estimation ##
    if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_in[[(t)]])>quantile(EC_in[[(t)]],quantl[i])){
      if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(ER_in[[(t)]])<mean(ER_in[[t]])){
        # 2 constraint case
        M1 <- cbind(rbind(1,quantile(EC_in[[(t)]],quantl[i]),mean(ER_in[[(t)]])),
                    rbind(sum(theta3[[(t)]]),EC_in[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
                    rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
        M2 <- cbind(rbind(sum(theta1[[(t)]]),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                    rbind(1,quantile(EC_in[[(t)]],quantl[i]),mean(ER_in[[(t)]])),
                    rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
        M <- cbind(rbind(sum(theta1[[(t)]]),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                   rbind(sum(theta3[[(t)]]),EC_in[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
                   rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
        gamma1 <- det(M1)/det(M)
        gamma2 <- det(M2)/det(M)
        alpha1 <- c(gamma1*sum(theta3[[t]]))
        alpha2 <- c(gamma2*sum(theta2[[t]]))
        w1 <- (1-alpha1-alpha2)*theta1[[t]]/sum(theta1[[t]])
        w2 <- alpha1*theta3[[t]]/sum(theta3[[t]])
        w3 <- alpha2*theta2[[t]]/sum(theta2[[t]])
        w[[(t)]] = w1 +  w2 + w3
      }
      else{
        # 1 centrality constraint case
        alpha=c((sum(theta3[[t]])*sum(theta1[[t]])*quantile(EC_in[[(t)]],quantl[i])-(sum(theta3[[t]]))^2)/(EC_in[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2))
        w[[(t)]] = alpha*theta3[[t]]/sum(theta3[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
      }}
    else{
      if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(ER_in[[(t)]])<mean(ER_in[[t]])){
        # 1 expected return constraint case 
        alpha=c((sum(theta2[[t]])*sum(theta1[[t]])*mean(ER_in[[t]])-(sum(theta2[[t]]))^2)/(ER_in[[(t)]]%*%theta2[[t]]*sum(theta1[[t]])-(sum(theta2[[t]]))^2))
        w[[(t)]] = alpha*theta2[[t]]/sum(theta2[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
      }
      else{
        # global minimum variance case 
        w[[(t)]] = theta1[[t]]/sum(theta1[[t]])
      }
    }
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_2constraint_Dantzig[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_2constraint_Dantzig[[i]]<-cumsum(return_network_vary_with_phi_2constraint_Dantzig[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_2constraint_Dantzig[[i]]<-w
}

###### network portfolio varying constraint with glasso ######
cumureturn_network_vary_with_phi_2constraint_glasso<-list()
return_network_vary_with_phi_2constraint_glasso<-list()
w_network_vary_with_phi_2constraint_glasso<-list()
quantl<-seq(0.1,0.9,0.1)
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## network portfolio varying constraint with glasso estimation ##
    if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(EC_in[[(t)]])>quantile(EC_in[[(t)]],quantl[i])){
      if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(ER_in[[(t)]])<mean(ER_in[[t]])){
        # 2 constraint case
        M1 <- cbind(rbind(1,quantile(EC_in[[(t)]],quantl[i]),mean(ER_in[[(t)]])),
                    rbind(sum(glasso.icov%*%EC_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]]),
                    rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
        M2 <- cbind(rbind(sum(glasso.icov),EC_in[[(t)]]%*%row_sums(glasso.icov),ER_in[[(t)]]%*%row_sums(glasso.icov)),
                    rbind(1,quantile(EC_in[[(t)]],quantl[i]),mean(ER_in[[(t)]])),
                    rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
        M <- cbind(rbind(sum(glasso.icov),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                   rbind(sum(glasso.icov%*%EC_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]]),
                   rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
        gamma1 <- det(M1)/det(M)
        gamma2 <- det(M2)/det(M)
        alpha1 <- c(gamma1*sum(glasso.icov%*%EC_in[[(t)]]))
        alpha2 <- c(gamma2*sum(glasso.icov%*%ER_in[[(t)]]))
        w1 <- (1-alpha1-alpha2)*row_sums(glasso.icov)/sum(glasso.icov)
        w2 <- alpha1*glasso.icov%*%EC_in[[(t)]]/sum(glasso.icov%*%EC_in[[(t)]])
        w3 <- alpha2*glasso.icov%*%ER_in[[(t)]]/sum(glasso.icov%*%ER_in[[(t)]])
        w[[(t)]] = c(w1 +  w2 + w3)
      }
      else{
        # 1 centrality constraint case
        alpha=c((sum(glasso.icov%*%EC_in[[t]])*sum(glasso.icov)*quantile(EC_in[[(t)]],quantl[i])-(sum(glasso.icov%*%EC_in[[t]]))^2)/(EC_in[[(t)]]%*%glasso.icov%*%EC_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_in[[t]]))^2))
        w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_in[[t]]/sum(glasso.icov%*%EC_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
      }}
    else{
      if(row_sums(glasso.icov)/sum(glasso.icov)%*%ER_in[[(t)]]<mean(ER_in[[t]])){
        # 1 expected return constraint case 
        alpha=c((sum(glasso.icov%*%ER_in[[t]])*sum(glasso.icov)*mean(ER_in[[t]])-(sum(glasso.icov%*%ER_in[[t]]))^2)/(ER_in[[(t)]]%*%glasso.icov%*%ER_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%ER_in[[t]]))^2))
        w[[(t)]] = c(alpha[1]*glasso.icov%*%ER_in[[t]]/sum(glasso.icov%*%ER_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
      }
      else{
        # global minimum variance case 
        w[[t]]=row_sums(glasso.icov)/sum(glasso.icov)
      }
    }
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_2constraint_glasso[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_2constraint_glasso[[i]]<-cumsum(return_network_vary_with_phi_2constraint_glasso[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_2constraint_glasso[[i]]<-w
}

###### network portfolio varying constraint with plug in no short ######
cumureturn_network_vary_with_phi_2constraint_noshort<-list()
return_network_vary_with_phi_2constraint_noshort<-list()
w_network_vary_with_phi_2constraint_noshort<-list()
quantl<-seq(0.1,0.9,0.1)
for (i in 1:length(quantl)) {
  w<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],quantile(EC_in[[(t)]],quantl[i]),mean(ER_in[[t]]),FALSE)
    w[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_2constraint_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_2constraint_noshort[[i]]<-cumsum(return_network_vary_with_phi_2constraint_noshort[[i]])
  w<-t(matrix(unlist(w),p,T.windows))
  colnames(w) = node.label
  w_network_vary_with_phi_2constraint_noshort[[i]]<-w
}

###### network portfolio varying constraint with Dantzig no short ######
# cumureturn_network_vary_with_phi_2constraint_Dantzig_noshort<-list()
# return_network_vary_with_phi_2constraint_Dantzig_noshort<-list()
# w_network_vary_with_phi_2constraint_Dantzig_noshort<-list()
# quantl<-seq(0.1,0.9,0.1)
# for (i in 1:length(quantl)) {
#   w<-list()
#   cumureturn_temporal<-list()
#   for(t in 1: length(W_in)){
#     ## compute global minimum variance portfolio ##
#     net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],quantile(EC_in[[(t)]],quantl[i]),mean(ER_in[[t]]),FALSE)
#     w[[(t)]] =net.gmin.port$weights
#     aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#     cumureturn_temporal[[t]]<-rowSums(aus)
#   }
#   return_network_vary_with_phi_2constraint_Dantzig_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
#   cumureturn_network_vary_with_phi_2constraint_Dantzig_noshort[[i]]<-cumsum(return_network_vary_with_phi_2constraint_Dantzig_noshort[[i]])
#   w<-t(matrix(unlist(w),p,T.windows))
#   colnames(w) = node.label
#   w_network_vary_with_phi_2constraint_Dantzig_noshort[[i]]<-w
# }

###### network portfolio varying constraint with glasso no short ######

###### network portfolio data-driven constraint with plug in ######
cumureturn_network_datadriven_phistar_2constraint<-list()
return_network_datadriven_phistar_2constraint<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],phi_star[t],mean(ER_in[[t]]),TRUE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_2constraint<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_2constraint<-cumsum(return_network_datadriven_phistar_2constraint)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_2constraint<-w

###### network portfolio data-driven constraint with Dantzig ######
cumureturn_network_datadriven_phistar_2constraint_Dantzig<-list()
return_network_datadriven_phistar_2constraint_Dantzig<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(EC_in[[(t)]])>phi_star[t]){
    if(c(theta1[[t]]/sum(theta1[[t]]))%*%c(ER_in[[(t)]])<phi_star[t]){
      # 2 constraint case
      M1 <- cbind(rbind(1,phi_star[t],mean(ER_in[[(t)]])),
                  rbind(sum(theta3[[(t)]]),EC_in[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
                  rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
      M2 <- cbind(rbind(sum(theta1[[(t)]]),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                  rbind(1,phi_star[t],mean(ER_in[[(t)]])),
                  rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
      M <- cbind(rbind(sum(theta1[[(t)]]),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                 rbind(sum(theta3[[(t)]]),EC_in[[(t)]]%*%theta3[[(t)]],ER_in[[(t)]]%*%theta3[[(t)]]),
                 rbind(sum(theta2[[(t)]]),EC_in[[(t)]]%*%theta2[[(t)]],ER_in[[(t)]]%*%theta2[[(t)]]))
      gamma1 <- det(M1)/det(M)
      gamma2 <- det(M2)/det(M)
      alpha1 <- c(gamma1*sum(theta3[[t]]))
      alpha2 <- c(gamma2*sum(theta2[[t]]))
      w1 <- (1-alpha1-alpha2)*theta1[[t]]/sum(theta1[[t]])
      w2 <- alpha1*theta3[[t]]/sum(theta3[[t]])
      w3 <- alpha2*theta2[[t]]/sum(theta2[[t]])
      w[[(t)]] = w1 +  w2 + w3
    }
    else{
      # 1 centrality constraint case
      alpha=c((sum(theta3[[t]])*sum(theta1[[t]])*phi_star[t]-(sum(theta3[[t]]))^2)/(EC_in[[(t)]]%*%theta3[[t]]*sum(theta1[[t]])-(sum(theta3[[t]]))^2))
      w[[(t)]] = alpha*theta3[[t]]/sum(theta3[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
    }}
  else{
    if(theta1[[t]]/sum(theta1[[t]])%*%ER_in[[(t)]]<mean(ER_in[[t]])){
      # 1 expected return constraint case 
      alpha=c((sum(theta2[[t]])*sum(theta1[[t]])*mean(ER_in[[t]])-(sum(theta2[[t]]))^2)/(ER_in[[(t)]]%*%theta2[[t]]*sum(theta1[[t]])-(sum(theta2[[t]]))^2))
      w[[(t)]] = alpha*theta2[[t]]/sum(theta2[[t]])+(1-alpha)*theta1[[t]]/sum(theta1[[t]])
    }
    else{
      # global minimum variance case 
      w[[(t)]] = theta1[[t]]/sum(theta1[[t]])
    }
  }
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_2constraint_Dantzig<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_2constraint_Dantzig<-cumsum(return_network_datadriven_phistar_2constraint_Dantzig)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_2constraint_Dantzig<-w

###### network portfolio data-driven constraint with glasso ######
cumureturn_network_datadriven_phistar_2constraint_glasso<-list()
return_network_datadriven_phistar_2constraint_glasso<-list()
phi_star<-centrality_equal_portfolio
w<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(EC_in[[(t)]])>phi_star[t]){
    if(c(row_sums(glasso.icov)/sum(glasso.icov))%*%c(ER_in[[(t)]])<phi_star[t]){
      # 2 constraint case
      M1 <- cbind(rbind(1,phi_star[t],mean(ER_in[[(t)]])),
                  rbind(sum(glasso.icov%*%EC_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]]),
                  rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
      M2 <- cbind(rbind(sum(glasso.icov),EC_in[[(t)]]%*%row_sums(glasso.icov),ER_in[[(t)]]%*%row_sums(glasso.icov)),
                  rbind(1,phi_star[t],mean(ER_in[[(t)]])),
                  rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
      M <- cbind(rbind(sum(glasso.icov),EC_in[[(t)]]%*%theta1[[(t)]],ER_in[[(t)]]%*%theta1[[(t)]]),
                 rbind(sum(glasso.icov%*%EC_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%EC_in[[(t)]]),
                 rbind(sum(glasso.icov%*%ER_in[[(t)]]),EC_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]],ER_in[[(t)]]%*%glasso.icov%*%ER_in[[(t)]]))
      gamma1 <- det(M1)/det(M)
      gamma2 <- det(M2)/det(M)
      alpha1 <- c(gamma1*sum(glasso.icov%*%EC_in[[(t)]]))
      alpha2 <- c(gamma2*sum(glasso.icov%*%ER_in[[(t)]]))
      w1 <- (1-alpha1-alpha2)*row_sums(glasso.icov)/sum(glasso.icov)
      w2 <- alpha1*glasso.icov%*%EC_in[[(t)]]/sum(glasso.icov%*%EC_in[[(t)]])
      w3 <- alpha2*glasso.icov%*%ER_in[[(t)]]/sum(glasso.icov%*%ER_in[[(t)]])
      w[[(t)]] = c(w1 +  w2 + w3)
    }
    else{
      # 1 centrality constraint case
      alpha=(sum(glasso.icov%*%EC_in[[t]])*sum(glasso.icov)*phi_star[t]-(sum(glasso.icov%*%EC_in[[t]]))^2)/(EC_in[[(t)]]%*%glasso.icov%*%EC_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%EC_in[[t]]))^2)
      w[[(t)]] = c(alpha[1]*glasso.icov%*%EC_in[[t]]/sum(glasso.icov%*%EC_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
    }}
  else{
    if(row_sums(glasso.icov)/sum(glasso.icov)%*%ER_in[[(t)]]<mean(ER_in[[t]])){
      # 1 expected return constraint case 
      alpha=(sum(glasso.icov%*%ER_in[[t]])*sum(glasso.icov)*mean(ER_in[[t]])-(sum(glasso.icov%*%ER_in[[t]]))^2)/(ER_in[[(t)]]%*%glasso.icov%*%ER_in[[t]]*sum(glasso.icov)-(sum(glasso.icov%*%ER_in[[t]]))^2)
      w[[(t)]] = c(alpha[1]*glasso.icov%*%ER_in[[t]]/sum(glasso.icov%*%ER_in[[t]])+(1-alpha[1])*row_sums(glasso.icov)/sum(glasso.icov))
    }
    else{
      # global minimum variance case 
      w[[t]]=row_sums(glasso.icov)/sum(glasso.icov)
    }
  }
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_2constraint_glasso<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_2constraint_glasso<-cumsum(return_network_datadriven_phistar_2constraint_glasso)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_2constraint_glasso<-w

###### network portfolio data-driven constraint with plug in no short ######
cumureturn_network_datadriven_phistar_2constraint_noshort<-list()
return_network_datadriven_phistar_2constraint_noshort<-list()
cumureturn_temporal<-list()
phi_star<-centrality_equal_portfolio
w<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],phi_star[t],mean(ER_in[[t]]),FALSE)
  w[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_2constraint_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_2constraint_noshort<-cumsum(return_network_datadriven_phistar_2constraint_noshort)
w<-t(matrix(unlist(w),p,T.windows))
colnames(w) = node.label
w_network_datadriven_phistar_2constraint_noshort<-w

# ###### network portfolio data-driven constraint with Dantzig no short ######
# cumureturn_network_datadriven_phistar_2constraint_Dantzig_noshort<-list()
# return_network_datadriven_phistar_2constraint_Dantzig_noshort<-list()
# cumureturn_temporal<-list()
# phi_star<-centrality_equal_portfolio
# w<-list()
# for(t in 1: length(W_in)){
#   ## compute global minimum variance portfolio ##
#   net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],phi_star[t],mean(ER_in[[t]]),FALSE)
#   w[[(t)]] =net.gmin.port$weights
#   aus<-as.matrix(repmat(w[[(t)]],7,1)*W_out[[t]])
#   cumureturn_temporal[[t]]<-rowSums(aus)
# }
# return_network_datadriven_phistar_2constraint_Dantzig_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
# cumureturn_network_datadriven_phistar_2constraint_Dantzig_noshort<-cumsum(return_network_datadriven_phistar_2constraint_Dantzig_noshort)
# w<-t(matrix(unlist(w),p,T.windows))
# colnames(w) = node.label
# w_network_datadriven_phistar_2constraint_Dantzig_noshort<-w

###### network portfolio data-driven constraint with Dantzig no short ######

#### Save portfolios ####
Portfolio.Scenario <- list("return_minVar"=return_minVar,"cumureturn_minVar"=cumureturn_minVar,"w_minVar"=w_minVar,
                           "return_minVar_Dantzig"=return_minVar_Dantzig,"cumureturn_minVar_Dantzig"=cumureturn_minVar_Dantzig,"w_minVar_Dantzig"=w_minVar_Dantzig,
                           "return_minVar_glasso"=return_minVar_glasso,"cumureturn_minVar_glasso"=cumureturn_minVar_glasso,"w_minVar_glasso"=w_minVar_glasso,
                           "return_minVar_noshort"=return_minVar_noshort,"cumureturn_minVar_noshort"=cumureturn_minVar_noshort,"w_minVar_noshort"=w_minVar_noshort,
                           "return_meanVar"=return_meanVar,"cumureturn_meanVar"=cumureturn_meanVar,"w_meanVar"=w_meanVar,
                           "return_meanVar_Dantzig"=return_meanVar_Dantzig,"cumureturn_meanVar_Dantzig"=cumureturn_meanVar_Dantzig,"w_meanVar_Dantzig"=w_meanVar_Dantzig,
                           "return_meanVar_glasso"=return_meanVar_glasso,"cumureturn_meanVar_glasso"=cumureturn_meanVar_glasso,"w_meanVar_glasso"=w_meanVar_glasso,
                           "return_meanVar_noshort"=return_meanVar_noshort,"cumureturn_meanVar_noshort"=cumureturn_meanVar_noshort,"w_meanVar_noshort"=w_meanVar_noshort,
                           "return_equal"=return_equal,"cumureturn_equal"=cumureturn_equal,"w_equal"=w_equal,
                           "return_network_1constraint"=return_network_1constraint,"cumureturn_network_1constraint"=cumureturn_network_1constraint,"w_network_1constraint"=w_network_1constraint,
                           "return_network_1constraint_Dantzig"=return_network_1constraint_Dantzig,"cumureturn_network_1constraint_Dantzig"=cumureturn_network_1constraint_Dantzig,"w_network_1constraint_Dantzig"=w_network_1constraint_Dantzig,
                           "return_network_1constraint_glasso"=return_network_1constraint_glasso,"cumureturn_network_1constraint_glasso"=cumureturn_network_1constraint_glasso,"w_network_1constraint_glasso"=w_network_1constraint_glasso,
                           "return_network_1constraint_noshort"=return_network_1constraint_noshort,"cumureturn_network_1constraint_noshort"=cumureturn_network_1constraint_noshort,"w_network_1constraint_noshort"=w_network_1constraint_noshort,
                           "return_network_vary_with_phi"=return_network_vary_with_phi,"cumureturn_network_vary_with_phi"=cumureturn_network_vary_with_phi,"w_network_vary_with_phi"=w_network_vary_with_phi,
                           "return_network_vary_with_phi_Dantzig"=return_network_vary_with_phi_Dantzig,"cumureturn_network_vary_with_phi_Dantzig"=cumureturn_network_vary_with_phi_Dantzig,"w_network_vary_with_phi_Dantzig"=w_network_vary_with_phi_Dantzig,
                           "return_network_vary_with_phi_glasso"=return_network_vary_with_phi_glasso,"cumureturn_network_vary_with_phi_glasso"=cumureturn_network_vary_with_phi_glasso,"w_network_vary_with_phi_glasso"=w_network_vary_with_phi_glasso,
                           "return_network_vary_with_phi_noshort"=return_network_vary_with_phi_noshort,"cumureturn_network_vary_with_phi_noshort"=cumureturn_network_vary_with_phi_noshort,"w_network_vary_with_phi_noshort"=w_network_vary_with_phi_noshort,
                           "return_network_datadriven_phistar"=return_network_datadriven_phistar,"cumureturn_network_datadriven_phistar"=cumureturn_network_datadriven_phistar,"w_network_datadriven_phistar"=w_network_datadriven_phistar,
                           "return_network_datadriven_phistar_Dantzig"=return_network_datadriven_phistar_Dantzig,"cumureturn_network_datadriven_phistar_Dantzig"=cumureturn_network_datadriven_phistar_Dantzig,"w_network_datadriven_phistar_Dantzig"=w_network_datadriven_phistar_Dantzig,
                           "return_network_datadriven_phistar_glasso"=return_network_datadriven_phistar_glasso,"cumureturn_network_datadriven_phistar_glasso"=cumureturn_network_datadriven_phistar_glasso,"w_network_datadriven_phistar_glasso"=w_network_datadriven_phistar_glasso,
                           "return_network_datadriven_phistar_noshort"=return_network_datadriven_phistar_noshort,"cumureturn_network_datadriven_phistar_noshort"=cumureturn_network_datadriven_phistar_noshort,"w_network_datadriven_phistar_noshort"=w_network_datadriven_phistar_noshort,
                           "return_network_2constraint"=return_network_2constraint,"cumureturn_network_2constraint"=cumureturn_network_2constraint,"w_network_2constraint"=w_network_2constraint,
                           "return_network_2constraint_Dantzig"=return_network_2constraint_Dantzig,"cumureturn_network_2constraint_Dantzig"=cumureturn_network_2constraint_Dantzig,"w_network_2constraint_Dantzig"=w_network_2constraint_Dantzig,
                           "return_network_2constraint_glasso"=return_network_2constraint_glasso,"cumureturn_network_2constraint_glasso"=cumureturn_network_2constraint_glasso,"w_network_2constraint_glasso"=w_network_2constraint_glasso,
                           "return_network_2constraint_noshort"=return_network_2constraint_noshort,"cumureturn_network_2constraint_noshort"=cumureturn_network_2constraint_noshort,"w_network_2constraint_noshort"=w_network_2constraint_noshort,
                           "return_network_vary_with_phi_2constraint"=return_network_vary_with_phi_2constraint,"cumureturn_network_vary_with_phi_2constraint"=cumureturn_network_vary_with_phi_2constraint,"w_network_vary_with_phi_2constraint"=w_network_vary_with_phi_2constraint,
                           "return_network_vary_with_phi_2constraint_Dantzig"=return_network_vary_with_phi_2constraint_Dantzig,"cumureturn_network_vary_with_phi_2constraint_Dantzig"=cumureturn_network_vary_with_phi_2constraint_Dantzig,"w_network_vary_with_phi_2constraint_Dantzig"=w_network_vary_with_phi_2constraint_Dantzig,
                           "return_network_vary_with_phi_2constraint_glasso"=return_network_vary_with_phi_2constraint_glasso,"cumureturn_network_vary_with_phi_2constraint_glasso"=cumureturn_network_vary_with_phi_2constraint_glasso,"w_network_vary_with_phi_2constraint_glasso"=w_network_vary_with_phi_2constraint_glasso,
                           "return_network_vary_with_phi_2constraint_noshort"=return_network_vary_with_phi_2constraint_noshort,"cumureturn_network_vary_with_phi_2constraint_noshort"=cumureturn_network_vary_with_phi_2constraint_noshort,"w_network_vary_with_phi_2constraint_noshort"=w_network_vary_with_phi_2constraint_noshort,
                           "return_network_datadriven_phistar_2constraint"=return_network_datadriven_phistar_2constraint,"cumureturn_network_datadriven_phistar_2constraint"=cumureturn_network_datadriven_phistar_2constraint,"w_network_datadriven_phistar_2constraint"=w_network_datadriven_phistar_2constraint,
                           "return_network_datadriven_phistar_2constraint_Dantzig"=return_network_datadriven_phistar_2constraint_Dantzig,"cumureturn_network_datadriven_phistar_2constraint_Dantzig"=cumureturn_network_datadriven_phistar_2constraint_Dantzig,"w_network_datadriven_phistar_2constraint_Dantzig"=w_network_datadriven_phistar_2constraint_Dantzig,
                           "return_network_datadriven_phistar_2constraint_glasso"=return_network_datadriven_phistar_2constraint_glasso,"cumureturn_network_datadriven_phistar_2constraint_glasso"=cumureturn_network_datadriven_phistar_2constraint_glasso,"w_network_datadriven_phistar_2constraint_glasso"=w_network_datadriven_phistar_2constraint_glasso,
                           "return_network_datadriven_phistar_2constraint_noshort"=return_network_datadriven_phistar_2constraint_noshort,"cumureturn_network_datadriven_phistar_2constraint_noshort"=cumureturn_network_datadriven_phistar_2constraint_noshort,"w_network_datadriven_phistar_2constraint_noshort"=w_network_datadriven_phistar_2constraint_noshort)
save(Portfolio.Scenario,file="Portfolios_0925.RData")
#### load data #####
# load("Portfolios_0923.RData")
# return_minVar <- Portfolio.Scenario$return_minVar
# return_minVar_noshort <- Portfolio.Scenario$return_minVar_noshort
# return_minVar_Dantzig <- Portfolio.Scenario$return_minVar_Dantzig
# return_meanVar <- Portfolio.Scenario$return_meanVar
# return_meanVar_noshort <- Portfolio.Scenario$return_meanVar_noshort
# return_meanVar_Dantzig <- Portfolio.Scenario$return_meanVar_Dantzig
# return_equal <- Portfolio.Scenario$return_equal
# return_network_1constraint <- Portfolio.Scenario$return_network_1constraint
# return_network_1constraint_Dantzig <- Portfolio.Scenario$return_network_1constraint_Dantzig
# return_network_1constraint_noshort <- Portfolio.Scenario$return_network_1constraint_noshort
# return_network_vary_with_phi <- Portfolio.Scenario$return_network_vary_with_phi
# return_network_vary_with_phi_Dantzig <- Portfolio.Scenario$return_network_vary_with_phi_Dantzig
# return_network_vary_with_phi_noshort <- Portfolio.Scenario$return_network_vary_with_phi_noshort
# return_network_datadriven_phistar <- Portfolio.Scenario$return_network_datadriven_phistar
# return_network_datadriven_phistar_Dantzig <- Portfolio.Scenario$return_network_datadriven_phistar_Dantzig
# return_network_datadriven_phistar_noshort <- Portfolio.Scenario$return_network_datadriven_phistar_noshort
# return_network_2constraint <- Portfolio.Scenario$return_network_2constraint
# return_network_2constraint_Dantzig <- Portfolio.Scenario$return_network_2constraint_Dantzig
# return_network_2constraint_noshort <- Portfolio.Scenario$return_network_2constraint_noshort
# return_network_vary_with_phi_2constraint <- Portfolio.Scenario$return_network_vary_with_phi_2constraint
# return_network_vary_with_phi_2constraint_Dantzig <- Portfolio.Scenario$return_network_vary_with_phi_2constraint_Dantzig
# return_network_vary_with_phi_2constraint_noshort <- Portfolio.Scenario$return_network_vary_with_phi_2constraint_noshort
# return_network_datadriven_phistar_2constraint <- Portfolio.Scenario$return_network_datadriven_phistar_2constraint
# return_network_datadriven_phistar_2constraint_Dantzig <- Portfolio.Scenario$return_network_datadriven_phistar_2constraint_Dantzig
# return_network_datadriven_phistar_2constraint_noshort <- Portfolio.Scenario$return_network_datadriven_phistar_2constraint_noshort
# 
# 
# cumureturn_minVar <- Portfolio.Scenario$cumureturn_minVar
# cumureturn_minVar_noshort <- Portfolio.Scenario$cumureturn_minVar_noshort
# cumureturn_minVar_Dantzig <- Portfolio.Scenario$cumureturn_minVar_Dantzig
# cumureturn_meanVar <- Portfolio.Scenario$cumureturn_meanVar
# cumureturn_meanVar_noshort <- Portfolio.Scenario$cumureturn_meanVar_noshort
# cumureturn_meanVar_Dantzig <- Portfolio.Scenario$cumureturn_meanVar_Dantzig
# cumureturn_equal <- Portfolio.Scenario$cumureturn_equal
# cumureturn_network_1constraint <- Portfolio.Scenario$cumureturn_network_1constraint
# cumureturn_network_1constraint_Dantzig <- Portfolio.Scenario$cumureturn_network_1constraint_Dantzig
# cumureturn_network_1constraint_noshort <- Portfolio.Scenario$cumureturn_network_1constraint_noshort
# cumureturn_network_vary_with_phi <- Portfolio.Scenario$cumureturn_network_vary_with_phi
# cumureturn_network_vary_with_phi_Dantzig <- Portfolio.Scenario$cumureturn_network_vary_with_phi_Dantzig
# cumureturn_network_vary_with_phi_noshort <- Portfolio.Scenario$cumureturn_network_vary_with_phi_noshort
# cumureturn_network_datadriven_phistar <- Portfolio.Scenario$cumureturn_network_datadriven_phistar
# cumureturn_network_datadriven_phistar_Dantzig <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_Dantzig
# cumureturn_network_datadriven_phistar_noshort <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_noshort
# cumureturn_network_2constraint <- Portfolio.Scenario$cumureturn_network_2constraint
# cumureturn_network_2constraint_Dantzig <- Portfolio.Scenario$cumureturn_network_2constraint_Dantzig
# cumureturn_network_2constraint_noshort <- Portfolio.Scenario$cumureturn_network_2constraint_noshort
# cumureturn_network_vary_with_phi_2constraint <- Portfolio.Scenario$cumureturn_network_vary_with_phi_2constraint
# cumureturn_network_vary_with_phi_2constraint_Dantzig <- Portfolio.Scenario$cumureturn_network_vary_with_phi_2constraint_Dantzig
# cumureturn_network_vary_with_phi_2constraint_noshort <- Portfolio.Scenario$cumureturn_network_vary_with_phi_2constraint_noshort
# cumureturn_network_datadriven_phistar_2constraint <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_2constraint
# cumureturn_network_datadriven_phistar_2constraint_Dantzig <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_2constraint_Dantzig
# cumureturn_network_datadriven_phistar_2constraint_noshort <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_2constraint_noshort
# 
# 
# w_minVar <- Portfolio.Scenario$w_minVar
# w_minVar_noshort <- Portfolio.Scenario$w_minVar_noshort
# w_minVar_Dantzig <- Portfolio.Scenario$w_minVar_Dantzig
# w_meanVar <- Portfolio.Scenario$w_meanVar
# w_meanVar_noshort <- Portfolio.Scenario$w_meanVar_noshort
# w_meanVar_Dantzig <- Portfolio.Scenario$w_meanVar_Dantzig
# w_equal <- Portfolio.Scenario$w_equal
# w_network_1constraint <- Portfolio.Scenario$w_network_1constraint
# w_network_1constraint_Dantzig <- Portfolio.Scenario$w_network_1constraint_Dantzig
# w_network_1constraint_noshort <- Portfolio.Scenario$w_network_1constraint_noshort
# w_network_vary_with_phi <- Portfolio.Scenario$w_network_vary_with_phi
# w_network_vary_with_phi_Dantzig <- Portfolio.Scenario$w_network_vary_with_phi_Dantzig
# w_network_vary_with_phi_noshort <- Portfolio.Scenario$w_network_vary_with_phi_noshort
# w_network_datadriven_phistar <- Portfolio.Scenario$w_network_datadriven_phistar
# w_network_datadriven_phistar_Dantzig <- Portfolio.Scenario$w_network_datadriven_phistar_Dantzig
# w_network_datadriven_phistar_noshort <- Portfolio.Scenario$w_network_datadriven_phistar_noshort
# w_network_2constraint <- Portfolio.Scenario$w_network_2constraint
# w_network_2constraint_Dantzig <- Portfolio.Scenario$w_network_2constraint_Dantzig
# w_network_2constraint_noshort <- Portfolio.Scenario$w_network_2constraint_noshort
# w_network_vary_with_phi_2constraint <- Portfolio.Scenario$w_network_vary_with_phi_2constraint
# w_network_vary_with_phi_2constraint_Dantzig <- Portfolio.Scenario$w_network_vary_with_phi_2constraint_Dantzig
# w_network_vary_with_phi_2constraint_noshort <- Portfolio.Scenario$w_network_vary_with_phi_2constraint_noshort
# w_network_datadriven_phistar_2constraint <- Portfolio.Scenario$w_network_datadriven_phistar_2constraint
# w_network_datadriven_phistar_2constraint_Dantzig <- Portfolio.Scenario$w_network_datadriven_phistar_2constraint_Dantzig
# w_network_datadriven_phistar_2constraint_noshort <- Portfolio.Scenario$w_network_datadriven_phistar_2constraint_noshort
# 
