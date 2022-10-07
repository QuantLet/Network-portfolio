# Version 1.0.1

# In this file, we draw portfolio figures

rm(list = ls())

setwd("~/Documents/Code/Network structure based portfolio/Dantzig-selector estimation covariance/Dantzig-selector_estimation_0925")

# Load Functions and other Files
source('./PackagesNetworkPortfolio.R')
source('./FunctionsNetworkPortfolio.R')

# load raw data
prices<-read_excel("SP500 securities.xlsx")
ZOO <- zoo(prices[,-1], order.by=as.Date(as.character(prices$Dates), format='%Y-%m-%d'))

#return
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)
p=dim(return)[2]

# out-of-sample return
return_oos <- returnstd[-c(1:493),]

# set label
node.label=colnames(returnstd)
node.label<-gsub("Equity","",node.label)
node.label<-gsub("UN","",node.label)
node.label<-gsub("UW","",node.label)
names(returnstd) = node.label

# generate date timeline 
date<-prices$Dates
date<-as.Date.factor(date[-c(1:501)], format="%Y-%m-%d")

# quantile
quantl<-seq(0.1,0.9,0.1)

#### Portfolio performance ####
##### Load portfolios #####
load("Portfolios_0925.RData")
return_minVar <- Portfolio.Scenario$return_minVar
return_minVar_noshort <- Portfolio.Scenario$return_minVar_noshort
return_minVar_Dantzig <- Portfolio.Scenario$return_minVar_Dantzig
return_minVar_glasso <- Portfolio.Scenario$return_minVar_glasso
return_meanVar <- Portfolio.Scenario$return_meanVar
return_meanVar_noshort <- Portfolio.Scenario$return_meanVar_noshort
return_meanVar_Dantzig <- Portfolio.Scenario$return_meanVar_Dantzig
return_meanVar_glasso <- Portfolio.Scenario$return_meanVar_glasso
return_equal <- Portfolio.Scenario$return_equal
return_network_1constraint <- Portfolio.Scenario$return_network_1constraint
return_network_1constraint_Dantzig <- Portfolio.Scenario$return_network_1constraint_Dantzig
return_network_1constraint_glasso <- Portfolio.Scenario$return_network_1constraint_glasso
return_network_1constraint_noshort <- Portfolio.Scenario$return_network_1constraint_noshort
return_network_vary_with_phi <- Portfolio.Scenario$return_network_vary_with_phi
return_network_vary_with_phi_Dantzig <- Portfolio.Scenario$return_network_vary_with_phi_Dantzig
return_network_vary_with_phi_glasso <- Portfolio.Scenario$return_network_vary_with_phi_glasso
return_network_vary_with_phi_noshort <- Portfolio.Scenario$return_network_vary_with_phi_noshort
return_network_datadriven_phistar <- Portfolio.Scenario$return_network_datadriven_phistar
return_network_datadriven_phistar_Dantzig <- Portfolio.Scenario$return_network_datadriven_phistar_Dantzig
return_network_datadriven_phistar_glasso <- Portfolio.Scenario$return_network_datadriven_phistar_glasso
return_network_datadriven_phistar_noshort <- Portfolio.Scenario$return_network_datadriven_phistar_noshort
return_network_2constraint <- Portfolio.Scenario$return_network_2constraint
return_network_2constraint_Dantzig <- Portfolio.Scenario$return_network_2constraint_Dantzig
return_network_2constraint_glasso <- Portfolio.Scenario$return_network_2constraint_glasso
return_network_2constraint_noshort <- Portfolio.Scenario$return_network_2constraint_noshort
return_network_vary_with_phi_2constraint <- Portfolio.Scenario$return_network_vary_with_phi_2constraint
return_network_vary_with_phi_2constraint_Dantzig <- Portfolio.Scenario$return_network_vary_with_phi_2constraint_Dantzig
return_network_vary_with_phi_2constraint_glasso <- Portfolio.Scenario$return_network_vary_with_phi_2constraint_glasso
return_network_vary_with_phi_2constraint_noshort <- Portfolio.Scenario$return_network_vary_with_phi_2constraint_noshort
return_network_datadriven_phistar_2constraint <- Portfolio.Scenario$return_network_datadriven_phistar_2constraint
return_network_datadriven_phistar_2constraint_Dantzig <- Portfolio.Scenario$return_network_datadriven_phistar_2constraint_Dantzig
return_network_datadriven_phistar_2constraint_glasso <- Portfolio.Scenario$return_network_datadriven_phistar_2constraint_glasso
return_network_datadriven_phistar_2constraint_noshort <- Portfolio.Scenario$return_network_datadriven_phistar_2constraint_noshort


cumureturn_minVar <- Portfolio.Scenario$cumureturn_minVar
cumureturn_minVar_noshort <- Portfolio.Scenario$cumureturn_minVar_noshort
cumureturn_minVar_Dantzig <- Portfolio.Scenario$cumureturn_minVar_Dantzig
cumureturn_minVar_glasso <- Portfolio.Scenario$cumureturn_minVar_glasso
cumureturn_meanVar <- Portfolio.Scenario$cumureturn_meanVar
cumureturn_meanVar_noshort <- Portfolio.Scenario$cumureturn_meanVar_noshort
cumureturn_meanVar_Dantzig <- Portfolio.Scenario$cumureturn_meanVar_Dantzig
cumureturn_meanVar_glasso <- Portfolio.Scenario$cumureturn_meanVar_glasso
cumureturn_equal <- Portfolio.Scenario$cumureturn_equal
cumureturn_network_1constraint <- Portfolio.Scenario$cumureturn_network_1constraint
cumureturn_network_1constraint_Dantzig <- Portfolio.Scenario$cumureturn_network_1constraint_Dantzig
cumureturn_network_1constraint_glasso <- Portfolio.Scenario$cumureturn_network_1constraint_glasso
cumureturn_network_1constraint_noshort <- Portfolio.Scenario$cumureturn_network_1constraint_noshort
cumureturn_network_vary_with_phi <- Portfolio.Scenario$cumureturn_network_vary_with_phi
cumureturn_network_vary_with_phi_Dantzig <- Portfolio.Scenario$cumureturn_network_vary_with_phi_Dantzig
cumureturn_network_vary_with_phi_glasso <- Portfolio.Scenario$cumureturn_network_vary_with_phi_glasso
cumureturn_network_vary_with_phi_noshort <- Portfolio.Scenario$cumureturn_network_vary_with_phi_noshort
cumureturn_network_datadriven_phistar <- Portfolio.Scenario$cumureturn_network_datadriven_phistar
cumureturn_network_datadriven_phistar_Dantzig <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_Dantzig
cumureturn_network_datadriven_phistar_glasso <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_glasso
cumureturn_network_datadriven_phistar_noshort <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_noshort
cumureturn_network_2constraint <- Portfolio.Scenario$cumureturn_network_2constraint
cumureturn_network_2constraint_Dantzig <- Portfolio.Scenario$cumureturn_network_2constraint_Dantzig
cumureturn_network_2constraint_glasso <- Portfolio.Scenario$cumureturn_network_2constraint_glasso
cumureturn_network_2constraint_noshort <- Portfolio.Scenario$cumureturn_network_2constraint_noshort
cumureturn_network_vary_with_phi_2constraint <- Portfolio.Scenario$cumureturn_network_vary_with_phi_2constraint
cumureturn_network_vary_with_phi_2constraint_Dantzig <- Portfolio.Scenario$cumureturn_network_vary_with_phi_2constraint_Dantzig
cumureturn_network_vary_with_phi_2constraint_glasso <- Portfolio.Scenario$cumureturn_network_vary_with_phi_2constraint_glasso
cumureturn_network_vary_with_phi_2constraint_noshort <- Portfolio.Scenario$cumureturn_network_vary_with_phi_2constraint_noshort
cumureturn_network_datadriven_phistar_2constraint <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_2constraint
cumureturn_network_datadriven_phistar_2constraint_Dantzig <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_2constraint_Dantzig
cumureturn_network_datadriven_phistar_2constraint_glasso <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_2constraint_glasso
cumureturn_network_datadriven_phistar_2constraint_noshort <- Portfolio.Scenario$cumureturn_network_datadriven_phistar_2constraint_noshort


w_minVar <- Portfolio.Scenario$w_minVar
w_minVar_noshort <- Portfolio.Scenario$w_minVar_noshort
w_minVar_Dantzig <- Portfolio.Scenario$w_minVar_Dantzig
w_minVar_glasso <- Portfolio.Scenario$w_minVar_glasso
w_meanVar <- Portfolio.Scenario$w_meanVar
w_meanVar_noshort <- Portfolio.Scenario$w_meanVar_noshort
w_meanVar_Dantzig <- Portfolio.Scenario$w_meanVar_Dantzig
w_meanVar_glasso <- Portfolio.Scenario$w_meanVar_glasso
w_equal <- Portfolio.Scenario$w_equal
w_network_1constraint <- Portfolio.Scenario$w_network_1constraint
w_network_1constraint_Dantzig <- Portfolio.Scenario$w_network_1constraint_Dantzig
w_network_1constraint_noshort <- Portfolio.Scenario$w_network_1constraint_noshort
w_network_1constraint_glasso <- Portfolio.Scenario$w_network_1constraint_glasso
w_network_vary_with_phi <- Portfolio.Scenario$w_network_vary_with_phi
w_network_vary_with_phi_Dantzig <- Portfolio.Scenario$w_network_vary_with_phi_Dantzig
w_network_vary_with_phi_glasso <- Portfolio.Scenario$w_network_vary_with_phi_glasso
w_network_vary_with_phi_noshort <- Portfolio.Scenario$w_network_vary_with_phi_noshort
w_network_datadriven_phistar <- Portfolio.Scenario$w_network_datadriven_phistar
w_network_datadriven_phistar_Dantzig <- Portfolio.Scenario$w_network_datadriven_phistar_Dantzig
w_network_datadriven_phistar_glasso <- Portfolio.Scenario$w_network_datadriven_phistar_glasso
w_network_datadriven_phistar_noshort <- Portfolio.Scenario$w_network_datadriven_phistar_noshort
w_network_2constraint <- Portfolio.Scenario$w_network_2constraint
w_network_2constraint_Dantzig <- Portfolio.Scenario$w_network_2constraint_Dantzig
w_network_2constraint_glasso <- Portfolio.Scenario$w_network_2constraint_glasso
w_network_2constraint_noshort <- Portfolio.Scenario$w_network_2constraint_noshort
w_network_vary_with_phi_2constraint <- Portfolio.Scenario$w_network_vary_with_phi_2constraint
w_network_vary_with_phi_2constraint_Dantzig <- Portfolio.Scenario$w_network_vary_with_phi_2constraint_Dantzig
w_network_vary_with_phi_2constraint_glasso <- Portfolio.Scenario$w_network_vary_with_phi_2constraint_glasso
w_network_vary_with_phi_2constraint_noshort <- Portfolio.Scenario$w_network_vary_with_phi_2constraint_noshort
w_network_datadriven_phistar_2constraint <- Portfolio.Scenario$w_network_datadriven_phistar_2constraint
w_network_datadriven_phistar_2constraint_Dantzig <- Portfolio.Scenario$w_network_datadriven_phistar_2constraint_Dantzig
w_network_datadriven_phistar_2constraint_glasso <- Portfolio.Scenario$w_network_datadriven_phistar_2constraint_glasso
w_network_datadriven_phistar_2constraint_noshort <- Portfolio.Scenario$w_network_datadriven_phistar_2constraint_noshort


#### Figures output ####
##### Cumulative returns of 3 strategies #####
### minVar, minVar no short, equal: (color) 1:3 ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal)
),
order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_3_strategies",".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[1:3], ylab="", xlab="")
# legend("topleft",legend=c("minVar", "minVar no short", "Equal"),
dev.off()
##### Cumulative returns of 5 strategies #####
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7  ###
Colors_Ret<-rainbow(10)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_meanVar),
                  data.frame(cumureturn_meanVar_noshort)
),
order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_5_strategies",".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7)], ylab="", xlab="",title=paste0("Cumulate return with network Dantzig"))
legend("topleft",legend=c("minVar", "minVar no short", "Equal", "meanVar", "meanVar noshort"),
       col=Colors_Ret[c(1:3,4,7)], lty=1, cex=0.8)
dev.off()

###### 1-constraint varying phi ######
### 1.GMV, 2.GMV no short, 3.equal: (color) 1:3 ###
### 4.network varying constraint: (color) 9 ###
### 5.network with phistar mean no short: (color) 5   ###
Colors_Ret<-rainbow(12)
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_1constraint_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  pngname<-paste0(getwd(),"/portfolio_comparison/5_strategies_varying_constraint/cumulative_return_with_constraint_quantile",quantl[i]*100,".png")
  png(file = pngname, width=500, height=400, bg = "transparent")
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("minVar", "minVar no short", "Equal", "network", "network no short"),
         col=Colors_Ret[c(1:3,9,5)], lty=1, cex=0.8)
  dev.off()
}

###### 1-constraint varying phi no short ######
### 1.GMV, 2.GMV no short, 3.equal: (color) 1:3 ###
### 4.network with phistar mean: (color) 9 ###
### 5.network varying phi no short: (color) 5 ###
Colors_Ret<-rainbow(12)
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[5]]),
                    data.frame(cumureturn_network_vary_with_phi_noshort[[i]])),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  pngname<-paste0(getwd(),"/portfolio_comparison/5_strategies_noshort_varying_constraint/cumulative_return_with_constraint_quantile",quantl[i]*100,".png")
  png(file = pngname, width=500, height=400, bg = "transparent")
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("minVar", "minVar no short", "Equal", "network", "network no short"),
         col=Colors_Ret[c(1:3,9,5)], lty=1, cex=0.8)
  dev.off()
}

##### Cumulative returns of 7 strategies #####
###### 1-constraint varying phi ######
### 1.GMV, 2.GMV no short, 3.equal, 4.meanVar, 5.meanVar no short: (color) 1:3,4,7 ###
### 6.network varying constraint: (color) 9  ###
### 7.network with phistar mean no short: (color) 5   ###
Colors_Ret<-rainbow(12)
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_meanVar),
                    data.frame(cumureturn_meanVar_noshort),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_1constraint_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  pngname<-paste0(getwd(),"/portfolio_comparison/7_strategies_varying_constraint/cumulative_return_with_constraint_quantile",quantl[i]*100,".png")
  png(file = pngname, width=500, height=400, bg = "transparent")
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("GMV", "GMV no short", "Equal","meanVar", "meanVar no short",  "network", "network no short"),
         col=Colors_Ret[c(1:3,4,7,9,5)], lty=1, cex=0.8)
  dev.off()
}
###### 1-constraint varying phi noshort ######
### 1.GMV, 2.GMV no short, 3.equal: (color) 1:3 ###
### 4.network with phistar mean: (color) 9 ###
### 5.network varying constraint no short: (color) 5 ###
Colors_Ret<-rainbow(12)
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_meanVar),
                    data.frame(cumureturn_meanVar_noshort),
                    data.frame(cumureturn_network_vary_with_phi[[5]]),
                    data.frame(cumureturn_network_vary_with_phi_noshort[[i]])),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  pngname<-paste0(getwd(),"/portfolio_comparison/7_strategies_noshort_varying_constraint/cumulative_return_with_constraint_quantile",quantl[i],".png")
  png(file = pngname, width=500, height=400, bg = "transparent")
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("minVar", "minVar no short", "Equal","meanVar", "meanVar no short", "network", "network no short"),
         col=Colors_Ret[c(1:3,4,7,9,5)], lty=1, cex=0.8)
  dev.off()
}

##### Cumulative returns of 8 strategies #####
######  1-constraint varying phi  ######
### 1.GMV, 2.GMV no short, 3.equal: (color) 1:3 ###
### 4-7.network with phistar=0.6:0.1:0.9: (color) 9,8,10,11 ###
### 8.network with phistar mean no short: (color) 5  ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi[[3]]),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi[[7]]),
                  data.frame(cumureturn_network_vary_with_phi[[9]]),
                  data.frame(cumureturn_network_1constraint)),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_5_strategies_with_constraint_quantile",
                quantl[3]*100,"_",quantl[5]*100,"_",quantl[7]*100,"_",quantl[9]*100,".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal",
                          paste("network","quantile=",quantl[3]),paste("network","quantile=",quantl[5]),
                          paste("network","quantile=",quantl[7]),paste("network","quantile=",quantl[9]),
                          "network no short"),
       col=Colors_Ret[c(1:3,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()

### 1.GMV, 2.GMV no short, 3.equal: (color) 1:3 ###
### 4-7.network with phistar=0.1:0.2:0.7: (color) 9,8,10,11 ###
### 8.network with phistar mean no short: (color) 5  ###
# cumulative returns in one figure
Colors_Ret<-rainbow(12)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi[[1]]),
                  data.frame(cumureturn_network_vary_with_phi[[3]]),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi[[7]]),
                  data.frame(cumureturn_network_1constraint)),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_5_strategies_with_constraint_quantile",
                quantl[1]*100,"_",quantl[3]*100,"_",quantl[5]*100,"_",quantl[7]*100,".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal",
                          paste("network","quantile=",quantl[1]),paste("network","quantile=",quantl[3]),
                          paste("network","quantile=",quantl[5]),paste("network","quantile=",quantl[7]),
                          "network no short"),
       col=Colors_Ret[c(1:3,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()

###### 1-constraint varying phi noshort ######
### 1.GMV, 2.GMV no short, 3.equal: (color) 1:3 ###
### 4.network with phistar mean: (color) 9  ###
### 5-8.network with phistar=0.6:0.1:0.9 no short: (color) 8,10,11,5   ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[1]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[3]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[7]])
                  ),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_5_strategies_noshort_with_constraint_quantile",
                quantl[1]*100,"_",quantl[3]*100,"_",quantl[5]*100,"_",quantl[7]*100,".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal",paste("network","mean centrality constraint"),
                          paste("network","noshort","quantile=",quantl[1]),paste("network","noshort","quantile=",quantl[3]),
                          paste("network","noshort","quantile=",quantl[5]),paste("network","noshort","quantile=",quantl[7])
),
col=Colors_Ret[c(1:3,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()

######  1-constraint varying phi with Dantzig  ######
### 1.GMV, 2.GMV no short, 3.equal: (color) 1:3 ###
### 4-7.network with phistar=0.1:0.2:0.7: (color) 9,8,10,11 ###
### 8.network with phistar mean no short: (color) 5  ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[1]]),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[3]]),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[7]]),
                  data.frame(cumureturn_network_1constraint)),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumureturn_8_strategies_1_constraint_varying_phi_with_Dantzig",
                quantl[1]*100,"_",quantl[3]*100,"_",quantl[5]*100,"_",quantl[6]*100,".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal",
                          paste("network","quantile=",quantl[1]),paste("network","quantile=",quantl[3]),
                          paste("network","quantile=",quantl[5]),paste("network","quantile=",quantl[6]),
                          "network no short"),
       col=Colors_Ret[c(1:3,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()


######  2-constraint varying phi  ######
### 1.GMV, 2.GMV no short, 3.equal: (color) 1:3 ###
### 4-7.network with phistar=0.6:0.1:0.9: (color) 9,8,10,11 ###
### 8.network with phistar mean no short: (color) 5  ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi_2constraint[[3]]),
                  data.frame(cumureturn_network_vary_with_phi_2constraint[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_2constraint[[7]]),
                  data.frame(cumureturn_network_vary_with_phi_2constraint[[9]]),
                  data.frame(cumureturn_network_1constraint)),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumureturn_8_strategies_2_constraint",
                quantl[3]*100,"_",quantl[5]*100,"_",quantl[7]*100,"_",quantl[9]*100,".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal",
                          paste("network","quantile=",quantl[3]),paste("network","quantile=",quantl[5]),
                          paste("network","quantile=",quantl[7]),paste("network","quantile=",quantl[9]),
                          "network no short"),
       col=Colors_Ret[c(1:3,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()

##### Cumulative wealth of 8 strategies #####
######  1-constraint varying phi with Dantzig  ######
### 1.GMV, 2.GMV no short, 3.equal: (color) 1:3 ###
### 4-7.network with phistar=0.1:0.2:0.7: (color) 9,8,10,11 ###
### 8.network with phistar mean no short: (color) 5  ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(exp(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_network_1constraint),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[1]]),
                  # data.frame(cumureturn_network_vary_with_phi_Dantzig[[3]]),
                  data.frame(cumureturn_network_vary_with_phi_glasso[[1]]),
                  # data.frame(cumureturn_network_vary_with_phi_Dantzig[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_2constraint_Dantzig[[1]]),
                  data.frame(cumureturn_network_vary_with_phi_Dantzig[[7]]),
                  data.frame(cumureturn_network_vary_with_phi_2constraint_glasso[[1]]))),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumuwealth_8_strategies_1_constraint_varying_phi_with_Dantzig",
                quantl[1]*100,"_",quantl[3]*100,"_",quantl[5]*100,"_",quantl[7]*100,".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,8,10,11,5)], ylab="", xlab="")
# legend("topleft",legend=c("GMV", "1-constraint plug-in", "Equal",
#                           paste("Dantzig","quantile=",quantl[1]),paste("glasso","quantile=",quantl[1]),
#                           paste("2-constraint Dantzig","quantile=",quantl[1]),paste("network","quantile=",quantl[7]),
#                           paste("2-constraint glasso","quantile=",quantl[1])),
# col=Colors_Ret[c(1:3,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()


######  2-constraint varying phi  ######
### 1.GMV, 2.GMV no short, 3.equal: (color) 1:3 ###
### 4-7.network with phistar=0.1:0.2:0.7: (color) 9,8,10,11 ###
### 8.network with phistar mean no short: (color) 5  ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(exp(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi_2constraint[[1]]),
                  data.frame(cumureturn_network_vary_with_phi_2constraint[[3]]),
                  data.frame(cumureturn_network_vary_with_phi_2constraint[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_2constraint[[7]]),
                  data.frame(cumureturn_network_1constraint_noshort))),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumuwealth_8_strategies_2_constraint",
                "_",quantl[1]*100,"_",quantl[3]*100,"_",quantl[5]*100,"_",quantl[7]*100,".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal",paste("network","quantile=",quantl[1]),
                          paste("network","quantile=",quantl[3]),paste("network","quantile=",quantl[5]),
                          paste("network","quantile=",quantl[7]),
                          "network no short"),
       col=Colors_Ret[c(1:3,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()

######  2-constraint varying phi with Dantzig  ######
### 1.GMV, 2.GMV no short, 3.equal: (color) 1:3 ###
### 4-7.network with phistar=0.1:0.2:0.7: (color) 9,8,10,11 ###
### 8.network with phistar mean no short: (color) 5  ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(exp(cbind(data.frame(cumureturn_minVar),
                      data.frame(cumureturn_minVar_noshort),
                      data.frame(cumureturn_equal),
                      data.frame(cumureturn_network_vary_with_phi_2constraint_Dantzig[[1]]),
                      data.frame(cumureturn_network_vary_with_phi_2constraint_Dantzig[[3]]),
                      data.frame(cumureturn_network_vary_with_phi_2constraint_Dantzig[[5]]),
                      data.frame(cumureturn_network_vary_with_phi_2constraint_Dantzig[[7]]),
                      data.frame(cumureturn_network_1constraint_noshort))),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumureturn_8_strategies_2_constraint_Dantzig",
                "_",quantl[1]*100,"_",quantl[3]*100,"_",quantl[5]*100,"_",quantl[7]*100,".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,8,10,11,5)], ylab="", xlab="")
# legend("topleft",legend=c("minVar", "minVar without short", "Equal",paste("network","quantile=",quantl[1]),
#                           paste("network","quantile=",quantl[3]),paste("network","quantile=",quantl[5]),
#                           paste("network","quantile=",quantl[7]),
#                           "network no short"),
#        col=Colors_Ret[c(1:3,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()




##### Cumulative returns of 10 strategies #####
###### 1-constraint varying phi ######
### 1.GMV, 2.GMV no short, 3.equal, 4.meanVar, 5.meanVar no short: (color) 1:3,4,7 ###
### 6-9.network with phistar=0.6:0.1:0.9: (color) 9,8,10,11  ###
### 10.network with phistar mean no short: (color) 5  ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_meanVar),
                  data.frame(cumureturn_meanVar_noshort),
                  data.frame(cumureturn_network_vary_with_phi[[3]]),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi[[7]]),
                  data.frame(cumureturn_network_vary_with_phi[[9]]),
                  data.frame(cumureturn_network_1constraint_noshort)),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_7_strategies_with_constraint_quantile",
                quantl[3]*100,"_",quantl[5]*100,"_",quantl[7]*100,"_",quantl[9]*100,".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal","meanVar", "meanVar without short",
                          paste("network","quantile=",quantl[3]),paste("network","quantile=",quantl[5]),
                          paste("network","quantile=",quantl[7]),paste("network","quantile=",quantl[9]),
                          "network noshort"),
       col=Colors_Ret[c(1:3,4,7,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()
###### 1-constraint varying phi noshort ######
### 1.GMV, 2.GMV no short, 3.equal, 4.meanVar, 5.meanVar no short: (color) 1:3,4,7 ###
### 6.network with phistar mean: (color) 9 ###
### 7-10.network with phistar=0.6:0.1:0.9 no short: (color) 8,10,11,5 ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_meanVar),
                  data.frame(cumureturn_meanVar_noshort),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[1]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[3]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[7]])
),
order.by=as.Date(as.character(date), format="%Y-%m-%d")
)
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_7_strategies_noshort_with_constraint_quantile",
                quantl[1],"_",quantl[3],"_",quantl[5],"_",quantl[7],".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal","meanVar", "meanVar without short",
                          "network mean centrality constraint",
                          paste("network noshort","quantile=",quantl[1]),paste("network noshort","quantile=",quantl[3]),
                          paste("network noshort","quantile=",quantl[5]),paste("network noshort","quantile=",quantl[7])
),
col=Colors_Ret[c(1:3,4,7,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()


#### Animations output ####
##### Cumulative returns of 5 strategies #####
###### 1-constraint varying phi ######
### 1.GMV, 2.GMV long-only, 3.equal: (color) 1:3 ###
### 4.network varying constraint: (color) 9 ###
### 5.network with phistar mean long-only: (color) 5  ###
Colors_Ret<-rainbow(12)
quantl<-seq(0.1,0.9,0.1)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_1constraint_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"),
  col=Colors_Ret[1:5], lty=1, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/cumulative_return_5_strategies_varying_constraint.gif"))
###### 1-constraint varying phi no short ######
### 1.GMV, 2.GMV no short, 3.equal: (color) 1:3 ###
### 4.network with phistat 0.8: (color) 9 ###
### 5.network with varying constraint no short: (color) 5  ###
Colors_Ret<-rainbow(12)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[5]]),
                    data.frame(cumureturn_network_vary_with_phi_noshort[[i]])),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="")
  # legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"),
  # col=Colors_Ret[1:5], lty=1:2, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/cumulative_return_5_strategies_varying_constraint_noshort.gif"))

###### 1-constraint varying phi with Dantzig ######
### 1.GMV, 2.GMV long-only, 3.equal: (color) 1:3 ###
### 4.network varying constraint with Dantzig: (color) 9 ###
### 5.network with phistar mean long-only: (color) 5  ###
Colors_Ret<-rainbow(12)
quantl<-seq(0.1,0.9,0.1)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi_Dantzig[[i]]),
                    data.frame(cumureturn_network_1constraint_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("GMV", "GMV long-only", "Equal", "network Dantzig", "network long-only"),
         col=Colors_Ret[1:5], lty=1, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/cumureturn_5_strategies_varying_constraint_Dantzig.gif"))

###### 1-constraint varying phi with glasso ######
### 1.GMV, 2.GMV long-only, 3.equal: (color) 1:3 ###
### 4.network varying constraint with glasso: (color) 9 ###
### 5.network with phistar mean long-only: (color) 5  ###
Colors_Ret<-rainbow(12)
quantl<-seq(0.1,0.9,0.1)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi_glasso[[i]]),
                    data.frame(cumureturn_network_1constraint_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("GMV", "GMV long-only", "Equal", "network Dantzig", "network long-only"),
         col=Colors_Ret[1:5], lty=1, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/cumureturn_5_strategies_varying_constraint_glasso.gif"))

##### Cumulative returns of 7 strategies #####
###### 1-constraint varying phi ######
### 1.GMV, 2.GMV no short, 3.equal, 4.meanVar, 5.meanVar no short: (color) 1:3,4,7 ###
### 6.network varying constraint: (color) 9 ###
### 7.network with phistar 0.8 no short: (color) 5   ###
Colors_Ret<-rainbow(12)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_meanVar),
                    data.frame(cumureturn_meanVar_noshort),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_1constraint_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,5)], ylab="", xlab="")
  # legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"),
  # col=Colors_Ret[1:5], lty=1:2, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/cumulative_return_7_strategies_varying_constraint.gif"))
###### 1-constraint varying phi noshort ######
### 1.GMV, 2.GMV no short, 3.equal, 4.meanVar, 5.meanVar no short: (color) 1:3,4,7 ###
### 6.network with phistat mean: (color) 9  ###
### n7.etwork with varying constraint no short: (color) 5   ###
Colors_Ret<-rainbow(12)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(quantl) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_meanVar),
                    data.frame(cumureturn_meanVar_noshort),
                    data.frame(cumureturn_network_vary_with_phi[[5]]),
                    data.frame(cumureturn_network_vary_with_phi_noshort[[i]])),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,5)], ylab="", xlab="")
  # legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"), col=Colors_Ret[1:5], lty=1, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/cumulative_return_7_strategies_noshort_varying_constraint.gif"))

