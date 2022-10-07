# Version 1.0.2

# In this file, we add turnover, using the new function p:turnover

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

#### Figures and animations output ####
##### Figure: cumulative returns of 3 strategies #####
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
##### Figure: cumulative returns of 5 strategies #####
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
##### Animation: cumulative returns with varying constraint of 5 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network varying constraint: (color) 9 ###
### network with phistar 0.8 no short: (color) 5  ###
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
  # legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"),
  # col=Colors_Ret[1:5], lty=1, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/pcumulative_return_5_strategies_varying_constraint.gif"))
###### network noshort varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistat 0.8: (color) 9 ###
### network with varying constraint no short: (color) 5  ###
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
image_write(animation, paste0(getwd(), "/portfolio_comparison/cumulative_return_5_strategies_noshort_varying_constraint.gif"))

##### Animation: cumulative returns with varying constraint of 7 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network varying constraint: (color) 9 ###
### network with phistar 0.8 no short: (color) 5   ###
Colors_Ret<-rainbow(12)
# phi_star<-seq(1,0,-0.05)
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
###### network noshort varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network with phistat 0.8: (color) 9  ###
### network with varying constraint no short: (color) 5   ###
Colors_Ret<-rainbow(12)
# phi_star<-seq(0.9,0.6,-0.05)
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

##### Figures: cumulative returns with varying constraint of 5 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network varying constraint: (color) 9 ###
### network with phistar 0.8 no short: (color) 5   ###
# phi_star<-seq(1,0,-0.05)
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

###### network noshort varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistar 0.8: (color) 9 ###
### network varying constraint no short: (color) 5 ###
# phi_star<-seq(0.9,0.6,-0.05)
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

##### Figures: cumulative returns with varying constraint of 7 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network varying constraint: (color) 9  ###
### network with phistar 0.8 no short: (color) 5   ###
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
  legend("topleft",legend=c("minVar", "minVar no short", "Equal","meanVar", "meanVar no short",  "network", "network no short"),
         col=Colors_Ret[c(1:3,4,7,9,5)], lty=1, cex=0.8)
  dev.off()
}
###### network noshort varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistar 0.8: (color) 9 ###
### network varying constraint no short: (color) 5 ###
# phi_star<-seq(0.9,0.6,-0.05)
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

##### Figure: cumulative returns with 4 network constraints of 5 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistar=0.6:0.1:0.9: (color) 9,8,10,11 ###
### network with phistar 0.8 no short: (color) 5  ###
# cumulative returns in one figure
Colors_Ret<-rainbow(12)
# phi_star<-seq(1,0,-0.05)
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
###### network noshort varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistar 0.8: (color) 9  ###
### network with phistar=0.6:0.1:0.9 no short: (color) 8,10,11,5   ###
# cumulative returns in one figure
Colors_Ret<-rainbow(12)
# phi_star<-seq(0.9,0.6,-0.05)
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

##### Figure: cumulative returns with 4 network constraints of 7 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network with phistar=0.6:0.1:0.9: (color) 9,8,10,11  ###
### network with phistar 0.8 no short: (color) 5  ###
# cumulative returns in one figure
Colors_Ret<-rainbow(12)
# phi_star<-seq(1,0,-0.05)
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
###### network noshort varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network with phistar 0.8: (color) 9 ###
### network with phistar=0.6:0.1:0.9 no short: (color) 8,10,11,5 ###
# cumulative returns in one figure
Colors_Ret<-rainbow(12)
# phi_star<-seq(0.9,0.6,-0.05)
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


#### Annual cumulative returns ####
n=numel(cumureturn_minVar)
cumureturn_minVar[n]/n*252
cumureturn_meanVar[n]/n*252
cumureturn_equal[n]/n*252
cumureturn_network_vary_with_phi[[1]][n]/n*252
cumureturn_network_1constraint[n]/n*252
cumureturn_network_vary_with_phi[[9]][n]/n*252
cumureturn_network_vary_with_phi_2constraint[[1]][n]/n*252
cumureturn_network_2constraint[n]/n*252
cumureturn_network_vary_with_phi_2constraint[[9]][n]/n*252
# long-only
cumureturn_minVar_noshort[n]/n*252
cumureturn_meanVar_noshort[n]/n*252
cumureturn_network_vary_with_phi_noshort[[1]][n]/n*252
cumureturn_network_1constraint_noshort[n]/n*252
cumureturn_network_vary_with_phi_noshort[[9]][n]/n*252
cumureturn_network_vary_with_phi_2constraint_noshort[[1]][n]/n*252
cumureturn_network_2constraint_noshort[n]/n*252
cumureturn_network_vary_with_phi_2constraint_noshort[[9]][n]/n*252
# Dantzig
n=length(cumureturn_minVar_Dantzig)
cumureturn_minVar_Dantzig[n]/n*252
cumureturn_meanVar_Dantzig[n]/n*252
cumureturn_network_vary_with_phi_Dantzig[[1]][n]/n*252
cumureturn_network_1constraint_Dantzig[n]/n*252
cumureturn_network_vary_with_phi_Dantzig[[9]][n]/n*252
cumureturn_network_vary_with_phi_2constraint_Dantzig[[1]][n]/n*252
cumureturn_network_2constraint_Dantzig[n]/n*252
cumureturn_network_vary_with_phi_2constraint_Dantzig[[9]][n]/n*252
# glasso
n=length(cumureturn_minVar_glasso)
cumureturn_minVar_glasso[n]/n*252
cumureturn_meanVar_glasso[n]/n*252
cumureturn_network_vary_with_phi_glasso[[1]][n]/n*252
cumureturn_network_1constraint_glasso[n]/n*252
cumureturn_network_vary_with_phi_glasso[[9]][n]/n*252
cumureturn_network_vary_with_phi_2constraint_glasso[[1]][n]/n*252
cumureturn_network_2constraint_glasso[n]/n*252
cumureturn_network_vary_with_phi_2constraint_glasso[[9]][n]/n*252

# cumureturn_network_datadriven_phistar[n]/n*252
# cumureturn_network_datadriven_phistar_noshort[n]/n*252
# cumureturn_network_vary_with_phi[[1]][n]/n*252
# cumureturn_network_vary_with_phi[[2]][n]/n*252
# cumureturn_network_vary_with_phi[[3]][n]/n*252
# cumureturn_network_vary_with_phi[[4]][n]/n*252
# cumureturn_network_vary_with_phi[[5]][n]/n*252
# cumureturn_network_vary_with_phi[[6]][n]/n*252
# cumureturn_network_vary_with_phi[[7]][n]/n*252
# cumureturn_network_vary_with_phi[[8]][n]/n*252
# cumureturn_network_vary_with_phi[[9]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[1]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[2]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[3]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[4]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[5]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[6]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[7]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[8]][n]/n*252
# cumureturn_network_vary_with_phi_noshort[[9]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[1]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[2]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[3]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[4]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[5]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[6]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[7]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[8]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint[[9]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[1]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[2]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[3]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[4]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[5]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[6]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[7]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[8]][n]/n*252
# cumureturn_network_vary_with_phi_2constraint_noshort[[9]][n]/n*252

#### std ####
std(return_minVar)*sqrt(252)
std(return_meanVar)*sqrt(252)
std(return_equal)*sqrt(252)
std(return_network_vary_with_phi[[1]])*sqrt(252)
std(return_network_1constraint)*sqrt(252)
std(return_network_vary_with_phi[[9]])*sqrt(252)
std(return_network_vary_with_phi_2constraint[[1]])*sqrt(252)
std(return_network_2constraint)*sqrt(252)
std(return_network_vary_with_phi_2constraint[[9]])*sqrt(252)
std(return_minVar_noshort)*sqrt(252)
std(return_meanVar_noshort)*sqrt(252)
std(return_network_vary_with_phi_noshort[[1]])*sqrt(252)
std(return_network_1constraint_noshort)*sqrt(252)
std(return_network_vary_with_phi_noshort[[9]])*sqrt(252)
std(return_network_vary_with_phi_2constraint_noshort[[1]])*sqrt(252)
std(return_network_2constraint_noshort)*sqrt(252)
std(return_network_vary_with_phi_2constraint_noshort[[9]])*sqrt(252)

# Dantzig
std(return_minVar_Dantzig)*sqrt(252)
std(return_meanVar_Dantzig)*sqrt(252)
std(return_network_vary_with_phi_Dantzig[[1]])*sqrt(252)
std(return_network_1constraint_Dantzig)*sqrt(252)
std(return_network_vary_with_phi_Dantzig[[9]])*sqrt(252)
std(return_network_vary_with_phi_2constraint_Dantzig[[1]])*sqrt(252)
std(return_network_2constraint_Dantzig)*sqrt(252)
std(return_network_vary_with_phi_2constraint_Dantzig[[9]])*sqrt(252)

# glasso
std(return_minVar_glasso)*sqrt(252)
std(return_meanVar_glasso)*sqrt(252)
std(return_network_vary_with_phi_glasso[[1]])*sqrt(252)
std(return_network_1constraint_glasso)*sqrt(252)
std(return_network_vary_with_phi_glasso[[9]])*sqrt(252)
std(return_network_vary_with_phi_2constraint_glasso[[1]])*sqrt(252)
std(return_network_2constraint_glasso)*sqrt(252)
std(return_network_vary_with_phi_2constraint_glasso[[9]])*sqrt(252)

# std(return_network_vary_with_phi[[1]])*sqrt(252)
# std(return_network_vary_with_phi[[2]])*sqrt(252)
# std(return_network_vary_with_phi[[3]])*sqrt(252)
# std(return_network_vary_with_phi[[4]])*sqrt(252)
# std(return_network_vary_with_phi[[5]])*sqrt(252)
# std(return_network_vary_with_phi[[6]])*sqrt(252)
# std(return_network_vary_with_phi[[7]])*sqrt(252)
# std(return_network_vary_with_phi[[8]])*sqrt(252)
# std(return_network_vary_with_phi[[9]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[1]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[2]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[3]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[4]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[5]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[6]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[7]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[8]])*sqrt(252)
# std(return_network_vary_with_phi_noshort[[9]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[1]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[2]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[3]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[4]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[5]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[6]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[7]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[8]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint[[9]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[1]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[2]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[3]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[4]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[5]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[6]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[7]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[8]])*sqrt(252)
# std(return_network_vary_with_phi_2constraint_noshort[[9]])*sqrt(252)

#### sharpe ratio ####
cumureturn_minVar[n]/n*252/(std(return_minVar)*sqrt(252))
cumureturn_meanVar[n]/n*252/(std(return_meanVar)*sqrt(252))
cumureturn_equal[n]/n*252/(std(return_equal)*sqrt(252))
cumureturn_network_vary_with_phi[[1]][n]/n*252/(std(return_network_vary_with_phi[[1]])*sqrt(252))
cumureturn_network_1constraint[n]/n*252/(std(return_network_1constraint)*sqrt(252))
cumureturn_network_vary_with_phi[[9]][n]/n*252/(std(return_network_vary_with_phi[[9]])*sqrt(252))
cumureturn_network_vary_with_phi_2constraint[[1]][n]/n*252/(std(return_network_vary_with_phi_2constraint[[1]])*sqrt(252))
cumureturn_network_2constraint[n]/n*252/(std(return_network_2constraint)*sqrt(252))
cumureturn_network_vary_with_phi_2constraint[[9]][n]/n*252/(std(return_network_vary_with_phi_2constraint[[9]])*sqrt(252))
cumureturn_minVar_noshort[n]/n*252/(std(return_minVar_noshort)*sqrt(252))
cumureturn_meanVar_noshort[n]/n*252/(std(return_meanVar_noshort)*sqrt(252))
cumureturn_network_vary_with_phi_noshort[[1]][n]/n*252/(std(return_network_vary_with_phi_noshort[[1]])*sqrt(252))
cumureturn_network_1constraint_noshort[n]/n*252/(std(return_network_1constraint_noshort)*sqrt(252))
cumureturn_network_vary_with_phi_noshort[[9]][n]/n*252/(std(return_network_vary_with_phi_noshort[[9]])*sqrt(252))
cumureturn_network_vary_with_phi_2constraint_noshort[[1]][n]/n*252/(std(return_network_vary_with_phi_2constraint_noshort[[1]])*sqrt(252))
cumureturn_network_2constraint_noshort[n]/n*252/(std(return_network_2constraint_noshort)*sqrt(252))
cumureturn_network_vary_with_phi_2constraint_noshort[[9]][n]/n*252/(std(return_network_vary_with_phi_2constraint_noshort[[9]])*sqrt(252))

# Dantzig
cumureturn_minVar_Dantzig[n]/n*252/(std(return_minVar_Dantzig)*sqrt(252))
cumureturn_meanVar_Dantzig[n]/n*252/(std(return_meanVar_Dantzig)*sqrt(252))
cumureturn_network_vary_with_phi_Dantzig[[1]][n]/n*252/(std(return_network_vary_with_phi_Dantzig[[1]])*sqrt(252))
cumureturn_network_1constraint_Dantzig[n]/n*252/(std(return_network_1constraint_Dantzig)*sqrt(252))
cumureturn_network_vary_with_phi_Dantzig[[9]][n]/n*252/(std(return_network_vary_with_phi_Dantzig[[9]])*sqrt(252))
cumureturn_network_vary_with_phi_2constraint_Dantzig[[1]][n]/n*252/(std(return_network_vary_with_phi_2constraint_Dantzig[[1]])*sqrt(252))
cumureturn_network_2constraint_Dantzig[n]/n*252/(std(return_network_2constraint_Dantzig)*sqrt(252))
cumureturn_network_vary_with_phi_2constraint_Dantzig[[9]][n]/n*252/(std(return_network_vary_with_phi_2constraint_Dantzig[[9]])*sqrt(252))

# glasso
cumureturn_minVar_glasso[n]/n*252/(std(return_minVar_glasso)*sqrt(252))
cumureturn_meanVar_glasso[n]/n*252/(std(return_meanVar_glasso)*sqrt(252))
cumureturn_network_vary_with_phi_glasso[[1]][n]/n*252/(std(return_network_vary_with_phi_glasso[[1]])*sqrt(252))
cumureturn_network_1constraint_glasso[n]/n*252/(std(return_network_1constraint_glasso)*sqrt(252))
cumureturn_network_vary_with_phi_glasso[[9]][n]/n*252/(std(return_network_vary_with_phi_glasso[[9]])*sqrt(252))
cumureturn_network_vary_with_phi_2constraint_glasso[[1]][n]/n*252/(std(return_network_vary_with_phi_2constraint_glasso[[1]])*sqrt(252))
cumureturn_network_2constraint_glasso[n]/n*252/(std(return_network_2constraint_glasso)*sqrt(252))
cumureturn_network_vary_with_phi_2constraint_glasso[[9]][n]/n*252/(std(return_network_vary_with_phi_2constraint_glasso[[9]])*sqrt(252))

#### skewness ####
skewness(return_minVar)
skewness(return_meanVar)
skewness(return_equal)
skewness(return_network_vary_with_phi[[1]])
skewness(return_network_1constraint)
skewness(return_network_vary_with_phi[[9]])
skewness(return_network_vary_with_phi_2constraint[[1]])
skewness(return_network_2constraint)
skewness(return_network_vary_with_phi_2constraint[[9]])
skewness(return_minVar_noshort)
skewness(return_meanVar_noshort)
skewness(return_network_vary_with_phi_noshort[[1]])
skewness(return_network_1constraint_noshort)
skewness(return_network_vary_with_phi_noshort[[9]])
skewness(return_network_vary_with_phi_2constraint_noshort[[1]])
skewness(return_network_2constraint_noshort)
skewness(return_network_vary_with_phi_2constraint_noshort[[9]])

# Dantzig
skewness(return_minVar_Dantzig)
skewness(return_meanVar_Dantzig)
skewness(return_network_vary_with_phi_Dantzig[[1]])
skewness(return_network_1constraint_Dantzig)
skewness(return_network_vary_with_phi_Dantzig[[9]])
skewness(return_network_vary_with_phi_2constraint_Dantzig[[1]])
skewness(return_network_2constraint_Dantzig)
skewness(return_network_vary_with_phi_2constraint_Dantzig[[9]])

# glasso
skewness(return_minVar_glasso)
skewness(return_meanVar_glasso)
skewness(return_network_vary_with_phi_glasso[[1]])
skewness(return_network_1constraint_glasso)
skewness(return_network_vary_with_phi_glasso[[9]])
skewness(return_network_vary_with_phi_2constraint_glasso[[1]])
skewness(return_network_2constraint_glasso)
skewness(return_network_vary_with_phi_2constraint_glasso[[9]])

# skewness(return_network_vary_with_phi[[1]])
# skewness(return_network_vary_with_phi[[2]])
# skewness(return_network_vary_with_phi[[3]])
# skewness(return_network_vary_with_phi[[4]])
# skewness(return_network_vary_with_phi[[5]])
# skewness(return_network_vary_with_phi[[6]])
# skewness(return_network_vary_with_phi[[7]])
# skewness(return_network_vary_with_phi[[8]])
# skewness(return_network_vary_with_phi[[9]])
# skewness(return_network_vary_with_phi_noshort[[1]])
# skewness(return_network_vary_with_phi_noshort[[2]])
# skewness(return_network_vary_with_phi_noshort[[3]])
# skewness(return_network_vary_with_phi_noshort[[4]])
# skewness(return_network_vary_with_phi_noshort[[5]])
# skewness(return_network_vary_with_phi_noshort[[6]])
# skewness(return_network_vary_with_phi_noshort[[7]])
# skewness(return_network_vary_with_phi_noshort[[8]])
# skewness(return_network_vary_with_phi_noshort[[9]])
# skewness(return_network_vary_with_phi_2constraint[[1]])
# skewness(return_network_vary_with_phi_2constraint[[2]])
# skewness(return_network_vary_with_phi_2constraint[[3]])
# skewness(return_network_vary_with_phi_2constraint[[4]])
# skewness(return_network_vary_with_phi_2constraint[[5]])
# skewness(return_network_vary_with_phi_2constraint[[6]])
# skewness(return_network_vary_with_phi_2constraint[[7]])
# skewness(return_network_vary_with_phi_2constraint[[8]])
# skewness(return_network_vary_with_phi_2constraint[[9]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[1]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[2]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[3]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[4]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[5]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[6]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[7]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[8]])
# skewness(return_network_vary_with_phi_2constraint_noshort[[9]])

#### kurtosis ####
kurtosis(return_minVar)
kurtosis(return_meanVar)
kurtosis(return_equal)
kurtosis(return_network_vary_with_phi[[1]])
kurtosis(return_network_1constraint)
kurtosis(return_network_vary_with_phi[[9]])
kurtosis(return_network_vary_with_phi_2constraint[[1]])
kurtosis(return_network_2constraint)
kurtosis(return_network_vary_with_phi_2constraint[[9]])
kurtosis(return_minVar_noshort)
kurtosis(return_meanVar_noshort)
kurtosis(return_network_vary_with_phi_noshort[[1]])
kurtosis(return_network_1constraint_noshort)
kurtosis(return_network_vary_with_phi_noshort[[9]])
kurtosis(return_network_vary_with_phi_2constraint_noshort[[1]])
kurtosis(return_network_2constraint_noshort)
kurtosis(return_network_vary_with_phi_2constraint_noshort[[9]])

# Dantzig
kurtosis(return_minVar_Dantzig)
kurtosis(return_meanVar_Dantzig)
kurtosis(return_network_vary_with_phi_Dantzig[[1]])
kurtosis(return_network_1constraint_Dantzig)
kurtosis(return_network_vary_with_phi_Dantzig[[9]])
kurtosis(return_network_vary_with_phi_2constraint_Dantzig[[1]])
kurtosis(return_network_2constraint_Dantzig)
kurtosis(return_network_vary_with_phi_2constraint_Dantzig[[9]])

# glasso
kurtosis(return_minVar_glasso)
kurtosis(return_meanVar_glasso)
kurtosis(return_network_vary_with_phi_glasso[[1]])
kurtosis(return_network_1constraint_glasso)
kurtosis(return_network_vary_with_phi_glasso[[9]])
kurtosis(return_network_vary_with_phi_2constraint_glasso[[1]])
kurtosis(return_network_2constraint_glasso)
kurtosis(return_network_vary_with_phi_2constraint_glasso[[9]])

# kurtosis(return_network_vary_with_phi[[1]])
# kurtosis(return_network_vary_with_phi[[2]])
# kurtosis(return_network_vary_with_phi[[3]])
# kurtosis(return_network_vary_with_phi[[4]])
# kurtosis(return_network_vary_with_phi[[5]])
# kurtosis(return_network_vary_with_phi[[6]])
# kurtosis(return_network_vary_with_phi[[7]])
# kurtosis(return_network_vary_with_phi[[8]])
# kurtosis(return_network_vary_with_phi[[9]])
# kurtosis(return_network_vary_with_phi_noshort[[1]])
# kurtosis(return_network_vary_with_phi_noshort[[2]])
# kurtosis(return_network_vary_with_phi_noshort[[3]])
# kurtosis(return_network_vary_with_phi_noshort[[4]])
# kurtosis(return_network_vary_with_phi_noshort[[5]])
# kurtosis(return_network_vary_with_phi_noshort[[6]])
# kurtosis(return_network_vary_with_phi_noshort[[7]])
# kurtosis(return_network_vary_with_phi_noshort[[8]])
# kurtosis(return_network_vary_with_phi_noshort[[9]])
# kurtosis(return_network_vary_with_phi_2constraint[[1]])
# kurtosis(return_network_vary_with_phi_2constraint[[2]])
# kurtosis(return_network_vary_with_phi_2constraint[[3]])
# kurtosis(return_network_vary_with_phi_2constraint[[4]])
# kurtosis(return_network_vary_with_phi_2constraint[[5]])
# kurtosis(return_network_vary_with_phi_2constraint[[6]])
# kurtosis(return_network_vary_with_phi_2constraint[[7]])
# kurtosis(return_network_vary_with_phi_2constraint[[8]])
# kurtosis(return_network_vary_with_phi_2constraint[[9]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[1]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[2]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[3]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[4]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[5]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[6]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[7]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[8]])
# kurtosis(return_network_vary_with_phi_2constraint_noshort[[9]])

#### drawdown ####
maxDrawdown(return_minVar)
maxDrawdown(return_meanVar)
maxDrawdown(return_equal)
maxDrawdown(return_network_vary_with_phi[[1]])
maxDrawdown(return_network_1constraint)
maxDrawdown(return_network_vary_with_phi[[9]])
maxDrawdown(return_network_vary_with_phi_2constraint[[1]])
maxDrawdown(return_network_2constraint)
maxDrawdown(return_network_vary_with_phi_2constraint[[9]])
maxDrawdown(return_minVar_noshort)
maxDrawdown(return_meanVar_noshort)
maxDrawdown(return_network_vary_with_phi_noshort[[1]])
maxDrawdown(return_network_1constraint_noshort)
maxDrawdown(return_network_vary_with_phi_noshort[[9]])
maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[1]])
maxDrawdown(return_network_2constraint_noshort)
maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[9]])

# Dantzig 
maxDrawdown(return_minVar_Dantzig)
maxDrawdown(return_meanVar_Dantzig)
maxDrawdown(return_network_vary_with_phi_Dantzig[[1]])
maxDrawdown(return_network_1constraint_Dantzig)
maxDrawdown(return_network_vary_with_phi_Dantzig[[9]])
maxDrawdown(return_network_vary_with_phi_2constraint_Dantzig[[1]])
maxDrawdown(return_network_2constraint_Dantzig)
maxDrawdown(return_network_vary_with_phi_2constraint_Dantzig[[9]])

# glasso
maxDrawdown(return_minVar_glasso)
maxDrawdown(return_meanVar_glasso)
maxDrawdown(return_network_vary_with_phi_glasso[[1]])
maxDrawdown(return_network_1constraint_glasso)
maxDrawdown(return_network_vary_with_phi_glasso[[9]])
maxDrawdown(return_network_vary_with_phi_2constraint_glasso[[1]])
maxDrawdown(return_network_2constraint_glasso)
maxDrawdown(return_network_vary_with_phi_2constraint_glasso[[9]])

# maxDrawdown(return_network_vary_with_phi[[1]])
# maxDrawdown(return_network_vary_with_phi[[2]])
# maxDrawdown(return_network_vary_with_phi[[3]])
# maxDrawdown(return_network_vary_with_phi[[4]])
# maxDrawdown(return_network_vary_with_phi[[5]])
# maxDrawdown(return_network_vary_with_phi[[6]])
# maxDrawdown(return_network_vary_with_phi[[7]])
# maxDrawdown(return_network_vary_with_phi[[8]])
# maxDrawdown(return_network_vary_with_phi[[9]])
# maxDrawdown(return_network_vary_with_phi_noshort[[1]])
# maxDrawdown(return_network_vary_with_phi_noshort[[2]])
# maxDrawdown(return_network_vary_with_phi_noshort[[3]])
# maxDrawdown(return_network_vary_with_phi_noshort[[4]])
# maxDrawdown(return_network_vary_with_phi_noshort[[5]])
# maxDrawdown(return_network_vary_with_phi_noshort[[6]])
# maxDrawdown(return_network_vary_with_phi_noshort[[7]])
# maxDrawdown(return_network_vary_with_phi_noshort[[8]])
# maxDrawdown(return_network_vary_with_phi_noshort[[9]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[1]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[2]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[3]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[4]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[5]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[6]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[7]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[8]])
# maxDrawdown(return_network_vary_with_phi_2constraint[[9]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[1]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[2]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[3]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[4]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[5]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[6]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[7]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[8]])
# maxDrawdown(return_network_vary_with_phi_2constraint_noshort[[9]])

#### autocorrelation ####
cor(return_minVar[1:n-1],return_minVar[2:n])
cor(return_meanVar[1:n-1],return_meanVar[2:n])
cor(return_equal[1:n-1],return_equal[2:n])
cor(return_network_vary_with_phi[[1]][1:n-1],return_network_vary_with_phi[[1]][2:n])
cor(return_network_1constraint[1:n-1],return_network_1constraint[2:n])
cor(return_network_vary_with_phi[[9]][1:n-1],return_network_vary_with_phi[[9]][2:n])
cor(return_network_vary_with_phi_2constraint[[1]][1:n-1],return_network_vary_with_phi_2constraint[[1]][2:n])
cor(return_network_2constraint[1:n-1],return_network_2constraint[2:n])
cor(return_network_vary_with_phi_2constraint[[9]][1:n-1],return_network_vary_with_phi_2constraint[[9]][2:n])
cor(return_minVar_noshort[1:n-1],return_minVar_noshort[2:n])
cor(return_meanVar_noshort[1:n-1],return_meanVar_noshort[2:n])
cor(return_network_vary_with_phi_noshort[[1]][1:n-1],return_network_vary_with_phi_noshort[[1]][2:n])
cor(return_network_1constraint_noshort[1:n-1],return_network_1constraint_noshort[2:n])
cor(return_network_vary_with_phi_noshort[[9]][1:n-1],return_network_vary_with_phi_noshort[[9]][2:n])
cor(return_network_vary_with_phi_2constraint_noshort[[1]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[1]][2:n])
cor(return_network_2constraint_noshort[1:n-1],return_network_2constraint_noshort[2:n])
cor(return_network_vary_with_phi_2constraint_noshort[[9]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[9]][2:n])

# Dantzig
cor(return_minVar_Dantzig[1:n-1],return_minVar_Dantzig[2:n])
cor(return_meanVar_Dantzig[1:n-1],return_meanVar_Dantzig[2:n])
cor(return_network_vary_with_phi_Dantzig[[1]][1:n-1],return_network_vary_with_phi_Dantzig[[1]][2:n])
cor(return_network_1constraint_Dantzig[1:n-1],return_network_1constraint_Dantzig[2:n])
cor(return_network_vary_with_phi_Dantzig[[9]][1:n-1],return_network_vary_with_phi_Dantzig[[9]][2:n])
cor(return_network_vary_with_phi_2constraint_Dantzig[[1]][1:n-1],return_network_vary_with_phi_2constraint_Dantzig[[1]][2:n])
cor(return_network_2constraint_Dantzig[1:n-1],return_network_2constraint_Dantzig[2:n])
cor(return_network_vary_with_phi_2constraint_Dantzig[[9]][1:n-1],return_network_vary_with_phi_2constraint_Dantzig[[9]][2:n])

# glasso
cor(return_minVar_glasso[1:n-1],return_minVar_glasso[2:n])
cor(return_meanVar_glasso[1:n-1],return_meanVar_glasso[2:n])
cor(return_network_vary_with_phi_glasso[[1]][1:n-1],return_network_vary_with_phi_glasso[[1]][2:n])
cor(return_network_1constraint_glasso[1:n-1],return_network_1constraint_glasso[2:n])
cor(return_network_vary_with_phi_glasso[[9]][1:n-1],return_network_vary_with_phi_glasso[[9]][2:n])
cor(return_network_vary_with_phi_2constraint_glasso[[1]][1:n-1],return_network_vary_with_phi_2constraint_glasso[[1]][2:n])
cor(return_network_2constraint_glasso[1:n-1],return_network_2constraint_glasso[2:n])
cor(return_network_vary_with_phi_2constraint_glasso[[9]][1:n-1],return_network_vary_with_phi_2constraint_glasso[[9]][2:n])


# cor(return_network_vary_with_phi[[1]][1:n-1],return_network_vary_with_phi[[1]][2:n])
# cor(return_network_vary_with_phi[[2]][1:n-1],return_network_vary_with_phi[[2]][2:n])
# cor(return_network_vary_with_phi[[3]][1:n-1],return_network_vary_with_phi[[3]][2:n])
# cor(return_network_vary_with_phi[[4]][1:n-1],return_network_vary_with_phi[[4]][2:n])
# cor(return_network_vary_with_phi[[5]][1:n-1],return_network_vary_with_phi[[5]][2:n])
# cor(return_network_vary_with_phi[[6]][1:n-1],return_network_vary_with_phi[[6]][2:n])
# cor(return_network_vary_with_phi[[7]][1:n-1],return_network_vary_with_phi[[7]][2:n])
# cor(return_network_vary_with_phi[[8]][1:n-1],return_network_vary_with_phi[[8]][2:n])
# cor(return_network_vary_with_phi[[9]][1:n-1],return_network_vary_with_phi[[9]][2:n])
# cor(return_network_vary_with_phi_noshort[[1]][1:n-1],return_network_vary_with_phi_noshort[[1]][2:n])
# cor(return_network_vary_with_phi_noshort[[2]][1:n-1],return_network_vary_with_phi_noshort[[2]][2:n])
# cor(return_network_vary_with_phi_noshort[[3]][1:n-1],return_network_vary_with_phi_noshort[[3]][2:n])
# cor(return_network_vary_with_phi_noshort[[4]][1:n-1],return_network_vary_with_phi_noshort[[4]][2:n])
# cor(return_network_vary_with_phi_noshort[[5]][1:n-1],return_network_vary_with_phi_noshort[[5]][2:n])
# cor(return_network_vary_with_phi_noshort[[6]][1:n-1],return_network_vary_with_phi_noshort[[6]][2:n])
# cor(return_network_vary_with_phi_noshort[[7]][1:n-1],return_network_vary_with_phi_noshort[[7]][2:n])
# cor(return_network_vary_with_phi_noshort[[8]][1:n-1],return_network_vary_with_phi_noshort[[8]][2:n])
# cor(return_network_vary_with_phi_noshort[[9]][1:n-1],return_network_vary_with_phi_noshort[[9]][2:n])
# cor(return_network_vary_with_phi_2constraint[[1]][1:n-1],return_network_vary_with_phi_2constraint[[1]][2:n])
# cor(return_network_vary_with_phi_2constraint[[2]][1:n-1],return_network_vary_with_phi_2constraint[[2]][2:n])
# cor(return_network_vary_with_phi_2constraint[[3]][1:n-1],return_network_vary_with_phi_2constraint[[3]][2:n])
# cor(return_network_vary_with_phi_2constraint[[4]][1:n-1],return_network_vary_with_phi_2constraint[[4]][2:n])
# cor(return_network_vary_with_phi_2constraint[[5]][1:n-1],return_network_vary_with_phi_2constraint[[5]][2:n])
# cor(return_network_vary_with_phi_2constraint[[6]][1:n-1],return_network_vary_with_phi_2constraint[[6]][2:n])
# cor(return_network_vary_with_phi_2constraint[[7]][1:n-1],return_network_vary_with_phi_2constraint[[7]][2:n])
# cor(return_network_vary_with_phi_2constraint[[8]][1:n-1],return_network_vary_with_phi_2constraint[[8]][2:n])
# cor(return_network_vary_with_phi_2constraint[[9]][1:n-1],return_network_vary_with_phi_2constraint[[9]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[1]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[1]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[2]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[2]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[3]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[3]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[4]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[4]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[5]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[5]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[6]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[6]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[7]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[7]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[8]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[8]][2:n])
# cor(return_network_vary_with_phi_2constraint_noshort[[9]][1:n-1],return_network_vary_with_phi_2constraint_noshort[[9]][2:n])

#### weights ####
a<-w_network_2constraint_noshort[1,]
a<-sort(a)
a[(p-9):p]
a
sort(w_network_2constraint_noshort[1,])[(p-9):p]
sort(w_network_2constraint_noshort[2,])[(p-9):p]
sort(w_network_2constraint_noshort[3,])[(p-9):p]
sort(w_network_2constraint_noshort[4,])[(p-9):p]
sort(w_network_2constraint_noshort[5,])[(p-9):p]
sort(w_network_2constraint_noshort[6,])[(p-9):p]

sort(w_network_2constraint_noshort[1,])[(p-9):p]


sort(colSums(w_minVar),decreasing = T)[1:10]
sort(colSums(w_meanVar),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi[[1]]),decreasing = T)[1:10]
sort(colSums(w_network_1constraint),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi[[9]]),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi_2constraint[[1]]),decreasing = T)[1:10]
sort(colSums(w_network_2constraint),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi_2constraint[[9]]),decreasing = T)[1:10]
sort(colSums(w_minVar_noshort),decreasing = T)[1:10]
sort(colSums(w_meanVar_noshort),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi_noshort[[1]]),decreasing = T)[1:10]
sort(colSums(w_network_1constraint_noshort),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi_noshort[[9]]),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi_2constraint_noshort[[1]]),decreasing = T)[1:10]
sort(colSums(w_network_2constraint_noshort),decreasing = T)[1:10]
sort(colSums(w_network_vary_with_phi_2constraint_noshort[[9]]),decreasing = T)[1:10]

sort(colSums(w_minVar_Dantzig),decreasing = T)[1:10]
sort(colSums(w_meanVar_Dantzig),decreasing = T)[1:10]
sort(colSums(w_network_1constraint_Dantzig),decreasing = T)[1:10]
sort(colSums(w_network_2constraint_Dantzig),decreasing = T)[1:10]

s<-t(apply(w_network_2constraint_noshort, 1, sort))
s<-t(apply(w_network_2constraint_noshort, 1, sort, decreasing=T))

#### turnover ####
p_turnover(w_minVar)
p_turnover(w_meanVar)
p_turnover(w_equal)
p_turnover(w_network_vary_with_phi[[1]])
p_turnover(w_network_1constraint)
p_turnover(w_network_vary_with_phi[[9]])
p_turnover(w_network_vary_with_phi_2constraint[[1]])
p_turnover(w_network_2constraint)
p_turnover(w_network_vary_with_phi_2constraint[[9]])
p_turnover(w_minVar_noshort)
p_turnover(w_meanVar_noshort)
p_turnover(w_network_vary_with_phi_noshort[[1]])
p_turnover(w_network_1constraint_noshort)
p_turnover(w_network_vary_with_phi_noshort[[9]])
p_turnover(w_network_vary_with_phi_2constraint_noshort[[1]])
p_turnover(w_network_2constraint_noshort)
p_turnover(w_network_vary_with_phi_2constraint_noshort[[9]])

# Dantzig
p_turnover(w_minVar_Dantzig)
p_turnover(w_meanVar_Dantzig)
p_turnover(w_network_vary_with_phi_Dantzig[[1]])
p_turnover(w_network_1constraint_Dantzig)
p_turnover(w_network_vary_with_phi_Dantzig[[9]])
p_turnover(w_network_vary_with_phi_2constraint_Dantzig[[1]])
p_turnover(w_network_2constraint_Dantzig)
p_turnover(w_network_vary_with_phi_2constraint_Dantzig[[9]])

# glasso
p_turnover(w_minVar_glasso)
p_turnover(w_meanVar_glasso)
p_turnover(w_network_vary_with_phi_glasso[[1]])
p_turnover(w_network_1constraint_glasso)
p_turnover(w_network_vary_with_phi_glasso[[9]])
p_turnover(w_network_vary_with_phi_2constraint_glasso[[1]])
p_turnover(w_network_2constraint_glasso)
p_turnover(w_network_vary_with_phi_2constraint_glasso[[9]])

