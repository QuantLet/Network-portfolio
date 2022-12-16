
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


# network of portfolio based on correlation matrix
network_port = network.portfolio(returnstd)
# eigenvalue
ec_port2 <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector
# compute portfolio frontier of network
covmat = cov(returnstd)
net.ef  = network.efficient.frontier(ec_port2, covmat, alpha.min=-0.1,
                                     alpha.max=2, nport=1000)
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

## smooth plots of ggplot,  just as  examples
library("tidyverse")
data()
BOD

ggplot(data = BOD,
       mapping = aes(x = Time,
                     y = demand))+
  geom_point(size = 5) +
  geom_line(colour =  "red")

ggplot(BOD, aes(Time, demand))+
  geom_point(size=3)+
  geom_line(colour="red")



ggplot(data = d, mapping = aes(x=xx, y=yy))+
  geom_line(colour="red")

ggplot(data = d, mapping = aes(x=xx, y=yy))+
  geom_point(size=0.1)

##
xx=net.ef$sd
yy=net.ef$nc
d=cbind(as.data.frame(xx),as.data.frame(yy))
#d=table(x,y)
class(BOD)
class(d)
dim(d)
as.data.frame(x)
d=data_frame(as.data.frame(x),as.data.frame(y))
as.data.frame(x)["x"]
class(as.data.frame(x))
cbind(as.data.frame(x),as.data.frame(y))

## this movie using ggplot can not work
xx=net.ef$sd
yy=net.ef$nc
d=cbind(as.data.frame(xx),as.data.frame(yy))
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in seq(1,-0.1,-0.02) ) {
  net.ef1<-network.efficient.frontier.moving(ec_port2, covmat, alpha=i)
  xx=net.ef1$sd
  yy=net.ef1$nc
  d1=cbind(as.data.frame(xx),as.data.frame(yy))
  
  p<-ggplot(NULL, mapping = aes(xx, yy))+
    geom_point(data=d,size=0.1)+
    geom_point(data=d1,colour="red")+
    theme_bw()
  p
}
dev.off()
animation <- image_animate(fig, fps = 5)
image_write(animation, paste0(getwd(), "/Efficient_frontiers_eigenvector_centrality_movie1.gif"))



p<-ggplot(NULL, mapping = aes(xx, yy))+
  geom_point(data=d,size=0.1)+
  geom_point(data=d1,colour="red")+
  theme_bw()

png(file = paste0("my2.png"), width=500, height=400, bg = "transparent")
print(p)
dev.off()


install.packages("gganimate","gapminder")
library("gganimate","gapminder")

d_move=data.frame()
for (i in seq(1,-0.1,-0.02)) {
  net.ef1<-network.efficient.frontier.moving(ec_port2, covmat, alpha=i)
  xx=net.ef1$sd
  yy=net.ef1$nc
  d_move=rbind(d1,cbind(as.data.frame(xx),as.data.frame(yy)))
}
dim(d1)
p<-ggplot(NULL, mapping = aes(xx, yy))+
  geom_point(data=d,size=0.1)+
  geom_point(data=d1,colour="red")+
  theme_bw()

# report error
p2<-p + transition_time(date=d1,yy)

p<-ggplot(d1, mapping = aes(xx, yy))+
  geom_point(data=d1,colour="red")+
  transition_time(xx)+
  theme_bw()

p2<-p + 
  geom_point(data=d,colour="red")




###  my1.png ####
xx=net.ef$sd
yy=net.ef$nc
d=cbind(as.data.frame(xx),as.data.frame(yy))
p<-ggplot(d, mapping = aes(xx, yy))+
  geom_point(size=0.1)+
  theme_bw()+
  xlim(0.00445,0.00672)+
  ylim(0.5,1.3)+
  xlab("Standard deviation")+
  ylab("Centrality")+
  theme(axis.line = element_line(), 
                  axis.text = element_text(size=20),
                  axis.title = element_text(size=30,face = "bold"),
                  panel.background = element_blank(), 
                  panel.border = element_blank(), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  legend.position = "none",
                  rect = element_rect(fill = "transparent"))
png("my1.png", width = 800, height = 640, bg = "transparent")
p
dev.off()

####  my4.png ####
xx=net.ef$sd
yy=net.ef$nc
d=cbind(as.data.frame(xx),as.data.frame(yy))
p<-ggplot(d[1:524,], mapping = aes(xx, yy))+
  geom_point(size=0.1)+
  theme_bw()+
  xlim(0.00445,0.00672)+
  ylim(0.5,1.3)+
  xlab("Standard deviation")+
  ylab("Centrality")+
  theme(axis.line = element_line(), 
        axis.text = element_text(size=20),
        axis.title = element_text(size=30,face = "bold"),
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        rect = element_rect(fill = "transparent"))
png("my4.png", width = 800, height = 640, bg = "transparent")
p
dev.off()

# xx=net.ef$sd
# yy=net.ef$nc
# class(xx)
# class(yy)
# d2=cbind(xx,yy)
# d2=as.matrix(cbind(xx,yy),rownames.force=NULL)
# rownames(d2) <- NULL
# d2<-unname(d2)
# d2
# class(d2)
# d2=sortrows(d2,k=2)

xx=net.ef$sd
yy=net.ef$nc
d=cbind(as.data.frame(xx),as.data.frame(yy))
d[order(d$yy, decreasing = TRUE), ]
# d1=d[1:500,]
# d1 %>%
#   ggplot(aes(x=xx, y=yy))+
#   geom_line(size=0.1)
# # make step_data
# step_data <-
#   d %>%
#   select(yy, xx) %>%
#   rename(yy_step = yy, xx_step = xx)
# 
# p <- step_data %>%
#   ggplot(aes(x=xx_step, y=yy_step ))+
#   geom_line(data=d1,size=0.1, mapping = aes(x=xx, y=yy),color="black")+
#   geom_point(size=1,color="red")

# make d_move
d_move=data_frame()
for (i in seq(1,0,-0.02)) {
  net.ef1<-network.efficient.frontier.moving(ec_port2, covmat, alpha=i)
  xx=net.ef1$sd
  yy=net.ef1$nc
  d_move=rbind(d1,cbind(as.data.frame(xx),as.data.frame(yy)))
}
step_data <-
  d_move %>%
  select(yy, xx) %>%
  rename(yy_step = yy, xx_step = xx)
step_data<-step_data[order(step_data$yy_step, decreasing = TRUE), ]
# cc_step=seq(1,0,-0.02)
cc_step=c(1:dim(d_move)[1])
# cc_sort = sort(cc_step,decreasing = TRUE)
# step_data<-cbind(step_data,cc_step)
p <- 
  step_data %>%
  ggplot(aes(x=xx_step, y=yy_step, width=1800, height=400 ))+
  geom_line(data=d[1:524,],size=1, mapping = aes(x=xx, y=yy),color="black")+
  geom_line(data=d[525:1000,],size=1, mapping = aes(x=xx, y=yy),color="black")+
  geom_point(size=8,color="red",pch=19)+
  xlim(0.00445,0.00672)+
  ylim(0.5,1.3)+
  xlab("Standard deviation")+
  ylab("Centrality")+
  theme(axis.line = element_line(), 
        axis.text = element_text(size=20),
        axis.title = element_text(size=30,face = "bold"),
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        rect = element_rect(fill = "transparent"))+
  transition_time(cc_step)
p_gif<-animate(p,
               nframes = nrow(step_data),
               fps = 50,
               # bg = "transparent",
                renderer = magick_renderer())
anim_save("Efficient_frontiers_eigenvector_centrality_movie.gif",p_gif)

# ggsave(Efficient_frontiers_eigenvector_centrality_movie.gif)










fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in seq(1,-0.1,-0.02) ) {
  plot(net.ef$sd,net.ef$nc,xlab="Standard deviation",ylab="Portfolio centrality",cex=0.1)
  #points(estd,  ec_port2, pch = 16)
  net.ef1<-network.efficient.frontier.moving(ec_port2, covmat, alpha=i)
  points(net.ef1$sd,  net.ef1$nc, pch = 16,col="red")
}
dev.off()
animation <- image_animate(fig, fps = 5)
image_write(animation, paste0(getwd(), "/Efficient_frontiers_eigenvector_centrality_movie.gif"))

x <- xx
y <- yy
qplot(x,y, geom='smooth', span =0.5)
lo <- loess(y~x)
plot(x,y)
lines(predict(lo), col='red', lwd=2)