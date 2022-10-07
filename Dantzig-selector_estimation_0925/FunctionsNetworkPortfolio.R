
p_turnover  = function(weights){
  out<-vector()
  for (i in 2:dim(weights)[1]) {
    # print(i)
    out[i-1]=sum(abs(weights[i,]-weights[i-1,]))/sum((abs(weights[i,])+abs(weights[i-1,]))>0)
  }
  # print(1)
  out = mean(out)
  return(out)
}

pturnoverDN  = function(weights, rets, freq){
  results = Return.portfolio(R = rets,   
                             weights = weights, 
                             rebalance_on = freq, verbose = T)
  bop = results$BOP.Weight #beginning of period weights
  bop
  eop = results$EOP.Weight #end of period weights
  eop
  f = abs(bop - eop)
  out = sum(f)*(1/(length(ep) - 1)) #  
  return(out)
}

mad_ew = function(x){
  a = abs(x - 1/ncol(ret))
  return(a)
}
trans_cost = function(weights, rets, freq, c){
  results = Return.portfolio(R = rets,   
                             weights = weights, 
                             rebalance_on = freq, verbose = T)
  
  bop = results$BOP.Weight #beginning of period weights
  bop
  eop = results$EOP.Weight #end of period weights
  eop
  out = c*row_sums(abs(eop - bop))
  return(out)
} 

calculatePerformanceMeasures = function(start,end){
  collNumbers = vector()
  collNumbers_tc = vector()
  collres = xts()
  weightsNumbers = vector()
  for (stratloop in 1:length(strats)){
    Rb = as.xts(rescoll[,  which(strats %in% c("EqualWeight","EW"))], 
                order.by = as.Date(rownames(rescoll)))
    portfolioret_net = na.omit(rescoll[,stratloop])
    strat_weights = weightscoll[[stratloop]] 
    strat_weights[is.nan(strat_weights)] = 0.0
    portfolioret_net_xts = as.xts(as.matrix(na.omit(rescoll[,stratloop])), 
                                  order.by = as.Date(na.omit(rownames(rescoll))))
    portfolioEquity_net = 1 + cumsum(portfolioret_net)
    cumWealth = tail(portfolioEquity_net, 1)
    firstsignal = start
    rettt = portfolioret_net[firstsignal:end]
    rettt_xts = portfolioret_net_xts[firstsignal:end]
    ret_data = rets_log
    stock_rets = ret_data[firstsignal:end]
    tc = trans_cost(strat_weights[firstsignal:end,], stock_rets, freq, c = transi)
    portfolioret_net_tc = portfolioret_net[firstsignal:(end - 1)] - tc
    portfolioEquity_net_tc = 1 + cumsum(portfolioret_net_tc)
    cumWealth_tc = tail(portfolioEquity_net_tc, 1)
    T = (commonDate[end] - commonDate[firstsignal])/365
    Return.ann = (portfolioEquity_net[end]/portfolioEquity_net[firstsignal - 1])^(1/T) - 1
    Return.ann_tc = (tail(portfolioEquity_net_tc, 1)/portfolioEquity_net_tc[firstsignal - 1])^(1/T) - 1
    Vola.ann = sd(rettt)*sqrt(252);
    Vola.ann_tc = sd(portfolioret_net_tc)*sqrt(252);
    Sharpe.ann = Return.ann/Vola.ann
    Sharpe.ann_tc = Return.ann_tc/Vola.ann_tc
    target_turnover = vector();
    for (i in 2:dim(strat_weights)[1]) {
      target_turnover[i] = sum(abs(matrix(strat_weights[i, ]) - matrix(strat_weights[i - 1,])))/dim(strat_weights)[2] 
    }
    Turnover = mean(na.omit(target_turnover))
    value = portfolioEquity_net 
    ma = unlist(lapply(c(2:length(value)),function(x) max(value[1:x])))
    dddisc = value[-1]/ma - 1
    datums = commonDateR[firstsignal:end]
    num = as.numeric(tail(datums,1)-datums[1])
    PR = as.vector(PainRatio(rettt_xts))
    TurnoverDM  = pturnoverDN(strat_weights[firstsignal:end,], stock_rets, freq)
    Return_annual = as.vector(Return.annualized(rettt_xts, geometric = F))
    AverageDrawdown = as.numeric(AverageDrawdown(rettt_xts))
    Sharpe =  as.numeric(SharpeRatio(rettt_xts))[1]
    StdDev.annualized = as.numeric(StdDev.annualized(rettt_xts))
    collNumbers = cbind(collNumbers, as.vector(c(cumWealth, 100*Sharpe.ann,  
                                                 Turnover, TurnoverDM)))#, 
    collNumbers_tc = cbind(collNumbers_tc, as.vector(c(cumWealth_tc,  
                                                       100*Sharpe.ann_tc, Turnover, TurnoverDM)))
    weightcoll = as.data.frame(strat_weights[first_signal:end])
    weightsNumbers = cbind(weightsNumbers, as.vector(c((mean(apply(weightcoll, 1, min))), mean(apply(weightcoll, 1, max)), 
      mean(apply(weightcoll, 1, sd)), mean(apply(weightcoll, 1, mad_ew)), mean(diff(apply(weightcoll, 1, range))))))
    collNumbers = round(collNumbers,4)
    weightsNumbers = round(weightsNumbers,4)
    res = as.xts(portfolioret_net[first_signal:end],order.by=index(ret)[first_signal:end])
    collres = cbind(collres,res)
    first(index(res))
    last(index(res))
    
  }
  return(list(collNumbers,collres,weightsNumbers, collNumbers_tc))
}

#Functions tangency.portfolio, globalMin.portfolio and efficient.frontier are copied 
#from the package "IntroCompFinR: Introduction to Computational Finance in R" @author Eric Zivot
tangency.portfolio =
  function(er,cov.mat,risk.free, shorts=TRUE)
  {
    call = match.call()
    
    #
    # check for valid inputs
    #
    asset.names = names(er)
    if(risk.free < 0)
      stop("Risk-free rate must be positive")
    er = as.vector(er)
    cov.mat = as.matrix(cov.mat)
    N = length(er)
    if(N != nrow(cov.mat))
      stop("invalid inputs")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    # remark: could use generalized inverse if cov.mat is positive semi-definite
    
    #
    # compute global minimum variance portfolio
    #
    gmin.port = globalMin.portfolio(er, cov.mat, shorts=shorts)
    if(gmin.port$er < risk.free)
      stop("Risk-free rate greater than avg return on global minimum variance portfolio")
    
    # 
    # compute tangency portfolio
    #
    if(shorts==TRUE){
      cov.mat.inv = solve(cov.mat)
      w.t = cov.mat.inv %*% (er - risk.free) # tangency portfolio
      w.t = as.vector(w.t/sum(w.t))          # normalize weights
    } else if(shorts==FALSE){
      Dmat = 2*cov.mat
      dvec = rep.int(0, N)
      er.excess = er - risk.free
      Amat = cbind(er.excess, diag(1,N))
      bvec = c(1, rep(0,N))
      result = quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
      w.t = round(result$solution/sum(result$solution), 6)
    } else {
      stop("Shorts needs to be logical. For no-shorts, shorts=FALSE.")
    }
    
    names(w.t) = asset.names
    er.t = crossprod(w.t,er)
    sd.t = sqrt(t(w.t) %*% cov.mat %*% w.t)
    tan.port = list("call" = call,
                     "er" = as.vector(er.t),
                     "sd" = as.vector(sd.t),
                     "weights" = w.t)
    class(tan.port) = "portfolio"
    return(tan.port)
  }



efficient.portfolio =
  function(er, cov.mat, target.return, shorts=TRUE)
  {
    call = match.call()
    
    #
    # check for valid inputs
    #
    asset.names = names(er)
    er = as.vector(er) # assign names if none exist
    N = length(er)
    cov.mat = as.matrix(cov.mat)
    if(N != nrow(cov.mat))
      stop("invalid inputs")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    # remark: could use generalized inverse if cov.mat is positive semidefinite
    
    #
    # compute efficient portfolio
    #
    if(shorts==TRUE){
      ones = rep(1, N)
      top = cbind(2*cov.mat, er, ones)
      bot = cbind(rbind(er, ones), matrix(0,2,2))
      A = rbind(top, bot)
      b.target = as.matrix(c(rep(0, N), target.return, 1))
      x = solve(A, b.target)
      w = x[1:N]
    } else if(shorts==FALSE){
      Dmat = 2*cov.mat
      dvec = rep.int(0, N)
      Amat = cbind(rep(1,N), er, diag(1,N))
      bvec = c(1, target.return, rep(0,N))
      result = quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=2)
      w = round(result$solution, 6)
    } else {
      stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
    }
    
    #
    # compute portfolio expected returns and variance
    #
    names(w) = asset.names
    er.port = crossprod(er,w)
    sd.port = sqrt(w %*% cov.mat %*% w)
    ans = list("call" = call,
                "er" = as.vector(er.port),
                "sd" = as.vector(sd.port),
                "weights" = w) 
    class(ans) = "portfolio"
    return(ans)
  }

efficient.frontier =
  function(er, cov.mat, nport=20, alpha.min=-0.5, alpha.max=1.5, shorts=TRUE)
  {
    call = match.call()
    
    #
    # check for valid inputs
    #
    asset.names = names(er)
    er = as.vector(er)
    N = length(er)
    cov.mat = as.matrix(cov.mat)
    if(N != nrow(cov.mat))
      stop("invalid inputs")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    
    #
    # create portfolio names
    #
    port.names = rep("port",nport)
    ns = seq(1,nport)
    port.names = paste(port.names,ns)
    
    #
    # compute global minimum variance portfolio
    #
    cov.mat.inv = solve(cov.mat)
    one.vec = rep(1, N)
    port.gmin = globalMin.portfolio(er, cov.mat, shorts)
    w.gmin = port.gmin$weights
    
    if(shorts==TRUE){
      # compute efficient frontier as convex combinations of two efficient portfolios
      # 1st efficient port: global min var portfolio
      # 2nd efficient port: min var port with ER = max of ER for all assets
      er.max = max(er)
      port.max = efficient.portfolio(er,cov.mat,er.max)
      w.max = port.max$weights    
      a = seq(from=alpha.min,to=alpha.max,length=nport) # convex combinations
      we.mat = a %o% w.gmin + (1-a) %o% w.max	         # rows are efficient portfolios
      er.e = we.mat %*% er							                 # expected returns of efficient portfolios
      er.e = as.vector(er.e)
    } else if(shorts==FALSE){
      we.mat = matrix(0, nrow=nport, ncol=N)
      we.mat[1,] = w.gmin
      we.mat[nport, which.max(er)] = 1
      er.e = as.vector(seq(from=port.gmin$er, to=max(er), length=nport))
      for(i in 2:(nport-1)) 
        we.mat[i,] = efficient.portfolio(er, cov.mat, er.e[i], shorts)$weights
    } else {
      stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
    }
    
    names(er.e) = port.names
    cov.e = we.mat %*% cov.mat %*% t(we.mat) # cov mat of efficient portfolios
    sd.e = sqrt(diag(cov.e))					        # std devs of efficient portfolios
    sd.e = as.vector(sd.e)
    names(sd.e) = port.names
    dimnames(we.mat) = list(port.names,asset.names)
    
    # 
    # summarize results
    #
    ans = list("call" = call,
                "er" = er.e,
                "sd" = sd.e,
                "weights" = we.mat)
    class(ans) = "Markowitz"
    ans
  }

# function to calculate recurrence vector
RP.P = 
  function(x,epsilon)
  {
    Recurrence_Matrix = matrix(0, length(x), length(x))
    for (i in 1:length(x)) {
      for (j in 1:length(x)) {
        if(epsilon-abs(x[i]-x[j])>=0) Recurrence_Matrix[i,j]=1
        else Recurrence_Matrix[i,j]=0
      }
    }
    P=matrix(0,length(x)-1,1)
    for (tau in 1:(length(x)-1)) {
      for (j in 1:(length(x)-tau)) {
        P[tau,1]=P[tau,1]+Recurrence_Matrix[j,j+tau]
        }
      P[tau,1]=P[tau,1]/(length(x)-tau)
      }
    P=P-mean(P)
    return(P)
  }

# function to calculate recurrence matrix
RP.Mtx = 
  function(x,epsilon)
  {
    Recurrence_Matrix = matrix(0, length(x), length(x))
    for (i in 1:length(x)) {
      for (j in 1:length(x)) {
        if(epsilon-abs(x[i]-x[j])>=0) Recurrence_Matrix[i,j]=1
        else Recurrence_Matrix[i,j]=0
      }
    }
    return(Recurrence_Matrix)
  }

# Construct network of portfolio based on correlation matrix
network.portfolio = 
  function(returnstd)
  {
    # correlation matrix
    Cormat<-cor(returnstd)                        # correlation matrix
    colnames(Cormat)<-colnames(returnstd)
    rownames(Cormat)<-colnames(returnstd)
    # distance matrix
    # Dist_mat<-sqrt(2-2*Covmat)                    # distance matrix
    # Dist_mat<-sqrt(1-Covmat)                    # distance matrix
    Dist_mat<-1-Cormat                    # distance matrix
    # Dist_mat<-sqrt(2*Covmat+2)                    # distance matrix
    Dist_mat<-as.matrix(Dist_mat)
    Dist_mat[is.nan(Dist_mat)]<-0 
    colnames(Dist_mat)<-colnames(returnstd)
    rownames(Dist_mat)<-colnames(returnstd)
    # construct network
    network=graph_from_adjacency_matrix(Dist_mat,weighted=T,
                                        mode="undirected", diag=F)  
    Edgelist_network<-get.edgelist(network)                           # edges of network
    weight_network<-E(network)$weight                                 # weight of network
    A<-cbind(Edgelist_network,weight_network)
    A<-as.matrix(A)
    links2_network<-as.data.frame(A)                                # links of network
    colnames(links2_network)<-c("from","to","weight")
    net_port<- graph_from_data_frame(d=links2_network, directed=F)  # net of whole data
    return(net_port)
  }

# Construct network of portfolio based on correlation matrix
# the difference beween network.portfolio is that this directly use correlation matrix
network.correlation = 
  function(returnstd)
  {
    # correlation matrix
    Cormat<-cor(returnstd)                        # correlation matrix
    colnames(Cormat)<-colnames(returnstd)
    rownames(Cormat)<-colnames(returnstd)
    
    # distance matrix
    # Dist_mat<-sqrt(2-2*Cormat)                    # distance matrix
    # Dist_mat<-sqrt(1-Cormat)                    # distance matrix
    # Dist_mat<-1-Cormat                    # distance matrix
    # Dist_mat<--Cormat                     # distance matrix
    Dist_mat<-Cormat-diag(1,dim(Cormat)[1])                    # distance matrix
    # Dist_mat<-1+Cormat                    # distance matrix
    # Dist_mat<-sqrt(2*Covmat+2)                    # distance matrix
    Dist_mat<-as.matrix(Dist_mat)
    Dist_mat[is.nan(Dist_mat)]<-0
    colnames(Dist_mat)<-colnames(returnstd)
    rownames(Dist_mat)<-colnames(returnstd)
    # construct network
    network=graph_from_adjacency_matrix(Dist_mat,weighted=T,
                                        mode="undirected", diag=F)
    Edgelist_network<-get.edgelist(network)                           # edges of network
    weight_network<-E(network)$weight                                 # weight of network
    A<-cbind(Edgelist_network,weight_network)
    A<-as.matrix(A)
    links2_network<-as.data.frame(A)                                # links of network
    colnames(links2_network)<-c("from","to","weight")
    net_port<- graph_from_data_frame(d=links2_network, directed=F)  # net of whole data
    return(net_port)
    
    # network=graph_from_adjacency_matrix(Cormat,weighted=T,
    #                                     mode="undirected", diag=F)  
    # Edgelist_network<-get.edgelist(network)                           # edges of network
    # return(network)
  }

# Construct network of portfolio based on correlation matrix
network.abs = 
  function(returnstd)
  {
    # correlation matrix
    CorMat<-cor(returnstd)                        # correlation matrix
    colnames(CorMat)<-colnames(returnstd)
    rownames(CorMat)<-colnames(returnstd)
    # construct network
    network=graph_from_adjacency_matrix(abs(CorMat)-diag(1,ncol=ncol(CorMat),nrow = nrow(CorMat)),
                                        weighted=T,
                                        mode="undirected", diag=F)  
    Edgelist_network<-get.edgelist(network)                           # edges of network
    weight_network<-E(network)$weight                                 # weight of network
    A<-cbind(Edgelist_network,weight_network)
    A<-as.matrix(A)
    links2_network<-as.data.frame(A)                                # links of network
    colnames(links2_network)<-c("from","to","weight")
    net_port<- graph_from_data_frame(d=links2_network, directed=F)  # net of whole data
    return(net_port)
  }
# 
# network.decomposition = 
#   function(returnstd)
#   {
#     # correlation matrix
#     CorMat<-cor(returnstd)                        # correlation matrix
#     colnames(CorMat)<-colnames(returnstd)
#     rownames(CorMat)<-colnames(returnstd)
#     # decomposition into positive and negative matrix
#     Lambda<-CorMat-diag(1,ncol=ncol(CorMat),nrow = nrow(CorMat))
#     Lambda1<-matrix(0,ncol=ncol(CorMat),nrow = nrow(CorMat))
#     Lambda2<-matrix(0,ncol=ncol(CorMat),nrow = nrow(CorMat))
#     for (i in seq(1,nrow(CorMat))) {
#       for (j in seq(1,nrow(CorMat))) {
#         if (CorMat[i,j]>0){Lambda1[i,j]<-CorMat[i,j]}
#         else if (CorMat[i,j]<0){Lambda2[i,j]<-CorMat[i,j]}
#       }
#     }
#     # construct network
#     network.positive=graph_from_adjacency_matrix(Lambda1,
#                                         weighted=T,
#                                         mode="undirected", diag=F)  
#     network.negative=graph_from_adjacency_matrix(Lambda2,
#                                                  weighted=T,
#                                                  mode="undirected", diag=F)  
#     # Edgelist_network<-get.edgelist(network)                           # edges of network
#     # weight_network<-E(network)$weight                                 # weight of network
#     # A<-cbind(Edgelist_network,weight_network)
#     # A<-as.matrix(A)
#     # links2_network<-as.data.frame(A)                                # links of network
#     # colnames(links2_network)<-c("from","to","weight")
#     # net_port<- graph_from_data_frame(d=links2_network, directed=F)  # net of whole data
#     return(network.positive)
#     return(network.negative)
#   }

# Construct network of portfolio based on correlation matrix 
# decompose correlation matrix into an indicator matrix and a matrix Lambda
# Lambda decomposed into a positive matrix and negative matrix
network.posi.naga = 
  function(returnstd)
  {
    # correlation matrix
    CorMat<-cor(returnstd)                        # correlation matrix
    colnames(CorMat)<-colnames(returnstd)
    rownames(CorMat)<-colnames(returnstd)
    # decompose coefficient matrix
    Lambda<- CorMat-diag(1,nrow=nrow(CorMat),ncol = ncol(CorMat))
    Lambda1<- Lambda
    Lambda1[Lambda1<0]<- 0
    Lambda2<- -Lambda
    Lambda2[Lambda2<0]<- 0
    # construct positive coefficient network
    network_positive=graph_from_adjacency_matrix(Lambda1,weighted=T,
                                        mode="undirected", diag=F)  
    Edgelist_network<-get.edgelist(network_positive)                           # edges of network
    weight_network<-E(network_positive)$weight                                 # weight of network
    A<-cbind(Edgelist_network,weight_network)
    A<-as.matrix(A)
    links2_network<-as.data.frame(A)                                # links of network
    colnames(links2_network)<-c("from","to","weight")
    network_positive<- graph_from_data_frame(d=links2_network, directed=F)  # net of positive coefficient matrix
    # construct negative coefficient network
    network_negative=graph_from_adjacency_matrix(Lambda2,weighted=T,
                                                 mode="undirected", diag=F)  
    Edgelist_network<-get.edgelist(network_positive)                           # edges of network
    weight_network<-E(network_positive)$weight                                 # weight of network
    A<-cbind(Edgelist_network,weight_network)
    A<-as.matrix(A)
    links2_network<-as.data.frame(A)                                # links of network
    colnames(links2_network)<-c("from","to","weight")
    network_negative<- graph_from_data_frame(d=links2_network, directed=F)  # net of negative coefficient matrix
    # output
    network_coefficient<-list("positive"=network_positive,"negative"=network_negative)
    return(network_coefficient)
  }

# Construct Minimum spanning tree of portfolio based on covariance
MST.portfolio = 
  function(returnstd)
  {
    # correlation matrix
    Covmat<-cor(returnstd)                        # correlation matrix
    colnames(Covmat)<-colnames(returnstd)
    rownames(Covmat)<-colnames(returnstd)
    # distance matrix
    Dist_mat<-sqrt(2-2*Covmat)                    # distance matrix
    Dist_mat<-as.matrix(Dist_mat)
    Dist_mat[is.nan(Dist_mat)]<-0 
    colnames(Dist_mat)<-colnames(returnstd)
    rownames(Dist_mat)<-colnames(returnstd)
    # construct network
    network=graph_from_adjacency_matrix(Dist_mat,weighted=T,
                                              mode="undirected", diag=F)  
    Edgelist_network<-get.edgelist(network)                           # edges of network
    weight_network<-E(network)$weight                                 # weight of network
    A<-cbind(Edgelist_network,weight_network)
    A<-as.matrix(A)
    links2_network<-as.data.frame(A)                                # links of network
    colnames(links2_network)<-c("from","to","weight")
    net_port<- graph_from_data_frame(d=links2_network, directed=F)  # net of whole data
    mst_port<- minimum.spanning.tree(net_port)                      # minimum spanning tree
    return(mst_port)
  }

network.globalMin.portfolio =
  function(nc, cov.mat, shorts=TRUE)
  {
    call = match.call()
    
    #
    # check for valid inputs
    #
    asset.names = names(nc)
    nc = as.vector(nc) # assign names if none exist
    cov.mat = as.matrix(cov.mat)
    N = length(nc)
    if(N != nrow(cov.mat))
      stop("invalid inputs")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    # remark: could use generalized inverse if cov.mat is positive semi-definite
    
    #
    # compute global minimum portfolio
    #
    if(shorts==TRUE){
      cov.mat.inv = solve(cov.mat)
      one.vec = rep(1,N)
      w.gmin = rowSums(cov.mat.inv) / sum(cov.mat.inv)
      w.gmin = as.vector(w.gmin)
    } else if(shorts==FALSE){
      Dmat = 2*cov.mat
      dvec = rep.int(0, N)
      Amat = cbind(rep(1,N), diag(1,N))
      bvec = c(1, rep(0,N))
      result = quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
      w.gmin = round(result$solution, 6)
    } else {
      stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
    }
    
    # if(shorts==TRUE){
    #   cov.mat.inv = solve(cov.mat)
    #   one.vec = rep(1,N)
    #   w.gmin = rowSums(cov.mat.inv) / sum(cov.mat.inv)
    #   w.gmin = as.vector(w.gmin)
    # } else if(shorts==FALSE){
    #   Dmat = 2*cov.mat
    #   dvec = rep.int(0, N)
    #   Amat = cbind(rep(1,N), diag(1,N))
    #   bvec = c(1, rep(0,N))
    #   result = quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
    #   w.gmin = round(result$solution, 6)
    # } else {
    #   stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
    # }
    
    names(w.gmin) = asset.names
    nc.gmin = crossprod(w.gmin,nc)
    sd.gmin = sqrt(t(w.gmin) %*% cov.mat %*% w.gmin)
    gmin.port = list("call" = call,
                     "nc" = as.vector(nc.gmin),
                     "sd" = as.vector(sd.gmin),
                     "weights" = w.gmin)
    class(gmin.port) = "portfolio"
    gmin.port
  }

network.efficient.portfolio =
  function(nc, cov.mat, target.nc, shorts=TRUE)
  {
    call = match.call()
    
    #
    # check for valid inputs
    #
    asset.names = names(nc)
    nc = as.vector(nc) # assign names if none exist
    N = length(nc)
    cov.mat = as.matrix(cov.mat)
    if(N != nrow(cov.mat))
      stop("invalid inputs")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    # remark: could use generalized inverse if cov.mat is positive semidefinite
    
    #
    # compute efficient portfolio
    #
    if(shorts==TRUE){
      # ones = rep(1, N)
      # top = cbind(2*cov.mat, nc, ones)
      # bot = cbind(rbind(nc, ones), matrix(0,2,2))
      # A = rbind(top, bot)
      # b.target = as.matrix(c(rep(0, N), target.nc, 1))
      # x = solve(A, b.target)
      # w = x[1:N]
      Dmat = 2*cov.mat
      dvec = rep.int(0, N)
      Amat = cbind(rep(1,N), -nc)
      bvec = cbind(1, -target.nc)
      result = quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
      w = round(result$solution, 6)
    } else if(shorts==FALSE){
      Dmat = 2*cov.mat
      dvec = rep.int(0, N)
      Amat = cbind(rep(1,N), -nc, diag(1,N))
      bvec = c(1, -target.nc, rep(0,N))
      result = quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
      w = round(result$solution, 6)
    } else {
      stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
    }
    
    #
    # compute portfolio expected returns and variance
    #
    names(w) = asset.names
    nc.port = crossprod(nc,w)
    sd.port = sqrt(w %*% cov.mat %*% w)
    ans = list("call" = call,
               "nc" = as.vector(nc.port),
               "sd" = as.vector(sd.port),
               "weights" = w) 
    class(ans) = "portfolio"
    return(ans)
  }

network.2constraint.portfolio =
  function(nc,er, cov.mat, target.nc,target.er, shorts=TRUE)
  {
    call = match.call()
    
    #
    # check for valid inputs
    #
    asset.names = names(nc)
    nc = as.vector(nc) # assign names if none exist
    er = as.vector(er) # assign names if none exist
    N = length(nc)
    cov.mat = as.matrix(cov.mat)
    if(N != nrow(cov.mat))
      stop("invalid inputs")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    # remark: could use generalized inverse if cov.mat is positive semidefinite
    
    #
    # compute efficient portfolio
    #
    if(shorts==TRUE){
      Dmat = 2*cov.mat
      dvec = rep.int(0, N)
      Amat = cbind(rep(1,N), -nc, er)
      bvec = cbind(1, -target.nc, target.er)
      result = quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
      w = round(result$solution, 6)
    } else if(shorts==FALSE){
      Dmat = 2*cov.mat
      dvec = rep.int(0, N)
      Amat = cbind(rep(1,N), -nc, diag(1,N), er)
      bvec = c(1, -target.nc, rep(0,N), target.er)
      result = quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
      w = round(result$solution, 6)
    } else {
      stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
    }
    
    #
    # compute portfolio expected returns and variance
    #
    names(w) = asset.names
    nc.port = crossprod(nc,w)
    er.port = crossprod(er,w)
    sd.port = sqrt(w %*% cov.mat %*% w)
    ans = list("call" = call,
               "nc" = as.vector(nc.port),
               "er" = as.vector(er.port),
               "sd" = as.vector(sd.port),
               "weights" = w) 
    class(ans) = "portfolio"
    return(ans)
  }

network.efficient.frontier =
  function(nc, cov.mat, nport=20, alpha.min=-0.5, alpha.max=1.5, shorts=TRUE)
  {
    call = match.call()
    
    #
    # check for valid inputs
    #
    asset.names = names(nc)
    er = as.vector(nc)
    N = length(nc)
    cov.mat = as.matrix(cov.mat)
    if(N != nrow(cov.mat))
      stop("invalid inputs")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    
    #
    # create portfolio names
    #
    port.names = rep("port",nport)
    ns = seq(1,nport)
    port.names = paste(port.names,ns)
    
    #
    # compute global minimum variance portfolio
    #
    cov.mat.inv = solve(cov.mat)
    one.vec = rep(1, N)
    port.gmin = network.globalMin.portfolio(nc, cov.mat, shorts)
    w.gmin = port.gmin$weights
    
    if(shorts==TRUE){
      # compute efficient frontier as convex combinations of two efficient portfolios
      # 1st efficient port: global min var portfolio
      # 2nd efficient port: min var port with ER = max of ER for all assets
      nc.min = min(nc)
      port.min = network.efficient.portfolio(nc,cov.mat,nc.min, shorts)
      w.min = port.min$weights    
      a = seq(from=alpha.min,to=alpha.max,length=nport) # convex combinations
      we.mat = a %o% w.gmin + (1-a) %o% w.min	         # rows are efficient portfolios
      nc.e = we.mat %*% nc							                 # expected returns of efficient portfolios
      nc.e = as.vector(nc.e)
    } else if(shorts==FALSE){
      we.mat = matrix(0, nrow=nport, ncol=N)
      we.mat[1,] = w.gmin
      we.mat[nport, which.min(nc)] = 1
      nc.e = as.vector(seq(from=port.gmin$nc, to=min(nc), length=nport))
      for(i in 2:(nport-1)) 
        we.mat[i,] = efficient.portfolio(nc, cov.mat, nc.e[i], shorts)$weights
    } else {
      stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
    }
    
    names(nc.e) = port.names
    cov.e = we.mat %*% cov.mat %*% t(we.mat) # cov mat of efficient portfolios
    sd.e = sqrt(diag(cov.e))					        # std devs of efficient portfolios
    sd.e = as.vector(sd.e)
    names(sd.e) = port.names
    dimnames(we.mat) = list(port.names,asset.names)
    
    # 
    # summarize results
    #
    ans = list("call" = call,
               "nc" = nc.e,
               "sd" = sd.e,
               "weights" = we.mat)
    class(ans) = "Markowitz"
    ans
  }

##### portfolio using Dantzig type selector with 1 constraints #####
linfun1=function(Sn,b,lambda)
{
  #equivalent to solving   min 1'u
  #such that		u-x>=0
  #						u+x>=0
  #						-hatS x>=-(lambda 1+b)
  #						hatS x>=-lambda 1+b
  #						x^T1=1
  a=rep(0,2*p)
  a[c(1,1+p)]=1
  A0=toeplitz(a)
  A0[upper.tri(A0)]=-A0[upper.tri(A0)]
  A1=cbind(matrix(0,p,p),-Sn)
  A2=-A1
  A=rbind(A0,A1,A2)
  rhs=c(rep(0,2*p),c(-b,b)-lambda*rep(1,2*p))
  C=rep(c(1,0),c(p,p))
  #	EE=rep(c(0,1),c(p,p))
  #	FF=1
  solvetheta=linp(#E=EE,F=FF,
    G=A,H=rhs,Cost=C,ispos=FALSE)$X[(p+1):(2*p)]
  # return(solvetheta)
  return(solvetheta)
}
##### portfolio using Dantzig type selector with 2 constraints #####
linfun2=function(Sn,b1,b2,lambda)
{
  #equivalent to solving   min 1'u1 + 1'u1
  #such that		u1-x1>=0
  #						  u1+x1>=0
  #						  u2-x2>=0
  #						  u2+x2>=0
  #				 	 -hatS x1>=-(lambda 1+b1)
  #				    hatS x1>=-lambda 1+b1
  #				 	 -hatS x2>=-(lambda 1+b2)
  #				    hatS x2>=-lambda 1+b2
  #						x^T1=1 (I do not think so)
  a=rep(0,4*p)
  a[c(1,1+2*p)]=1
  A0=toeplitz(a)
  A0[upper.tri(A0)]=-A0[upper.tri(A0)]
  A1=cbind(matrix(0,p,2*p),-Sn,matrix(0,p,p))
  A2=-A1
  A3=cbind(matrix(0,p,3*p),-Sn)
  A4=-A1
  A=rbind(A0,A1,A2,A3,A4)
  rhs=c(rep(0,4*p),c(-b1,b1,-b2,b2)-lambda*rep(1,4*p))
  C=rep(c(1,0),c(2*p,2*p))
  #	EE=rep(c(0,1),c(p,p))
  #	FF=1
  theta=linp(G=A,H=rhs,Cost=C,ispos=FALSE)
  solvetheta=list("theta1"=theta$X[(2*p+1):(3*p)],
                  "theta2"=theta$X[(3*p+1):(4*p)])
  # return(solvetheta)
  return(solvetheta)
}
