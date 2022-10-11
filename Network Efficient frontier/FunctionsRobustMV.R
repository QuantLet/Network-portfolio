
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


globalMin.portfolio =
  function(er, cov.mat, shorts=TRUE)
  {
    call = match.call()
    
    #
    # check for valid inputs
    #
    asset.names = names(er)
    er = as.vector(er) # assign names if none exist
    cov.mat = as.matrix(cov.mat)
    N = length(er)
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
    
    names(w.gmin) = asset.names
    er.gmin = crossprod(w.gmin,er)
    sd.gmin = sqrt(t(w.gmin) %*% cov.mat %*% w.gmin)
    gmin.port = list("call" = call,
                      "er" = as.vector(er.gmin),
                      "sd" = as.vector(sd.gmin),
                      "weights" = w.gmin)
    class(gmin.port) = "portfolio"
    gmin.port
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


