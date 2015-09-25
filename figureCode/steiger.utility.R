require(coda) ## For HPD

max.likelihood.omega2 = Vectorize(function(Fstat, df1, df2){
  optimize(likelihood2.omega2, interval = c(0,1), Fstat = Fstat, df1 = df1, df2 = df2, log = TRUE, maximum = TRUE)$maximum
},"Fstat")

likelihood2.omega2 = Vectorize(function(omega2, Fstat, df1, df2, log = FALSE, norm = 0){
  Ntot = df1+df2+1
  lambda.sq = Ntot * omega2 / (1 - omega2)
  likelihood2(lambda.sq, Fstat, df1, df2, log, norm)
},"omega2")


likelihood.omega2 = function(omega2, Fstat, df1, df2, log = FALSE){
  norm = log(plike.norm.omega2(Fstat, df1, df2))
  likelihood2.omega2(omega2, Fstat, df1, df2, log, norm)
}

plike2.omega2 = Vectorize(function(omega2, Fstat, df1, df2){
  integrate(likelihood2.omega2, 0, omega2, Fstat = Fstat, df1 = df1, df2 = df2)[[1]]
},"omega2")

plike.omega2 = function(omega2, Fstat, df1, df2) {
  if(omega2<=0) return(0)
  if(omega2>=1) return(1)
  norm = integrate(likelihood2.omega2, 0, 1, Fstat = Fstat, df1 = df1, df2 = df2)[[1]]
  plike2.omega2(omega2, Fstat, df1, df2) / plike.norm.omega2(Fstat, df1, df2)
}

plike.norm.omega2 = function(Fstat, df1, df2){
  integrate(likelihood2.omega2, 0, 1, Fstat = Fstat, df1 = df1, df2 = df2)[[1]]
}

qlike2.omega2 = Vectorize(function(targetp, Fstat, df1, df2, plikeC = 1){
  if(targetp > 1 | targetp < 0) return(NA)
  if(targetp == 1) return(1)
  if(targetp == 0) return(0)
  optimize(function(omega2) (plike2.omega2(omega2, Fstat, df1, df2)/plikeC - targetp)^2,interval=c(0,1))$minimum
},"targetp")

qlike.omega2 = function(targetp, Fstat, df1, df2){
  plikeC = plike.norm.omega2(Fstat, df1, df2)
  qlike2.omega2(targetp, Fstat, df1, df2, plikeC)
}

likelihoodCI.omega2 = function(Fstat,df1,df2,conf.level=.95){
  alpha = 1-conf.level
  qlike.omega2(c(alpha/2,1-alpha/2),Fstat, df1, df2) 
}

like.sample.omega2.icdf = function(Fstat, df1, df2, iterations = 10000){
  plikeC = plike.norm.omega2(Fstat, df1, df2)
  mcmc(qlike2.omega2(runif(iterations), Fstat, df1, df2, plikeC))
}


like.sample.omega2.mh = function(Fstat, df1, df2, iterations = 10000, nFitPoints = 10, a.beta = 1, b.beta = 1){
  fitPoints = (1:nFitPoints) / (nFitPoints + 1)
  likeVals = qlike.omega2(fitPoints, Fstat, df1,df2 )
  startPar = c(0,0)
  optFunc = function(par, likeVals, fitPoints)
    sum(qbeta(fitPoints,exp(par[1]),exp(par[2])) - likeVals)^2
  sol = optim(startPar,optFunc, likeVals = likeVals, fitPoints = fitPoints)
  pars = exp(sol$par)
  samples = 1:iterations
  samples[1] = qlike.omega2(.5, Fstat, df1, df1)
  for(m in 2:iterations){
    omega2.cand = rbeta(1, pars[1], pars[2])
    b = likelihood2.omega2(omega2.cand, Fstat, df1, df2, log = TRUE) + 
          dbeta(omega2.cand, a.beta, b.beta, log=TRUE) - 
      likelihood2.omega2(samples[m-1], Fstat, df1, df2, log = TRUE) -
          dbeta(samples[m-1], a.beta, b.beta, log=TRUE) +
      dbeta(samples[m-1], pars[1], pars[2], log=TRUE) - 
      dbeta(omega2.cand, pars[1], pars[2], log=TRUE)
    
    samples[m] = ifelse(rexp(1) > -b, omega2.cand, samples[m - 1])
  }
  mcmc(samples)
}

likeHPD.omega2 = function(Fstat, df1, df2, conf.level = .95, iterations = 10000, type = 1){
  if(type == 1){
    samples = like.sample.omega2.mh(Fstat, df1, df2, iterations)
  }else if(type == 2){
    samples = like.sample.omega2.icdf(Fstat, df1, df2, iterations)
  }else{
    stop("Invalid type")
  }
  hpd = HPDinterval(samples, conf.level)
  attr(hpd, "samples") = samples
  names(hpd) = c("lo", "up")
  hpd
}

steigerCI.omega2 = function(Fstat, df1, df2, conf.level=.95){
  Ntot = df1 + df2 + 1
  ci = steigerCI(Fstat, df1, df2, conf.level)
  return(ci / (ci + Ntot))
}


likelihood2 = Vectorize(function(lambda.sq, Fstat, df1, df2, log = FALSE, norm = 0){
  z = df(Fstat, df1, df2, ncp = lambda.sq, log = TRUE) - norm
  if(log){
    return(z)
  }else{
    return(exp(z))
  }
},"lambda.sq")

likelihood = function(lambda.sq, Fstat, df1, df2, log = FALSE){
  norm = log(plike.norm(Fstat, df1, df2))
  likelihood2(lambda.sq, Fstat, df1, df2, log, norm)
}

plike2 = Vectorize(function(lambda.sq, Fstat, df1, df2){
  integrate(likelihood2, 0, lambda.sq, Fstat = Fstat, df1 = df1, df2 = df2)[[1]]
},"lambda.sq")

plike = function(lambda.sq, Fstat, df1, df2) {
  norm = integrate(likelihood2, 0, Inf, Fstat = Fstat, df1 = df1, df2 = df2)[[1]]
  plike2(lambda.sq, Fstat, df1, df2) / plike.norm(Fstat, df1, df2)
}

plike.norm = function(Fstat, df1, df2){
  integrate(likelihood2, 0, Inf, Fstat = Fstat, df1 = df1, df2 = df2)[[1]]
}

qlike2 = Vectorize(function(targetp, Fstat, df1, df2, plikeC = 1){
  if(targetp > 1 | targetp < 0) return(NA)
  if(targetp == 1) return(Inf)
  if(targetp == 0) return(-Inf)
  upper = 10 * Fstat * df1
  optimize(function(lambda.sq) (plike2(lambda.sq, Fstat, df1, df2)/plikeC - targetp)^2,interval=c(0,upper))$minimum
},"targetp")

qlike = function(targetp, Fstat, df1, df2){
  plikeC = plike.norm(Fstat, df1, df2)
  qlike2(targetp, Fstat, df1, df2, plikeC)
}

likelihoodCI = function(Fstat,df1,df2,conf.level=.95){
  alpha = 1-conf.level
  qlike(c(alpha/2,1-alpha/2),Fstat, df1, df2) 
}

## For making a fake data set with given F
fake.data.F = function(Fstat, df1, df2){
  J = as.integer(df1) + 1L
  N = as.integer(df2)%/%J + 1L 
  if(df2 %% J) stop("Degrees of freedom not consistent.")
  x = qnorm((1:N)/(N+1))
  x = x/sd(x)
  if( J %% 2 ){ # even number of groups
    mu = sqrt(Fstat/(J-1) * df1/N)
    mus = c(0,rep(c(-mu, mu), each = (J-1)/2))
  }else{
    mu = sqrt(Fstat/J * df1/N)
    mus = rep(c(-mu, mu), each = J/2)
  }
  y = t(matrix(x,N,J)) + mus
  y = data.frame(y = as.vector(y), grp = factor(rep(1:J,N)))
}


like.sample.icdf = function(Fstat, df1, df2, iterations = 10000){
  plikeC = plike.norm(Fstat, df1, df2)
  mcmc(qlike2(runif(iterations), Fstat, df1, df2, plikeC))
}


like.sample.mh = function(Fstat, df1, df2, iterations = 10000, nFitPoints = 10){
  fitPoints = (1:nFitPoints) / (nFitPoints + 1)
  likeVals = qlike(fitPoints, Fstat, df1,df2 )
  startPar = c(0,0)
  optFunc = function(par, likeVals, fitPoints)
    sum(qgamma(fitPoints,exp(par[1]),exp(par[2])) - likeVals)^2
  sol = optim(startPar,optFunc, likeVals = likeVals, fitPoints = fitPoints)
  pars = exp(sol$par)
  samples = 1:iterations
  samples[1] = qlike(.5, Fstat, df1, df1)
  for(m in 2:iterations){
    lambda.cand = rgamma(1, pars[1], pars[2])
    b =  df(Fstat, df1, df2, ncp = lambda.cand, log = TRUE) - 
      df(Fstat, df1, df2, ncp = samples[m-1], log = TRUE)  +
      dgamma(samples[m-1], pars[1], pars[2], log=TRUE) - 
      dgamma(lambda.cand, pars[1], pars[2], log=TRUE)
    
    samples[m] = ifelse(rexp(1) > -b, lambda.cand, samples[m - 1])
  }
  mcmc(samples)
}

likeHPD = function(Fstat, df1, df2, conf.level = .95, iterations = 10000, type = 1){
  if(type == 1){
    samples = like.sample.mh(Fstat, df1, df2, iterations)
  }else if(type == 2){
    samples = like.sample.icdf(Fstat, df1, df2, iterations)
  }else{
    stop("Invalid type")
  }
  hpd = HPDinterval(samples, conf.level)
  attr(hpd, "samples") = samples
  names(hpd) = c("lo", "up")
  hpd
}

BayesHPD1.omega2 = function(Fstat, df1, df2, conf.level = .95, iterations = 10000, nFitPoints = 10, a.beta = 1, b.beta = 1){
  samples = like.sample.omega2.mh(Fstat, df1, df2, iterations, nFitPoints, a.beta, b.beta)
  hpd = HPDinterval(samples, conf.level)
  attr(hpd, "samples") = samples
  names(hpd) = c("lo", "up")
  hpd
}

Bayes.posterior.omega2 = function(y, conf.level = .95, iterations = 10000){
  J = nlevels(y$grp)
  N = nrow(y) / J
  aov.results = summary( aov( y ~ grp, data = y) )
  SSE = aov.results[[1]][2,2]
  sig2 = 1 / rgamma( iterations, J*(N-1)/2, SSE/2 )
  lambda = matrix(NA,iterations)
  group.means = tapply( y$y, y$grp, mean )
  for(m in 1:iterations){
    mu =  rnorm( J, group.means, sqrt( sig2[m] / N ) )
    lambda[m] = N * sum( (mu - mean(mu) )^2 / sig2[m] )
  }
  mcmc( lambda / ( lambda + N*J ) )
}

BayesHPD2.omega2 = function(Fstat, df1, df2, conf.level = .95, iterations = 10000){
  y = fake.data.F(Fstat, df1, df2)
  omega2 = Bayes.posterior.omega2(y, conf.level, iterations)
  hpd = HPDinterval(omega2, cl)
  attr(hpd, "samples") = omega2
  names(hpd) = c("lo", "up")
  hpd
}


steigerCI = function(Fstat, df1, df2, conf.level=.95){
  alpha = 1-conf.level
  
  ncp.est = Fstat * df1
  pval = 1 - pf(Fstat, df1, df2)
  if( (1-pval) < alpha/2 ){
    return(c(lo=0,up=0))
  }else if((1-pval) < (1-alpha/2)){
    lower = c(lo=0)
    upper = upperSteigerCI(Fstat, alpha, df1, df2)
  }else{
    lower = lowerSteigerCI(Fstat, alpha, df1, df2)
    upper = upperSteigerCI(Fstat, alpha, df1, df2)
  }
  return(c(lower,upper))
}


lowerSteigerCI = function(Fstat, alpha, df1, df2)
{
  targetp = 1 - alpha/2
  lo = optimize(function(q) (pf(Fstat, df1, df2, ncp = q) - targetp)^2,
           interval = c(0, Fstat * df1))$minimum
  return(c(lo = lo))
}

upperSteigerCI = function(Fstat, alpha, df1, df2)
{
  targetp = alpha/2
  up = optimize(function(q) (pf(Fstat, df1, df2, ncp = q) - targetp)^2,
           interval = c(Fstat * df1, Fstat * df1 * 10))$minimum
  return(c(up = up))
}

