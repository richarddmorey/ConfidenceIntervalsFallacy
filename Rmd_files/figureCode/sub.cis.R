
bayesCIprob = Vectorize(function(q){
  q = abs(q)
  if(q < (1/4)){
    return(
      1/2 - 8/3*q^2
    )
  }else if(q<(3/4)){
    return(
      3/4 - 2*q + 4/3*q^2
    )
  }else{
    return(0)
  }
},"q")

freqCIprob = Vectorize(function(q){
  (2*(q + 1/2) * (1-q-1/2))*(abs(q)<1/2)
},"q")


pxbar2 <- Vectorize(function(xbar,theta,scale){
  if(xbar<(theta-scale)) return(0) 
  if(xbar>(theta+scale)) return(1)
  z = (xbar - theta)/scale
  if(xbar<theta) return((z + 1)^2/2)
  if(xbar>theta) return(1 - (1 - z)^2/2)
},"xbar")


pC2in <- Vectorize(function(thetaStar,theta,scale){
  w = scale * (1 - 1 / sqrt(2))
  pxbar2(thetaStar + w, theta, scale) - pxbar2(thetaStar - w, theta, scale)
},"thetaStar")

welchCIprob = Vectorize(function(q){
  q = abs(q)
  if(q<1/2){
    1/2 - q
  }else{
    return(0)
  }
},"q")



likelihood.sub = function(x, width = 1){
  c(max(x) - width/2, min(x) + width/2)
}

nonpara.sub = function(x, width = 1){
  sort(x)
}

ump.sub = function(x, width = 1){
  if(diff(sort(x)) > width/2){
    return(likelihood.sub(x, width))
  }else{
    return(nonpara.sub(x, width))
  } 
}

bayes.sub = function(x, width = 1){
  mean(x) + c(-1,1) * diff(likelihood.sub(x, width))/4
}

sampling_dist.sub = function(x, width = 1){
  mean(x) + c(-1,1) * width/2 * (1 - 1/sqrt(2))
}

width.sub = function(fun, width = 1, x.points = 100){
  w = cbind(0, seq(0,width, len = x.points))
  prec = apply(apply(w, 1, likelihood.sub, width = width),2,diff)
  width = apply(apply(w, 1, fun, width = width),2,diff)
  list(precision = prec, width = width, proportion = width / prec)
}




