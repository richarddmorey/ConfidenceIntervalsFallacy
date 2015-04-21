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




