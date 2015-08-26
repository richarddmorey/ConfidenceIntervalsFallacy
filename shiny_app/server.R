require(lattice)
require(stats)
require(shiny)
require("RColorBrewer")
require("SVGAnnotation")
require("XML")
require(grImport)
require(shinyjs)

options(shiny.usecairo=FALSE)

################
# CI stuff

between = function(x,v){
  if(all(x==v)) return(TRUE)
  (min(v)<x) & (max(v)>x)
}


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


#######################

shinyServer(function(input,output,session) {
  
  values = reactiveValues(CIcounter=c(N=0,bayes=0,SD=0,NP=0,UMP=0))
  
  observeEvent(input$whichPlot, {
    if(input$whichPlot == "Choose your own / Sample"){
      shinyjs::enable("sample")
      shinyjs::enable("sample1000")
      shinyjs::enable("values")
    }else{
      shinyjs::disable("values")
      shinyjs::disable("sample1000")
      shinyjs::disable("sample")
      if(input$whichPlot=="Figure 1A"){
        updateSliderInput(session = session, "values",
                          value = c(-4,-3.5))
      }else if(input$whichPlot=="Figure 1B"){
        updateSliderInput(session = session, "values",
                          value = c(-4.5,4.5))
      }
    }
  })
  
  observeEvent(input$whichValue, {
    values$CIcounter['bayes'] = 0
    values$CIcounter['SD'] = 0
    values$CIcounter['UMP'] = 0
    values$CIcounter['NP'] = 0
    values$CIcounter['N'] = 0   
  })
  
  observeEvent(input$testValue, {
    values$CIcounter['bayes'] = 0
    values$CIcounter['SD'] = 0
    values$CIcounter['UMP'] = 0
    values$CIcounter['NP'] = 0  
    values$CIcounter['N'] = 0   
  })

  observeEvent(input$sample1000, {
    ci2.width=.5*(1-1/sqrt(2))
    
    M = 1000
    isolate({values$CIcounter['N'] = values$CIcounter['N'] + M})
    x = matrix(runif(M*2,-5, 5),M,2)
    x.a = (x + 5)/10
    
    if(input$whichValue == 'other'){
      testVal = input$testValue / 10 + .5
    }else{
      testVal = .5
    }
    
    z = apply(x.a, 1, function(x.a){
      x.a = sort(x.a)
      # Samp dist
      sampdist = c(mean(x.a)-ci2.width,mean(x.a)+ci2.width)
      
      # Cred. Interval
      post.width = 1 - diff(x.a)
      cred.int = c(x.a[2]-.5 + .25*post.width, x.a[1]+.5 - .25*post.width)
      
      # UMP
      ump = sort(x.a)
      if(diff(ump) > .5){
        ump = c(ump[2] - .5, ump[1] + .5) 
      }
      
      c(bayes=between(testVal,cred.int),
        UMP=between(testVal,ump),
        SD=between(testVal,sampdist),
        NP=between(testVal,x.a))
    })
    
    z = rowSums(z)
    
    isolate({values$CIcounter['bayes'] = values$CIcounter['bayes'] + z['bayes']})
    isolate({values$CIcounter['SD'] = values$CIcounter['SD'] + z['SD']})
    isolate({values$CIcounter['UMP'] = values$CIcounter['UMP'] + z['UMP']})
    isolate({values$CIcounter['NP'] = values$CIcounter['NP'] + z['NP']})
    
    updateSliderInput(session = session, "values",
                      value = sort(x[M,]))
    
  })  
    
  observeEvent(input$sample, {
    x = sort(runif(2,-5, 5))
    x.a = (x + 5)/10
    isolate({values$CIcounter['N'] = values$CIcounter['N'] + 1})
    
    if(input$whichValue == 'other'){
      testVal = input$testValue / 10 + .5
    }else{
      testVal = .5
    }
    
    # Samp dist
    ci2.width=.5*(1-1/sqrt(2))
    sampdist = c(mean(x.a)-ci2.width,mean(x.a)+ci2.width)
    
    # Cred. Interval
    post.width = 1 - diff(x.a)
    cred.int = c(x.a[2]-.5 + .25*post.width, x.a[1]+.5 - .25*post.width)
    
    # UMP
    ump = sort(x.a)
    if(diff(ump) > .5){
      ump = c(ump[2] - .5, ump[1] + .5) 
    }
    
    updateSliderInput(session = session, "values",
                          value = x)
    
    
    isolate({values$CIcounter['bayes'] = values$CIcounter['bayes'] + between(testVal,cred.int)})
    isolate({values$CIcounter['UMP'] = values$CIcounter['UMP'] + between(testVal,ump)})
    isolate({values$CIcounter['SD'] = values$CIcounter['SD'] + between(testVal,sampdist)})
    isolate({values$CIcounter['NP'] = values$CIcounter['NP'] + between(testVal,x.a)})
    
  })
  
  output$sampleStats <- reactive({
    paste("Sampled", values$CIcounter['N']," bubble pairs. Of these,")
  })
  
  output$BayesStats <- reactive({
    y = values$CIcounter['bayes']
    n = values$CIcounter['N']
    perc = ifelse(n==0,"-",round(y/n * 100,1))
    paste(y," (",perc,"%) Bayes intervals,",sep="")
  })
  
  output$UMPStats <- reactive({
    y = values$CIcounter['UMP']
    n = values$CIcounter['N']
    perc = ifelse(n==0,"-",round(y/n * 100,1))
    paste(y," (",perc,"%) UMP intervals,",sep="")
  })  

  output$NonparaStats <- reactive({
    y = values$CIcounter['NP']
    n = values$CIcounter['N']
    perc = ifelse(n==0,"-",round(y/n * 100,1))
    paste(y," (",perc,"%) nonpara. intervals, and",sep="")
  })
  
  output$SampDistStats <- reactive({
    y = values$CIcounter['SD']
    n = values$CIcounter['N']
    perc = ifelse(n==0,"-",round(y/n * 100,1))
    if(input$whichValue == "other" & input$testValue != 0){
      sign1 = ifelse(input$testValue < 0, "-", "+")
      val = paste0("the false value θ' = θ ", sign1, " ", abs(input$testValue) )
    }else{
      val = "the true value θ."
    }
    paste(y," (",perc,"%) samp. dist. intervals contained ",val,sep="")
  })
  
  output$svg.grid <- reactive({
    #from lattice package documentation

    input$sample
    
    x.a = sort(c(input$values) + 5)/10
    submarinePic = readPicture('vectorFigs/submarine.xml')
    ci2.width=.5*(1-1/sqrt(2))
    
    if(input$whichValue == 'true' | input$whichPlot != "Choose your own / Sample" | input$testValue == 0){
      testVal = .5
      includeCol = "green"
      excludeCol = "red"
    }else{
      testVal = input$testValue / 10 + .5
      includeCol = "red"
      excludeCol = "green"
    }
    
    
    doc = svgPlot( {
      
      layout(matrix(c(1,2,2,2),1,4))
      par(mgp=c(3.5,2,0),mar=c(4.5,0,.5,0),cex=1.3)
      plot(0,0,ty='n',ylim=c(-1,5.3),xlim=c(-1,1),axes=FALSE,xlab="",ylab="")
      graphics::text(0:5*0+1,0:5,c("Bubbles","Likelihood","Samp. Dist.","Nonpara.","UMP","Bayes"),adj=1)
      
      ####
      
      par(mar=c(4.5,1.5,.5,.5),cex=1.3)
      plot(0,0,ty='n',ylim=c(-1,5.3),xlim=c(-.5,1.5),axes=F,ylab="",xlab="Location")
      abline(h=0:5)
      axis(1,at=1:5/2-1,lab=expression(theta - 10,
                                       theta - 5,
                                       theta,
                                       theta + 5,
                                       theta + 10
      ))
      points(x.a,0*x.a,pch=21,col="black",bg="lightblue",cex=1)
      rect(0,-1,1,5,col=rgb(0,0,0,.1),lty=0)
      
      
      abline(v=testVal,lty=2)

      # Likelihood
      post.height=.2
      rect(x.a[2]-.5-.005,1 - post.height/2,x.a[1]+.5+.005,1 + post.height/2, col=rgb(0,0,1,.2),lty=0)
      
      # Sampling distribution
      arrows(mean(x.a)-ci2.width,2,mean(x.a)+ci2.width,2,code=3,ang=90,len=.1,lwd=2,col="red")
      if(between(testVal,mean(x.a) + c(-1,1)*ci2.width)){
        points(-.5, 2, pch=19, col=includeCol, cex=2.1)
      }else{
        points(-.5, 2, pch=4, col=excludeCol, cex=2.1)
      }
      
      # Nonparametric
      if(diff(x.a)==0){
        segments(mean(x.a),3+.15,mean(x.a),3-.15,lwd=2,col="red")
      }else{
        arrows(x.a[1],3,x.a[2],3,code=3,ang=90,len=.1,lwd=2,col="red")
      }
      if(between(testVal,x.a)){
        points(-.5, 3, pch=19, col=includeCol, cex=2.1)
      }else{
        points(-.5, 3, pch=4, col=excludeCol, cex=2.1)
      }
      
      # UMP
      ump = sort(x.a)
      if(diff(ump) > .5){
        ump = c(ump[2] - .5, ump[1] + .5) 
      }
      if(diff(x.a)==0 | diff(x.a)==1){
        segments(mean(x.a),4+.15,mean(x.a),4-.15,lwd=2,col="red")
      }else{
        arrows(ump[1],4,ump[2],4,code=3,ang=90,len=.1,lwd=2,col="red")
      }
      if(between(testVal,ump)){
        points(-.5, 4, pch=19, col=includeCol, cex=2.5)
      }else{
        points(-.5, 4, pch=4, col=excludeCol, cex=2.5)
      }
      
      # Cred. Interval
      post.width = 1 - diff(x.a)
      cred.int = c(x.a[2]-.5 + .25*post.width, x.a[1]+.5 - .25*post.width)
      if(diff(x.a)==1){
        segments(mean(x.a),5+.15,mean(x.a),5-.15,lwd=2,col="blue")
      }else{
      arrows(cred.int[1],5,cred.int[2],5,code=3,ang=90,len=.1,lwd=2,col="blue")
      }
      if(between(testVal,cred.int)){
        points(-.5, 5, pch=19, col=includeCol, cex=2.5)
      }else{
        points(-.5, 5, pch=4, col=excludeCol, cex=2.5)
      }
      picture(submarinePic, 0, -1, 1, -.2)

    }, height = 5, width = 6, pointsize = 10)  
    
    tempsvg <- tempfile(fileext=".svg")
    on.exit(unlink(tempsvg))
    saveXML(doc, tempsvg)
    svgoutput <- readLines(con = tempsvg, n=-1)
    svgoutput
  })
  
   output$svg.grid2 <- renderPlot({
  
    #from lattice package documentation
    
    input$sample
    
    x.a = sort(c(input$values) + 5)/10
    ci2.width=.5*(1-1/sqrt(2))
    
    polColor = col=rgb(1,0,0,.2)
    outPoint1 = .5 * diff(x.a)
    outPoint4 = 1 - .5 * diff(x.a) 
    
    if(input$whichCI %in% c("Nonparametric", "UMP")){
      x1 = c(0,.5,.5,1,1,0,0,.5,.5,0)
      y1 = c(.5,.5,0,0,.5,.5,1,1,.5,.5)
      outPoint2 = outPoint1 + .5 - min(.5,abs(diff(x.a)))
    }else if(input$whichCI == "Bayes"){
      x1 = c(0,.25,1,.75,0)
      y1 = c(1,.25,0,.75,1)
      polColor=rgb(0,0,1,.2)
      outPoint2 = outPoint1 + .5*(.5 - outPoint1)
    }else if(input$whichCI == "Sampling Distribution"){
      q = sqrt(2)/2
      x1 = c(0,0,q,1,1,1-q)
      y1 = c(1,q,0,0,1-q,1)
      outPoint2 = outPoint1 + max(0,.5 - outPoint1 - ci2.width)
    }
    outPoint3 = 1 - outPoint2

    par(mfrow=c(1,2),cex=1.3,mgp=c(3.5,2,0),mar=c(4.5,5.5,.1,.3))
      plot(0,0,ty='n',ylim=c(0,1),xlim=c(0,1),axes=FALSE,ylab=expression(y[2]),xlab=expression(y[1]))
      polygon(x1,y1,col=polColor,border=NA)
      axis(1,at=2:4/2-1,lab=expression(theta - 5,
                                       theta,
                                       theta + 5
      ),las=1)
      axis(2,at=2:4/2-1,lab=expression(theta - 5,
                                       theta,
                                       theta + 5
      ),las=1)
      rect(0,0,1,1,border="black")
      
      mtext("A",3,-2,adj=.9,cex=2)
      
      points(x.a[1],x.a[2],pch=21,col="black",bg="lightblue")
 
      
      plot(0,0,ty='n',ylim=c(-1,1),xlim=c(0,1),axes=FALSE,ylab=expression(y[2]-y[1]),xlab=expression(bar(y)))
      polygon((x1+y1)/2,y1-x1,col=polColor,border=NA)
      axis(2,at=-2:2*5/10,lab=-2:2*5,las=1)
      axis(1,at=2:4/2-1,lab=expression(theta - 5,
                                       theta,
                                       theta + 5
      ),las=1)
      polygon(c(0,.5,1,.5),c(0,-1,0,1))
      mtext("B",3,-2,adj=.9,cex=2)
 
      abline(h = x.a[2]-x.a[1],col="gray",lty=1)
      segments(outPoint1,diff(x.a),outPoint4,diff(x.a),col="red",lwd=3)
      segments(outPoint2,diff(x.a),outPoint3,diff(x.a),col="green",lwd=3)
      
      points(mean(x.a),x.a[2]-x.a[1],pch=21,col="black",bg="lightblue")
        
  }, width = 725, height = 350)
  
   output$widthInfo <- reactive({
     ci2.width=.5*(1-1/sqrt(2))
     width = diff(sort(c(input$values)))
     CItype = input$whichCI
     denom = 1 - width/10
     if(input$whichCI %in% c("Nonparametric", "UMP")){
       if(width>5){
         prob = "100%" 
       }else{
         num = min(1 - width/10, width/10)
         prob = paste(round(num/denom*100,1),"%",sep="")
       }
     }else if(input$whichCI == "Bayes"){
         prob = "50%" 
     }else if(input$whichCI == "Sampling Distribution"){
       if(width>(10-20*ci2.width)){
         prob = "100%" 
       }else{
         num = 2*ci2.width
         prob = paste(round(num/denom*100,1),"%",sep="")
       }
     }
     paste("50% ",CItype," intervals have a ", prob, " probability of containing the hatch θ when the distance between the bubbles is ",width," meters. ",sep="")
   })

   output$svg.grid5 <- renderPlot({
     
     #from lattice package documentation
     
     input$sample
     
     x.a = sort(c(input$values) + 5)/10
     xx = seq(-1,2, len=200)   

     par(mgp=c(3,1,0),mar=c(1,4.6,3.5,1),cex=2.2,mfcol=c(3,1),cex.axis=2.2,cex.lab=2.2)
     
     plot(0,0,ty='n',ylim=c(0,2),xlim=c(-.5,1.5),axes=F,ylab="Prior",xlab="")
     axis(3,at=1:5/2-1,lab=expression(theta - 10,
                                      theta - 5,
                                      theta,
                                      theta + 5,
                                      theta + 10
     ))
     axis(2,at=0)
     
     if(input$whichPrior=="Objective/Noninformative"){
       abline(h=1, col="blue", lwd=2)
     }else{
       lines(xx,dnorm(xx,.25,1)*4,  col="blue", lwd=3)
     }
     abline(h=0,col="gray")
     box()
     
     
     par(mar=c(1,4.6,1,1))
     
     plot(0,0,ty='n',ylim=c(0,2),xlim=c(-.5,1.5),axes=F,ylab="Likelihood",xlab="")
     box()
     abline(h=0,col="gray")
     
     rect(x.a[1] - .5, 1.5 - .1, x.a[1] + .5, 1.5 + .1, col=rgb(0,0,1,.2),lty=0)
     rect(x.a[2] - .5, 1.5 - .1, x.a[2] + .5, 1.5 + .1, col=rgb(0,0,1,.2),lty=0)
     segments(min(x.a) + .5, 0, min(x.a) + .5, 1, col="blue", lwd=2)
     segments(max(x.a) - .5, 0, max(x.a) - .5, 1,col="blue", lwd=2)
     segments(max(x.a) - .5, 1, min(x.a) + .5, 1,  col="blue", lwd=2)

     points(x.a,0*x.a + 1.5,pch=21,col="black",bg="lightblue",cex=2)
     
     axis(2,at=0)
     
     par(mar=c(4.5,4.6,1,1))
     
     plot(0,0,ty='n',ylim=c(0,2),xlim=c(-.5,1.5),axes=F,ylab="Posterior",xlab="Location")
     box()
     abline(h=0,col="gray")
     
     if(input$whichPrior=="Objective/Noninformative"){
       segments(min(x.a) + .5, 0, min(x.a) + .5, 1,  col="blue", lwd=2)
       segments(max(x.a) - .5, 0, max(x.a) - .5, 1,  col="blue", lwd=2)
       segments(max(x.a) - .5, 1, min(x.a) + .5, 1,  col="blue", lwd=2)
       left.side = mean(x.a) - (1/4 - diff(x.a)/4)
       right.side = mean(x.a) + (1/4 - diff(x.a)/4)
       rect(left.side,0,right.side,1, col =rgb(0,0,0,.1),lty=0)
      }else{
       xx = seq(max(x.a)-.5,min(x.a)+.5,len=100)
       lines(xx,dnorm(xx,.25,1)*4,  col="blue", lwd=2)
       total.area  = pnorm(min(x.a) + .5,.25,1) - pnorm(max(x.a) - .5,.25,1)
       left.side = qnorm(pnorm(max(x.a) - .5,.25,1) + total.area*.25, .25, 1)
       right.side = qnorm(pnorm(max(x.a) - .5,.25,1) + total.area*.75, .25, 1)
       segments(max(x.a)-.5, 0 , max(x.a)-.5, dnorm(max(x.a)-.5, .25,1)*4,col="blue",lwd=3)
       segments(min(x.a)+.5, 0 , min(x.a)+.5, dnorm(min(x.a)+.5, .25,1)*4,col="blue",lwd=3)
       
       xx = seq(left.side,right.side,len=100)
       polygon(c(xx, rev(xx)), c(xx*0, dnorm(rev(xx),.25,1)*4), col=rgb(0,0,0,.1),lty=0)
       
      }
     arrows(left.side-.001, 
            par()$usr[4]*.15,
            right.side+.001,
            par()$usr[4]*.15,
            code = 3,
            angle = 90, col="blue",lwd=2,length=.1)
     
     graphics::text((max(x.a)-.5), 
                    par()$usr[4]*.3,
                    "25%",cex=2,adj=as.numeric(diff(x.a)>.5))
     graphics::text((min(x.a)+.5), 
                    par()$usr[4]*.3,
                    "25%",cex=2,adj=as.numeric(diff(x.a)<.5))
     graphics::text((left.side+right.side)/2, 
                    par()$usr[4]*(.3 + (diff(x.a)>.8)*.4),
                    "50%",cex=2)
     
     
     axis(2,at=0)
     
     axis(1,at=1:5/2-1,lab=expression(theta - 10,
                                      theta - 5,
                                      theta,
                                      theta + 5,
                                      theta + 10
     ))

   }, width = 600, height = 400)
   
   output$incProbs <- reactive({

     testVal = input$testValue / 10
     
     aa = 100*bayesCIprob(testVal)
     bb = 100*freqCIprob(testVal) # NP
     cc = 100*pC2in(testVal,theta=0,scale=.5) #SD
     dd = 100*welchCIprob(testVal) # UMP
     
     if(input$testValue==0){
       val = paste0("the true value θ" )
     }else{   
       sign1 = ifelse(input$testValue < 0, "-", "+")
       val = paste0("the false value θ' = θ ", sign1, " ", abs(input$testValue) )
     }
     return(
       paste0(
        "Prob. that interval contains ", val, ":<br/>",
        "Bayes: ",round(aa,1),"%<br/>",
        "UMP: ",round(dd,1),"%<br/>",
        "Nonparametric: ",round(bb,1),"%<br/>",
        "Sampling distribution: ",round(cc,1),"%<br/>",
        "Trivial: ",50,"%"
       )
     )
     
   })
   
   output$svg.grid4 <- renderPlot({

    
     qq = seq(-3/4,3/4,len=200)
     aa = bayesCIprob(qq)
     bb = freqCIprob(qq) # NP
     cc = pC2in(qq,theta=0,scale=.5) #SD
     dd = welchCIprob(qq) # UMP
    
     
     testVal = input$testValue / 10
     
      
       par(las=1)
       
       plot(qq,aa,ty='n',ylab="Probability CI contains value",xlab=expression(paste("Alternative ", theta,"'")),axes=FALSE)
       
       if(input$fig4ShowBayes) lines(qq,aa,col="blue",lwd=2)
       if(input$fig4ShowNP) lines(qq,bb,col="darkred",lwd=2,lty=2)
       if(input$fig4ShowSD) lines(qq,cc,col="darkred",lwd=2,lty=5)
       if(input$fig4ShowUMP) lines(qq,dd,col="darkred",lwd=2,lty=3)
       if(input$fig4ShowTrivial) abline(h=.5,col="gray",lwd=2,lty=4)
       
       box()
       axis(2)
       axis(1,at=-3/4,lab=expression(theta-7.5 ))
       axis(1,at=-2/4,lab=expression(theta-5 ))
       axis(1,at=-1/4,lab=expression(theta-2.5 ))
       axis(1,at=0,lab=expression(theta))
       axis(1,at=3/4,lab=expression(theta+7.5 ))
       axis(1,at=2/4,lab=expression(theta+5 ))
       axis(1,at=1/4,lab=expression(theta+2.5 ))
       
       if(input$whichValue == 'other')
        abline(v = testVal, lty=2, col="black")
       
       if(input$fig4ShowTrivial) graphics::text(.6, .47, "T", col="darkgray", cex = 1)
       if(input$fig4ShowNP) graphics::text(.3, .4, "NP", col="darkred", cex = 1)
       if(input$fig4ShowSD) graphics::text(.5, 3.5, "SD", col="darkred", cex = 1)
       if(input$fig4ShowUMP) graphics::text(-.1, .3, "UMP", col="darkred", cex = 1)
       if(input$fig4ShowBayes) graphics::text(-.6, .1, "B", col="blue", cex = 1)
       
       if(input$fig4ShowSD) legend(-3/4,.4,legend=c("SD"), col="darkred",lty=5,lwd=2,bty='n',text.col="darkred")
       

   })
   
   output$svg.grid3 <- renderPlot({
     #from lattice package documentation
     
     input$sample
     
     x.a = sort(c(input$values) + 5)/10
     ci2.width=.5*(1-1/sqrt(2))
     
     
     par(las=1,cex=1.3)
     
     bayes.prec = width.sub(bayes.sub, 10)
     samp.prec = width.sub(sampling_dist.sub, 10)
     nonpara.prec = width.sub(nonpara.sub, 10)
     ump.prec = width.sub(ump.sub, 10)
     
     plot(samp.prec$precision, samp.prec$width, ty='n', 
          xlab = "Uncertainty (width of likelihood)", 
          ylab = "CI width", ylim = c(0,10))
     if(input$fig3ShowSD) lines(samp.prec$precision, samp.prec$width, lty = 5, lwd = 2, col = "darkred")
     if(input$fig3ShowBayes) lines(bayes.prec$precision, bayes.prec$width, lty = 1, lwd = 2, col = "blue")
     if(input$fig3ShowUMP) segments(0, 0, 5, 5, lty=3, lwd=2, col="darkred") # UMP 1
     if(input$fig3ShowNP) segments(0, 10, 5, 5, lty=2, lwd = 2, col = "darkred") # nonpara

     if(input$fig3ShowUMP & input$fig3ShowNP) segments(5, 5, 10, 0, lty=4, lwd=2, col="darkred") # UMP 2
     if(input$fig3ShowNP & !input$fig3ShowUMP) segments(5, 5, 10, 0, lty=2, lwd=2, col="darkred") # UMP 2
     if(!input$fig3ShowNP & input$fig3ShowUMP) segments(5, 5, 10, 0, lty=3, lwd=2, col="darkred") # UMP 2

     #lines(nonpara.prec$precision, nonpara.prec$width,lty = 2, lwd = 2, col = "darkred")
     #lines(ump.prec$precision, ump.prec$width, lty = 3, lwd = 2, col = "darkred")
     #lines(ump.prec$precision, ump.prec$precision, lty = 3, lwd = 2, col = rgb(0,0,0,.3))
     
     if(input$fig3ShowNP) graphics::text(2, 7, "NP", col="darkred", cex = 1)
     if(input$fig3ShowSD) graphics::text(.5, 3.5, "SD", col="darkred", cex = 1)
     if(input$fig3ShowUMP) graphics::text(.5, 1.5, "UMP", col="darkred", cex = 1)
     if(input$fig3ShowBayes) graphics::text(8.8, 5, "B", col="blue", cex = 1)
 
     abline(v = 10 - 10*diff(x.a), col="gray", lty=1, lwd=2)
     if(diff(x.a)>.5){
       graphics::text(10 - 10*diff(x.a), 9, "Current bubbles", adj=0, col="gray")
     }else{
       graphics::text(10 - 10*diff(x.a), 9, "Current bubbles", adj=1, col="gray")
     }
     
     abline(h=0,col="lightgray")
     
   })
   
   
})
