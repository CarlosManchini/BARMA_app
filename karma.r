# Created by Fabio M Bayer (bayer@ufsm.br), july/2017
#
# Some informations:
# diag = 0 : without graphs (useful for simulations)
# diag = 1 : plot diagnostic graphs
# diga = 2 : make pdf with diagnostic graphs
#
# (a,b) the interval of data
#
# h : number of forecast steps


karma<- function (yab, ar=NA, ma=NA, a=0, b=1,link="aoz",
                  diag=0,h=6,X=NA,X_hat=NA,resid=3, lambda=1)
{
  source("kum-mu-phi.r")
  source("karma.fit.r")
  source("aoz_aux.r")
  source("aoz_karma.fit.r")
  # source("ZOA_karma.fit.r")
  
  if (min(yab) <= a || max(yab) >= b){
    stop("OUT OF RANGE (a,b)!")
  }else{
    if(a != 0 || b != 1)
    {
      y <- (yab-a)/(b-a)
    }else{
      y <- yab
    }
  }
  
  
  if(is.ts(y)==T)
  {
    freq<-frequency(y)
  }else stop("data can be a time-series object")
  
  
  if(any(is.na(ar))==F) names_phi<-c(paste("phi",ar,sep=""))
  
  if(any(is.na(ma))==F) names_theta<-c(paste("theta",ma,sep=""))
  
  if(any(is.na(X))==F)
  {
    names_beta<-c(paste("beta",1:ncol( as.matrix(X) ),sep=""))
  }
  
  p <- max(ar)
  q <- max(ma)
  n <- length(y)
  
  m <- max(p,q,na.rm=T)
  
  p1 <- length(ar)
  q1 <- length(ma)
  
  ### tranformar em caracter
  linktemp <- substitute(link)
  if (!is.character(linktemp))
  {
    linktemp <- deparse(linktemp)
    if (linktemp == "link")
      linktemp <- eval(link)
  }
  
  ### verificando funcao
  if (linktemp == "aoz")
  {
    link1 <- structure(list(link = linktemp,
                            linkfun = linkfun.ao,
                            linkinv = linkinv.ao,
                            mu.eta  = mu.eta.ao,
                            diflink = diflink.ao ))
    fit1 <- karma.fit.aoz(y, ar, ma, a, b, link1, names_phi, names_theta, names_beta, diag, h, X, X_hat,resid=resid, lambda) # model estimation AO
  }
  else if (any(linktemp == c("logit", "probit", "cloglog"))){
    stats <- make.link(linktemp)
    link1 <- structure(list(link = linktemp,
                            linkfun = stats$linkfun,
                            linkinv = stats$linkinv,
                            mu.eta = stats$mu.eta,
                            diflink = function(t) 1/(stats$mu.eta(stats$linkfun(t)))  ))
    fit1 <- karma.fit(y, ar, ma, a, b, link1, names_phi, names_theta, names_beta, diag, h, X, X_hat,resid=resid) # model estimation
  }
  else stop(paste(linktemp, "link not available, available links are \"aoz\", \"logit\", ",
                  "\"probit\" and \"cloglog\""))
  
  
  return(fit1)
}

