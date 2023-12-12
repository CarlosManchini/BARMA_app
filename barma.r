# Implementado por Fabio M Bayer (bayer@ufsm.br) em 15/10/2015
# Alteracoes em 17/06/2015
#
# Algumas informacoes:
# diag = 0 : nao plota graficos
# diag = 1 : plota graficos na tela
# diga = 2 : gera graficos em pdf e plota graficos na tela
#
# h : quantidade de passos a frente para fazer previsao
#
# O objeto de entrada da funcao deve ser serie temporal (ts)
#
# Tem quatro tipos de residuos a serem utilizados, com resid de 1 a 4
#
# Exemplos de uso:
#
# 1) BARMA(2,3) com funcao de ligacao logit e previsao 6 passos a frente
# fit <- barma(y,ar=c(1,2),ma=c(1,2),resid=2)
# 
# Obs: Perceba que pode ser os lags que voce desejar.
# 
# 2) imprimindo graficos em arquivos pdf e com ligacao probit
# fit <- barma(y,ar=c(1,2),ma=c(1,2),diag=2,link="probit",resid=3)


barma<- function (y, ar=NA, ma=NA, link = NA,diag=1,h=6,X=NA,X_hat=NA,resid=3,lambda=NA) #lambda¿ só interfere o do barma.fit.aoz
{  
  #source("barma.fit.r") 
  #source("barma.fit.aoz.r") # dividir funcoes is better
  
  if (min(y) <= 0 || max(y) >= 1)
    stop("OUT OF RANGE (0,1)!")
  
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
  
  
  linktemp <- substitute(link)
  if (!is.character(linktemp))
  {
    linktemp <- deparse(linktemp)
    if (linktemp == "link")
      linktemp <- eval(link)
  }
  
  if(linktemp == "aoz"){
    source("barma.fit.aoz.r") #ou barma.fit.aoz.OK.r sem covariaveis
    source("aux_aoz.r")
    link1 <- structure(list(link = linktemp, 
                            linkfun = linkfun.aoz,
                            linkinv = linkinv.aoz, 
                            mu.eta = mu.eta.aoz, 
                            diflink = diflink.aoz
    ))
    
    fit1 <- barma.fit.aoz(y, ar, ma, link1, names_phi, names_theta, names_beta, diag, h, X, X_hat,resid=resid,lambda) # lambda?
    
    return(fit1)
  }else{
    
    source("barma.fit.r")
    
    if (any(linktemp == c("logit", "probit", "cloglog")))
      stats <- make.link(linktemp)
    
    link1 <- structure(list(link = linktemp, 
                            linkfun = stats$linkfun,
                            linkinv = stats$linkinv, 
                            mu.eta = stats$mu.eta, 
                            diflink = function(t) 1/(stats$mu.eta(stats$linkfun(t)))
    ))
    
    fit1 <- barma.fit(y, ar, ma, link1, names_phi, names_theta, names_beta, diag, h, X, X_hat,resid=resid) # model estimation
    
    return(fit1)
  }
}

# source("simu.barma.full.r")
# 
# set.seed(369)
# #Y<-simu.barma.full(60,phi=c(0.45,0.3),theta=c(0.5),prec=100,link="logit") ; print(y)
# y<-simu.barma.full(200,phi=c(0.45,0.3),theta=c(0.5),prec=100,link="aoz",lambda=1.5); print(y)
# 
# # barma.full(y,ar=c(1,2),ma=c(1), link = "cloglog")
#   # barma.full(y,ar=c(1,2),ma=c(1), link = "aoz",lambda = 1)
# 
# #prec=120