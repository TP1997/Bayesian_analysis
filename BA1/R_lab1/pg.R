### Poisson data with Gamma conjugate prior: alfa =a, beta=1/beta in our notes!
pg.sum<-function(y,alfa,beta)
{
  #pg.sum(data, prior_alpha, prior_beta): gives parameters of Gamma posterior
  
  #pg.sum(data,priorialfa,prioribeta)
  #Funktio laskee posteriorijakauman parametrit (gammajakauma)
  #sekä keskiarvon, varianssin ja moodin.
  #Poisson-otos, priori gammajakauma.
  if (alfa>0 & beta>0 & min(y)>=0 & max(abs(y-round(y,digits=0)))==0)
  {
    n<-length(y)
    s<-sum(y)
    alfa1<-alfa+s
    beta1<-beta+n
    ka<-alfa1/beta1
    var<-alfa1/beta1**2
    mo<-(alfa1-1)/beta1
    list(alfa1=alfa1,beta1=beta1,Mean=ka,Var=var,Mode=mo)
  }
  else("Datan arvojen pitää olla ei-negatiivisia kokonaislukuja, alfan ja betan pitää olla positiivisia lukuja")
}

pg.hdi<-function(y,alfa, beta,p)
{
  # 100p% highest posterior density interval
  
  #pg.hdi(data,priorialfa,prioribeta,HDI-alueen ala)
  #Funktio laskee posteriorijakauman 100p prosentin HDI:n.
  #Poisson-otos, priori gammajakauma.
  n<-length(y)
  s<-sum(y)
  alfa1<-pg.sum(y,alfa,beta)$alfa1
  beta1<-pg.sum(y,alfa,beta)$beta1
  e<-sqrt(pg.sum(y,alfa,beta)$Var)/1000
  t<-0
  a<-pg.sum(y,alfa,beta)$Moodi
  b<-a
  if(p<1)
  {
    while (t<p)
    {if (dgamma(a-e,alfa1,beta1)>dgamma(b+e,alfa1,beta1))
    {t<-t+e*dgamma((2*a-e)/2,alfa1,beta1)
    a<-a-e}
      else
      {t<-t+e*dgamma((2*b+e)/2, alfa1,beta1)
      b<-b+e}}
    list(Ala=a, Yla=b)
  }
  else ("HDI-alueen pinta-alan pitää olla pienempi kuin 1")
} 

pg.trip<-function(y,alfa,beta)
{
  # function for triple plots
  
  #pg.trip(data,priorialfa,prioribeta)
  #Funktio piirtää posteriori-, priori- ja uskottavuusjakauman.
  #Poisson-otos, priori gammajakauma.
  if(alfa>=1 & beta>=1)
  {
    s<-sum(y)
    n<-length(y)
    alfa1<-pg.sum(y,alfa,beta)$alfa1
    beta1<-pg.sum(y,alfa,beta)$beta1
    ala<-max(0,min(alfa/beta-3*alfa/beta**2,alfa1/beta1-3*alfa1/beta1**2,0.5*s/n))
    yla<-max(qgamma(0.99,alfa1,beta1),qgamma(0.99,alfa,beta),qpois(0.95,s/n))
    x<-seq(max(0,ala-max(alfa/beta**2,alfa1/beta1**2)),yla+max(alfa/beta**2,alfa1/beta1**2),length=300)
    z<-seq(0,n*round(yla+1,digits=0),by=1)
    post<-dgamma(x,alfa1,beta1)
    prior<-dgamma(x,alfa,beta)
    usk<-dpois(z,s)
    top<-1.1*max(post,prior,usk)
    plot(x,post,type="l",col="1",xlim=c(ala,yla),ylim=c(0,top),main="Posterior (black), prior (red), lkhd (green)", ylab="density")
    par(new=T)
    plot(x,prior,type="l",col="2",xlim=c(ala,yla),ylim=c(0,top),ylab="")
    par(new=T)
    if(n>6)
      plot(z/n,usk*n,col="3",type="l",xlim=c(ala,yla),ylim=c(0,top),ylab="",xlab="")
    else
      plot(z/n,usk*n,col="3",pch=16,type="b",xlim=c(ala,yla),ylim=c(0,top),ylab="",xlab="")
  }
  else("Käytä prioria, jossa alfa ja beta ovat vähintään 1")
}