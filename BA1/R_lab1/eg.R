eg.sum<-function(y,alfa,beta)
{
  #exponential data with gamma prior : Gamma(alpha, beta)
  #eg.sum(data, prior_alpha, prior_beta) : gives the parameters for the Gamma posterior dist.
  
  #eg.sum(data,priorialfa,prioribeta)
  #Funktio laskee posteriorijakauman parametrit (gammajakauma)
  #sekä keskiarvon, varianssin ja moodin.
  #Eksponentiaalinen otos, konjugaattipriori gammajakauma.
  if (alfa>0 & beta>0 & min(y)>=0)
  {
    n<-length(y)
    s<-sum(y)
    alfa1<-alfa+n
    beta1<-beta+s
    ka<-alfa1/beta1
    var<-alfa1/beta1**2
    mo<-(alfa1-1)/beta1
    list(alfa1=alfa1,beta1=beta1,Mean=ka,Var=var,Mode=mo)
  }
  else("Datan arvojen sekä alfan ja betan pitää olla positiivisia lukuja")
}

eg.hdi<-function(y,alfa, beta,p)
{
  # Highest density intervals
  #eg.hdi(data,priorialfa,prioribeta,HDI-alueen ala)
  #Funktio laskee posteriorijakauman 100p prosentin HDI:n.
  #Eksponentiaalinen otos, konjugaattipriori gammajakauma.
  n<-length(y)
  s<-sum(y)
  alfa1<-eg.sum(y,alfa,beta)$alfa1
  beta1<-eg.sum(y,alfa,beta)$beta1
  e<-sqrt(eg.sum(y,alfa,beta)$Var)/1000
  t<-0
  a<-eg.sum(y,alfa,beta)$Moodi
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

eg.trip<-function(y,alfa,beta)
{
  #function for triple plot
  
  #eg.trip(data,priorialfa,prioribeta)
  #Funktio piirtää posteriori-, priori- ja uskottavuusjakauman.
  #Eksponentiaalinen otos, konjugaattipriori gammajakauma.
  s<-sum(y)
  n<-length(y)
  alfa1<-eg.sum(y,alfa,beta)$alfa1
  beta1<-eg.sum(y,alfa,beta)$beta1
  ala<-0
  if(n==1)
    yla<-max(alfa/beta+4*alfa/beta**2,alfa1/beta1+4*alfa1/beta1**2,1.5/mean(y),mean(y)+4*1,qgamma(0.99,alfa1,beta1),qgamma(0.99,alfa,beta))
  else
    yla<-max(alfa/beta+4*alfa/beta**2,alfa1/beta1+4*alfa1/beta1**2,1/mean(y)+4*sqrt(var(y/n)),mean(y)+4*sqrt(var(y/n)),qgamma(0.99,alfa1,beta1), qgamma(0.99,alfa,beta))
  x<-seq(max(0.001,ala),yla+max(2,alfa1/beta1**2),length=300)
  post<-dgamma(x,alfa1,beta1)
  prior<-dgamma(x,alfa,beta)
  usk<-dgamma(x,n+1,s)
  top<-1.1*max(post,prior,usk)
  plot(x,post,type="l",col="1",xlim=c(ala,yla),ylim=c(0,top),main="Posterior (black), prior (red), lkhd (green)", ylab="density")
  par(new=T)
  plot(x,prior,type="l",col="2",xlim=c(ala,yla),ylim=c(0,top),ylab="")
  par(new=T)
  plot(x,usk,col="3",type="l",xlim=c(ala,yla),ylim=c(0,top),ylab="")
}