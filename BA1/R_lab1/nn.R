### Computes posterior mean and variance for normal conjugate prior for unkown mean
nn.sum<-function(y,m,w,v)
{
  #nn.sum(data, prior mean, prior variance, known variance /sample variance)
  #nn.sum(data,priorika,priorivarianssi,otosvarianssi)
  #Funktio laskee normaalisen posteriorijakauman parametrit
  #eli keskiarvon ja varianssin.
  #Normaali otos (varianssi tunnettu) ja normaali priori.
  n<-length(y)
  v1<-1/(1/w+n/v)
  m1<-(m/w+sum(y)/v)*v1
  list(Mean=m1, Var=v1)
}

### Plots of conjugate prior and posterior normal distributions
nn.trip<-function(y,m,w,v)
{
  # function for triple plots -  posterior:black  prior:red  likelihood:green
  
  #nn.trip(data,priorika,priorivarianssi,otosvarianssi)
  #Funktio piirtää posteriori-, priori- ja uskottavuusjakauman.
  #Normaali otos (varianssi tunnettu) ja normaali priori.
  v1<-nn.sum(y,m,w,v)$Var
  m1<-nn.sum(y,m,w,v)$Mean
  n<-length(y)
  ala<-min(m1-3*sqrt(v1),m-3*sqrt(w),mean(y)-3*sqrt(v))
  yla<-max(m1+3*sqrt(v1),m+3*sqrt(w),mean(y)+3*sqrt(v))
  x<-seq(ala-max(sqrt(v1),sqrt(w),sqrt(v)),yla+max(sqrt(v1),sqrt(w),sqrt(v)),length=300)
  post<-dnorm(x,m1,sqrt(v1))
  prior<-dnorm(x,m,sqrt(w))
  usk<-dnorm(x,mean(y),sqrt(v/n))
  top<-1.1*max(post,prior,usk)
  plot(x,post,type="l",col="1",xlim=c(ala,yla),ylim=c(0,top),main="Posterior (black), prior (red), lkhd (green)", ylab="density")
  par(new=T)
  plot(x,prior,type="l",col="2",xlim=c(ala,yla),ylim=c(0,top),ylab="")
  par(new=T)
  plot(x,usk,type="l",col="3",xlim=c(ala,yla),ylim=c(0,top),ylab="")
}

### Highest density intervals
nn.hdi<-function(y,m,w,v,p)
{
  #nn.hdi(data,priorika,priorivarianssi,otosvarianssi,HDI-alueen ala)
  #Funktio laskee 100p prosentin HDI:n posteriorijakaumalle.
  #Normaali otos (varianssi tunnettu) ja normaali priori.
  v1<-nn.sum(y,m,w,v)$Var
  m1<-nn.sum(y,m,w,v)$Mean
  e<-sqrt(v1)/1000
  t<-0
  a<-m1
  b<-a
  if(p<1)
  {
    while (t<p)
    {if (dnorm(a-e,m1,sqrt(v1))>dnorm(b+e,m1,sqrt(v1)))
    {t<-t+e*dnorm((2*a-e)/2,m1,sqrt(v1))
    a<-a-e}
      else
      {t<-t+e*dnorm((2*b+e)/2, m1, sqrt(v1))
      b<-b+e}}
    list(Ala=a,Yla=b)
  }
  else ("HDI-alueen pinta-alan pitää olla pienempi kuin 1")
}

# Predictive distribution for next observation
nn.pred<-function(y,m,w,v)
{
  #nn.pred(data,priorika,priorivarianssi,otosvarianssi)
  #Funktio laskee prediktiivisen posteriojakauman parametrit.
  #Normaali otos (varianssi tunnettu) ja normaali priori.
  v1<-nn.sum(y,m,w,v)$Var
  m1<-nn.sum(y,m,w,v)$Mean
  vpred<-v1+v
  mpred<-m1
  list(Predmean=mpred,Predvar=vpred)
}