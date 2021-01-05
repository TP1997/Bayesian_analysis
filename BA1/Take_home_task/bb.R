#  beta(p,q):  p=alpha, q= beta

bb.sum<-function(y,z,alfa,beta)
{
  # binomial - beta conjugate pairs
  #bb.sum(data, maximum value of the observation, prior parameter_a,prior parameter_b)
  #The function gives the parameters of the posterior distribution (Beta)  
  
  #bb.sum(data,havainnon maksimiarvo,priorialfa,prioribeta)
  #Funktio laskee posteriorijakauman parametrit (betajakauma)
  #sekä keskiarvon, varianssin ja moodin.
  #Binomiotos, priori betajakauma.
  n<-length(y)
  s<-sum(y)
  m<-n*z
  if(z>=max(y) & alfa>0 & beta>0 & min(y)>=0 & max(abs(y-round(y,digits=0)))==0
     & z-round(z,digits=0)==0)
  {
    alfa1<-alfa+s
    beta1<-beta+m-s
    ka<-alfa1/(alfa1+beta1)
    var<-alfa1*beta1/(alfa1+beta1)**2/(alfa1+beta1+1)
    mo<-min(1,max(0,(alfa1-1)/(alfa1+beta1-2)))
    list(alfa1=alfa1,beta1=beta1,Mean=ka,Var=var,Mode=mo)
  }
  else
    ("Datan arvojen pitää olla ei-negatiivisia kokonaislukuja ja pienempiä tai yhtä suuria kuin ")
  #maksimiarvo, alfan ja betan pitää olla positiivisia lukuja")
}

bb.hdi<-function(y,z,alfa, beta,p)
{
  # function for the 100p % highest posterior density interval
  
  #bb.hdi(data,havainnon maksimiarvo,priorialfa,prioribeta,HDI-alueen ala)
  #Funktio laskee 100p prosentin HDI:n posteriorijakaumalle, kun
  #uskottavuusfunktio on binominen ja priori betajakauma.
  alfa1<-bb.sum(y,z,alfa,beta)$alfa1
  beta1<-bb.sum(y,z,alfa,beta)$beta1
  e<-sqrt(alfa1/beta1)/1000
  t<-0
  a<-bb.sum(y,z,alfa,beta)$Moodi
  b<-a
  if(p<1)
  {
    while (t<p)
    {if (dbeta(a-e,alfa1,beta1)>dbeta(b+e,alfa1,beta1))
    {t<-t+e*dbeta((2*a-e)/2,alfa1,beta1)
    a<-a-e}
      else
      {t<-t+e*dbeta((2*b+e)/2, alfa1,beta1)
      b<-b+e}}
    list(Ala=a, Yla=b)
  }
  else ("HDI-alueen pinta-alan pitää olla pienempi kuin 1")
}

bb.trip<-function(y,z,alfa,beta)
{
  # function for triple plot -  posterior:black  prior:red  likelihood:green
  
  #bb.trip(data,havainnon maksimiarvo,priorialfa,prioribeta)
  #Funktio piirtää posteriori-, priori- ja uskottavuusjakauman.
  #Binomiotos, priori betajakauma.
  if(alfa>=1 & beta>=1)
  {
    n<-length(y)
    s<-sum(y)
    alfa1<-bb.sum(y,z,alfa,beta)$alfa1
    beta1<-bb.sum(y,z,alfa,beta)$beta1
    kapr<-alfa/(alfa+beta)
    varpr<-alfa*beta/((alfa+beta)**2*(alfa+beta+1))
    ala<-max(0,min(bb.sum(y,z,alfa,beta)$Mean-4*sqrt(bb.sum(y,z,alfa,beta)$Var),kapr-4*sqrt(varpr)))
    yla<-min(1,max(bb.sum(y,z,alfa,beta)$Mean+4*sqrt(bb.sum(y,z,alfa,beta)$Var),kapr+4*sqrt(varpr)))
    x<-seq(0.00001,0.99999,length=300)
    x2<-seq(0,z*n,by=1)
    post<-dbeta(x,alfa1,beta1)
    prior<-dbeta(x,alfa,beta)
    usk<-dbinom(x2,z*n,mean(y)/z)
    top<-1.1*max(post,prior,usk)
    if (z*n>10)
    {
      plot(x,post,type="l",col="1",xlim=c(ala,yla),ylim=c(0,top),main="Posterior (black), prior (red), lkhd (green)", ylab="density")
      par(new=T)
      plot(x,prior,type="l",col="2",xlim=c(ala,yla),ylim=c(0,top),ylab="")
      par(new=T)
      plot(x2/z/n,usk*z*n,col="3",type="l",xlim=c(ala,yla),ylim=c(0,top),xlab="",ylab="")
    }
    else
    {
      top<-1.1
      plot(x,post/max(post,prior),type="l",col="1",xlim=c(ala,yla),ylim=c(0,1.1),main="Posterior (scaled, black), prior (scaled, red), lkhd (green)", ylab="density")
      par(new=T)
      plot(x,prior/max(post,prior),type="l",col="2",xlim=c(ala,yla),ylim=c(0,1.1),ylab="")
      par(new=T)
      plot(x2/z/n,usk,col="3",pch=16,type="b",xlim=c(ala,yla),ylim=c(0,top),xlab="",ylab="")
    }
  }
  else ("Käytä prioria, jossa alfa ja beta ovat vähintään 1")
}
