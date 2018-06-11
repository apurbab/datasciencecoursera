# Calculate K (Capital requirement for a consumer loan)

b<-function(pd){
  a1<-(0.11852-0.05478*(log(pd,base=exp(1)))^2)
}
correlasstes<-function(){
  
r1<-0.03*(1-exp(-35*pd))/(1-exp(-35))+0.16*(1-(1-exp(-35*pd))/(1-exp(-35)))
  
}  

calck<-function(lgd,pd,m){
  r=correlasstes()
  a<-(1-r)^(-0.5)
  b<-(r/(1-r))^0.5
  c<-qnorm(pd,mean=0,sd=1,log=false)
  d<-qnorm(0.999,mean=0,sd=1,log=false)
  
  k=(lgd*pnorm((a*c)+(b*d),mean=0,sd=1)-pd*lgd) * (((1-1.5*b(pd))^-1)*(1+(m-2.5))*b(pd))
  
}

