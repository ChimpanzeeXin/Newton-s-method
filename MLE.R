#Simulate censored data~Exp(1/5)
#R is the indicator function:
#If Yi>10, R=0(censored)
setseed(123)
Y<-rexp(30,0.2)
R<-ifelse(Y>10,0,1)
Y[R==0]=10

#derive the 1st and 2nd derivative of Log-likelihood function,
#p is the parameter that to be estimated
ln<-function(p,Y,R){
  m<-sum(R==1)
  ln<-m*log(p)-p*sum(Y)
  attr(ln,'gradient')<-m/p-sum(Y)#1st derivative
  attr(ln,'hessian')<--m/(p^2)#2nd derivative
  ln
}

#Updating step of Newton's method
#pnew is the updated parameter
newmle<-function(p,ln,...){
  l<-ln(p,...)
  pnew<-p-attr(l,'gradient')/attr(l,'hessian')
  pnew
}

#plot and output of iterative values
#p--initial value;times--iteration times
#epsilon--the tolerance of when to stop
library(animation)
x<-seq(0,0.6,0.01)
plot(x,attr(ln(x,Y,R),"gradient"),type="l",col='blue',
     xlab=expression(theta),ylab="Score function",ylim=c(-50,160))
abline(0,0)

plot_iterative<-function(p,time_interval,times,epsilon){
  oopt = ani.options(interval = time_interval, nmax =times)
  for (i in 1:ani.options("nmax")){
    y=attr(ln(p,Y,R),"gradient")+attr(ln(p,Y,R),'hessian')*(x-p)
    p<-newmle(p,ln,Y=Y,R=R)
    lines(x,y)
    print(p)
    if(abs(attr(ln(p,Y,R),"gradient"))<epsilon)
      break
    ani.pause()
  }
}
plot_iterative(0.3,1,10,0.01)


