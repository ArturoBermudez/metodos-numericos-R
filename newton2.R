x<-c(.5,1,1.5,2,2.5,3)
fx<-c(1.65,1.03,.74,.61,.53,.45)
#x<-c(0.2,0.3,0.4,0.5)
#fx<-c(3.2,3.3,3.4,4.5)
#x<-c(0,1,2.5,3)
#fx<-c(0,0.4,1,1.5)
n=length(x)
tb<-array(0,dim=c(n,(n+1)))
multi<-array(0,dim=c(n,n))
coefi<-array(0,dim=c(n,1))
pol<-array(0,dim=c(n,n))
orden<-array(0,dim=c(1,n))
newton<-array(0,dim=c(1,n))
cn<-array(0,dim=c(1,(n-1)))
for (k in 1:n) {
      tb[k,1]=x[k]
      tb[k,2]=fx[k]
      tb[k,(3:(n+1))]=0
}
for (k in 1:(n-1)) {
   for (m in 1:(n-k)) {
       num=tb[(m+1),(k+1)] - tb[m,(k+1)]
       den=tb[(m+k),1] - tb[m,1]
       tb[m,(k+2)]=num/den
   }
}
tb
newton=tb[1,(2:(n+1))]
for (k in 1:n) {
   orden=x[1:(k-1)] 
   cn=t(coef(poly.calc(orden)))
   if (k==1) { cn[1,1]=cn[1,2] }
   print(cn)
   pol=newton[k]*cn  
   #print(pol)
   multi[k,(1:k)]=pol[1,(1:k)]
}
multi
for (k in 1:n) { coefi[k]=sum(multi[,k]) }
coefi
min(x)
max(x)
T<-seq(min(x),max(x),by=0.01)
t=t(T)
m=length(t)
y <- t*0+coefi[1];
for (i in 1:m) {
    for (k in 2:n) {  y[i]= y[i]+coefi[k]*t[i]^(k-1) }
}
plot(t, y, xlab="x", ylab="Puntos x", main=expression(paste("Polinomio interpolador de Newton")), col=2,type="l", ylim=c(min(y),max(y)))
points(x,fx,type="o",col=3)

