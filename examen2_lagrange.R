x<-c(2.3,2.5,3.3,5.1)
fx<-c(9.97,12.18,27.11,164.02)
n=length(x)
n
fn<-array(0,dim=c(n,(n-1)))
fd<-array(0,dim=c(n,(n-1)))
cn<-array(0,dim=c(n,n))
fac<-array(0,dim=c(n,1))
multi<-array(0,dim=c(n,n))
A<-array(0,dim=c(n,1))
for (k in 1:n) {
    for (m in 1:n) {
       if (k==m) {
          ind=1 }
       else  {
          fn[k,m-ind]=x[m]
          fd[k,m-ind]=x[k]-x[m]
       }
     }
     ind=0
}
fn
fd
for (k in 1:n) {
   cn[k,]=coef(poly.calc(fn[k,]))
   fac[k]=fd[k,1]
   for (m in 2:(n-1)) {
      fac[k]=fac[k]*fd[k,m]
   }
}
cn
fac
print(t(fx))
for (k in 1:n) {
   multi[k,]=cn[k,]%*%t(fx[k])/fac[k]
}
multi
for (k in 1:n) {
   A[k]=sum(multi[,k])
}
A
T<-seq(x[1],x[n],by=0.01)
t=t(T)

#forma 1
p<-polynomial(A)
p
plot(p,xlim =c(x[1],x[n]),type="l",len=100,col=c("blue"),xlab="x", ylab="f(x)",main="Polinomio interpolador de Lagrange")
lines(t,exp(t),col=c("red"),lwd=2.5)
points(x,fx,type="p",col=c("green"),pch=19)
legend(x[1], exp(x[n]), c("Lagrange", "Exponencial","Puntos dados"), 
        lwd=c(2.5,2.5,NA), col=c("blue", "red","green"), lty=c(1,1,NA),
        pch=c(NA,NA,19), merge=TRUE, bg="lightyellow")
#m=length(t)
#y <- t*0+A[1];
#m
#for (i in 1:m) {
#    for (k in 2:n) {
#       y[i]= y[i]+A[k]*t[i]^(k-1)
#    }
#}
#plot(t, y, xlab="x", ylab="Puntos x", main="Polinomio interpolador de Lagrange", col=2,type="l", ylim=c(min(y),max(y)))
#points(x,fx,type="p",col=3)
#par(new=TRUE)
#plot(t, exp(t), type="l")
