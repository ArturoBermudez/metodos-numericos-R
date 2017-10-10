x<-c(2.3,2.5,3.3,5.1)
fx<-c(9.97,12.18,27.11,164.02)
n=length(x)
V<-array(0,dim=c(n,n))
for (k in 1:n) {
    V[,k]=x^(k-1)          # matriz de Vandermonde
}
V
A=solve(V)%*%fx
A=t(A)
A
T<-seq(x[1],x[n],by=0.01)
t=t(T)

#forma 1
p<-polynomial(A)
p
plot(p,xlim =c(x[1],x[n]),type="l",len=100,col=c("blue"),
       xlab="x", ylab="f(x)", main="Polinomio Interpolador de Vandermonde")
lines(t,exp(t),col=c("red"),lwd=2.5)
points(x,fx,type="p",col=c("green"),pch=19)
legend(x[1], exp(x[n]), c("Vandermonde", "Exponencial","Puntos dados"), 
       lwd=c(2.5,2.5,NA), col=c("blue", "red","green"), lty=c(1,1,NA),
       pch=c(NA,NA,19), merge=TRUE, bg="lightyellow")
lines(x[3],50,col="green")

