x<-c(1,3,6,8,9,11,13)
fx<-c(16,31,12,21,9,42,19)
n=length(x)
V<-array(0,dim=c(n,n))
for (k in 1:n) {
    V[,k]=x^(k-1)          # matriz de Vandermonde
}
V
A=solve(V)%*%fx
A=t(A)
A
par(mfrow=c(1,2)) 
T<-seq(x[1],x[n],by=0.01)
t=t(T)
m=length(t)
y <- t*0+A[1];
m
n
for (i in 1:m) {
    for (k in 2:n) {
       y[i]= y[i]+A[k]*t[i]^(k-1)
    }
}
print(round(max(y)))
plot(x, fx, xlab="x", ylab="fx", main=expression(paste("funcion")), type="l", col=3, ylim=c(min(y),max(y)))
points(x,fx,type="p",col=3)
plot(t, y, xlab="x", ylab="Puntos x", main=expression(paste("Polinomio interpolador de Vandermonde ")), type="l", col=2,ylim=c(min(y),max(y)))
points(x,fx,type="o",col=3)
