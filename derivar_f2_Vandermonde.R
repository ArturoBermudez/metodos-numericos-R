a1=-5
a2=5
puntos=8
x<-seq(a1,a2,(a2-a1)/puntos)
fx<-exp(-x^2)
x
fx
n=length(x)
V<-array(0,dim=c(n,n))
for (k in 1:n) {
    V[,k]=x^(k-1)   }       # matriz de Vandermonde
A=solve(V)%*%fx
A=t(A)
T<-seq(min(x),max(x),by=0.01)
t=t(T)
p<-polynomial(A)
p
B<-A*0
for (i in 1:(length(A)-1)) { B[i]=A[i+1]*i }
A
B
p2<-polynomial(B)
plot(p2,xlim =c(x[1],x[n]),lty=2,ylim=c(-10,10),lwd=2.5,
        type="l",len=100,col="black",xlab="x", ylab="f(x)",
        main="Derivacion de polinomio por Vandermonde")
lines(t,(-2*t*exp(-t^2)),col="black",lwd=2.5, type="l")
legend("bottom", c("derivada de la funcion analitica", 
         "derivada del polinomio interpolador"), 
         lwd=c(2.5,2.5), lty=c(1,2),inset=0.1)

