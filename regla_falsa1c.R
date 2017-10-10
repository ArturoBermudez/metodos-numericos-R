a1=4                   # limite inferior del intervalo
a2=20                  # limite superior del intervalo
r=0
z<-c()                 # vector de los cruces por cero
fx <- function(x) { 
     ecuacion = 1 + 2*x - 3*x^2*exp(-x) + 2*x^3*sin(x)*exp(-x/5) 
     return (ecuacion)  }
for (k in a1:a2) {
   l=k+1
   gk=fx(k)
   gl=fx(l)
   if (gk*gl < 1e-3) {
      Err = 1
      tol = 1e-5
      c = 0
      a = k
      b = l
      while (Err > tol & c < 30) {
         fa=fx(a)
         fb=fx(b)
         h = a - fa*(b-a)/(fb - fa)
         fh=fx(h)
         c = c + 1
         disc = fh*fa
         if (abs(disc) <= tol) {
             Err = 0  
             Cero = h }
         if (disc > tol) {
             a = h
             b = b }
         if (disc < tol) {
             a = a
             b = h }
         Err = abs(disc)  }
         fh=fx(h)
         r = r+1 
         z[r]=h    }   }
z
print(fx(z))
t<-seq(a1,a2,0.01)
y=fx(t)
wr=fx(z)
plot.new()
plot.window(xlim =c(a1,a2),ylim=c(min(y),400))
#title(main="Función")
axis(1, at=4:20, las=1 )
axis(2, at=seq(-300,400,100), las=1)
title(xlab="x")
title(ylab="y")
ex1<-expression(y==1+2*x-3*x^2*e^-x+2*x^3*sin(x)*e^(-x/2))
#abline(v=4:20, h=seq(-300,400,100), col="gray90")
segments(seq(4,20,1),-300,seq(4,20,1),400,col="gray90",lty=1)
segments(4,seq(-300,400,100),20,seq(-300,400,100),col="gray90",lty=1)
abline(h=0,col="black")
lines(t,y,col="red",lwd=2.5, type="l")
#legend(4,360,c("Método de la regla falsa"),col="red",lty=1,lwd=2,bg="lightyellow")
title(main=ex1)
title(sub="Método de la regla falsa")
points(z,wr,col="green",type="p",pch=19)
#text(z,-30,round(z,7),srt=30,col="dark red",adj=0)
#text(z,-30,round(z,7),col="dark blue",adj=0)
text((z+.2),-20,paste("x",seq(0,4,1),sep=""),col="dark blue",adj=0)
legend(4,-100,paste("x",seq(0,4,1),"=",z,sep=""),pch=19,col="green",bg="lightyellow")
wr


