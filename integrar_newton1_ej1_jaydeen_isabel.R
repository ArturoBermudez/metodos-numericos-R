# jaydeen lizarraga lepe e isabel angelica garcia monasterio
a1=0
a2=7
puntos=8
x<-seq(a1,a2,(a2-a1)/puntos)
fx<-sqrt(43*x^3)*sin(.3+exp(-x))
n=length(x)
x
fx
tb<-array(0,dim=c(n,(n+1)))
multi<-array(0,dim=c(n,n))
A<-array(0,dim=c(n,1))
pol<-array(0,dim=c(n,n))
orden<-array(0,dim=c(1,n))
newton<-array(0,dim=c(1,n))
cn<-array(0,dim=c(1,(n-1)))
for (k in 1:n) {
      tb[k,1]=x[k]
      tb[k,2]=fx[k]
      tb[k,(3:(n+1))]=0  }
for (k in 1:(n-1)) {
   for (m in 1:(n-k)) {
       num=tb[(m+1),(k+1)] - tb[m,(k+1)]
       den=tb[(m+k),1] - tb[m,1]
       tb[m,(k+2)]=num/den  }   }
newton=tb[1,(2:(n+1))]
for (k in 1:n) {
   orden=x[1:(k-1)] 
   cn=t(coef(poly.calc(orden)))
   if (k==1) { cn[1,1]=cn[1,2] }
   pol=newton[k]*cn  
   multi[k,(1:k)]=pol[1,(1:k)]  }
for (k in 1:n) { A[k]=sum(multi[,k]) }
#incre=0.01
incre=0.5
A
p<-polynomial(A)
p
T<-seq(min(x),max(x),incre)
t=t(T)
mr=length(t)
mr
y <- t*0+A[1];
for (i in 1:mr) {
    for (k in 2:(puntos+1)) {  y[i]= y[i]+A[k]*t[i]^(k-1)   }   }

T2<-seq(min(x),max(x),0.01)
t2=t(T2)
mr2=length(t2)
y2 <- t2*0+A[1];
y2
for (i in 1:mr2) {
    for (k in 2:(puntos+1)) {  y2[i]= y2[i]+A[k]*t2[i]^(k-1)   }   }

areaTT=0
areaRR=0
y
areaR<-c()
areaT<-c()
for (i in 1:(mr-1)) { 
    areaRR=areaRR+incre*y[i]
    areaR[i]=incre*y[i]
    areaTT=areaTT+((y[i]+y[i+1])*incre/2)
    areaT[i]=((y[i]+y[i+1])*incre/2)
}

t
par(mfrow=c(1,2))

plot.new()
plot.window(xlim =c(a1,a2),ylim=c(0,40))
axis(1, at=a1:a2, las=1 )
axis(2, at=seq(0,40,5), las=1)
title(xlab="x")
title(ylab="y")
ex1<-expression(sqrt(43*x^3)*sin(.3+e^(-x)))
rect(seq(0,6.5,.5),0,seq(0.5,7,.5),y[seq(1,(7/.5),1)],col="gray90",border="gray70")
segments(0,0,7,0,col="gray70")
lines(t2,y2,col="red",lwd=2.5, type="l")
title(main=ex1)
title(sub="Método de rectangulos")
points(t,y,col="green",pch=19)
legend("topright", paste("Area=",round(areaRR,4)), bg="lightyellow")
text(seq((incre/2),(a2-(incre/2)),incre),0.1,round(areaR,4),srt=90,col="dark red",adj=0)
text(seq((incre/2),(a2-(incre/2)),incre),6,round(cumsum(areaR[seq(1,(a2/incre),1)]),4),srt=90,col="dark blue",adj=0)

plot.new()
plot.window(xlim =c(a1,a2),ylim=c(0,40))
axis(1, at=a1:a2, las=1 )
axis(2, at=seq(0,40,5), las=1)
title(xlab="x")
title(ylab="y")
ex1<-expression(sqrt(43*x^3)*sin(.3+e^(-x)))
segments(seq(0,7,.5),0,seq(0,7,.5),y,col="gray90",lty=1)
segments(seq(0,6.5,.5),y[seq(1,14,1)],seq(0.5,7,.5),y[seq(2,15,1)],col="gray90",lty=1)
segments(0,0,7,0,col="gray70")
#lines(t2,y2,col="red",lwd=2.5, type="l")
title(main=ex1)
title(sub="Método de trapecios")
points(t,y,col="green",pch=19)
legend("topright", paste("Area=",round(areaTT,4)),bg="lightyellow")
text(seq((incre/2),(a2-(incre/2)),incre),0.1,round(areaT,4),srt=90,col="dark red",adj=0)
text(seq((incre/2),(a2-(incre/2)),incre),6,round(cumsum(areaT[seq(1,(a2/incre),1)]),4),srt=90,col="dark blue",adj=0)
