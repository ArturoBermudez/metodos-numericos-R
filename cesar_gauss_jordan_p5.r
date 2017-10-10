a<-matrix(c(1,1,1,1,20,3,5,1,4,64,9,-6,-1,1,-10,8,3,1,-7,-22), nrow=4, ncol=5, byrow=T)
n=nrow(a)           # numero de renglones=incognitas
n
for (k in 1:n) {
   print(a)
   a[k,]=a[k,]/a[k,k]
   for (m in 1:n) {
       if (m!=k) {
          mt = a[m,k]
          a[m,]=a[m,]-mt*a[k,]
       }
   }
}
print(a)
