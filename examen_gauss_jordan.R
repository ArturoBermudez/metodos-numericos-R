a<-matrix(c(4,-9,2,5,2,-4,6,3,1,-1,3,4), nrow=3, ncol=4, byrow=T)
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
