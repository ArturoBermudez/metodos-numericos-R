a<-matrix(c(10,1,1,24,-1,20,1,21,1,-2,100,300), nrow=3, ncol=4, byrow=T)                   # matriz extendida (original 4x4 + resultados) = 4x5
n=nrow(a)                  # numero de renglones=incognitas
a                          # matriz aumentada
iter=8                     # 8 iteraciones
k1=diag(a[,(1:n)])         # asigna la diagonal de A a un vector (solo las incognitas)
d <- array(0,dim=c(n,n))   # crea una matriz con ceros de dimensiones n x n
l <- array(0,dim=c(n,n))
u <- array(0,dim=c(n,n))
d=diag(k1)                 # asigna el vector con la diagonal de A a la diagonal de D
a[,(1:n)]=a[,(1:n)]-d      # elimina la diagonal de la matriz A
d=solve(d)                 # calcula la inversa de la matriz D
x <- array(0,dim=c(n,iter)) # crea un vector X de resultados en ceros de dimensiones n x iter
for (k in 1:(n-1)) {       # ciclo para hacer la asignacion de las matrices triangulares
   l[(k:n),k]=a[(k:n),k]
   u[k,(k:n)]=a[k,(k:n)]
}
f=-d%*%(l+u)
g=d%*%a[,(n+1)]
for (k in 2:iter) {         # ciclo iterativo del metodo de Jacobi
   x[,k]=f%*%x[,(k-1)]+g
}
x                            # mostrar los resultados de las iteraciones