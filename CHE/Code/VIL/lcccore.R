f1=function(x){
  (alpha*m*x^(m-1)*exp(-alpha*x^m))^2
}

f2=function(y){
  (exp(alpha*y^m-rho*y))
}

#q<-matrix(double(1),nrow=1,ncol=T)
C<-matrix(double(1),nrow=1,ncol=T)
canduoi=0
cantren=Inf

#library(pracma)
for (i in 1:length(T)){
  # timestep=i/d
  v1=integrate(f1,lower=canduoi,upper=Inf)
  v2=integrate(f2,lower=0,upper=T[i])
  C[i]=(exp(-rho*T[i])*(pi)+(0/rho)*(1-exp(-rho*T[i]))+(ci)*v1$value*v2$value)/(1-exp(-rho*T[i]))
}

Min_C<-min(C[c(10:length(T))])
T_optimal<-which.min(C)/d

cat("Minimum impact: ")
print(Min_C)
cat("Optimal Time Window for PI:")
print(T_optimal)


