#perceptron
entrada<- c(-1, 7, 5)
peso<- c(0.8, 0.1, 0)

soma<- function(e,p){
s<- 0
for(i in 1:3){
  print(entrada [i])
  print(peso [i])
s<- s + (e[i]*p[i])
  }
return(s)
}

soma_dot <- function(e,p){
  return(e %*% p)
}
s<- soma_dot(entrada,peso)


#Step fuction degree
stepfunction = function( soma){
  if (soma>=1){
    return(1)
  }
  return(0)
}
r<- stepfunction(s)
