#teste_perceptron
#perceptron III 

entrada<- matrix(c(0,0,0,1,1,0,1,1), nrow = 4,ncol = 2, byrow = T)
saida<-  matrix(c(0,1,1,1))
peso<- matrix(c(0.0,0.0))
taxa_Aprendizagem<- 0.0001

stepFunction = function(calc){
  if(calc >1 ){
    return(1)
  }
  return(0)
}

calc_saida<- function(registro){
  soma<- registro %*% peso
  return(stepFunction(soma))
}

totalerro = 1
while (totalerro!= 0) {
  totalerro<- 0
  
  for (i in 1:length(saida)) { 
    saidaCalculada<- calc_saida(c(entrada[i,]))
    erro<- abs(saida[i] - saidaCalculada)
    totalerro<- totalerro + erro
    for (j in 1:length(peso)) {
      peso[j]<- peso[j]+(taxa_Aprendizagem * entrada[i,j] *erro)
      print(paste('atualizado', peso[j]))

      }
  }
  print(paste('total de erros', totalerro))
}
print('rede treinada')
print(calc_saida(c(entrada[1,])))
print(calc_saida(c(entrada[2,])))
print(calc_saida(c(entrada[3,])))
print(calc_saida(c(entrada[4,])))


       