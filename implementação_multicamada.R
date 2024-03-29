sigmoide <- function(soma) {
  return (1/ (1+ exp(-soma)))
}

sigmoideDerivada <- function (sig) {
  return(sig*(1-sig))
}

##a<- sigmoid(50)

entradas<- matrix(c(0,0,0,1,1,0,1,1), nrow = 4, ncol = 2, byrow = TRUE) 
saidas<- matrix(c(0,1,1,0))
pesos0<- matrix(c(-0.424,-0.740, -0.961, -0.358, -0.577, -0.469), nrow = 2, ncol = 3, byrow = TRUE ) 
pesos1<- matrix(c(-0.017, -0.893, - 0.148), nrow = 3, ncol = 1, byrow = TRUE)
epocas<- 100

for (j in 1:epocas) {
camadaEntrada<- entradas
somaSinapse0<- camadaEntrada %*% pesos0
camadaOculta<- sigmoide(somaSinapse0)

somaSinapse1<- camadaOculta %*% pesos1
camadaSaida<- sigmoide(somaSinapse1)

erroCamadaSaida<- saidas - camadaSaida
mediaAbsoluta<- mean(abs(erroCamadaSaida))

derivadaSaida<- sigmoideDerivada(camadaSaida)
deltaSaida<- erroCamadaSaida  
}

