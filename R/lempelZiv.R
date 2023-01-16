#' @title Calcula la complejidad de Lempel Ziv normalizada, con el alfabeto tradicional
#' @description Función que a partir del alfabeto tradicional (0,1), usando como umbral la media de la señal
#' @param signal vector de datos con la serie de tiempo a calcular
#' @return Complejidad de Lempel Ziv normalizada
#'
lzc <- function(signal){
  meanLZC <- mean(signal)
  s<-map(signal, function(x) if(x < meanLZC) 0 else 1)
  denominator <- length(s)/log(x=length(s), base=2)
  return(lempel.ziv(s)/denominator)
}
