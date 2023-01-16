#' @title Calcula la complejidad de Lempel ZIv permutada
#' @description Función que a partir del alfabeto tradicional (0,1), calcula la complejidad de Lempel Ziv permutada mediante los patrones ordinales
#' @param signal vector de datos con la serie de tiempo a calcular
#' @param embdim embedding o tamaño de los patrones que se desean buscar en la serie de tiempo
#' @return Complejidad de Lempel Ziv permutada normalizada
#'
#'
plzc <- function(signal, embdim){
  opd = ordinal_pattern_time_series(x = signal, ndemb = embdim)
  opd <- opd[!is.na(opd)]
  threshold = mean(opd)
  newSignal = c()
  for (variable in opd) {
    if(variable < threshold){
      newSignal <- append(newSignal, 0)
    }
    else{
      newSignal <- append(newSignal, 1)
    }
  }
  lzc <- lempel.ziv(newSignal, c(0,1))
  n <- length(newSignal)
  denominator <- n/log2(n)
  normalizedLzc <- lzc/denominator

  normalizedLzc

}
