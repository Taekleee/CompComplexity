library(statcomp)
#' @title Calcula la entropía permutada
#' @description Función que a partir de los patrones ordinales que tiene la señal, calcula la entropía permutada
#' @param signal vector de datos con la serie de tiempo a calcular
#' @param embdim embedding o tamaño de los patrones que se desean buscar en la serie de tiempo
#' @return Entropía permutada
#'

pe <- function(signal, embdim){
  opd = ordinal_pattern_distribution(x = signal, ndemb = embdim)
  permutation_entropy(opd)
}
