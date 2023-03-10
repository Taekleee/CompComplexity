#' @title Calcula la complejidad de Lempel Ziv normalizada, con el alfabeto que se desee
#' @description Función que a partir del alfabeto ingresado calcula la complejidad de Lempel Ziv
#' @param s vector de datos con la serie de tiempo a calcular
#' @param alphabet alfabeto que se desea utilizar para el cálculo de la complejidad de Lempel Ziv
#' @return Complejidad de Lempel Ziv sin normalizar
#'
lempel.ziv <- function(s,
                       alphabet= c(0,1) ){
  n = sum(!is.na(s))
  s = s[!is.na(s)]
  if(sum(s%in%alphabet) != n){
    stop("Alphabet error")
  }
  voc = s[1]
  cmpl = 1
  r = 1
  i = 1
  while(r+i <= n){
    q = ""
    repeat{
      q = paste(q, s[r+i], sep="")
      if( q %in% voc){
        cmpl[r+i] = cmpl[r+i-1]
        i = i+1
      }
      if(!(q %in% voc) || !(r+i <= n)){
        break
      }
    }
    if(r+i > n){
      break
    }
    voc = c(voc, q)
    cmpl[r+i] = cmpl[r+i-1] + 1
    r = r+i
    i = 1
  }
  return(length(voc))

}
