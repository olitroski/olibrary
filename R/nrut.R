#' @title Olito's function para calcular un distinct en una tabla
#' @description Calcula el numero de elementos diferentes en una variable de un data.frame
#' @param df data.frame o data.table
#' @param var string variable a testear, debe ir entre comillas
#' @param format logical default TRUE para agregar separador de miles
#' @return numero
#' @export
#' @examples
#' # data <- data.frame(id = rep(1:1234, 2), val = runif(1234*2))
#' # nrut(data, 'id')
#' # nrut(data, 'id', format = FALSE)
#'
nrut <- function(df, var, format = TRUE){
    if (format == TRUE){
        return(format(length(unique(df[[var]])), nsmall = 0, big.mark = '.', decimal.mark = ','))
    } else {
        return(length(unique(df[[var]])))
    }
}

