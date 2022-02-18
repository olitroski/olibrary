#' @title Olito's function para formatear un número
#' @description Formatea numero intercalando comas en los miles
#' @param num Número a formatear
#' @param decimal define el n de decimales a incluir (aproxima)
#' @param sep define el separador de miles, por defecto es punto.
#' @return char con el resultado
#' @export
#' @examples
#' # formatNum(123456789.287)
#' # formatNum("123456789.287")
#' # formatNum("123456789.987", decimal = 1)
#' # formatNum("123456789.987", decimal = 1, sep = ',')
formatNum <- function(num, decimal = 0, sep = '.'){
    # Comprobar clases
    clase <- class(num)
    if (clase == "character"){
        options(warn = -1)
        temp <- as.numeric(num)
        if (is.na(temp) == TRUE){
            stop("Objeto no puede transformarse a numero")
        } else {
            go <- TRUE
        }

    } else if (clase == "numeric"){
        go <- TRUE
    } else {
        stop("El objeto no es clase numeric o character")
    }

    # Fomatea
    if (go <- TRUE){
        if (decimal > 0 & sep == '.'){
            temp <- as.numeric(num)
            temp <- format(temp, nsmall = decimal, big.mark = sep, decimal.mark = ',')
            return (temp)

        } else if (decimal > 0 & sep == ','){
            temp <- as.numeric(num)
            temp <- format(temp, nsmall = decimal, big.mark = sep, decimal.mark = '.')
            return (temp)

        } else {
            temp <- as.numeric(num)
            temp <- format(temp, nsmall = decimal, big.mark = sep)
            return (temp)
        }
    } else {
        stop("Error desconocido...XD")
    }
}

