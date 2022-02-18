#' @title Olito Text Cleaner
#' @description Función para limpiar texto, aplica: minuscula, espacios externos sobrantes, puntuaciones, letras solas = TRUE, tides + ñ, números.
#' @param txt texto a limpiar
#' @param solas logical para sacar letras guachas, default en FALSE
#' @return char texto limpio
#' @export
#' @examples
#' # cleanText("   25#%Olá,k--     :A1S29É...    ")
#' # cleanText("   25#%Olá,k--     :A1S29É...    ", solas = TRUE)
#' # cleanText(c("Olá4  ", " k... ", "Asé"))
#' @import stringr
#'
cleanText <- function(txt, solas = FALSE){
    text <- tolower(txt)
    text <- str_replace_all(text, "[[:punct:]]", " ")
    text <- str_replace_all(text, "[[:digit:]]", "")

    # letras solas rodeadas de espacio
    if (solas == TRUE){
        text <- str_replace_all(text, "\\s[bcdefghjklmnopqrstuvwxyz]\\s", "")
    }

    # Acentos y enie
    text <- str_replace_all(text, "\u00E1", "a")
    text <- str_replace_all(text, "\u00E9", "e")
    text <- str_replace_all(text, "\u00ED", "i")
    text <- str_replace_all(text, "\u00F3", "o")
    text <- str_replace_all(text, "\u00FA", "u")
    text <- str_replace_all(text, "\u00F1", "n")

    # espacios sobrantes
    text <- str_squish(text)
    return(text)
}

