#' @title Calcula una tabla de contingencia entre 2 variables
#'
#' @description Funcion que calcula una tabla de contingencia entre dos variables de cualquier tipo las cuales se pasan a factor. Calcula la frecuencia, porcentaje por fila y columna y celda, ademas de test de independencia Chi-cuadrado y prueba exacta de Fisher.
#'
#' @param rvar Variable que ir? en las filas de la tabla
#' @param cvar Variable que ir? en las columnas de la tabla
#' @param data Data Frame que contiene las variables
#' @param clip Opciones de portapapeles, 0 = return, 1 = freq, 2 = row, 3 = col, 4 = col, 5 = chi2, 6 = 1 & 5
#'
#' @return Devuelve una dataframe si 1 var o lista con las 4 tablas y el test si contingencia. Seg?n clip se pasa al clipboard.
#'
#' @export
#'
#' @examples
#' # otable(rvar = "cyl", data = mtcars)
#' # otable(rvar = "cyl", cvar = "am", data = mtcars, clip = 0)
#' # otable(rvar = "cyl", cvar = "am", data = mtcars, clip = 1)
#'
#' @importFrom stats reshape
#' @importFrom utils write.table
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom janitor adorn_totals
#' @importFrom data.table setDT
#' @importFrom data.table .N


otable <- function(rvar = NULL, cvar = NULL, data = NULL, clip = 0){
    # Los bindings
    Freq <- total <- NULL

    # Si es que si lo hay row var (1 variable)
    if (class(cvar) == "NULL"){
        # Tablas
        df <- paste("table(data$", rvar, ")", sep = "")
        df <- eval(parse(text=df))

        freq <- data.frame(df)
        names(freq) <- c(rvar, "freq")
        margin <- data.frame("total", margin.table(df))
        names(margin) <- names(freq)
        freq <- rbind(freq, margin)

        pct <- data.frame(prop.table(df))
        names(pct) <- c(rvar, "pct")
        margin <- data.frame("total", margin.table(prop.table(df)))
        names(margin) <- names(pct)
        pct <- rbind(pct, margin)
        pct <- select(pct, pct)

        df <- cbind(freq, pct)
        df <- mutate(df, pct = round(pct, 3))

        # Resultado
        if (clip == 1){
            write.table(df, "clipboard-128", sep="\t", row.names=FALSE)
        }
        return(df)


        # tablas de 2x2
    } else {
        # Es razonable usar loops porque nunca voy a tener una tabla muy grande
        # Transformar a data.table
        if (length(grep("data.table", class(data))) == 0){
            setDT(data)
        }

        # Tabla madre
        dt <- data[, .N, by = list(get(rvar), get(cvar))]
        names(dt) <- c(rvar, cvar, "N")
        dt <- reshape(dt, timevar = cvar, idvar = rvar, direction = "wide")


        # Tabla de frecuencias  ---------------
        freq <- dt
        freq <- adorn_totals(freq, 'row')
        freq <- adorn_totals(freq, 'col')

        # Porcentaje por columnas -------------
        pctcol <- as.data.frame(dt)
        pctcol <- adorn_totals(pctcol, "col")
        for (j in 2:ncol(pctcol)){
            pctcol[, j] <- pctcol[, j]/sum(pctcol[, j], na.rm = TRUE)
        }
        pctcol <- adorn_totals(pctcol, "row")


        # Porcentaje por filas ---------------
        pctrow <- as.data.frame(dt)
        pctrow <- adorn_totals(pctrow, "row")
        for (i in 1:nrow(pctrow)){
            pctrow[i, 2:ncol(pctrow)] <- pctrow[i, 2:ncol(pctrow)] / sum(pctrow[i, 2:ncol(pctrow)], na.rm = TRUE)
        }
        pctrow <- adorn_totals(pctrow, "col")

        # Porcentaje por celdas --------------
        pctcell <- as.data.frame(dt)
        N <- sum(pctcell[, 2:ncol(pctcell)], na.rm = TRUE)
        pctcell <- adorn_totals(pctcell, "row")
        pctcell <- adorn_totals(pctcell, "col")

        for (i in 1:nrow(pctcell)){
            for (j in 2:ncol(pctcell)){
                pctcell[i, j] <- pctcell[i, j]/N
            }
        }

        # Test de independencia ---------------
        chi <- summary(table(data[[rvar]], data[[cvar]]))
        chi <- data.frame(table = paste(rvar, "*", cvar),
                          ChiSq = sprintf("%.3f", chi$p.value))


        # Resultado
        lista <- list(freq=freq, row=pctrow, col=pctcol, cell=pctcell, pvalue = chi)

        ## Resultados seg?n tipo de clip
        if (clip == 0){
            write.table(lista, "clipboard-128", sep="\t", row.names=FALSE)
            return(lista)

        } else if (clip == 1){
            tab <- lista$freq
            write.table(tab, "clipboard-128", sep="\t", row.names=FALSE)
            return(tab)

        } else if (clip == 2){
            tab <- lista$row
            write.table(tab, "clipboard-128", sep="\t", row.names=FALSE)
            return(tab)

        } else if (clip == 3){
            tab <- lista$col
            write.table(tab, "clipboard-128", sep="\t", row.names=FALSE)
            return(tab)

        } else if (clip == 4){
            tab <- lista$cell
            write.table(tab, "clipboard-128", sep="\t", row.names=FALSE)
            return(tab)

        } else if (clip == 5){
            tab <- lista$pvalue
            write.table(tab, "clipboard-128", sep="\t", row.names=FALSE)
            return(tab)

        } else {
            stop()
        }

    }

}

## Data de prueba
# rvar <- "gear"
# cvar <- "am"
# otable(rvar = "cyl", data = mtcars)
# otable(rvar = "cyl", cvar = "am", data = mtcars)
# tab <- otable(rvar = "cyl", cvar = "am", data = mtcars)
# tab$freq


