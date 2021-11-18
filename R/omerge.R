#' @title wrapper de merge estilo stata
#' @description Junta dos bases de datos con un merge y reporteria,
#' si da error puede ser que la version de la funcion se antigua. Entonces usar la opciones old = TRUE
#' asi el output sera como en la version original: una lista. La version old = FALSE es un data.table
#' @param xdf DataFrame, con "idvar" actua de "master"
#' @param ydf DataFrame, con "idvar" actua de "using"
#' @param byvar String, Variable idvar
#' @param keep Logical, si guarda o no el resultado, si no solo da el reporte y un NULL
#' @param output Logical, si muestra o no el output
#' @param old Retro compatibilidad, si TRUE devuelve la lista con el merge separado
#' @return Lista con el merge separado en solo using, master, merge y un reporte, version nueva solo el data.table
#' @export
#' @examples
#' # Datos para cruzar
#' # data <- mtcars
#' # data$cars <- row.names(mtcars)
#' # xdf <- data[-c(1:8),   c("cars", "mpg", "cyl", "disp", "hp", "drat", "wt")]
#' # ydf <- data[-c(26:32), c("cars", "qsec", "vs", "am", "gear", "carb")]
#' #
#' # omerge(xdf, ydf, "cars", keep = FALSE)
#' # omerge(xdf, ydf[, c("cars", "vs")], byvar = "cars")
#' # omerge(xdf, ydf, "cars", output = FALSE, old = TRUE)
#' # omerge(xdf, ydf, "cars", keep = FALSE, output = FALSE)
#' @importFrom data.table :=
#' @importFrom data.table setDT
#' @importFrom data.table merge.data.table
#' @importFrom data.table setnames
#'
omerge <- function(xdf = NULL, ydf = NULL, byvar = NULL, keep = TRUE, output = TRUE, old = FALSE){
    fromX <- fromY <- Status <- NULL

    # Guardar nombres variables
    xnames <- names(xdf)
    ynames <- names(ydf)

    # Agregar marcadores para detectar origen
    setDT(xdf)
    xdf[, fromX := 1]
    setDT(ydf)
    ydf[, fromY := 2]

    # Hacer el merge con todo
    df <- merge.data.table(x = xdf, y = ydf, by = byvar, all = TRUE, sort = FALSE)
    setDT(df)
    df[, merge := ifelse(is.na(fromX), 0, 1) + ifelse(is.na(fromY), 0, 2)]
    df[, merge := factor(merge, levels = c(1,2,3), labels = c("Master", "Using", "Match"))]
    df[, c('fromX', 'fromY') := NULL]

    # --- Tabla reporte ----------------------------
    tabla <- df[, .N, by = merge]
    # tabla <- rbind(tabla, data.frame(merge = "Total", N = sum(tabla[, N])))
    setnames(tabla, 'merge', 'Status')

    # Por si faltan categorias
    labels <- c("Master", "Using", "Match")
    labels <- labels[!(labels %in% tabla$Status)]
    if (length(labels) > 0){
        tabla <- rbind(tabla, data.frame(Status = labels, N = 0, stringsAsFactors = FALSE))
    }

    # Terminar la tabla
    tabla <- tabla[order(Status)]
    tabla[, Status := as.character(Status)]
    tabla <- as.data.frame(tabla)
    tabla[4, "Status"] <- "<Total>"
    tabla[4, "N"] <- sum(tabla$N, na.rm = TRUE)

    # Reporte
    if (output){
        cat("--- Reporte merge --- \n")
        cat("Key:    ", byvar, "\n",sep = "")
        cat("Master: ", paste(xnames[-grep(byvar, xnames)], collapse = ", "), " \n", sep = "")
        cat("Using:  ", paste(ynames[-grep(byvar, ynames)], collapse = ", "), " \n\n", sep = "")
        print(tabla, row.names = FALSE)
        cat("--------------------- \n")
    }

    # Guardar Resultado -- con retro compatibilidad --
    if (keep == FALSE){
        return("Solo muestra reporte...")

    } else if (keep == TRUE & old == TRUE){
        return(list(master = df[merge == "Master"],
                    using  = df[merge == "Using"],
                    match  = df[merge == "Match"],
                    all    = df,
                    report = tabla))

    } else if (keep == TRUE & old == FALSE){
        return(df)
    }
}
