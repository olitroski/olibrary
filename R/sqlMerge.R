#' @title Olito's SQLite Merge for large files
#' @description Merge implementation in SQLite for dealing with memory issues when normal merge functions get out of RAM
#' @param x Xdf data.frame class
#' @param y Ydf data.frame class
#' @param by Key variable, same type and name accross Xdf and Ydf
#' @param drop Logical for remove original Xdf and Ydf
#' @param replace Logical ovewrite an existing db file
#' @return data.table object
#' @export
#' @examples
#' # library(RSQLite)
#' # library(DBI)
#' # library(glue)
#' # library(data.table)
#' # temp <- mtcars
#' # temp$id <- row.names(temp)
#' # row.names(temp) <- NULL
#' # yDF <- temp[1:26, c(12, 1:5)]
#' # xDF <- temp[7:32, c(12, 6:11)]
#' # sqlMerge(xDF, yDF, by = 'id', replace = TRUE)
#' @importFrom glue glue
#' @importFrom data.table fifelse
#' @importFrom data.table setcolorder
#' @importFrom data.table setnames
#' @importFrom data.table :=
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbExecute
#' @importFrom DBI dbConnect
#' @importFrom DBI dbListTables
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbGetQuery
#' @importFrom DBI dbDisconnect
#' @importFrom methods is
#'
sqlMerge <- function(x = NULL, y = NULL, by = NULL, drop = TRUE, replace = FALSE){
    mergeX <- mergeY <- temp <- NULL

    # Testear la clase porque debe venir como
    if (length(class(x)) > 1 | length(class(y)) > 1){
        stop("clase > 2, solo debe ser data.frame")
    # } else if (class(x) != "data.frame" | class(y) != "data.frame"){
    } else if (!is(x, "data.frame") | !is(y, "data.frame")){
        stop("clase debe ser data.frame")
    }

    # Guardar nombre objetos (se debe hacer al inicio)
    xname <- toString(substitute(x))
    yname <- toString(substitute(y))

    # Agregar marcador
    x[["mergeX"]] <- "x"
    y[["mergeY"]] <- "y"

    # Crear base de datos
    dbName <- glue("{xname}-{yname}_by-{by}.db")

    if (file.exists(dbName) & replace == TRUE){
        cat('Overwriting dbFile...\n')
        conn <- dbConnect(SQLite(), dbName)
        tablas <- dbListTables(conn)
        for (t in tablas){
            dbExecute(conn, glue("drop table {t}"))
        }

    } else if (file.exists(dbName) & replace == FALSE){
        stop('dbFile exists, use option "replace = TRUE"')

    } else if (file.exists(dbName) == FALSE & (replace == FALSE | replace == TRUE)){
        cat('Creating dataBase...\n')
        conn <- dbConnect(SQLite(), dbName)

    } else {
        stop('Unexpected error :)')
    }

    # Pasar 'x' e 'y' a una db y borrarlos si 'drop'
    dbWriteTable(conn, "x", x)
    dbWriteTable(conn, "y", y)
    cat("dbTableList:", paste(dbListTables(conn), collapse = ", "), "\n")

    # Borrar si es que
    if (drop == TRUE){
        cat("Removing objects...\n")
        eval(parse(text = glue("rm({xname}, envir = .GlobalEnv)")))
        eval(parse(text = glue("rm({yname}, envir = .GlobalEnv)")))
        gc()
    }

    # Merge
    query <- glue("
    create table merge as
    	SELECT x.*, y.*
    	FROM   x
    	LEFT JOIN y
    	ON x.{by} = y.{by}
    	UNION ALL
    	SELECT x.*, y.*
    	FROM   y
    	LEFT JOIN x
    	ON x.{by} = y.{by}
    	WHERE x.{by} IS NULL
	")
    cat("dbExecute merge...\n")
    dbExecute(conn, query)

    # ---- Hermoseando -----------------------------------------
    cat('Tidying db...\n')
    dataMerge <- dbGetQuery(conn, "select * from merge")
    dbDisconnect(conn)

    # Consolidar merge
    setDT(dataMerge)
    dataMerge[, merge := fifelse(is.na(mergeX) == FALSE & is.na(mergeY) == TRUE,
                                 'Only in X',
                                 fifelse(is.na(mergeX) == TRUE & is.na(mergeY) == FALSE,
                                         'Only in Y', 'Match'))]

    # Consolidar ID
    id2 <- as.character(glue("{by}:1"))
    dataMerge[, temp := ifelse(merge == 'Only in Y', get(id2), get(by))]

    # Reordenar
    dataMerge[, c(by, id2, 'mergeX', 'mergeY') := NULL]

    varNames <- names(dataMerge)
    varNames <- varNames[-grep('temp', varNames)]
    varNames <- c('temp', varNames)
    setcolorder(dataMerge, varNames)

    setnames(dataMerge, 'temp', by)

    # reporte
    report <- dataMerge[, .N, by = merge]
    print(report)
    return(dataMerge)

}

