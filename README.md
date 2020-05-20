# oLibrary
Librería personal, *oLibrary* es por *Olito's Library* .

La idea principal es contar con set de funciones para hacer estadística exploratoria, gráficos y tablas de forma sencilla y con un output fácil de procesar en forma de `data.frame` y tablas que se autocopian al portapapeles para pasarlas a Excel.

## Instalación

La mejor forma será usar `devtools` para cargar desde github.com la vuelta sería la siguiente. Ojo que el devtools demora harto en instalar.

```R
# Instalar y cargar devtools
install.packages("devtools", type = "source")
library("devtools")

# Instalar
install_github("olitroski/olibrary")
library("olibrary")
help(package = "olibrary")
```
Puede ser necesario tener instalado previamente **Rtools** en el caso de Windows o **Xcode** para MacOS. En caso de no poder instalar por obtener errores se pueden hacer los siguientes chequeos.

* La versión de R es muy moderna: R 4.0 tiene problemas al dia de hoy con devtools, instalar mejor la versión 3.6.3
* En ocasiones por temas de permisos y actualizaciones no se permite borrar automaticamente un paquete, generalmente se puede re-instalar manualmente con install.packages(lib, type = "source)
* Preferir install.packages(lib, type = "source") porque asi no compila o instala librerias muy modernas
* Si todo falla, borrar todos los paquetes y reinstalar. En el caso de Windows en la carpeta documentos hay una carpeta R que dentro tiene las librerías, hay que borrar, luego desinstalar el R y reinstalar uno nuevo.
* Si ya nada funciona, descargar el repositorio e instalar manualmente.

## Funciones incluidas

Son muchas que iré subiendo a poco. Las variables se ingresan como un **string** a propósito, elegí no usar Non-Standard Evaluation porque en la práctica tendría que modificar algunos script antiguos.

```R
# Una variable
"mpg"

# Varias variables
c("mpg", "cyl", "am")
```

Cualquier cosa o más detalles **revisar la documentación de las funciones**.

### Data Management

#### okeep

```R
okeep <- function(keep = NULL, dframe = FALSE, fun = FALSE, char = FALSE, num = FALSE)
```

Similar a la función `keep` de Stata. Permite ingresar una o más variables o un tipo de objeto para borrar todo el resto.

#### omerge

```R
omerge <- function(xdf = NULL, ydf = NULL, byvar = NULL, keep = FALSE, output = TRUE)
```

Es un wrapper de un merge para que entregue un reporte de pertenencia de las observaciones, similar al de Stata. Se hizo de nuevo y quedó más ágil, pero conserva elementos para retro compatibilidad.

Es necesario que `keep = TRUE` para que se guarden los datos.

```
--- Reporte variables merge --- 
Key:    cars
Master: mpg, cyl, disp, hp, drat, wt 
Using:  qsec, vs, am, gear, carb 

               Status Conteo
       Only in master      7
        Only in using      8
 Matched observations     17
        --- Total ---     32
```

#### ordervar

```R
ordervar <- function(datos, varmove, after = 1)
```

Análogo a `order` de Stata. Mueve variables para ordenar una base de datos, se puede ingresar un vector de texto para varias variables, incluye un after que puede ser numero o texto

### Estadísticas

#### osumm

```R
osumm(numvar, grpvar = NULL, data, rnd = 2, clip = FALSE)
```

Calcula estadísticas descriptivas para varias variables, agrupadas o no. Solo eso. Variable corresponde a las variables numéricas y si hay categorías se informan a continuación.

```
  variable am   mean     sd median  N miss    SW
       mpg  0  17.15   3.83   17.3 19    0 0.899
       mpg  1  24.39   6.17   22.8 13    0 0.536
      disp  0 290.38 110.17  275.8 19    0 0.424
      disp  1 143.53  87.20  120.3 13    0 0.003
```

La última variable es un Shapiro-Wilks que hace el cálculo si el `N < 5000` porque daría error, `clip` es para que se pegue al porta papeles del sistema, y `rnd` es para el redondeo.

#### otable

```R
otable(rvar = NULL, cvar = NULL, data = NULL, clip = 0)
```

Calcula una tabla de frecuencias o una de contingencia en caso de agregar dos variables, además calcula un chi-cuadrado y porcentajes por columna, fila y total.

`clip` es para que mostrar solo un segmento en el caso de tablas de contingencia, si queda en cero muestra todo. En el caso de una variable `clip = 1` hace la copia.

```
    cyl  am.0  am.1 total
      4 0.273 0.727 1.000
      6 0.571 0.429 1.000
      8 0.857 0.143 1.000
  total 0.594 0.406 1.000
```

Por ejemplo los porcentajes por fila