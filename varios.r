# El directorio se crea con... directo al directorio
create("C:/Users/olitr/Desktop/lepack")

# Agrgar librerias la descricion
use_package("dplyr")
use_package("stringr")

# ---- Editar los script ----
# Carga los script a la memoria para edicion y todo. En <build> está también
load_all()

# ---- Test ----
# Agregar carpeta tests al directorio
# import testthat
use_testthat()
context()
# Hay que investigar bien esto


# ---- documentacion ----
# Crea la carpeta man documentar y saca la documentacion desde las descripciones
# de los script. En <build> esta también
document()





