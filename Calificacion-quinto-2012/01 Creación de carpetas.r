######## Crear estructura de carpetas de la calificación mediante la Estimacuión de Áreas Pequeñas ########
## Elaborado: Stalyn Guerrero 
## Proyecto SAE 14-01 - 2016
## Subdirección de Estadística ICFES
#########################################################################
## Función para crear la carpeta  en caso que no exista
safe.dir.create <- function(path) {
  if (!dir.exists(path) && !dir.create(path)) 
    stop(gettextf("cannot create directory '%s'", path), 
         domain = NA)
}

#########################################################################
## Definir el directorio de trabajo donde se realizará el proceso de calificación 
dir <- "C:/Users/sguerrero/Dropbox/investigacion icfes/SAE/SAE.git/SAE/"
## Comprabar la exitencia del directorio 
safe.dir.create(dir) 
## Se crean las capetas necesarias 
safe.dir.create(code_dir <- file.path(dir, "src"))    ## Carpeta de funciones
safe.dir.create(code_dir <- file.path(dir, "src/ETC"))
safe.dir.create(code_dir <- file.path(dir, "src/Colegio"))
safe.dir.create(code_dir <- file.path(dir, "src/Funciones"))
safe.dir.create(docs_dir <- file.path(dir, "input"))  ## Bases de datos ha emplear 
safe.dir.create(docs_dir <- file.path(dir, "input/ETC")) 
safe.dir.create(docs_dir <- file.path(dir, "input/ETC/Covariable")) 
safe.dir.create(docs_dir <- file.path(dir, "input/ETC/Covariable"))
safe.dir.create(docs_dir <- file.path(dir, "input/ETC/Varianza")) 
safe.dir.create(docs_dir <- file.path(dir, "input/ETC/Base"))
safe.dir.create(docs_dir <- file.path(dir, "input/Colegio")) 
safe.dir.create(docs_dir <- file.path(dir, "input/Colegio/Covariable")) 
safe.dir.create(docs_dir <- file.path(dir, "input/Colegio/Covariable/Estudiantes")) 
safe.dir.create(docs_dir <- file.path(dir, "input/Colegio/Varianza")) 
safe.dir.create(docs_dir <- file.path(dir, "input/Colegio/Base"))
safe.dir.create(data_dir <- file.path(dir, "output")) ## Salidas (resultados, graficas, etc)
safe.dir.create(data_dir <- file.path(dir, "doc")) ## Documento 

file.copy( file.path(dir,"01 Creación de carpetas.r"),file.path(dir, "src/Funciones"))

