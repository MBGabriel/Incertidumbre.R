#### Ejemplo de aplicación #### 

#'Incertidumbre.R'
#Versión: 2025.01
#Fecha: 2025-12-28
#Autor: G. Morales
#E.mail: <ogmorales@sena.edu.co>; <morales.b.gabriel@gmail.com>
#Dir: Calle 22 # 11E-05. Vía oriente. Pasto, Nariño. Colombia.
#Licencia GPL (>=2)

#Requerimientos:
#La función requiere de los paquetes: "metRology" y "nortest".
#Se recomienda revisar el archivo: 'Instrucciones.pdf'.

#1. Activar paquetes ####
  
  library(metRology)
  library(nortest)

#2. Definir el Mensurando ####
  
  #Suponer el mensurando 'y'. 
  #Definido según la función matemática: f(y) = ((a*b)+c)
  
  Mensurando <- expression((a*b)+c)

#3. Identificar las fuentes y cuantificar las componentes ####

  #Fuentes de incertidumbre (variables x)
  x  <- list(a = 2, b =5, c=1) 
  
  #Incertidumbres estándar u(x)
  u  <- list(0.1, 0.5, 0.05) 
  
  #Distribución de las incertidumbres estándar u(x).
  d  <- list("unif", "norm", "norm") 
  
  #Nota: Según el tipo de evaluación (tipo A o tipo B),
  #la distribución puede ser: 
    #normal = "norm",  
    #triangular = "tri", 
    #rectangular o uniforme = "unif"
    #distribución t = "t".
    
  #Grados de libertad de las incertidumbres estándar u(x). 
  df <- list(Inf, 9, 7)
  
  #Nota: Para las evaluaciones tipo B, df = Inf
  
#4. Definir la correlación de variables ####

  #Matriz de correlación
  u.cor <- diag(1, length(names(x)))
  dimnames(u.cor) <- list(names(x),names(x))
  
  #Correlación de variables
  u.cor["a","b"] <- u.cor["b","a"] <- 0.5
  
  #Si no existe correlación de variables, no es necesario crear 'u.cor'. 

#5. Activar la función: 'Incertidumbre.R' ####

  source("Incertidumbre.R")
  
  #Si la función se encuentra en una carpeta diferente al directorio de trabajo
  #Usar la dirección de ubicación de la función dentro de 'source'. Ejemplo:
  
  source("~/Documentos/IR.2025.01/Incertidumbre.R")
  
#6. Ejecutar la función 'Incertidumbre' ####

  Incertidumbre(Mensurando, x, u, d, df, u.cor)

  #Si no existe correlación de variables, obviar 'u.cor'.

  Incertidumbre(Mensurando, x, u, d, df)

#Listo: Ya puede ver el resumen de resultados en la consola de R.
#En la carpeta de '\Resultados', se crean los archivos:
  # 'Informe.txt' 
  # 'Grafico.jpg' 
#Fin