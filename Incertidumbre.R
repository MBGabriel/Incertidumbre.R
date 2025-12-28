####Función 'Incertidumbre.R'####

#Versión: 2025.01
#Fecha: 2025-12-28
#Autor: G. Morales
#E.mail: <ogmorales@sena.edu.co>; <morales.b.gabriel@gmail.com>
#Dir: Calle 22 # 11E-05. Vía oriente. Pasto, Nariño. Colombia.
#Licencia GPL (>=2)

## Requerimientos ####

#La función requiere de los paquetes: "metRology" y "nortest".
#Se recomienda revisar el archivo: 'Instrucciones.pdf'.

## Estructura de la función ####

Incertidumbre <- function(Mensurando, x, u, d, df, u.cor = NULL){
  if (is.null(u.cor)){
    u.cor <- diag(1, length(names(x)))
    dimnames(u.cor) <- list(names(x),names(x))
    }
  
### Métodos de estimación de incertidumbre ####
 
u_GUM <- uncert(Mensurando, x, u, method = "GUM", cor = u.cor)

u_Kragten <- uncert(Mensurando, x, u, method="kragten", cor = u.cor) 

set.seed(1)
u_MC <-uncertMC(Mensurando, x, u, df, method="MC", distrib = d, B = 10000)

### Grados de libertad efectivos y factor de cobertura ####
#Según Welch-Satterthwaite 
  
#coeficientes de sensibilidad desde el método de Kragten
ci = unlist(u_GUM$budget$c)

#Grados de libertad efectivos
df_eff <- round(w.s(unlist(u), unlist(df), ci=ci,
                    uc=sqrt(sum((ci*unlist(u))^2))),0)

#t crítico con nivel de confianza del 95% para dos colas
# (t = k, factor de cobertura)
t95 <-qt(0.975,df_eff)
  
### Normalidad y asimetría #####
#Aplica para el método de Monte Carlo 

#Prueba de normalidad: Anderson-Darling
Anderson <- ad.test(u_MC$MC$y)
  
#Coeficiente de asimetría de Pearson
Pearson <- round((3*(mean(u_MC$MC$y)-median(u_MC$MC$y)))/sd(u_MC$MC$y),3)
  
### Resultados ####
  
# Tablas
Datos <- data.frame("x" = unlist(x),
                    "u.x" = unlist(u),
                    "distribución" = unlist(d),
                    "df" = unlist(df))

  #En donde, 
      # x   : Valor de la variable, 
      # u.x : Incertidumbre estándar,
      # Distribución : Distribución de u.x,
      # df  : Grados de libertad de u.x.

Resumen <- data.frame("Método" = c("GUM", "Kragten", "Monte Carlo"),
                      "y" = c(u_GUM$y,u_Kragten$y, u_MC$y),
                      "u.y" = c(u_GUM$u.y, u_Kragten$u.y, u_MC$u.y), 
                      "k" = round(c(t95, t95, t95),2), 
                      "U" = c(t95*u_GUM$u.y, t95*u_Kragten$u.y,t95*u_MC$u.y)) 
  #En donde, 
      # y   : Valor del mensurando, 
      # u.y : Incertidumbre estándar combinada,
      # k   : Factor de cobertura (nivel de confianza de 95% y df efectivos),
      # U   : Incertidumbre expandida U = k*u.y.

Mediana <- median(u_MC$MC$y)
Q1 <- quantile(u_MC$MC$y, c(0.025))
Q3 <- quantile(u_MC$MC$y, c(0.975))

#### Resumen ####

#Resumen en la consola
cat("\nIncertidumbre.R","\nVersión: 2025.01",
"\n\nResumen de la estimación de incertidumbre\n")
cat(c("\nNúmero: ", format(Sys.time(), "%Y%m%d%H%M%S")))
cat(c("\nFecha:", date()))
cat("\n\nMensurando (y) = ")
print(Mensurando)
cat("\nDatos\n")
print(Datos)
cat("\nCorrelaciones\n")
print(u.cor)
cat("\nAdvertencia: La función no tiene en cuenta la correlación de variables 
para la estimación de incertidumbre por el método de Monte Carlo.\n",
"\nResultados\n")
print(Resumen)
cat("\nU = (k * u.y)","\n\nEn donde: 'k' es el factor de cobertura, asumiendo 
una distribución t de dos colas con un nivel de confianza del 95 % y\n", 
(df_eff), "grados de libertad efectivos, y 'u.y' es la incertidumbre combinada.",
"\n\nEl método de Monte Carlo usa 10 000 de iteraciones. Tener en cuenta
la normalidad y asimetría de la simulación:",
"\n\nAnderso-Darling, p-valor: ", (Anderson$p.value),
"\nCoeficiente de asimetría de Pearson:",(Pearson),
"\n\nSi la simulación no se ajusta a un modelo normal, reportar la mediana y un
intervalo de cobertura del 95 %.","\n\nMediana: ", Mediana,"[",(Q1),",",(Q3),"]",
"\n\nVer gráficos de la estimación por el método de Monte Carlo\n",
"\nDentro del directorio de trabajo, se ha creado la carpeta '~/Resultados' con
los archivos: 'Informe.txt' y 'Grafico.jpg, correspondientes a un informe 
detallado de la estimación de incertidumbre.\n\nFin del resumen\n\n")

#Gráficos
par(mfcol=c(1,3))
plot.uncertMC(u_MC, main = "Simulación de Monte Carlo")
barplot(-drop1.uncertMC(u_MC),main = "Contribuciones")
par(mfcol=c(1,1)) #los gráficos aparecen en pantalla
  
#### Informe ####

#Detectar archivo de la función
ruta_script <- tryCatch(normalizePath(sys.frames()[[1]]$ofile),
                        error = function(e) NULL)
ruta_base <- if (is.null(ruta_script)) getwd() else dirname(ruta_script)

#Carpeta '/Resultados' dentro de la carpeta base
ruta <- file.path(ruta_base, "Resultados")

#Se crea la carpeta si no existe
dir.create(ruta, recursive = TRUE, showWarnings = FALSE)

#Nombres de archivo
tiempo <- format(Sys.time(), "%Y%m%d%H%M%S")
archivo_txt <- file.path(ruta, paste0("Informe_", tiempo, ".txt"))
archivo_jpg <- file.path(ruta, paste0("Grafico_", tiempo, ".jpg"))

#Se exporta resultados a un archivo 'Informe.txt'
capture.output({
  cat("Incertidumbre.R","\nVersión: 2025.01\n")
  cat("\n===INFORME DE LA ESTIMACIÓN DE INCERTIDUMBRE===\n\n")
  cat(c("Informe número: ", format(Sys.time(), "%Y%m%d%H%M%S")))
  cat(c("\nFecha:", date(),"\n\n"))
  cat("Resumen de resultados\n\n")
  cat("Mensurando (y) = ")
  print(Mensurando)
  cat("\nDatos\n")
  print(Datos)
  cat("\nCorrelaciones\n")
  print(u.cor)
  cat("\nAdvertencia: La función no tiene en cuenta la correlación de 
variables para la estimación de incertidumbre por el método de Monte Carlo.\n")
  cat("\nIncertidumbre de medición\n\n")
  print(Resumen)
  cat("\nU = (k * u.y)","\n\nEn donde: 'k' es el factor de cobertura, asumiendo 
una distribución t de dos colas con un nivel de confianza del 95 % y\n", 
(df_eff), "grados de libertad efectivos, y 'u.y' es la incertidumbre combinada.",
"\n\nEl método de Monte Carlo usa 10 000 de iteraciones. Tener en cuenta
la normalidad y asimetría de la simulación:",
"\n\nAnderso-Darling, p-valor: ", (Anderson$p.value),
"\nCoeficiente de asimetría de Pearson:",(Pearson),
"\n\nSi la simulación no se ajusta a un modelo normal, reportar la mediana y un
intervalo de cobertura del 95 %.","\n\nMediana: ", Mediana,"[",(Q1),",",(Q3),"]",
"\n\nVer gráficos de la estimación por el método de Monte Carlo\n")
  cat("\nResultados detallados\n")
  print(list(u_GUM, u_Kragten, u_MC))
  print(Anderson)
  print(c("Coeficiente de asimetría de Pearson:",Pearson))
  cat("\nFin del informe")
  }, file = archivo_txt) #Se crea un archivo: 'Informe.txt'

#Se exporta gráficos a un archivo 'Grafico.jpg'
jpeg(archivo_jpg, width = 1200, height = 400, quality = 100)
par(mfcol=c(1,3))
plot.uncertMC(u_MC, main = "Simulación de Monte Carlo")
barplot(-drop1.uncertMC(u_MC),main = "Contribuciones")
par(mfcol=c(1,1)) #Se crea un archivo: 'Grafico.jpg'
dev.off()
}

#Fin de la función