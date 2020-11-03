#librerías
library(ggplot2)
library(readxl)
library(dplyr)
library(hrbrthemes)
library(modeest)
library(psych)
library(tinytex)
library(readxl)
hrbrthemes::import_roboto_condensed()
library(tidyverse)
library(summarytools)
library(car)
library(forcats)


#buscar la ruta del archivo de excel
##file.choose()
#copiar ruta de la consola y guardar en variable

file_exel_col <-"db_col.xlsx"
file_test <- "respuestas_test.xlsx"
#como ver las hojas del excel
excel_sheets(file_exel_col)
excel_sheets(file_test)
#importar bd ordenada
db_col <- data.frame(read_excel(file_exel_col))
db_test <- data.frame(read_excel(file_test))
#salvar como dbR
save(db_col, file="db_col.RData")
save(db_test, file="db_test.RData")

#conversión de factores 

##db_test$upz_bin <- NULL #borrado de columna errada

#En db_col (datos tomados del colegio)


db_col$upz_fct <- factor(fct_collapse(db_col$upz,
                                      No = c("ARTES GRAFICAS Y DISEÑO","CIENCIAS DEL DEPORTE","CISCO", "HUMANIDADES Y MEDIOS DE COMUNICACIÓN", "MUSICA", "DIBUJO ARQUITECTONICO",
                                             "N.A", "PENSAMIENTO AMBIENTAL", "PREINGENIERIAS", "TÉCNICO EN CONTABILIZACION DE OPERACIONES COMERCIALES Y FINANCIERAS", "TÉCNICO EN DANZAS",
                                             "TÉCNICO EN DESARROLLO DE OPERACIONES LOGÍSTICAS EN LA CADENA DE ABASTECIMIENTO", "TECNICO EN DIBUJO ARQUITECTONICO",
                                             "TECNICO EN PREPRENSA DIGITAL PARA MEDIOS IMPRESOS", "TÉCNICO EN PRODUCCIÓN DE MEDIOS AUDIOVISUALES DIGITALES", "TÉCNICO EN REDES ELÉCTRICAS DOMICILIARIAS",
                                             "TÉCNICO EN VENTA DE PRODUCTOS Y SERVICIOS", "N.A"),
                                      Si = c("TÉCNICO EN DISEÑO E INTEGRACIÓN DE MULTIMEDIA", "TÉCNICO EN INFORMATICA","TÉCNICO EN MANTENIMIENTO DE AUTOMATISMOS INDUSTRIALES", "TÉCNICO EN PROGRAMACIÓN DE SOFTWARE")
))

save(db_col, file="db_col.RData")


#En db_test (Datos tomados del test)

db_test$upz_fct <- factor(fct_collapse(db_test$upz,
                                      No = c("Ciencias del deporte","CISCO","Humanidades y medios de comunicación", "Pre ingenierías", "Técnico en dibujo arquitectónico", "Técnico en pre-prensa digital para medios impresos", "Técnico en redes eléctricas domiciliarias", "Técnico en venta de productos y servicios", "Actualmente no curso ninguna modalidad", "Otro"),
                                      Si = c("Técnico en diseño e integración multimedia", "Técnico en informática","Técnico en mantenimiento de automatismos industriales", "Técnico en programación de software")
))


#--Respuestas A

db_test$PPregunta.7 <- factor(fct_collapse(db_test$Pregunta.7,
                                           "1" = "A",
                                           "0" = c("B", "C", "D", " ")))

db_test$PPregunta.12 <- factor(fct_collapse(db_test$Pregunta.12,
                                           "1" = "A",
                                           "0" = c("B", "C", "D", " ")))

db_test$PPregunta.14 <- factor(fct_collapse(db_test$Pregunta.14,
                                           "1" = "A",
                                           "0" = c("B", "C", "D", " ")))
db_test$PPregunta.18 <- factor(fct_collapse(db_test$Pregunta.18,
                                           "1" = "A",
                                           "0" = c("B", "C", "D", " ")))
db_test$PPregunta.21 <- factor(fct_collapse(db_test$Pregunta.21,
                                           "1" = "A",
                                           "0" = c("B", "C", "D", " ")))
db_test$PPregunta.23 <- factor(fct_collapse(db_test$Pregunta.23,
                                           "1" = "A",
                                           "0" = c("B", "C", "D", " ")))
db_test$PPregunta.27 <- factor(fct_collapse(db_test$Pregunta.27,
                                           "1" = "A", 
                                           "0" = c("B", "C", "D", " ")))



#--REspuestas B
db_test$PPregunta.1 <- factor(fct_collapse(db_test$Pregunta.1,
                                           "1" = "B",
                                           "0" = c("A", "C", "D", " ")))

db_test$PPregunta.8 <- factor(fct_collapse(db_test$Pregunta.8,
                                           "1" = "B",
                                           "0" = c("A", "C", "D", " ")))
db_test$PPregunta.13 <- factor(fct_collapse(db_test$Pregunta.13,
                                           "1" = "B", 
                                           "0" = c("A", "C", "D", " ")))
db_test$PPregunta.17 <- factor(fct_collapse(db_test$Pregunta.17,
                                           "1" = "B",
                                           "0" = c("A", "C", "D", " ")))
db_test$PPregunta.19 <- factor(fct_collapse(db_test$Pregunta.19,
                                           "1" = "B",
                                           "0" = c("A", "C", "D", " ")))
db_test$PPregunta.22 <- factor(fct_collapse(db_test$Pregunta.22,
                                           "1" = "B", 
                                           "0" = c("A", "C", "D", " ")))
db_test$PPregunta.25 <- factor(fct_collapse(db_test$Pregunta.25,
                                           "1" = "B",
                                           "0" = c("A", "C", "D", " ")))
db_test$PPregunta.26 <- factor(fct_collapse(db_test$Pregunta.26,
                                           "1" = "B",
                                           "0" = c("A", "C", "D", " ")))


#--Respuestas C
db_test$PPregunta.2 <- factor(fct_collapse(db_test$Pregunta.2,
                                           "1" = "C", 
                                           "0" = c("A", "B", "D", " ")))

db_test$PPregunta.5 <- factor(fct_collapse(db_test$Pregunta.5,
                                           "1" = "C",
                                           "0" = c("A", "B", "D", " ")))

db_test$PPregunta.10 <- factor(fct_collapse(db_test$Pregunta.10,
                                           "1" = "C",
                                           "0" = c("A", "B", "D", " ")))
db_test$PPregunta.11 <- factor(fct_collapse(db_test$Pregunta.11,
                                           "1" = "C",
                                           "0" = c("A", "B", "D", " ")))
db_test$PPregunta.20 <- factor(fct_collapse(db_test$Pregunta.20,
                                           "1" = "C",
                                           "0" = c("A", "B", "D", " ")))
db_test$PPregunta.24 <- factor(fct_collapse(db_test$Pregunta.24,
                                           "1" = "C",
                                           "0" = c("A", "B", "D", " ")))
db_test$PPregunta.28 <- factor(fct_collapse(db_test$Pregunta.28,
                                           "1" = "C",
                                           "0" = c("A", "B", "D", " ")))


#--REspuestas D

db_test$PPregunta.3 <- factor(fct_collapse(db_test$Pregunta.3,
                                           "1" = "D",
                                           "0" = c("A", "B", "C", " ")))
db_test$PPregunta.4 <- factor(fct_collapse(db_test$Pregunta.4,
                                           "1" = "D",
                                           "0" = c("A", "B", "C", " ")))
db_test$PPregunta.6 <- factor(fct_collapse(db_test$Pregunta.6,
                                           "1" = "D",
                                           "0" = c("A", "B", "C", " ")))
db_test$PPregunta.9 <- factor(fct_collapse(db_test$Pregunta.9,
                                           "1" = "D",
                                           "0" = c("A", "B", "C", " ")))
db_test$PPregunta.15 <- factor(fct_collapse(db_test$Pregunta.15,
                                           "1" = "D",
                                           "0" = c("A", "B", "C", " ")))
db_test$PPregunta.16 <- factor(fct_collapse(db_test$Pregunta.16,
                                            "1" = "D", 
                                            "0" = c("A", "B", "C", " ")))


db_test <- data.frame(db_test)

##install.packages("xlsReadWrite")#instalamos el paquete xlsReadWrite
library(openxlsx) #abrimos el paquete
write.xlsx(db_test, file="db_test.xlsx")

db_test1 <- "db_test1.xlsx"
db_test1 <- data.frame(read_excel(db_test1))
save(db_test1, file="db_tes1.RData")

#Mezclar las bases de datos

#Hago copias, por si acaso...
db_col1 <- data.frame(db_col)
db_test1 <- data.frame(db_test1)

write.xlsx(db_col1, file="db_col1.xlsx")
write.xlsx(db_test1, file="db_test1.xlsx")

##Uniendo las dos tablas -  


db_basica <- merge (db_col1, db_test1, by="apellidos", all.y = TRUE)
db_basica <- select(db_basica, -Ejemplo.I, -Ejemplo.II, -Ejemplo.III, -No, -sexo.x, -curso.x, -cfk.x, -upz_fct.x, -nombre.x, -nom_completo, -upz.x, -col_origen)
write.xlsx(db_basica, file="db_basica.xlsx")



db_limpia <-  select(db_basica, result=puntaje_total,sexo= sexo.y, cfk =cfk.y, upz=upz_fct.y, edad = edad)
write.xlsx(db_limpia, file="db_limpia.xlsx")






  


