#Nombre: Alejandro Cotes
#Código: 202122256
#Version: R 4.3.1
#instalacion de las librerias de apoyo para correr el modelo
{
  require("readxl"); require("dplyr");require("stringr")
}
#file.choose()
#punto 1.1
#Importacion de la base de Módulo sitio o ubicación
{
  location_1<-read_excel("C:/Users/Alejo/Desktop/AlejandroCotes/Diccionario_de_datos_2022_ANONIMIZADO_EMICRON.xlsx", sheet = "D.1")
  location_2<-read_excel("C:/Users/Alejo/Desktop/AlejandroCotes/Diccionario_de_datos_2022_ANONIMIZADO_EMICRON.xlsx", sheet = "D.2")
  location_3<-read_excel("C:/Users/Alejo/Desktop/AlejandroCotes/Diccionario_de_datos_2022_ANONIMIZADO_EMICRON.xlsx", sheet = "D.3")
  location_4<-read_excel("C:/Users/Alejo/Desktop/AlejandroCotes/Diccionario_de_datos_2022_ANONIMIZADO_EMICRON.xlsx", sheet = "D.4")
  location_5<-read_excel("C:/Users/Alejo/Desktop/AlejandroCotes/Diccionario_de_datos_2022_ANONIMIZADO_EMICRON.xlsx", sheet = "D.5")
  location_6<-read_excel("C:/Users/Alejo/Desktop/AlejandroCotes/Diccionario_de_datos_2022_ANONIMIZADO_EMICRON.xlsx", sheet = "D.6")
  location_7<-read_excel("C:/Users/Alejo/Desktop/AlejandroCotes/Diccionario_de_datos_2022_ANONIMIZADO_EMICRON.xlsx", sheet = "D.7")
  location_8<-read_excel("C:/Users/Alejo/Desktop/AlejandroCotes/Diccionario_de_datos_2022_ANONIMIZADO_EMICRON.xlsx", sheet = "D.8")
  #Unificacion de las bases de datos del Módulo sitio o ubicación
  location<-rbind(location_1,location_2,location_3,location_4,location_5,location_6,location_7,location_8)
}
#punto 1.2
#Importacion de la base de Módulo de identificacion
{
  identification_1<-read_excel("C:/Users/Alejo/Desktop/AlejandroCotes/Diccionario_de_datos_2022_ANONIMIZADO_EMICRON.xlsx", sheet = "B.1")
  identification_2<-read_excel("C:/Users/Alejo/Desktop/AlejandroCotes/Diccionario_de_datos_2022_ANONIMIZADO_EMICRON.xlsx", sheet = "B.2")
  
  #Unificacion de las bases de datos del Módulo de identificacion
  identification<-rbind(identification_1,identification_2)
}
#Exportacion de la base de datos a formato RDS
{
  saveRDS(location, file = "C:/Users/Alejo/Desktop/AlejandroCotes/location.rds")
  saveRDS(identification, file = "C:/Users/Alejo/Desktop/AlejandroCotes/identification.rds")
}
#importacion de la base de datos modificada de formato RDS
{
  identification <- readRDS("C:/Users/Alejo/Desktop/AlejandroCotes/identification.rds")
  location <- readRDS("C:/Users/Alejo/Desktop/AlejandroCotes/location.rds")
}
#punto 2.1
#Generar variable 'business_type'
{
identification$business_type <- ifelse(identification$grupos4 == 1, "Agricultura",
                                      ifelse(identification$grupos4 == 2, "Industria manufacturera",
                                             ifelse(identification$grupos4 == 3, "Comercio",
                                                    ifelse(identification$grupos4 == 4, "Servicios", NA))))
}
#punto 2.2
# Supongamos que los rangos etarios son: 18-30, 31-45, 46-60, 61-100
{
  identification$grupo_etario <- cut(identification$edad, breaks = c(18, 30, 45, 60, 100),
                                   labels = c("18-30", "31-45", "46-60", "61-100"))
}
#punto 2.3
#Generar variable 'ambulante'
{
location$ambulante <- ifelse(location$P3053 %in% c(3, 4, 5), 1, 0)
}
#punto 3.1
#Almacene en un objeto llamado identification_sub las variables DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, grupo_etario, ambulante, COD_DEPTO y F_EXP.
{
identification_sub <- identification[, c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA", "grupo_etario", "ambulante", "COD_DEPTO", "F_EXP")]
}
#punto 3.2
#Del objeto location seleccione solo las variables DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, ambulante, P3054, P469, COD_DEPTO, F_EXP y guárdelo en nuevo objeto llamado location_sub.
{
location_sub <- location[, c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA", "ambulante", "P3054", "P469", "COD_DEPTO", "F_EXP")]
}
#punto 4.1
#unificacion de las bases de datos location_sub y identification_sub para crear la nueva base de estudio 
{
  nueva_base_estudio <- merge(location_sub, identification_sub, 
                              by.x = c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA"), 
                              by.y = c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA"), 
                              all = TRUE)
}
#punto 5.1
if (!requireNamespace("skimr", quietly = TRUE)) {
  install.packages("skimr")
}
library(skimr)

# Resumen estadístico con skim()
skim_summary <- skim(merged_data)

# Resumen estadístico con summary()
summary_data <- summary(merged_data)

# Conteo de NA por variable
na_count <- colSums(is.na(merged_data))

# Imprimir resultados
print("Resumen estadístico con skim():")
print(skim_summary)
print("Resumen estadístico con summary():")
print(summary_data)
print("Conteo de NA por variable:")
print(na_count)
#punto 5.2

# Cargar la librería dplyr si aún no está cargada
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Ejemplo de uso de group_by y summarise para extraer variables descriptivas

# Cantidad de asociados por departamento
asociados_por_departamento <- merged_data %>%
  group_by(departamento) %>%
  summarise(total_asociados = sum(asociados, na.rm = TRUE))

# Cantidad de asociados por grupo etario
asociados_por_grupo_etario <- merged_data %>%
  group_by(grupo_etario) %>%
  summarise(total_asociados = sum(asociados, na.rm = TRUE))

# Otros hallazgos que desees extraer usando group_by y summarise

# Imprimir los resultados
print("Cantidad de asociados por departamento:")
print(asociados_por_departamento)
print("Cantidad de asociados por grupo etario:")
print(asociados_por_grupo_etario)
