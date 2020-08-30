install.packages("stringr")
install.packages("eeptools")
library(lubridate)
library(sqldf)
library(readxl)
library(dplyr)
library(stringr)
library(eeptools)
set.seed(346)

setwd("C:/Users/Hp i3/Desktop/CARPETA_BUSTOS_DURAN_MARILAO")
#subimos las bases
base=read_excel("Base_10000.xlsx",col_names = TRUE)
test=read_excel("PrediccionV2_SM.xlsx",col_names = TRUE)


#QUE VARIABLES DE TEST NO ESTAN EN BASE
names(test) %in% names(base)
#LA COLUMNA 1,14,15,34
which(names(test) %in% names(base)==FALSE)
#QUE SON LAS VARIABELS
test[,which(names(test) %in% names(base)==FALSE)]
#ELIMINAMOS VARIBALES DE TEST QUE NO ESTAN EN BASE (QUEDA CON 37)
test=test[,-which(names(test) %in% names(base)==FALSE)]


#QUE VARIABLES DE BASE NO ESTAN EN TEST
names(base) %in% names(test)
#la columna 27 y 35 de BASE no estan eN test
which(names(base) %in% names(test)==FALSE)
#QUE SON LAS VARIABELS
base[,which(names(base) %in% names(test)==FALSE)]
#ELIMINAMOS VARIABLES DE BASE QUE NO ESTAN EN TEST (QUEDA CON 37)
base=base[,-which(names(base) %in% names(test)==FALSE)]


#REMPLAZAMOS LOS STRINGS NA's DE CLASE POR NA PARA TEST
test<-test%>%
  mutate(CLASE=NA)

#PRE-PROCESAMEINTO para poder unir ambas bases####################################
#unificacion de clases y formatos(tienen igual class?, que class?, formato iguales?)
names(base)

#ordenamos las variables de test para que tengan la misma posicion que "base"
#creamos un vector con nombre de variables en orden adecuado
A=names(base)
#cramos un vector vacio que se llenará con el orden adecuado
orden=c()
for (i in 1:ncol(test)) {
  orden[i]=which(colnames(test)==A[i])
}
orden
#ordenamos test
test=test[,orden]
View(cbind(names(base),names(test)))
rm(i,orden)

#analisis rapido de si tienen igual class?  que class? y que formato? 
for (i in 1:ncol(base)) {
  tes=test[[i]]
  bas=base[[i]]
  cat("Analizando:",A[i],"\n",
      "Tienen misma class?:",class(base[[i]])==class(test[[i]]),"\n",
      "Test",A[i],"con class:",class(test[[i]]),"\n",
      "Base",A[i],"con class:",class(base[[i]]),"\n",
      "Test",A[i],"con formato:",tes[1],tes[2],tes[3],"\n",
      "Base",A[i],"con formato:",bas[1],bas[2],bas[3],"\n")
}

rm(list = c("A","bas","i","tes"))



#de base hay que exraer solo fecha yyyy-mm-dd y transformar a formato date
base$LOT_FECHA_RECEPCION=as.Date(str_sub(base$LOT_FECHA_RECEPCION,1,10))
#de base test hay que transformar a formato date
test$LOT_FECHA_RECEPCION=date(test$LOT_FECHA_RECEPCION)



#de base hay que exraer solo fecha yyyy-mm-dd y pasar a formato date
base$LOT_FECHA_RECEP_BENEFICIOS=date(str_sub(base$LOT_FECHA_RECEP_BENEFICIOS,1,10))
#de test pasar a formato date
test$LOT_FECHA_RECEP_BENEFICIOS=date(test$LOT_FECHA_RECEP_BENEFICIOS)


#transformamos a chr por ahora
test$POLIZA_SEGURO=as.character(test$POLIZA_SEGURO)


#transformar a formato date
base$SIN_FECHA_OCURRENCIA=date(base$SIN_FECHA_OCURRENCIA)
test$SIN_FECHA_OCURRENCIA=date(test$SIN_FECHA_OCURRENCIA)


#transformar a formato date
base$SIN_FEC_DENUNCIA=date(base$SIN_FEC_DENUNCIA)
test$SIN_FEC_DENUNCIA=date(test$SIN_FEC_DENUNCIA)


# transformar a formato date
base$SIN_FEC_INIC_VIG_PRODUCTO=date(base$SIN_FEC_INIC_VIG_PRODUCTO)
test$SIN_FEC_INIC_VIG_PRODUCTO=date(test$SIN_FEC_INIC_VIG_PRODUCTO)


# transformar a formato date
base$FECHA_NACIMIENTO=date(base$FECHA_NACIMIENTO)
test$FECHA_NACIMIENTO=date(test$FECHA_NACIMIENTO)


#transformamos a chr por ahora
test$SIN_CUOTAS_COBERTURA=as.character(test$SIN_CUOTAS_COBERTURA)


# transformar a formato date
base$SIN_FEC_INIC_VIG_SINIESTRADO=date(base$SIN_FEC_INIC_VIG_SINIESTRADO)
test$SIN_FEC_INIC_VIG_SINIESTRADO=date(test$SIN_FEC_INIC_VIG_SINIESTRADO)


# transformar a formato date
base$HEV_TIME_MARK=date(base$HEV_TIME_MARK)
test$HEV_TIME_MARK=date(test$HEV_TIME_MARK)


#transformamos a formato chr por ahora
test$CUOTAS_PAG=as.character(test$CUOTAS_PAG)


#transformamos a formato chr por ahora
test$MONTO_APROBADO=as.character(test$MONTO_APROBADO)
base$MONTO_APROBADO=as.character(base$MONTO_APROBADO)

#transformamos a formato chr
test$RAMO_LEGAL=as.character(test$RAMO_LEGAL)


#binarzar CLASE
base=base%>%
  mutate(CLASE=ifelse(base$CLASE=="POSIBLE FRAUDE",1,0))

#SELECCION DE VARIABLES##########################################################

#uniendo ambas bases
base_full=rbind(base,test)
str(base_full)
#Eliminacion de variables por los expuesto en informe
base_full=base_full%>%
  select(-SIN_ID, -LOT_NUMEROLOTE, -NAME, -LASTNAME, -ID_PRODUCTO_TECNICO,
         -EVA_ID, -CUO_NUMERO_CUOTA, -NAME_DENUNCIANTE,-RAMO_LEGAL,
         -LOT_FECHA_RECEP_BENEFICIOS,-SIN_PAGO_ESPECIAL,-CUOTAS_PAG,-CAS_DESCRIPCION,
         -TIPO_LIQUIDADOR,-LOT_FECHA_RECEP_BENEFICIOS,-DNI_DENUNCIANTE)
#base queda con 22 variables
names(base_full)
str(base_full)

#PREPROCESAMIENTO##################################################################

# convirtiendo los "NULL" en NA (PASAR ANTES NULLS DE MONTO_APROBADO A 0)
for (i in 1:ncol(base_full)) {
  if (class(base_full[[i]])=="character") {
    base_full[which(base_full[,i]=="NULL"),i]=NA 
  }
}
rm(i)
any(base_full$MONTO_APROBADO=='NULL',na.rm = TRUE)
any(base_full$SIN_NOMBRE_CIUDAD=='NULL',na.rm = TRUE)
any(base_full$CAU_DESCRIPCION=='NULL',na.rm = TRUE)
any(base_full$CLASE=='NULL',na.rm = TRUE)
summary(is.na(base_full)) #con NA SIN_NOMBRE_CIUDAD, SUMA, CLASE
summary(base_full)


names(base_full)
Escalador=function(x){(x-min(x))/(max(x)-min(x))}
#1- LOT_FECHA_RECEPCION
class(base_full$LOT_FECHA_RECEPCION)
head(base_full$LOT_FECHA_RECEPCION)
base_full$LOT_FECHA_RECEPCION=as.numeric(base_full$LOT_FECHA_RECEPCION)
head(base_full$LOT_FECHA_RECEPCION)
base_full$LOT_FECHA_RECEPCION=Escalador(base_full$LOT_FECHA_RECEPCION)

#2- TIPO_EMPRESA_SEGURO
class(base_full$TIPO_EMPRESA_SEGURO) #chr
unique(base_full$TIPO_EMPRESA_SEGURO)
#aseguramos que todas estén en mayus para no tener duplicidad por min y may
base_full$TIPO_EMPRESA_SEGURO=toupper(base_full$TIPO_EMPRESA_SEGURO)#a mayusc
#cramos vairbales binarias para TIPO_EMPRESA_SEGURO especificados en informe
base_full=base_full%>% #ACTIVIDADES DIVERSAS DE INVERSION Y SERVICIOS
  mutate(TES_ACTIVIDADES_DIVERSAS_DE_INVERSION_Y_SERVICIOS=ifelse(base_full$TIPO_EMPRESA_SEGURO=="ACTIVIDADES DIVERSAS DE INVERSION Y SERVICIOS",1,0))

base_full=base_full%>% #ADECUACION DE OBRAS DE CONSTRUCCION
  mutate(TES_ADECUACION_DE_OBRAS_DE_CONSTRUCCION=ifelse(base_full$TIPO_EMPRESA_SEGURO=="ADECUACION DE OBRAS DE CONSTRUCCION",1,0))

base_full=base_full%>% #ARCHIVOS ESTATALES
  mutate(TES_ARCHIVOS_ESTATALES=ifelse(base_full$TIPO_EMPRESA_SEGURO=="ARCHIVOS ESTATALES",1,0))

base_full=base_full%>% #CURTIEMBRE Y MANUFACTURAS DE CUERO DIFERENTES
  mutate(TES_CURTIEMBRE_Y_MANUFACTURAS_DE_CUERO_DIFERENTES=ifelse(base_full$TIPO_EMPRESA_SEGURO=="CURTIEMBRE Y MANUFACTURAS DE CUERO DIFERENTES",1,0))

base_full=base_full%>% #FABRICACION DE PRODUCTOS MINERALES NO METALIC
  mutate(TES_FABRICACION_DE_PRODUCTOS_MINERALES_NO_METALIC=ifelse(base_full$TIPO_EMPRESA_SEGURO=="FABRICACION DE PRODUCTOS MINERALES NO METALIC",1,0))

base_full=base_full%>% #OTRAS ACTIVIDADES EMPRESARIALES
  mutate(TES_OTRAS_ACTIVIDADES_EMPRESARIALES=ifelse(base_full$TIPO_EMPRESA_SEGURO=="OTRAS ACTIVIDADES EMPRESARIALES",1,0))

#por ahora no la eliminamos TIPO EMPRESA SEGURO
#base_full=base_full%>%
#  select(-TIPO_EMPRESA_SEGURO)

base_full$TIPO_EMPRESA_SEGURO=as.factor(base_full$TIPO_EMPRESA_SEGURO)

#3- POLIZA_SEGURO
class(base_full$POLIZA_SEGURO) #chr
base_full=base_full%>% #Extraemos solo las 2 primeras cifras
  mutate(POLIZA_SEGURO=str_sub(base_full$POLIZA_SEGURO,1,2))

base_full=base_full%>% #creacion de variable binaria que comienza con 52
  mutate(PS_52=ifelse(base_full$POLIZA_SEGURO=="52",1,0))

base_full=base_full%>% #creacion de variable binaria que comienza con 53
  mutate(PS_53=ifelse(base_full$POLIZA_SEGURO=="53",1,0))

base_full=base_full%>% #creacion de variable binaria que comienza con 57
  mutate(PS_57=ifelse(base_full$POLIZA_SEGURO=="57",1,0))

base_full=base_full%>% #creacion de variable binaria que comienza con 60
  mutate(PS_60=ifelse(base_full$POLIZA_SEGURO=="60",1,0))

#por ahora no a eliminamos
#base_full=base_full%>%
#  select(-POLIZA_SEGURO)

base_full$POLIZA_SEGURO=as.factor(base_full$POLIZA_SEGURO)

#4- SIN_FEC_OCURRENCIA
class(base_full$SIN_FECHA_OCURRENCIA)
head(base_full$SIN_FECHA_OCURRENCIA)
base_full$SIN_FECHA_OCURRENCIA=as.numeric(base_full$SIN_FECHA_OCURRENCIA)
head(base_full$SIN_FECHA_OCURRENCIA)
base_full$SIN_FECHA_OCURRENCIA=Escalador(base_full$SIN_FECHA_OCURRENCIA)

#5- SIN_FEC_DENUNCIA
class(base_full$SIN_FEC_DENUNCIA)
head(base_full$SIN_FEC_DENUNCIA)
base_full$SIN_FEC_DENUNCIA=as.numeric(base_full$SIN_FEC_DENUNCIA)
head(base_full$SIN_FEC_DENUNCIA)
base_full$SIN_FEC_DENUNCIA=Escalador(base_full$SIN_FEC_DENUNCIA)

#6- PRODUCTO
class(base_full$PRODUCTO)
head(base_full$PRODUCTO)
#extremos solo los 2 primeros caracteres como forma de clusterizacion
base_full=base_full%>%
  mutate(PRODUCTO=str_sub(base_full$PRODUCTO,1,2))

base_full=base_full%>% #creacion de variable binaria que comienza con EX
  mutate(PRODUCTO_EX=ifelse(PRODUCTO=="EX",1,0))

base_full=base_full%>% #creacion de variable binaria que comienza con 59
  mutate(PRODUCTO_59=ifelse(PRODUCTO=="59",1,0))

base_full=base_full%>%
  select(-PRODUCTO)

#7- SIN_FEC_INIC_VIG_PRODUCTO [SUJETA A ELIMINAR EN FUNCION DE ACC]
class(base_full$SIN_FEC_INIC_VIG_PRODUCTO)
head(base_full$SIN_FEC_INIC_VIG_PRODUCTO)
base_full$SIN_FEC_INIC_VIG_PRODUCTO=as.numeric(base_full$SIN_FEC_INIC_VIG_PRODUCTO)
base_full$SIN_FEC_INIC_VIG_PRODUCTO=Escalador(base_full$SIN_FEC_INIC_VIG_PRODUCTO)

#8- DNI_ASEGURADO
class(base_full$DNI_ASEGURADO)
head(base_full$DNI_ASEGURADO)
#creacion de variables asociadas a DNI's fraudulentos
base_full=base_full%>%
  mutate(DNI_ASEGURADO_10900762512=ifelse(DNI_ASEGURADO=="10900762512",1,0))
base_full=base_full%>%
  mutate(DNI_ASEGURADO_10900122212=ifelse(DNI_ASEGURADO=="10900122212",1,0))
base_full=base_full%>%
  mutate(DNI_ASEGURADO_109K0135012=ifelse(DNI_ASEGURADO=="109K0135012",1,0))
#eliminamos la variable
base_full=base_full%>%
  select(-DNI_ASEGURADO)

#9- FECHA_NACIMIENTO
class(base_full$FECHA_NACIMIENTO)

#cramos variable EDAD
base_full=base_full%>%
  mutate(EDAD=floor(age_calc(base_full$FECHA_NACIMIENTO,today(), units = "years")))

base_full=base_full%>%
  select(-FECHA_NACIMIENTO)

#creacion de variable binaria si edad esta en rango de 35-50
base_full=base_full%>%
  mutate(EDAD_35_50=ifelse(EDAD>=35 & EDAD<=50,1,0))

#por ahora no eliminamos edad
#base_full=base_full%>%
#  select(-EDAD)

#escalamos la edad
Escalador=function(x){(x-min(x))/(max(x)-min(x))}
base_full$EDAD=Escalador(base_full$EDAD)

#10- SIN_CUOTAS_COBERTURA
class(base_full$SIN_CUOTAS_COBERTURA)
head(base_full$SIN_CUOTAS_COBERTURA)
unique(base_full$SIN_CUOTAS_COBERTURA)
#pasamos la variable a class numeric
base_full$SIN_CUOTAS_COBERTURA=as.numeric(base_full$SIN_CUOTAS_COBERTURA)
#sustituimos los 0's por 1's
base_full$SIN_CUOTAS_COBERTURA[base_full$SIN_CUOTAS_COBERTURA==0]=1
any(base_full$SIN_CUOTAS_COBERTURA==0)
#creamos un variable binaria para cuotas entre
base_full=base_full%>%
  mutate(SCC_1_6=ifelse(SIN_CUOTAS_COBERTURA>=1 & SIN_CUOTAS_COBERTURA<=6,1,0))
#escalamos la variable ya que es numerica
base_full$SIN_CUOTAS_COBERTURA=Escalador(base_full$SIN_CUOTAS_COBERTURA)

#11- BANCO
class(base_full$BANCO)
anyNA(base_full$BANCO)
unique(base_full$BANCO)
#aseguamos que todas estene en mayus para no tener duplicidad por min y may
base_full$BANCO=toupper(base_full$BANCO)
#creamos variabels binarias para bancos con mas propencion a posible Fraude
base_full=base_full%>% #variable binaria de banco ABB
  mutate(BANCO_ABB=ifelse(BANCO=="ABB",1,0))

base_full=base_full%>% #variable binaria de banco RCO
  mutate(BANCO_RCO=ifelse(BANCO=="RCO",1,0))

base_full=base_full%>% #variable binaria de banco SCC
  mutate(BANCO_SCC=ifelse(BANCO=="SCC",1,0))

base_full=base_full%>% #variable binaria de banco AFA
  mutate(BANCO_AFA=ifelse(BANCO=="AFA",1,0))

base_full=base_full%>% #variable binaria de banco MFO
  mutate(BANCO_MFO=ifelse(BANCO=="MFO",1,0))

base_full=base_full%>% #variable binaria de banco NBA
  mutate(BANCO_NBA=ifelse(BANCO=="NBA",1,0))

base_full=base_full%>%
  mutate(BANCO_OBC=ifelse(BANCO=="OBC",1,0))
#por ahora no eliminamos banco
#base_full=base_full%>%
#  select(-BANCO)

base_full$BANCO=as.factor(base_full$BANCO)

#12- SIN_FEC_INIC_VIG_SINIESTRADO
class(base_full$SIN_FEC_INIC_VIG_SINIESTRADO)
anyNA(base_full$SIN_FEC_INIC_VIG_SINIESTRADO)
head(base_full$SIN_FEC_INIC_VIG_SINIESTRADO)
base_full$SIN_FEC_INIC_VIG_SINIESTRADO=as.numeric(base_full$SIN_FEC_INIC_VIG_SINIESTRADO)
head(base_full$SIN_FEC_INIC_VIG_SINIESTRADO)
base_full$SIN_FEC_INIC_VIG_SINIESTRADO=Escalador(base_full$SIN_FEC_INIC_VIG_SINIESTRADO)

#13- USER_LIQUIDADOR
class(base_full$USER_LIQUIDADOR)
anyNA(base_full$USER_LIQUIDADOR)
head(base_full$USER_LIQUIDADOR)
#aseguamos que todas estén en mayus para no tener duplicidad por min y may
base_full$USER_LIQUIDADOR=toupper(base_full$USER_LIQUIDADOR)
#Generando nuevas columnas de empresas externas relevantes, sobre USER_LIQUIDADOR
base_full=base_full%>%
  mutate(UL_Emp1=ifelse(base_full$USER_LIQUIDADOR=="EMPRESAEXTERNA1",1,0))
base_full=base_full%>%
  mutate(UL_Emp2=ifelse(USER_LIQUIDADOR=="530815",1,0))
#Se borra USER_LIQUIDADOR que ya fue analizada
base_full=base_full%>%
  select(-USER_LIQUIDADOR)


#14- HEV_TIME_MARK
class(base_full$HEV_TIME_MARK)
anyNA(base_full$HEV_TIME_MARK)
head(base_full$HEV_TIME_MARK)
base_full$HEV_TIME_MARK=as.numeric(base_full$HEV_TIME_MARK)
head(base_full$HEV_TIME_MARK)
base_full$HEV_TIME_MARK=Escalador(base_full$HEV_TIME_MARK)

#15- HEV_DETAILS
class(base_full$HEV_DETAILS)
anyNA(base_full$HEV_DETAILS)
unique(base_full$HEV_DETAILS)
#aseguamos que todas estén en mayus para no tener duplicidad por min y may
base_full$HEV_DETAILS=toupper(base_full$HEV_DETAILS)
#cramos la variable binaria para ESTADOEVALUACIONES:DICTAMEN APROBADO
base_full=base_full%>%
  mutate(HD_Dictamen_Aprobado=ifelse(base_full$HEV_DETAILS=="ESTADOEVALUACIONES:DICTAMEN APROBADO",1,0))

base_full$HEV_DETAILS=as.factor(base_full$HEV_DETAILS)
base_full=base_full%>%
  select(-HEV_DETAILS)
#PARA ESTA MUESTRA COMO SOLO HAY 2 DISTINTOS EN ESTA VARIABLE CREAR UNA VARIBALE BIN
#DE UN TOTAL DE 2 DIST NO APORTA, MEJOR ELIMINAR VARIABLES HEV_DETAILS


#16- SIN_ESTADO_ACTUAL
class(base_full$SIN_ESTADO_ACTUAL)
anyNA(base_full$SIN_ESTADO_ACTUAL)
unique(base_full$SIN_ESTADO_ACTUAL)
#hay valores mal computados En Evaluaci¿n, En Evaluacion, En Evaluación
#se unifica Pendiente con Pendiente Datos
base_full$SIN_ESTADO_ACTUAL[base_full$SIN_ESTADO_ACTUAL=="En Evaluaci¿n"]="En Evaluacion"
base_full$SIN_ESTADO_ACTUAL[base_full$SIN_ESTADO_ACTUAL=="En Evaluación"]="En Evaluacion"
#aseguamos que todas estén en mayus para no tener duplicidad por min y may
base_full$SIN_ESTADO_ACTUAL=toupper(base_full$SIN_ESTADO_ACTUAL)
#creamos las variables binarias
base_full=base_full%>%
  mutate(SEA_En_Evaluacion=ifelse(base_full$SIN_ESTADO_ACTUAL=="EN EVALUACION",1,0))
base_full=base_full%>%
  mutate(SEA_Rechazado=ifelse(base_full$SIN_ESTADO_ACTUAL=="RECHAZADO",1,0))
#por ahora no se borra SIN_ESTADO_ACTUAL
#base_full=base_full%>%
#  select(-SIN_ESTADO_ACTUAL)

base_full$SIN_ESTADO_ACTUAL=as.factor(base_full$SIN_ESTADO_ACTUAL)


#17- MONTO_APROBADO
class(base_full$MONTO_APROBADO)
anyNA(base_full$MONTO_APROBADO)
head(base_full$MONTO_APROBADO)
#ante existencia de NA lo cambiamos por monto $0
base_full$MONTO_APROBADO[is.na(base_full$MONTO_APROBADO)]="0"
#cambiamos la clase a numeric
base_full$MONTO_APROBADO=as.numeric(base_full$MONTO_APROBADO)
#escalamos o normalizamos la variable numerica
base_full$MONTO_APROBADO=Escalador(base_full$MONTO_APROBADO)


#18- LOT_EXTERNO
class(base_full$LOT_EXTERNO)
anyNA(base_full$LOT_EXTERNO)
unique(base_full$LOT_EXTERNO)
#creamos variables binarias
base_full=base_full%>%
  mutate(LOT_EXTERNO_N=ifelse(LOT_EXTERNO=="N",1,0))
base_full=base_full%>%
  mutate(LOT_EXTERNO_Y=ifelse(LOT_EXTERNO=="Y",1,0))

#eliminamos la vareiable
base_full=base_full%>%
  select(-LOT_EXTERNO)


#19- SIN_NOMBRE_CIUDAD
class(base_full$SIN_NOMBRE_CIUDAD)
anyNA(base_full$SIN_NOMBRE_CIUDAD)
head(base_full$SIN_NOMBRE_CIUDAD)
#aseguamos que todas estén en mayus para no tener duplicidad por min y may
base_full$SIN_NOMBRE_CIUDAD=toupper(base_full$SIN_NOMBRE_CIUDAD)
#pasamos a factor
base_full$SIN_NOMBRE_CIUDAD=as.factor(base_full$SIN_NOMBRE_CIUDAD)
#ya que segun la muestra contiene NA se prefiere completar usando mice y luego 
#binarizar para que no existan muchas varibales posteriormente con NA's que 
#llenar (ANTES APLICAR MICE)


#20- CAU_DESCRIPCION
class(base_full$CAU_DESCRIPCION)
anyNA(base_full$CAU_DESCRIPCION)
head(base_full$CAU_DESCRIPCION)
unique(base_full$CAU_DESCRIPCION)
#aseguamos que todas estén en mayus para no tener duplicidad por min y may
base_full$CAU_DESCRIPCION=toupper(base_full$CAU_DESCRIPCION)
#pasamos a factor
base_full$CAU_DESCRIPCION=as.factor(base_full$CAU_DESCRIPCION)
#ya que segun la muestra en ocaciones tiene NA se prefiere completar usando mice y 
#luego binarizar para que no existan muchas varibales posteriormente con NA's que 
#llenar (ANTES APLICAR MICE)

#21- CLASE
#binarizamos la clase
base_full$CLASE=as.factor(base_full$CLASE)

#22- SUMA
#sustituimos sus NA por 0
anyNA(base_full$SUMA)
base_full$SUMA[is.na(base_full$SUMA)]=0
#escalar
base_full$SUMA=Escalador(base_full$SUMA)


str(base_full)
summary(is.na(base_full))
rm(Escalador)

#DATAMINING MICE SOLO PARA LLENADO DE MISING VALUES##################################

#Extraemos columna CLASE para que no se llenen NA's
CLASE=base_full[,which(colnames(base_full)=="CLASE")]
#Creamos nueva base para llenado de NA sin CLASE
base_full_mice=base_full%>%
  select(-CLASE)

rm(base_full)
#En funcion de muestra puede tenr NA varables SIN_NOMBRE_CIUDAD,CAU_DESCRIPCION
summary(is.na(base_full_mice))
sum(is.na(base_full_mice))

#APLICAMOS MICE PARA EL LLENADO DE NA
library(mice)
library(caret)
modelo_mice_complete<-mice(base_full_mice,m = 5,method = 'cart')
base_full_mice_complete=complete(modelo_mice_complete)


anyNA(base_full_mice_complete)
anyNA(base_full_mice_complete$SIN_NOMBRE_CIUDAD)
anyNA(base_full_mice_complete$CAU_DESCRIPCION)


rm(list = c("modelo_mice_complete","base","test","base_full_mice"))

#Ahora que no hay NA creamos variables bianrias de SIN_NOMBRE_CIUDAD y CAU_DESCRIPCION
#binarias de SIN_NOMBRE_CIUDAD: variable "Stgo"
base_full_mice_complete=base_full_mice_complete%>%
  mutate(SNC_Stgo=ifelse(SIN_NOMBRE_CIUDAD=="SANTIAGO",1,0))
#Se borra SIN_NOMBRE_CIUDAD ya analizada y son 220 si lo dejamos como factor (mucho)
base_full_mice_complete=base_full_mice_complete%>%
  select(-SIN_NOMBRE_CIUDAD)


#binarias de CAU_DESCRIPCION:"Despido", "Enfermedad", "Robo", "Siniestro Pagado E.G" y
#"Otras"
base_full_mice_complete=base_full_mice_complete%>%
  mutate(CD_Despido=ifelse(CAU_DESCRIPCION=="DESPIDO",1,0))

base_full_mice_complete=base_full_mice_complete%>%
  mutate(CD_Enfermedad=ifelse(CAU_DESCRIPCION=="ENFERMEDAD",1,0))

base_full_mice_complete=base_full_mice_complete%>%
  mutate(CD_Robo=ifelse(CAU_DESCRIPCION=="ROBO",1,0))

base_full_mice_complete=base_full_mice_complete%>%
  mutate(CD_Siniestro_Pagado=ifelse(CAU_DESCRIPCION=="SINIESTRO PAGADO E.G.",1,0))

base_full_mice_complete=base_full_mice_complete%>%
  mutate(CD_Otras=ifelse(CAU_DESCRIPCION=="OTRA",1,0))

#por ahora no borramos la variable CAU_DESCRIPCION
#base_full_mice_complete=base_full_mice_complete%>%
#  select(-CAU_DESCRIPCION)

#quedamos con 52 varibales predictoras
ncol(base_full_mice_complete)

#variables no borradas y sus niveles de factor
#unique(base_full_mice_complete$TIPO_EMPRESA_SEGURO) #22 levels factor
#unique(base_full_mice_complete$POLIZA_SEGURO) #13
#unique(base_full_mice_complete$BANCO) #26 lev
#unique(base_full_mice_complete$HEV_DETAILS) #5
#unique(base_full_mice_complete$SIN_ESTADO_ACTUAL)#6
#unique(base_full_mice_complete$CAU_DESCRIPCION)#15

str(base_full_mice_complete)
#ANALISIS PREVIO A DATA MINING MODELOS DM###########################################
#analisis de correlacion solo para variables numericas (no factores)
#analisis de correlacion entre variables predictivas para ver posible colinealidad
#creamos un vector de las variables numericas num.
vector_num_int=c()
for (i in 1:ncol(base_full_mice_complete)) {
  if (class(base_full_mice_complete[[i]])=="numeric") {
    vector_num_int[i]=i
  }
}
rm(i)
vector_num_int=vector_num_int[!is.na(vector_num_int)]

#analisis de correlacion
MatrizCorrelacion=cor(base_full_mice_complete[,vector_num_int])
View(MatrizCorrelacion)
#vemos que hay variables con Na de correlacion lo que se debe a que son constantes
#solo poseen un valor
unique(base_full_mice_complete$TES_ARCHIVOS_ESTATALES) # solo 0's
unique(base_full_mice_complete$CD_Otras)  #solo tiene 0's columna 52

#eliminamos las variables anteriores ya que no aportan al modelo
base_full_mice_complete=base_full_mice_complete%>%
  select(-TES_ARCHIVOS_ESTATALES, -CD_Otras)
#generamos un nuevao vector sin las variables eliminadas
vector_num_int=c()
for (i in 1:ncol(base_full_mice_complete)) {
  if (class(base_full_mice_complete[[i]])=="numeric") {
    vector_num_int[i]=i
  }
}
rm(i)
vector_num_int=vector_num_int[!is.na(vector_num_int)]

#nuevo analisis de correlacion entre variables predictivas para ver posible colinealidad
MatrizCorrelacion=cor(base_full_mice_complete[,vector_num_int])
View(MatrizCorrelacion)
#identificamos las variables con alta correlacion y por tanto colinealidad
highlyCorrelated=findCorrelation(MatrizCorrelacion,cutoff = 0.75)
highlyCorrelated

#vector con el nombre de las variables altamente correlacionadas con otras
#son susceptibles a eliminacion
Variables_Corr=colnames(MatrizCorrelacion[,highlyCorrelated])
Variables_Corr
rm(list = c("MatrizCorrelacion","highlyCorrelated","vector_num_int"))
#No siempre que exista colinealidad hay que proceder a eliminar las variables
#ya que por marco teorico su permanencia puede tene sentido.


#JUNTANDO base_full_mice_complete CON LA CLASE
base_full_mice_complete=cbind(base_full_mice_complete,CLASE)
#borramos antigua clase
rm(CLASE)

#separamos las antiguas bases "base" de "test"
base_full_DM=base_full_mice_complete%>%
  filter(is.na(CLASE)==FALSE)

base_full_DM_FianalTest=base_full_mice_complete%>%
  filter(is.na(CLASE)==TRUE)

#borramos la antigua base
rm(base_full_mice_complete)



#DE base_full_DM separamos entre base_training y base_validacion
index=createDataPartition(base_full_DM$CLASE,p = .70,list = FALSE)

base_DM_training=base_full_DM[index,] #base con la cual entrenamos el modelo
table(base_DM_training$CLASE)

base_DM_validacion=base_full_DM[-index,] #base para validar el modelo
table(base_DM_validacion$CLASE)
rm(index)

#De base_DM_validacion creamos 2 conjuntos, el primero para validar y
#ajustar cada modelo y el segundo para controlar el sobre ajuste entre modelos Hold Out
index=createDataPartition(base_DM_validacion$CLASE, p= .50, list = FALSE)
base_DM_v1=base_DM_validacion[index,]
table(base_DM_v1$CLASE)
base_dm_v2=base_DM_validacion[-index,]
table(base_dm_v2$CLASE)
rm(index)
#Asi tenemos 3 particiones 
#70% base_DM_training para entrenamiento del modelo
#15% base_DM_v1 para 1° validacion y optimizacion de c/modelo
#15% base_dm_v2 para 2° Hold Out y control de sobre ajuste




#DATA MINIMG ###############################################################
#RECURSIVE FEDFORWARD ELIMINATION########################################
#para saber con que sub conjunto de variables se logra el mejor accuracy
# usamos un control usando un random forest selection function
#Realizamos 2 RFE en el primero balanceamos una base son los 10.000 datos y luego RFE
# y otro sin balancear
#BALANCEO ROSE
library(ROSE)
table(base_full_DM$CLASE)
600/(600+9400)

# el p es la clase minoritaria o posible fraude
base_full_DM_rose4060<-ovun.sample(CLASE ~ .,data = base_full_DM,
                                   method = "both",p=0.40,seed=1)$data
table(base_full_DM_rose4060$CLASE)
3966/(3966+6034)

#aplicamos RFE
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# corremos el algoritmo RFE
results <- rfe(base_full_DM_rose4060[,1:50], base_full_DM_rose4060[,51], sizes=c(1:50), rfeControl=control)
# resumen del resultado
print(results)
# vector de las variabels elegidas
variables_RFE7_rose4060=predictors(results)
# grafico de los resultados
plot(results, type=c("g", "o"))
#dice que con 9 variabels se logra el mejor accuracy
rm(control,base_full_DM_rose4060)


#SELECCION POR CORRELACION######################################################
#Metodologia analoga a RFE pero utilizando correlaciones
#La idea es sacar correlacion individuales de una variable predictiva y la clase
#y seleccionar un subconjunto de variables con mayor correlacion

#importante ajustar cada particion de datos y no la base entera para no generar
#particiones de datos con instancias disitntas, lo cual no permite comapracion entre
#modleos totalmente pareja
#cramos otra base para no confundrinos
bfull=base_DM_training
bv1=base_DM_v1
bv2=base_dm_v2
bfull_FT=base_full_DM_FianalTest
#pasamos CLASE A numeric
bfull$CLASE=as.numeric(as.character(bfull$CLASE))
bv1$CLASE=as.numeric(as.character(bv1$CLASE))
bv2$CLASE=as.numeric(as.character(bv2$CLASE))
bfull_FT$CLASE=as.numeric(as.character(bfull_FT$CLASE))
#para identificar los factores
factores=c()
for (i in 1:ncol(bfull)) {
  if (class(bfull[,i])=="factor") {
    print(i)
    factores[i]=i
    factores=factores[!is.na(factores)]
  }
}
rm(i)
names(bfull[,factores])
#las variables binarizadas son
View(names(bfull[,15:51]))
#para no tener duplicados al binarizar todos los factores identificamos las bianrias
#ya creadas
names(bfull[,15:19]) #tipo empresa seguro
names(bfull[,20:23]) #poliza seguro
names(bfull[,32:38]) #banco
names(bfull[,42:43]) #sin estado actual
names(bfull[,47:50]) #cau descripcion
#eliminamos variables ya binarizadas que se pueden duplicar luego de binarizar los factores
bfull=bfull[,-c(15:19,20:23,32:38,42:43,47:50)]
bv1=bv1[,-c(15:19,20:23,32:38,42:43,47:50)]
bv2=bv2[,-c(15:19,20:23,32:38,42:43,47:50)]
bfull_FT=bfull_FT[,-c(15:19,20:23,32:38,42:43,47:50)]
rm(factores)

#binarizamos los factores
library(dummies)
dummies=dummyVars(~., data=bfull, sep = "_")
bfull=predict(dummies, bfull)
rm(dummies)
bfull=as.data.frame(bfull)

dummies=dummyVars(~., data=bv1, sep = "_")
bv1=predict(dummies, bv1)
rm(dummies)
bv1=as.data.frame(bv1)

dummies=dummyVars(~., data=bv2, sep = "_")
bv2=predict(dummies, bv2)
rm(dummies)
bv2=as.data.frame(bv2)

dummies=dummyVars(~., data=bfull_FT, sep = "_")
bfull_FT=predict(dummies, bfull_FT)
rm(dummies)
bfull_FT=as.data.frame(bfull_FT)

#para acortar los nombres
names(bfull)
for (i in 1:98) {
  names(bfull)[i]=paste("V",i,sep = "")
  names(bv1)[i]=paste("V",i,sep = "")
  names(bv2)[i]=paste("V",i,sep = "")
  names(bfull_FT)[i]=paste("V",i,sep = "")
}
rm(i)
#juntamos las bases
bfull_10000=rbind(bfull,bv1,bv2)
table(bfull_10000$CLASE)

#balanceamos 50 50
library(ROSE)
bfull_10000_rose5050<-ovun.sample(CLASE~.,data = bfull_10000,method = "both",
                            p=0.5,seed=1)$data

table(bfull_10000_rose5050$CLASE)
4953/(4953+5047)
rm(bfull_10000)

#hacemos la correlacion de variables predictivas respecto a CLASE
library(corrr)
library(dplyr)
correlacion1=bfull_10000_rose5050 %>% 
  correlate() %>% 
  focus(CLASE)

#Hay desviacion estandar cero en algunas variables
#por lo que son constantes, no aportan al modelo
#Se eliminan de la base por la cual filtraremos por correlacion
View(correlacion1)
which(is.na(correlacion1[,2]))
bfull_10000_rose5050=bfull_10000_rose5050[,-which(is.na(correlacion1[,2]))]


#hacemos de nuevo la correlacion
correlacion1=bfull_10000_rose5050 %>% 
  correlate() %>% 
  focus(CLASE)
View(correlacion1)
#subconjunto de variables con correlacion

correlacion2=correlacion1 %>% 
  filter(CLASE >=0.07 | CLASE <=-0.07 )

correlacion2=correlacion2[,1]

aux_variables_correlacion= c()
for ( i in 1:23){
  aux_variables_correlacion[i]=correlacion2[[i,1]]
}
rm(i)
rm(correlacion2)
rm(correlacion1)
aux_variables_correlacion

#Despues de la correlacion cambiaos CLASE a factor
bfull$CLASE=as.factor(bfull$CLASE)
bv1$CLASE=as.factor(bv1$CLASE)
bv2$CLASE=as.factor(bv2$CLASE)
bfull_FT$CLASE=as.factor(bfull_FT$CLASE)



#01 - MEJOR MODELO rf VARIABLES CORRELACION0.07 BALANCEO ROSE25#################
#creamos una base de entrenamento con un subconjunto de
#variables predictivas
which(names(bfull)%in%aux_variables_correlacion)
B=bfull[,c(which(names(bfull)%in%aux_variables_correlacion),99)]

#balanceamos 25%
library(ROSE)
B<-ovun.sample(CLASE~.,data = B,method = "both",
               p=0.25,seed=1)$data

table(B$CLASE)
1839/(1839+5161)

#entrenamos
library(randomForest)
mod_rf=randomForest(CLASE~.,data =B, mtry=4, ntree=440)

#prediciento v1
pred_rf=predict(mod_rf,bv1)
library(caret)
cm_rf_Corr007_rose25=confusionMatrix(pred_rf,bv1$CLASE, positive = '1')
cm_rf_Corr007_rose25 
#metricas aprox
#Accuracy : 0.9047
#Sensitivity : 0.45556
#Specificity : 0.93333


#predicionedo v2
pred_rf=predict(mod_rf,bv2)
cm_rf_Corr007_rose25_v2=confusionMatrix(pred_rf,bv2$CLASE, positive = '1')
cm_rf_Corr007_rose25_v2 
rm(list = c("pred_rf","mod_rf"))
#metricas aprox
# Accuracy : 0.8833
# Sensitivity : 0.35556        
#Specificity : 0.91702

rm(B)
#PREDICCION FINAL 10 000 datos###################################################
table(bfull$CLASE)
table(bv1$CLASE)
table(bv2$CLASE)
bfull_FT_train=rbind(bfull,bv1,bv2)
table(bfull_FT_train$CLASE)

bfull_FT_train_Cor007_rose25<-ovun.sample(CLASE~.,data =bfull_FT_train,
                                          method = "both",p=0.25,seed=1)$data

rm(bfull_FT_train)
table(bfull_FT_train_Cor007_rose25$CLASE)
2577/(7423+2577)
#ENTRENAMOS EL  MODELO
MOD_RF01=randomForest(CLASE~.,data =bfull_FT_train_Cor007_rose25, mtry=4, ntree=440)

#PREDICCION
PRED_RF01=predict(MOD_RF01,bfull_FT)
table(PRED_RF01)
5/(5+160)
View(PRED_RF01)
which(PRED_RF01==1)
rm(MOD_RF01,bfull_FT_train_Cor007_rose25)

#EXPORTAMOS EN UN TXT para GAMS
PRED_RF01_TXT=c()
for (i in 1:length(PRED_RF01)) {
  PRED_RF01_TXT[i]=paste("Y('",i,"')=",PRED_RF01[i],";",sep = "")
}
rm(i)
View(PRED_RF01_TXT)
write.table(PRED_RF01_TXT,
            file = "C:/Users/Hp i3/Google Drive/2019 S.8 Primavera/Business Inteligence/Clases/Tareas/Tarea_02/Predicciones R/Y(k)_rf_cor007_rose25.inc",
            row.names = F,sep = "\n",col.names = F,quote = F)

rm(PRED_RF01_TXT)

#MODEO SELECCIONADO
#02 - MEJOR MODELO rf VARIABLES CORRELACION0.07 BALANCEO ROSE30#####################
#creamos una base de entrenamento con un subconjunto de
#variables predictivas
which(names(bfull)%in%aux_variables_correlacion)
B=bfull[,c(which(names(bfull)%in%aux_variables_correlacion),99)]

#balanceamos 30%
B<-ovun.sample(CLASE~.,data = B,method = "both",
               p=0.3,seed=1)$data

table(B$CLASE)
2153/(2153+4847)

#entrenamos
mod_rf=randomForest(CLASE~.,data =B, mtry=4, ntree=440)

#prediciento v1
pred_rf=predict(mod_rf,bv1)
cm_rf_Corr007_rose30=confusionMatrix(pred_rf,bv1$CLASE, positive = '1')
cm_rf_Corr007_rose30 
#ratios aprox
#Accuracy : 0.894
#Sensitivity : 0.51111         
#Specificity : 0.91844 

#aprovechamos predicionedo v2 o hold out
pred_rf=predict(mod_rf,bv2)
cm_rf_Corr007_rose30_v2=confusionMatrix(pred_rf,bv2$CLASE, positive = '1')
cm_rf_Corr007_rose30_v2 
rm(list = c("pred_rf","mod_rf"))
#ratios aprox
#Accuracy : 0.8767 
#Sensitivity : 0.4000         
#Specificity : 0.9071


#PREDICCION FINAL 10 000 datos#######################################################
table(bfull$CLASE)
table(bv1$CLASE)
table(bv2$CLASE)
bfull_FT_train=rbind(bfull,bv1,bv2)
table(bfull_FT_train$CLASE)
class(bfull_FT_train$CLASE)

#balanceamos 30%
bfull_FT_train_Cor007_rose30<-ovun.sample(CLASE~.,data =bfull_FT_train,
                                          method = "both",p=0.3,seed=1)$data


rm(bfull_FT_train)
table(bfull_FT_train_Cor007_rose30$CLASE)
3036/(3036+6964)
#ENTRENAMOS EL  MODELO
MOD_RF01=randomForest(CLASE~.,data =bfull_FT_train_Cor007_rose30, mtry=4, ntree=440)

#PREDICCION
PRED_RF01=predict(MOD_RF01,bfull_FT)
table(PRED_RF01)
7/(7+158)
View(PRED_RF01)
which(PRED_RF01==1)
rm(MOD_RF01,bfull_FT_train_Cor007_rose30)

#EXPORTAMOS EN UN TXT
PRED_RF01_TXT=c()
for (i in 1:length(PRED_RF01)) {
  PRED_RF01_TXT[i]=paste("Y('",i,"')=",PRED_RF01[i],";",sep = "")
}
rm(i)
View(PRED_RF01_TXT)
write.table(PRED_RF01_TXT,
            file = "C:/Users/Hp i3/Google Drive/2019 S.8 Primavera/Business Inteligence/Clases/Tareas/Tarea_02/Predicciones R/Y(k)_rf_cor007_rose30.inc",
            row.names = F,sep = "\n",col.names = F,quote = F)

rm(PRED_RF01_TXT)
#EXPORTAMOS PREDICCION EN EXCEL
#EXPORTAR PREDICCION

write.csv(PRED_RF01,file="PREDICCION_MODELO_R.csv")


#Parte de data mining para encontrar mejor modelo
#variando el balanceo
B=bfull[,c(which(names(bfull)%in%aux_variables_correlacion),99)]
racc_v1=c()
rsty_v1=c()
rsfy_v1=c()
racc_v2=c()
rsty_v2=c()
rsfy_v2=c()
balanceo=seq(0.1,0.4,0.01)
library(ROSE)
for (i in 1:length(balanceo)) {
  BAS=ovun.sample(CLASE~.,data = B,method = "both",
                  p=balanceo[i],seed=1)$data
  mod_rf=randomForest(CLASE~.,data =BAS, mtry=4, ntree=500)
  pred_rf=predict(mod_rf,bv1)
  cm_rf=confusionMatrix(pred_rf,bv1$CLASE, positive = '1')
  racc_v1[i]=cm_rf$overall[[1]]
  rsty_v1[i]=cm_rf$byClass[[1]]
  rsfy_v1[i]=cm_rf$byClass[[2]]
  pred_rf=predict(mod_rf,bv2)
  cm_rf=confusionMatrix(pred_rf,bv2$CLASE, positive = '1')
  racc_v2[i]=cm_rf$overall[[1]]
  rsty_v2[i]=cm_rf$byClass[[1]]
  rsfy_v2[i]=cm_rf$byClass[[2]]
  
}
rm(i,BAS,mod_rf,pred_rf,cm_rf)
ratiosb=cbind(balanceo,racc_v1,rsty_v1,rsfy_v1,racc_v2,rsty_v2,rsfy_v2)
View(ratiosb)
rm(list=c("balanceo","racc_v1","rsty_v1","rsfy_v1","racc_v2","rsty_v2","rsfy_v2"))
rm(ratiosb)


#vriando mtry
racc_v1=c()
rsty_v1=c()
rsfy_v1=c()
racc_v2=c()
rsty_v2=c()
rsfy_v2=c()
ntrees=seq(400,600,20)
for (i in 1:length(ntrees)) {
  mod_rf=randomForest(CLASE~.,data =B, mtry=4, ntree=ntrees[i])
  pred_rf=predict(mod_rf,bv1)
  cm_rf=confusionMatrix(pred_rf,bv1$CLASE, positive = '1')
  racc_v1[i]=cm_rf$overall[[1]]
  rsty_v1[i]=cm_rf$byClass[[1]]
  rsfy_v1[i]=cm_rf$byClass[[2]]
  pred_rf=predict(mod_rf,bv2)
  cm_rf=confusionMatrix(pred_rf,bv2$CLASE, positive = '1')
  racc_v2[i]=cm_rf$overall[[1]]
  rsty_v2[i]=cm_rf$byClass[[1]]
  rsfy_v2[i]=cm_rf$byClass[[2]]
  
}
rm(i,mod_rf,pred_rf,cm_rf)
ratioss=cbind(ntrees,racc_v1,rsty_v1,rsfy_v1,racc_v2,rsty_v2,rsfy_v2)
View(ratioss)
rm(list=c("ntrees","racc_v1","rsty_v1","rsfy_v1","racc_v2","rsty_v2","rsfy_v2"))
rm(ratios)


#BALANCEO  DE TODA LA DATA###########################################################
#BALANCEO ROSE
library(ROSE)
table(base_DM_training$CLASE)
420/(420+6580)

# el p es la clase minoritaria o posible fraude
base_DM_training_rose22<-ovun.sample(CLASE ~ .,data = base_DM_training,
                                   method = "both",p=0.22,seed=1)$data
table(base_DM_training_rose22$CLASE)
1615/(1615+5385)




#De el RFE sin balanceo anterior se obtienen las siguientes variables
variables_RFE28=c("SIN_FEC_INIC_VIG_PRODUCTO","POLIZA_SEGURO","BANCO","CAU_DESCRIPCION","CD_Enfermedad","SIN_FEC_INIC_VIG_SINIESTRADO","EDAD","TIPO_EMPRESA_SEGURO","SUMA","CD_Despido","TES_ADECUACION_DE_OBRAS_DE_CONSTRUCCION","PRODUCTO_EX","TES_FABRICACION_DE_PRODUCTOS_MINERALES_NO_METALIC","DNI_ASEGURADO_10900762512",
                  "SIN_FECHA_OCURRENCIA","PS_52","HEV_TIME_MARK","TES_ACTIVIDADES_DIVERSAS_DE_INVERSION_Y_SERVICIOS","SIN_CUOTAS_COBERTURA","EDAD_35_50","SIN_FEC_DENUNCIA","LOT_FECHA_RECEPCION","LOT_EXTERNO_Y","SIN_ESTADO_ACTUAL","UL_Emp1","BANCO_SCC","LOT_EXTERNO_N","SEA_En_Evaluacion","CLASE")
          

#GRID SEARCH#####################################

#SVM_GS###########################################################################
set.seed(125)
library(MASS)
library(randomForest)
library(ggplot2)
library(tibble)
library(dplyr)
library(ipred)
library(caret)
#la clase es necesaria pasarla a nivel o level
B=base_DM_training_rose22
V=base_DM_v1
levels(B$CLASE)=c("NO","PF")
levels(V$CLASE)=c("NO","PF")
ctrl_svmag=trainControl(method='cv',number=10,verboseIter = TRUE,
                      summaryFunction = twoClassSummary,classProbs = TRUE)

mod_svmag=train(CLASE~.,B,method='svmPoly',trControl=ctrl_svmag,
                tuneLength= 6, metric='ROC')

#plot(mod_svmag)
#print(mod_svmag)
pred_svmag=predict(mod_svmag,V)
cm_svmag=confusionMatrix(pred_svmag,V$CLASE,positive = 'PF')
cm_svmag
print(cm_svmag)
rm(list=c("B","V","ctrl_svmag","mod_svmag","pred_svmag","cm_svmag"))

#SVM_normal
library(caret)
BASS=base_DM_training_rose22
V1=base_DM_v1
levels(BASS$CLASE)=c("NO","PF")
levels(V1$CLASE)=c("NO","PF")

#viendo la cantidad de folds optima
rat_acc_svm=c()
rat_sty_svm=c()
rat_sfy_svm=c()
vect_folds=seq(4,12,4)
for (i in 1:3) {
  ctrl_svm=trainControl(method = "cv",number = vect_folds[i],verboseIter = TRUE,
                        summaryFunction = twoClassSummary,classProbs = TRUE)
  mod_svm=train(CLASE ~ ., BASS,method = "svmPoly",
                metric ="ROC",trControl = ctrl_svm)
  pred_svm=predict(mod_svm,V1)
  cm_svm=confusionMatrix(pred_svm,V1$CLASE,positive = "PF")
  rat_acc_svm[i]=cm_svm$overall[[1]]
  rat_sty_svm[i]=cm_svm$byClass[[1]]
  rat_sfy_svm[i]=cm_svm$byClass[[2]]
  assign(paste("cm_svm",vect_folds[i],sep = "_"),cm_svm)
}

rm(list = c("i","ctrl_svm","mod_svm","pred_svm","BASS","V1","cm_svm"))
cm_svm_4
cm_svm_8
cm_svm_12
ratios_svm=cbind(vect_folds,rat_acc_svm,rat_sty_svm,rat_sfy_svm)
rm(list = c("vect_folds","rat_acc_svm","rat_sty_svm","rat_sfy_svm"))
rm(list = c("cm_svm_4","cm_svm_8","cm_svm_12"))





#DT_GS############################################################################
#Ahora que tenemos la base_DM_training_ROSE balanceado, podemos hacer un 
#grid search-->
ctrl_dtgs=trainControl(method = "repeatedcv",number = 10,repeats = 3,
                       verboseIter = TRUE,summaryFunction = twoClassSummary,
                       classProbs = TRUE)
#La siguiente funcion necesita que la variable clase esté en level en vez de 
#factor
class(base_DM_training_rose22$CLASE)
BASS=base_DM_training_rose22
V1=base_DM_v1
levels(BASS$CLASE)=c("NO","PF")
levels(V1$CLASE)=c("NO","PF")
#en base_DM_training_rose
mod_dtgs=train(CLASE ~ ., BASS,method = "C5.0",trControl = ctrl_dtgs,
                tuneLength = 10,metric ="ROC")

pred_dtgs=predict(mod_dtgs,V1)
cm_dtgs=confusionMatrix(pred_dtgs,V1$CLASE, positive='PF')
cm_dtgs
rm(BASS,V1)
rm(mod_dtgs,pred_dtgs,ctrl_dtgs)
rm(cm_dtgs)

#DT_GS## con base RFE28
ctrl_dtgs=trainControl(method = "repeatedcv",number = 10,repeats = 3,
                       verboseIter = TRUE,summaryFunction = twoClassSummary,
                       classProbs = TRUE)
#La siguiente funcion necesita que la variable clase esté en level en vez de 
#factor
class(base_DM_training_rose22$CLASE)
which(names(base_DM_training_rose22)%in%variables_RFE28)
BASS=base_DM_training_rose22[,which(names(base_DM_training_rose22)%in%variables_RFE28)]
V1=base_DM_v1[,which(names(base_DM_training_rose22)%in%variables_RFE28)]
levels(BASS$CLASE)=c("NO","PF")
levels(V1$CLASE)=c("NO","PF")
#grid search
mod_dtgs=train(CLASE ~ ., BASS,method = "C5.0",trControl = ctrl_dtgs,
               tuneLength = 10,metric ="ROC")

pred_dtgs=predict(mod_dtgs,V1)
cm_dtgs=confusionMatrix(pred_dtgs,V1$CLASE, positive='PF')
cm_dtgs
rm(BASS,V1)
rm(mod_dtgs,pred_dtgs,ctrl_dtgs)
rm(cm_dtgs)





#RANDOM FOREST GS############################################
BASS=base_DM_training_rose22
V1=base_DM_v1
levels(BASS$CLASE)=c("NO","PF")
levels(V1$CLASE)=c("NO","PF")

#configuracion de entrenamiento
ctrl_rfgs=trainControl(method="repeatedcv", number=10, repeats=3, 
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE)
#valores que puede tomar mtry (parametro de rf)
grid_rfgs=expand.grid(.mtry=c(1:ncol(BASS)-1))
#lista vacia para guradar los modelos
list_rfgs=list()
#probando distintos ntrees
for (i in seq(400,800,20)) {
  mod_rfgs=train(CLASE~.,data = BASS, method="rf",metric="ROC",tuneGrid=grid_rfgs,
                 ntree=i, trControl=ctrl_rfgs)
  key=toString(i)
  list_rfgs[[key]]=mod_rfgs
}
bosques=resamples(list_rfgs)
summary(bosques)
dotplot(results)
rf_list$`500`$bestTune
rf_list$`500`$results
rf_list$`500` %>%
plot()

rm(list = c("BASS","V1","ctrl_rfgs","grid_rfgs","mod_rfgs"))
rm(list_rfgs)
rm(i)
                    
                    
#FIN