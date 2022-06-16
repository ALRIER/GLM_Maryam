#======= Asesoría Álvaro 1 reunión ======#

# carga de datos
library(haven)
Data <- read_sav("C:/Users/Manuel Vargas/Desktop/UNAL/Asesorías/Alvaro (Fratelli)/Data.sav")


#------------------------ FINAL -------------------------#
#===================================================================================================#
####==================================== LIMPIEZA DE DATOS ====================================######
#===================================================================================================#

library(dplyr)

Subdata = Data[,c(2,3,4,10,17,18,20,27,28,34,35,36,37,73,77,81,82,304,305,38,39,40,41,9,19,26,
                  98,99,183,184,185,186,197,198,276,277,278,310,311,308,309,320,321,418,419,425:435,437:447)]



length(Subdata)

View(Subdata[,46:55])

Subdata$articulosAD = rowSums(Subdata[,46:56])
Subdata$articulosDD = rowSums(Subdata[,57:67])


Subdata[45,54] = 1 # Había un valor con error de digitiación

# Transformación de educación

Subdata$SD_NivelEduAD[Subdata$SD_NivelEduAD >=11] = 0
Subdata$SD_NivelEduActual[Subdata$SD_NivelEduActual >= 11] = 0


# Sector del trabajo

Subdata$AL29.1SectorTrabajoAD = recode(Subdata$AL29.1SectorTrabajoAD,
                                       "0" = "Agropecuario",
                                       "1" = "Industrial","3" = "Otro",
                                       "4"="Otro",
                                       "6" = "Otro", "8"="Otro")

Subdata$AL29.2SectorTrabajoDD = recode(Subdata$AL29.1SectorTrabajoAD,
                                       "0" = "Agropecuario",
                                       "1" = "Industrial","3" = "Otro",
                                       "8"="Otro"
)



#### Edad

Subdata$edad_despl = ifelse(Subdata$SD_EdadEnElDesplazamiento <=18, "<18",
                            ifelse(Subdata$SD_EdadEnElDesplazamiento <= 40, "40",">40"))


Subdata$EducAct = Subdata$SD_NivelEduActual

#----------------- Nivel de educación -----------------#
Subdata$EducAct = ifelse(Subdata$EducAct <=1,"Sin educ",ifelse(
  Subdata$EducAct <= 3, "Prim", ifelse(Subdata$EducAct <=6, "Secun", "Posg")
  
))


# Modificación de trabajo -----------------#

Subdata$ASE36.2FuenteIngresosDespués[Subdata$ASE36.2FuenteIngresosDespués=="Subsidio, Trabajo"] = "Trabajo"
Subdata$ASE36.2FuenteIngresosDespués[Subdata$ASE36.2FuenteIngresosDespués=="Propiedades"] = "Trabajo"


Subdata$ASE36.1FuenteIngresosAD[Subdata$ASE36.1FuenteIngresosAD=="Subsidio, Trabajo"] = "Trabajo"
Subdata$ASE36.1FuenteIngresosAD[Subdata$ASE36.1FuenteIngresosAD=="Subsidio, Trabajo"] = "Trabajo"
Subdata$ASE36.1FuenteIngresosAD[Subdata$ASE36.1FuenteIngresosAD=="Subsidio"] = "Trabajo"
Subdata$ASE36.1FuenteIngresosAD[Subdata$ASE36.1FuenteIngresosAD=="Propiedades"] = "Trabajo"

#-----------------------------------#

#---------- Número de hijos -------------------#
Subdata$AL30.1SuTrabajoEraAD = ifelse(Subdata$AL30.1SuTrabajoEraAD == 0, "Formal","Informal")
Subdata$AL30.2SuTrabajoDD = ifelse(Subdata$AL30.2SuTrabajoDD == 0, "Formal","Informal")

Subdata$`SD_NumHij@sAD` = ifelse(Subdata$`SD_NumHij@sAD` <= 1, "0-1",ifelse(Subdata$`SD_NumHij@sAD`<=4,"2-4","> 4"))
Subdata$`SD_NumHij@sDD` = ifelse(Subdata$`SD_NumHij@sDD` <= 1, "0-1",ifelse(Subdata$`SD_NumHij@sDD`<=4,"2-4","> 4"))


Subdata = Subdata[,-c(46:67)]  # Estoy eliminando las dummies


# El desplazamiento mejoró su calidad de vida. Esa pregunta tiene fallas. La omitiremos.


# Para la cantidad de personas quqe dependen de usted, hacer gráfico descriptivo. Lo mismo con # de personas


Subdata$LV58.1TipoViviendaHabitabaAD = recode(Subdata$LV58.1TipoViviendaHabitabaAD,
                                              "0" = "Otro", "1" = "Casa","2" = "Apto",
                                              "3" = "Pieza", "7" = "Campo")


Subdata$LV58.2TipoViviendaHabitabaDD = recode(Subdata$LV58.2TipoViviendaHabitabaDD,
                                              "0" = "Otro", "1" = "Casa",
                                              "3" = "Pieza","4"="Carpa" ,"7" = "Campo",
                                              "9" = "Otro")

# VPrest= vivienda prestada: esto incluye a personas que viven sin pagar (cuenta familiares)
Subdata$LV59.1DondeViviaEraAD = recode(Subdata$LV59.1DondeViviaEraAD,
                                       "1" = "Prop", "2" = "VPrest", "3" = "Arrend",
                                       "4" = "Arrend","5" = "VPrest")

Subdata$LV59.2DondeViveEsDD = recode(Subdata$LV59.1DondeViviaEraAD,
                                     "2" = "VPrest","3"="Arrend", "6"="Subs")

# NR: No responde
Subdata$S64.1EstadoDSaludAD = recode(Subdata$S64.1EstadoDSaludAD,
                                     "0" = "NR", "1" = "Malo",
                                     "2" = "Regular", "3" = "Bueno")


Subdata$S64.2EstadoDSaludDD = recode(Subdata$S64.2EstadoDSaludDD,
                                     "0" = "NR", "1" = "Malo",
                                     "2" = "Regular", "3" = "Bueno")

# Aquí no hay mayor cambio, tal vez se puedan dejar así... (univariados)
table(Subdata$AN81.1FrecuenciaCompraAlimentosAlMesAD)
table(Subdata$AN81.2FrecuenciaCompraAlimentosAlMesDD)


table(Subdata$AN92.1FrecuenciaConsumoPescadoAD)
table(Subdata$AN92.2FrecuenciaConsumoPescadoDD)


Subdata$AN92.1FrecuenciaConsumoPescadoAD = recode(Subdata$AN92.1FrecuenciaConsumoPescadoAD,
                                                  "0" = "NA", "1" = "Diario",
                                                  "2" = "Sem", "3" = "Quinc", "4" = "Mens","5" = "Nunca")

Subdata$AN92.2FrecuenciaConsumoPescadoDD = recode(Subdata$AN92.2FrecuenciaConsumoPescadoDD,
                                                  "0" = "NA", "1" = "Diario",
                                                  "2" = "Sem", "3" = "Quinc", "4" = "Mens","5" = "Nunca")

table(Subdata$AN92.1FrecuenciaConsumoPescadoAD,Subdata$AN92.2FrecuenciaConsumoPescadoDD)


Subdata$AN92.1FrecuenciaConsumoPescadoAD = recode(Subdata$AN92.1FrecuenciaConsumoPescadoAD,
                                                  "0" = "NA", "1" = "Diario",
                                                  "2" = "Sem", "3" = "Quinc", "4" = "Mens","5" = "Nunca")

Subdata$AN93.1FrecuenciaConsumoPollOAD = recode(Subdata$AN93.1FrecuenciaConsumoPollOAD,
                                                "0" = "NA", "1" = "Diario",
                                                "2" = "Sem", "3" = "Quinc", "4" = "Mens","5" = "Nunca")

Subdata$AN93.2FrecuenciaConsumoPolloDD = recode(Subdata$AN93.2FrecuenciaConsumoPolloDD,
                                                "0" = "NA", "1" = "Diario",
                                                "2" = "Sem", "3" = "Quinc", "4" = "Mens","5" = "Nunca")

table(Subdata$AN93.1FrecuenciaConsumoPollOAD,Subdata$AN93.2FrecuenciaConsumoPolloDD)


Subdata$EH98.1AQuéSistemaEducativoPerteneciaAD = recode(Subdata$EH98.1AQuéSistemaEducativoPerteneciaAD,
                                                        "0" = "No resp","1" = "Público","2" = "Privado")

Subdata$EH98.2AQuéSistemaEducativoPerteneDD = recode(Subdata$EH98.2AQuéSistemaEducativoPerteneDD,
                                                     "0" = "No resp","1" = "Público","2" = "Privado")

## 
table(Subdata$V134.1CadaCuantoAdquiriaLaRopaAD)

table(Subdata$V134.2CadaCuantoAdquiriaLaRopaDD)

Subdata$V134.1CadaCuantoAdquiriaLaRopaAD = recode(Subdata$V134.1CadaCuantoAdquiriaLaRopaAD,
                                                  "2" = "Quincenal",
                                                  "3" = "Mensual",
                                                  "4" = "Anual")

Subdata$V134.2CadaCuantoAdquiriaLaRopaDD = recode(Subdata$V134.2CadaCuantoAdquiriaLaRopaDD,
                                                  "2" = "Mensual",
                                                  "3" = "Mensual",
                                                  "4" = "Anual")


#Subdata$articulosAD = recode(Subdata$articulosAD,
#                            "3" = "3-5", "4" = "3-5","5" = "3-5",
#                           "6" = "6-7", "7" = "6-7",
#                          "8" = ">8", "10" = ">8","11"=">8")

#Subdata$articulosDD = recode(Subdata$articulosDD,
#                            "0" = "0-2", "1" = "0-2","2" = "0-2",
#                           "3" = "3-4", "4" = "3-4",
#                          "5" = ">4", "6" = ">4")

### Separación de datos en ANTEA y DESPUES  ###


antes = c(1,14,16,20,22,24,25,27,29,31,33,35,38,40,42,44,46,48)

despues = c(1,4,15,17,21,23,28,30,32,34,39,41,43,45,47,48,49)


ANTES = Subdata[,antes]
DESP = Subdata[,despues]

colnames(ANTES) <- c("Sexo","InFue","Ingr","SecTr","Trab","Hijos",
                     "#Per","PerDep","Vivienda","Prop","Salud",
                     "CompAlim","C.Pollo","C.Pesc",
                     "Sis.Edu","Ropa","Articulos","edad")
# ACM para antes
table(Subdata$EducAct)
table(Subdata$articulosAD)
table(Subdata$articulosDD)

table(Subdata$V134.2CadaCuantoAdquiriaLaRopaDD)
table(Subdata$V134.1CadaCuantoAdquiriaLaRopaAD)


table(Subdata$EH98.1AQuéSistemaEducativoPerteneciaAD)
table(Subdata$EH98.2AQuéSistemaEducativoPerteneDD)

# La de sistema educativo no entra en ACM. Tampoco frecuencia de alimentos




library(FactoClass)
library(FactoMineR)
library(factoextra)
tabla = as.data.frame(ANTES)
class(tabla)


variables = Fac.Num(tabla)
dim(variables$numeric)
dim(variables$factor)
cate_antes = variables$factor

cate_antes = cbind(cate_antes,variables$numeric$Ingr)
antes=antes[,-1]
cate_antes<-antes
cate_antes=cate_antes[,-14]
ACMANT = MCA(cate_antes, graph = F)

# Vamos a ver cuántos ejes retener
fviz_contrib(ACMANT, choice = "var",repel =TRUE, axes = c(1,2))
# Nos vamos a quedar con 5 ejes

# Vamos a aver las variables que más contribuyen al primer plano factorial
fviz_contrib(ACMANT, choice = "var", axes = c(1,2))
fviz_contrib(ACMANT, choice = "var", axes = c(3,4))

# Vamos a leer el plano 1-2 y el 3-4

#fviz_cos2(ACMANT, choice = "var", axes = c(1,2))

cotribucionantes<-round(ACMANT$var$contrib,3)
options(ggrepel.max.overlaps = Inf)
fviz_mca_var(ACMANT, repel = TRUE, axes = c(3,4))


#fviz_mca_var(ACMANT, choice = "quanti.sup")

# Categorías a leer en el primer plano:
# Masculino, No responde, Agropecuario, Industrial, Sector otro,
# Formal, Informal, >4, 0-1, Apto, Pieza (no mucho)..., Malo, NR, Regular,
# C. Pollo diario, C. Pescado diario, Pescado Nunca, Pesc Quincenal, Quincenal 
# Público, anual

fviz_mca_var(ACMANT, repel = TRUE)

# Categorías a leer en el segundo plano (ejes 3 y 4):
# Industrial, Vivienda campo, V prest, C.Pollo mensual, Pes_sem, Privado, 40


# CLUSTERING: Se va a hacer una combinación de métodos factoriales 
# con el agrupamiento jerárquico
set.seed(123)
cluster = HCPC(ACMANT)
cluster$desc.var$category

# Para el cluster 1: 
# El 91.7% de las personas que compran su ropa anualmente se
# encuentran en este grupo.

# El 68.75% de las personas que están en este grupo compran
# ropa con frecuencia anual. 

# El 100% de las personas que no consumen pescado se encuentran en esta clase.

# El 81.25% de las personas de estre grupo tienen entre 0 y 1 hijos.



#-------------------- Después --------------------------#
DESP = cbind(DESP, na.omit(Data$AN81.1FrecuenciaCompraAlimentosAlMesAD))
colnames(DESP) <- c("Sexo","#Hij","InFue","Ingr","SecTr","Trab",
                    "PerDep","Vivienda","Prop","Salud",
                    "C.Pollo","C.Pesc",
                    "Sis.Edu","Ropa","Articulos","edad","educ","CompAlim")

# Frecuencia de alimentos es univariado

tablad = as.data.frame(DESP)
class(tablad)
variables = Fac.Num(tablad)
dim(variables$numeric)
dim(variables$factor)
cate_de = variables$factor

ACMD = MCA(cate_de)#, level.ventil = 0.05)

fviz_mca_var(ACMD, repel = TRUE)

fviz_contrib(ACMD, choice = "var",repel =TRUE, axes = c(1,2))

contribuciondespues<-round(ACMD$var$contrib,3)

# Ejes 1 y 2:
# Masculino, >4 hijos, 2-4 hijos, Agropecuario, Industrial Sector otro, 
# Formal, Informal. Viviendas, V presstada, "Salud regular"
# Pollo diairio, Pollo nunca, Pollo quincenal, Pollo semanal,
# Pesc mensual, Pes NA, Pes quincenal, No resp,
# Público, Mensual, Quincenal, , <18, >40, 40, Prim, Sin educ...


round(ACMD$var$coord,3)

# El primer eje contrapone el sexo masculino con 
# el trabajo formal y aquellas personas(mujeres) menores de 18 años.

set.seed(123)
cluster = HCPC(ACMD)
cluster$desc.var


contantes<-as.data.frame(cotribucionantes)
contdespues<-as.data.frame(contribuciondespues)
setwd("/home/alrier/Documentos")
write.csv(contdespues, file = "contribucionDespues.csv", fileEncoding = "UTF-8",
          row.names = T)

write.csv(DESP, file = "SubdataDespues.csv", fileEncoding = "UTF-8",
          row.names = T)
