pkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","raster","sf","ggspatial","cluster","factoextra",
              "NbClust","tidyr","forecast","semPlot","semTools","corrplot",
              "corrr","haven","psych","dplyr","lavaan","readr","cvms","tm",
              "NLP","SnowballC","RColorBrewer","wordcloud","wordcloud2",
              "RefManageR","bibliometrix","GGally","quanteda","ggplot2",
              "ggpubr","Factoshiny","syuzhet","tm","RColorBrewer","tokenizers",
              "stringr","sentimentr","stringi","stopwords","twitteR",
              "mscstexta4r","plyr","corrplot","psych","corrr","latticeExtra",
              "semPlot","lavaan","readr","lme4","sjPlot","gvlma","Rcmdr",
              "tidymodels","caret","lmtest")

pkg(packages)



#split data 50
#cargo el documento
data <- read_csv("Documentos/Mis articulos/T_d_c.csv")
#Hago el split de los datos con el filtro elegido
data50 <-data%>% filter(retweet_count >=50)
#corro el odelo 
modpoisson50<-glm(retweet_count~ENTIDAD_CODIGO+favorite_count+is_retweet+Photo+Vídeo+Only_text+Url+
                    quote+hour+weekday+Hashtag_Cod+month,family= poisson, data = data50(link = "log"))
#corro el sumario de datos
S50<-summary(modpoisson50)
#corro coeficientes restando 1 para ver efectivamente como queda la relación
exp(S50coefficients)-1
#Bondad de ajuste del modelo
lrtest(modpoisson50)
#split data 100
data <- read_csv("Documentos/Mis articulos/T_d_c.csv")
data100 <-data%>% filter(retweet_count >=100)
modpoisson100<-glm(retweet_count~ENTIDAD_CODIGO+favorite_count+is_retweet+Photo+Vídeo+Only_text+Url+
                     quote+hour+weekday+Hashtag_Cod+month,family= poisson, data = data100(link = "log"))
S100<-summary(modpoisson100)
exp(S100$coefficients)-1
lrtest(modpoisson100)
#split data 200
data <- read_csv("Documentos/Mis articulos/T_d_c.csv")
data200 <-data%>% filter(retweet_count >=200)
modpoisson200<-glm(retweet_count~ENTIDAD_CODIGO+favorite_count+is_retweet+Photo+Vídeo+Only_text+Url+
                     quote+hour+weekday+Hashtag_Cod+month,family= poisson, data = data200(link = "log"))
S200<-summary(modpoisson200)
exp(S200$coefficients)-1
lrtest(modpoisson200)
#split data 500
data <- read_csv("Documentos/Mis articulos/T_d_c.csv")
data500 <-data%>% filter(retweet_count >=500)
modpoisson500<-glm(retweet_count~ENTIDAD_CODIGO+favorite_count+is_retweet+Photo+Vídeo+Only_text+Url+
                     quote+hour+weekday+Hashtag_Cod+month,family= poisson, data = data500(link = "log"))
S500<-summary(modpoisson500)
exp(S500$coefficients)-1
lrtest(modpoisson500)
#split data 1000
data <- read_csv("Documentos/Mis articulos/T_d_c.csv")
data1000 <-data%>% filter(retweet_count >=100)
modpoisson<-glm(retweet_count~ENTIDAD_CODIGO+favorite_count+is_retweet+Photo+Vídeo+Only_text+Url+
                  quote+hour+weekday+Hashtag_Cod+month,family= poisson, data = data1000(link = "log"))
S<-summary(modpoisson)
exp(S$coefficients)-1
lrtest(modpoisson)
#exporto los documentos a un excel 
write.table(MFCM, file="most frequent cited manuscripts source 
            covid.csv", sep=";", row.names= T) 
#aquí engancho el data para que quede fijo
attach(data)
################# Lineal modelo generalizado ################################
##############################################################################

#Aquí tengo el modelo poisson con las variables correspondientes. 
modpoisson<-glm(retweet_count~ENTIDAD_CODIGO+favorite_count+is_retweet+Photo+Vídeo+Only_text+Url+
                  quote+hour+weekday+Hashtag_Cod+month,family= poisson(link = "log")) 
#Sacoel sumario del modelo para ver como se ajusta el modelo
S<-summary(modpoisson)
S
#aquí tomo cada valor del error standar y lo divido entre el 
#valor Z para tener el valor Z o P valor del modelo y ver la 
# significancia estadística de los valores. 
4.199/0.002168
#saco los coeficientes del modelo que me dirán la predicción
#de las variables sobre mi variable Y  
exp(S$coefficients)-1
#Bondad de ajuste del modelo
lrtest(modpoisson)

#gvlma(S)   ----> Solo sirve para lm y no para GLM
#+is_retweet

#GLM BINOM
#si la variable es dicotomica entonces se usa binomial con un link de tipo
#LOGIT O PROVIT.
#cambio el attach para partir la database en una porción menor
attach(data)
modbinom1<-glm(is_retweet~ENTIDAD_CODIGO+favorite_count+Photo+Vídeo+Only_text+Url+
                 quote+hour+weekday+Hashtag_Cod+month,family= binomial(link = "logit")) 
S1<-summary(modbinom1)
S1
#corro step para comparar modelos y que R me diga cual es mejor.
step(modbinom1)
exp(S1$coefficients)-1
lrtest(modbinom1)
#comparar los dos modelos para ver cual es más significativa
#Efectivamente se puede nservar que el modelo poisson es más significativo
#debido a que los tweeets son un conteo en sí mismos, por tal razón contar tweets
#tiene más sentido que convertirlos a categoricas en 0,1. 
#aqí se debe citar a Neldery waterman para referirse a las mejorías del link log y no 
#al link logit en este tipo de casos
#lrtest(modbinom,modpoisson)
#entonces se observa que el poisson es mucho mejor
#monto el binom2
modbinom2<-glm(is_retweet~ENTIDAD_CODIGO+Photo+Vídeo+Only_text+Url+
                 hour+weekday+month,family= binomial(link = "logit"))
S2<-summary(modbinom2)
S2
#corro step para comparar modelos y que R me diga cual es mejor.
exp(S2$coefficients)-1
lrtest(modbinom2, modbinom1)

#Reviso la tabla de Odds ratio para identificar la razón de riego del modelo
tab_model(mod1)
exp(1)


#por cada entidad de codigo el conteo de retweet aumenta en 0.77, si aumenta 
#only_text en 1 unidad, entonces los retweets aumentan en 0.68, si aumenta el 
#video 1 unidad el retweet aumenta en 2 conteos. (la relacion siempre de 1 a lo 
#que quede despues de 1)
xtable(S)
write.table(S, file="Modelo2.csv", sep=";", row.names= T)


#### Prueba con LM para verificar los cambios en el modelo #####
mod2<-lm(retweet_count~ENTIDAD_CODIGO+favorite_count+Photo+Vídeo+Only_text+Url+
           quote+hour+weekday+Hashtag_Cod+month,family= poisson(link = "provit"))
S2<-summary(mod2)
S2
anova(mod2)



##############################################################################
######### Creando tidymodels ### building tidymodels ########################
#############################################################################
#first see de documentation to get guide
?tidymodels
#first i will split my data in 2 parts for my models.
L <- data %>% initial_split(prop = .8)

#procesing the data
Dt = training(L) %>% 
  recipe(retweet_count~ENTIDAD_CODIGO)%>%
  #step_corr(all_predictors())%>% step_center(all_predictors(),
  #-all_outcomes()) %>% step_scale(all_predictors(), -all_outcomes()) %>%
  prep()
#pre-procesing
gt = Dt%>%
  bake(testing(L))
#LM
g_freq = linear_reg()%>% #it could be a random forest, or other method.
  set_engine("lm") %>%  #i declare the pakage that i will use in this case is lm
  fit(retweet_count~ENTIDAD_CODIGO, data=Dt %>%juice())
#finaly i declare the fit of the model and call the juice wich "exprime" el modelo
#para ver si se ajusta al máximo.
g_freq
summary(g_freq)
gvlma(g_freq)


#Modelo poisson sobre todas las variables del estudio teniendo en cuenta a 
#ASE32.1CuantasPersonasTieneIngresoHogarDD y Ates del Desplazamiento

attach(Data)

#ASE32.1CuantasPersonasTieneIngresoHogarDD
#,family= poisson(link = "log"))
modpoisson1
a<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~SD_CuantasPersonasVivenConUstedAD+AD14CiudadViviaAD+AL27.1OcupaciónAD+AL28.1TipoContratoAD+AL30.1SuTrabajoEraAD+AL31.1CuántoGanabaMensualmenteAD+ASE33.1VinculoFamiliarTieneMayorIngresoHogarAD+ASE33.1.1FamiliarTieneMayorIngresoHogarAD+ASE33.1.2FamiliarTieneMayorIngresoHogarAD+ASE33.1.3FamiliarTieneMayorIngresoHogarAD+ASE33.1.4FamiliarTieneMayorIngresoHogarAD+ASE34.1QuienAportaMásDineroCasaAD+
                   ASE34.1.1QuienAportaMásDineroCasaAntesPadreAD+ASE34.1.6QuienAportaMásDineroCasaAntesYoAD+ASE36.1FuenteIngresosAD+ASE37.1IngresosMensualesCasaDolaresAD+ASE38.1.2PersonaEncargadaDeFamiliaADMadre+ASE38.1.3PersonaEncargadaDeFamiliaADHijos+ASE38.1.4PersonaEncargadaDeFamiliaADHermanos+ASE38.1.5PersonaEncargadaDeFamiliaADYo+ASE38.1.6PersonaEncargadaDeFamiliaADOtro+ASE39.1PersonasDependenUstedAD+ASP41.1antesSuViviendaContabaConServicioEnergíaAD+
                   ASP44.1SuViviendaContabaConServicioRecogidaBasuraAD+ASP45.1SuViviendaContabaConServicioLíneaTelefonicaFijaAD+ASP47.1SuViviendaContabaConServicioInternetAD+ASP48.1SuViviendaContabaConServicioGasPropanoAD+ASP49.1CuálEsElServicioMásCaroAD+ASP49.1.1CuálEsElServicioMásCaroADAlcantarillado+ASP49.1.2CuálEsElServicioMásCaroADGas+ASP49.1.3CuálEsElServicioMásCaroADLuz+ASP49.1.4CuálEsElServicioMásCaroADAgua+ASP49.1.5CuálEsElServicioMásCaroADTelefonia+
                   ASP50.1CostosMensualesServiciosADUSD+ASP51.1ComoEsLaCalidadDeLosServiciosAD+ASP52.1QueHacíaConLosResiduosAD+TL153.1.7ActividadesRealizaEnTiempoLibreADHacerMercado+TL153.1.8ActividadesRealizaEnTiempoLibreADArreglosmecánicos+TL153.1.9ActividadesRealizaEnTiempoLibreADVertelevisión+TL154.1ElmarotrosLugaresNaturalesPreferidosParaTiempoLibreAD+TL155.1EnTiempoLibreComparteFamiliaAD+TL156.1EnTiempoLibrePrefiereActividadesDomésticasAD+
                   TL153.1ActividadesRealizaEnTiempoLibreAD+TL153.1.1ActividadesRealizaEnTiempoLibreADTrabajar+TL153.1.2ActividadesRealizaEnTiempoLibreADRezar+TL153.1.3ActividadesRealizaEnTiempoLibreADComer+TL153.1.4ActividadesRealizaEnTiempoLibreADAyudarLaboresDomestica+TL153.1.5ActividadesRealizaEnTiempoLibreADDormir+TL153.1.6ActividadesRealizaEnTiempoLibreADBuscartrabajo+AL147.1TeníaMarcasPreferidasAseoPersonalAD+AL148.1GastoMensualProductosAseoPersonalADUSD+
                   AL149.1UstedoFamiliaSeBañanAireLibreAD+AL150.1DondeBañabanRegularmenteAD+C151.1HaSolicitadoCréditosoPrestamosDineroAD+TL152.1LugarDondePasaTiempoLibreAD+TL152.1.1LugarDondePasaTiempoLibreADCentrosComerciales+TL152.1.2LugarDondePasaTiempoLibreADParque+TL152.1.3LugarDondePasaTiempoLibreADCasa+TL152.1.4LugarDondePasaTiempoLibreADCasaAmigosFamiliares+AL145.1GastoMensualProductosLimpiezaUSDAD+AL146.1ProductosAseoPersonalUtilizabaAD+
                   AL146.1.1ProductosAseoPersonalUtilizabaADJabónBarra+AL146.1.2ProductosAseoPersonalUtilizabaADShampoo+AL146.1.3ProductosAseoPersonalUtilizabaADJabónintimo+AL146.1.4ProductosAseoPersonalUtilizabaADCremaAfeitar+AL146.1.5ProductosAseoPersonalUtilizabaADPapelHigiénico+AL146.1.6ProductosAseoPersonalUtilizabaADCremacuerpo+AL146.1.7ProductosAseoPersonalUtilizabaADAcondicionador+AL146.1.8ProductosAseoPersonalUtilizabaADDesodorante+
                   AL144.1ProductosDLimpiezAMásUsadosAD+AL144.1.1ProductosDLimpiezAMásUsadosADJabónbarra+AL144.1.2ProductosDLimpiezAMásUsadosADBlanqueador+AL144.1.3ProductosDLimpiezAMásUsadosADJabónCocina+AL144.1.4ProductosDLimpiezAMásUsadosADLimpiavidrios+AL144.1.5ProductosDLimpiezAMásUsadosADTrapero+AL144.1.6ProductosDLimpiezAMásUsadosADLimpiaPisos+AL144.1.7ProductosDLimpiezAMásUsadosADJabónpolvo+AL144.1.8ProductosDLimpiezAMásUsadosADEscoba+
                   T137.1QuéElementosTeCnologicosEstabanSuoHogarAD+T137.1.1QuéElementosTeCnologicosEstabanSuoHogarADTelevisor+T137.1.2QuéElementosTeCnologicosEstabanSuoHogarADEquipoDSonido+T137.1.3QuéElementosTeCnologicosEstabanSuoHogarADNevera+T137.1.4QuéElementosTeCnologicosEstabanSuoHogarADLavadora+T137.1.5QuéElementosTeCnologicosEstabanSuoHogarADEstufa+T137.1.6QuéElementosTeCnologicosEstabanSuoHogarADHornoMicroonda+
                   T137.1.7QuéElementosTeCnologicosEstabanSuoHogarADComputador+T137.8QuéElementosTeCnologicosEstabanSuoHogarADTeléfonoFijo+T137.1.9QuéElementosTeCnologicosEstabanSuoHogarADCelular+T137.1.10QuéElementosTeCnologicosEstabanSuoHogarADRadio+T137.1.11QuéElementosTeCnologicosEstabanSuoHogarADLicuadora+T138.1ElectrodomesticoMásImportanteSuCasaAD+T138.1.1ElectrodomesticoMásImportanteSuCasaADTelevisor+T138.1.2ElectrodomesticoMásImportanteSuCasaADNevera+
                   T138.1.3ElectrodomesticoMásImportanteSuCasaADLavadora+T138.1.4ElectrodomesticoMásImportanteSuCasaADComputador+T138.1.5ElectrodomesticoMásImportanteSuCasaADCelular+T138.2.1ElectrodomesticoMásImportanteSuCasaADTelevisor+T138.2.2ElectrodomesticoMásImportanteSuCasaADNevera+T138.2.3ElectrodomesticoMásImportanteSuCasaADLavadora+T138.2.4ElectrodomesticoMásImportanteSuCasaADCelular+T139.1ElectrodomesticoPreferidoPorHijosAD+T139.1.1ElectrodomesticoPreferidoPorHijosADTelevisor+
                   T139.1.2ElectrodomesticoPreferidoPorHijosADLavadora+T139.1.3ElectrodomesticoPreferidoPorHijosADComputador+T139.1.4ElectrodomesticoPreferidoPorHijosADCelular+T140.1ElectrodomesticosUtilizabanParaEntretenimientoAD+T140.1.1ElectrodomesticosUtilizabanParaEntretenimientoADTeleviso+T140.1.2ElectrodomesticosUtilizabanParaEntretenimientoADEquipoDs+T140.1.3ElectrodomesticosUtilizabanParaEntretenimientoADCelulari+T140.1.4ElectrodomesticosUtilizabanParaEntretenimientoADCelularb+
                   T141.1CómoObteníaElectrodomesticosAD+T142.1DóndeAdquirióLosELectrodomesticosAD+AL143.1ComprabaProductosAseoParaCasaAD+R122.2.1Activ.RealizáParaDivertirseADHablarAmigos+R122.2.2Activ.RealizáParaDivertirseADJugarFútbol+R122.2.3Activ.RealizáParaDivertirseADCaminarFamilia+R122.2.4Activ.RealizáParaDivertirseADArreglarLimpiar+R122.2.5ActivRealizabaParaDivertirseADAsistirMesquitaMisaCultosR+R123.1HaConsumidoAgunaDrogaAD+R128.1.1TipoDPropiedadesADCasa+
                   R128.1.2TipoDPropiedadesADApartamento+R125.1CuántoDineroGastabaEnRecreaciónMesAD+R126.1TeníaPropiedadesAD+R128.1.3TipoDPropiedadesADCasa+R130.1CómoObtuvoPropiedadADTeniaPapelesDPropiedadAD+V135.1TienePreferenciAPorAlgunaMarcaAD+V134.1CadaCuantoAdquiriaLaRopaAD+V133.1CómoAdquiriaRopaAD+V136.1TienePreferenciaAlgunaMarcaDCalzadoAD+S72.1SusHijosNacieroEnAD+S72.1.1SusHijosNacieroEnADHospital+S72.2SusHijosNacieroEnADCasa+S72.1.3SusHijosNacieroEnADOtro+
                   EH98.1AQuéSistemaEducativoPerteneciaAD+EH99.1LosNiñosRecibiánEducaciónAD+EH100.1ComoObteniaLosÚtilesEscolaresAD+EH101.1QuienLeDonabaORegalabaÚtilesAD+EH102.1DemorabaMásDeUnaHoraLlegarAlColegioAD+EH104.1LeCobrabanEducaciónHijosAD+EH105.1HijosHanReprobadoAlgunAñoAD+EH105.2HijosHanReprobadoAlgunAñoDD+EH106.1PorqueReprobaronAD+EH107.1FrecuenciaCompraÚtilesEscolaresAD+EH103.1MedioTrasporteUtilizabaParAllegarEscuelaAD+AN87.1MercadosDonadosAD+AN88.1SubsidioParaCompraDAlimentosAD+
                   AN89.1ComidaConsumdaDiariamenteAD+AN89.1.1ComidaConsumdaDiariamenteADDesayuno+AN89.1.2ComidaConsumdaDiariamenteADMediasNueves+AN89.1.3ComidaConsumdaDiariamenteADAlmuerzo+AN89.1.4ComidaConsumdaDiariamenteADComida+AN89.1.5ComidaConsumdaDiariamenteADOnces+AN89.2.4ComidaConsumdaDiariamenteADComida+AN90.1FrecuenciaConsumoCarneDResAD+AN94.1FrecuenciaConsumoComidasRapidasAD+EH95.1ViviaConHijosAD+EH108.1CuantoGastabaCompraÚtilesAD+EH109.1NivelEducativOMásAltoAlcanzadoHijosAD+
                   AD15Motivosdesplazamiento+AD16TipoDelDesplazamiento+AD17AQueGobernaturaLlego+AD18ElLugarDondeLlegoAVivir+AD19DesplazamientoMejoróCalidadVida+AD20CuantasVecesDesplazado+AD21DondeViveActualmente+AD22SeDesplazoPorNoConseguirEmpleo+AD23SeDesplazoPorLugarDondeViviaEraPeligroso+AD24SeDesplazoPorVecinosNoEstabanConformes+AD25AñoUltimoDesplazamiento+AD26ActorDesplazamiento+ASE34.1.1QuienAportaMásDineroCasaAntesPadreAD+ASE34.1.2QuienAportaMásDineroCasaAntesMadreAD+
                   ASE34.1.3QuienAportaMásDineroCasaAntesHijosAD+ASE34.1.4QuienAportaMásDineroCasaAntesHermanosAD+SD_NivelEduAD+AL29.1SectorTrabajoAD+ASE32.1CuantasPersonasTieneIngresoHogarAD+S78.1NoRecibioServiciosAFaltaDEquiposOProfesionalesAD+S79.1AlSolicitarServicioDSaludAtendíanInmediatamenteAD+S80.1ConsiderabaLaPrestaciónDelServicioDSAludAD+AN81.1FrecuenciaCompraAlimentosAlMesAD+AN84.1CultivandoAD+ASE38.1PersonaEncargadaDeFamiliaAD+ASE38.1.1PersonaEncargadaDeFamiliaADPadre+
                   ASP40.1SuViviendaContabaConServicioGasAD+ASP43.1SuViviendaContabaConServicioAlcantarilladoAD+ASP46.2SuViviendaContabaConServicioTVCableAD+LV53.1TechoAD+LV53.1.1TechoADTejaDBarro+LV53.1.2TechoADConcreto+LV53.1.3TechoADLata+LV53.2TechoAD+LV53.2.1TechoADZinc+LV53.2.2TechoADPrefabricado+LV53.2.3TechoADConcreto+LV53.2.4TechoADLata+LV53.2.5TechoADMadera+LV53.2.6TechoADTela+LV54.1ParedesAD+LV54.1.1ParedesADPlastico+LV54.1.2ParedesADConcreto+LV54.1.3ParedesADLata+
                   LV54.1.4ParedesADTela+LV54.1.5ParedesADBloque+LV54.1.6ParedesADPiedra+EH113.1AlgúnMomentoHijosDejaronEstudiarPortrabajarAD+EH114.1PagaEducaciónHijosAD+EH114.1.1CuantoAD+EH115.1PagoEducaciónEsUnaCargaEconómicaAD+EH116.1HijosTenianMarcasPreferidasDÚtilesAD+EH117.1HacíaTruequeObtenerÚtilesEscolaresAD+EH118.1ComprabanÚtilesEscolaresAntesHijosIniciaranClaseAD+EH119.1FirmóDocumentoFormalizarInscripciónColegioAD+EH120.1NecesitabaUtilizarUniformeAD+
                   R121.1LugaresDondeGustabaDivertirseAD+R121.1.1LugaresDondeGustabaDivertirseADParque+R121.1.2LugaresDondeGustabaDivertirseADCasa+R121.1.3LugaresDondeGustabaDivertirseADPueblo+R121.1.4LugaresDondeGustabaDivertirseADCasaAmigosFamiliares+R121.1.5LugaresDondeGustabaDivertirseADMar+R122.1Activ.RealizabaMásFrecuenciaParaDivertirseAD+R122.1ActivRealizabaParaDivertirseADHablarAmigos+R122.1.1ActivRealizabaParaDivertirseADCaminarFamilia+
                   AR122.1.2ActivRealizabaParaDivertirseADrreglarLimpiar+R122.1.3ActivRealizabaParaDivertirseADAsistirMesquitaMisaCultosR+R122.1.4ActivRealizabaParaDivertirseADOtro+R122.2Activ.RealizáParaDivertirseAD+R124.1AlgúnfamiliarconsumíaDrogaAD+R127.1CuantasPropiedadesTeniaAD+R128.1TipoDPropiedadesAD,family= poisson(link = "log"))


modpoisson1<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~,family= poisson(link = "log"))

S1<-summary(a)
exp(S1$coefficients)-1
lrtest(modpoisson1)

