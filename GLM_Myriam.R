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

#Upload the data
library(haven)
Data <- read_sav("C:/Users/ARivera/Documents/Mariam/Data.sav")
View(Data)
Data[is.na(Data)] <- 0

is.na(Data$AL31.1CuántoGanabaMensualmenteAD)
ej<-Data$AL31.1CuántoGanabaMensualmenteAD
view(ej)
attach(Data)
#Data Cleaning process-------------------------
#replacing some missing values as 0
Data[Data == "No Responde"] <- "0"
str(ej)
Data[Data == "0"] <- 0



#i will review the variable income to see if its clean and OK to work with it
view(Data$ASE37.1IngresosMensualesCasaDolaresAD)
view(Data$ASE37.2IngresosMensualesCasaDolaresDD)

#here i will transform the variable ASE37.1 into a new one wich is muneric
incomes.ASE37.1AD<-as.numeric(Data$ASE37.1IngresosMensualesCasaDolaresAD)
incomes.ASE37.2DD<-as.numeric(Data$ASE37.2IngresosMensualesCasaDolaresDD)

#now i will statr with the modeling process with each group of variables.
a<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~SD_CuantasPersonasVivenConUstedAD+SD_NivelEduAD+SD_Sexo+SD_EdadEnElDesplazamiento+SD_EdadActual+SD_LugarNacimiento+SD_TieneMismaParejaAD+SD_EstadoCivilAD+SD_EstadoCivilDD+SD_NumHij@sAD+SD_NumHij@sDD+SD_EdadHijosDe0a3años+SD_EdadHijosDe4a7años+SD_EdadHijosDe8a12años+SD_EdadHijosDe13a18años+SD_EdadHijosDe19a29años+SD_EdadHijosDe30añosymás+SD_NivelEduActual+SD_CuantasPersonasVivenConUstedDD,family= poisson(link = "log"))
Data$ASE32.1CuantasPersonasTieneIngresoHogarAD
attach(Data)
#Model ASPAD-------------------------------------
ASPAD<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~ASP41.1antesSuViviendaContabaConServicioEnergíaAD+ASP44.1SuViviendaContabaConServicioRecogidaBasuraAD+ASP45.1SuViviendaContabaConServicioLíneaTelefonicaFijaAD+ASP47.1SuViviendaContabaConServicioInternetAD+ASP48.1SuViviendaContabaConServicioGasPropanoAD+ASP49.1CuálEsElServicioMásCaroAD+ASP49.1.1CuálEsElServicioMásCaroADAlcantarillado+ASP49.1.2CuálEsElServicioMásCaroADGas+ASP49.1.3CuálEsElServicioMásCaroADLuz+ASP49.1.5CuálEsElServicioMásCaroADTelefonia+ASP50.1CostosMensualesServiciosADUSD+ASP51.1ComoEsLaCalidadDeLosServiciosAD+ASP52.1QueHacíaConLosResiduosAD+ASP40.1SuViviendaContabaConServicioGasAD+ASP43.1SuViviendaContabaConServicioAlcantarilladoAD+ASP46.2SuViviendaContabaConServicioTVCableAD+ASP42.1SuViviendaContabaConServicioAguaAD, family= poisson(link = "log"))

ASPADsummary<-summary(ASPAD)
expASPAD<-exp(ASPADsummary$coefficients)-1
lrtestASPAD<-lrtest(expASPAD)
#Model ALAD-------------------------------
ALAD<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~AL147.1TeníaMarcasPreferidasAseoPersonalAD+AL148.1GastoMensualProductosAseoPersonalADUSD+AL149.1UstedoFamiliaSeBañanAireLibreAD+AL150.1DondeBañabanRegularmenteAD+AL145.1GastoMensualProductosLimpiezaUSDAD+AL146.1.1ProductosAseoPersonalUtilizabaADJabónBarra+AL146.1.2ProductosAseoPersonalUtilizabaADShampoo+AL146.1.3ProductosAseoPersonalUtilizabaADJabónintimo+AL146.1.4ProductosAseoPersonalUtilizabaADCremaAfeitar+AL146.1.5ProductosAseoPersonalUtilizabaADPapelHigiénico+AL146.1.6ProductosAseoPersonalUtilizabaADCremacuerpo+AL146.1.7ProductosAseoPersonalUtilizabaADAcondicionador+AL146.1.8ProductosAseoPersonalUtilizabaADDesodorante+AL144.1.1ProductosDLimpiezAMásUsadosADJabónbarra+AL144.1.2ProductosDLimpiezAMásUsadosADBlanqueador+AL144.1.3ProductosDLimpiezAMásUsadosADJabónCocina+AL144.1.4ProductosDLimpiezAMásUsadosADLimpiavidrios+AL144.1.5ProductosDLimpiezAMásUsadosADTrapero+AL144.1.6ProductosDLimpiezAMásUsadosADLimpiaPisos+AL144.1.7ProductosDLimpiezAMásUsadosADJabónpolvo+AL144.1.8ProductosDLimpiezAMásUsadosADEscoba+AL143.1ComprabaProductosAseoParaCasaAD+AL27.1OcupaciónAD+AL28.1TipoContratoAD+AL30.1SuTrabajoEraAD+AL31.1CuántoGanabaMensualmenteAD+AL29.1SectorTrabajoAD,family= poisson(link = "log"))
ALADsummary<-summary(ALAD)
expALAD<-exp(ALADsummary$coefficients)-1
lrtestALAD<-lrtest(expALAD)
#Model TLAD-------------------------------
TLAD<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~TL153.1ActividadesRealizaEnTiempoLibreAD+TL153.1.1ActividadesRealizaEnTiempoLibreADTrabajar+TL153.1.2ActividadesRealizaEnTiempoLibreADRezar+TL153.1.3ActividadesRealizaEnTiempoLibreADComer+TL153.1.4ActividadesRealizaEnTiempoLibreADAyudarLaboresDomestica+TL153.1.5ActividadesRealizaEnTiempoLibreADDormir+TL153.1.6ActividadesRealizaEnTiempoLibreADBuscartrabajo+TL152.1LugarDondePasaTiempoLibreAD+TL152.1.1LugarDondePasaTiempoLibreADCentrosComerciales+TL152.1.2LugarDondePasaTiempoLibreADParque+TL152.1.3LugarDondePasaTiempoLibreADCasa+TL152.1.4LugarDondePasaTiempoLibreADCasaAmigosFamiliares+TL153.1.7ActividadesRealizaEnTiempoLibreADHacerMercado+TL153.1.8ActividadesRealizaEnTiempoLibreADArreglosmecánicos+TL153.1.9ActividadesRealizaEnTiempoLibreADVertelevisión+TL154.1ElmarotrosLugaresNaturalesPreferidosParaTiempoLibreAD+TL155.1EnTiempoLibreComparteFamiliaAD+TL156.1EnTiempoLibrePrefiereActividadesDomésticasAD,family= poisson(link = "log"))
TLADsummary<-summary(TLAD)
expTLAD<-exp(TLADsummary$coefficients)-1
lrtestTLAD<-lrtest(expTLAD)

C151.1HaSolicitadoCréditosoPrestamosDineroAD
#Model TAD-------------------------------
TAD<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~T137.1QuéElementosTeCnologicosEstabanSuoHogarAD+T137.1.1QuéElementosTeCnologicosEstabanSuoHogarADTelevisor+T137.1.2QuéElementosTeCnologicosEstabanSuoHogarADEquipoDSonido+T137.1.3QuéElementosTeCnologicosEstabanSuoHogarADNevera+T137.1.4QuéElementosTeCnologicosEstabanSuoHogarADLavadora+T137.1.5QuéElementosTeCnologicosEstabanSuoHogarADEstufa+T137.1.6QuéElementosTeCnologicosEstabanSuoHogarADHornoMicroonda+T137.1.7QuéElementosTeCnologicosEstabanSuoHogarADComputador+T137.8QuéElementosTeCnologicosEstabanSuoHogarADTeléfonoFijo+T137.1.9QuéElementosTeCnologicosEstabanSuoHogarADCelular+T137.1.10QuéElementosTeCnologicosEstabanSuoHogarADRadio+T137.1.11QuéElementosTeCnologicosEstabanSuoHogarADLicuadora+T138.1ElectrodomesticoMásImportanteSuCasaAD+T138.1.1ElectrodomesticoMásImportanteSuCasaADTelevisor+T138.1.2ElectrodomesticoMásImportanteSuCasaADNevera+T138.1.3ElectrodomesticoMásImportanteSuCasaADLavadora+T138.1.4ElectrodomesticoMásImportanteSuCasaADComputador+T138.1.5ElectrodomesticoMásImportanteSuCasaADCelular+T138.2.1ElectrodomesticoMásImportanteSuCasaADTelevisor+T138.2.2ElectrodomesticoMásImportanteSuCasaADNevera+T138.2.3ElectrodomesticoMásImportanteSuCasaADLavadora+T138.2.4ElectrodomesticoMásImportanteSuCasaADCelular+T139.1ElectrodomesticoPreferidoPorHijosAD+T139.1.1ElectrodomesticoPreferidoPorHijosADTelevisor+T139.1.2ElectrodomesticoPreferidoPorHijosADLavadora+T139.1.3ElectrodomesticoPreferidoPorHijosADComputador+T139.1.4ElectrodomesticoPreferidoPorHijosADCelular+T140.1ElectrodomesticosUtilizabanParaEntretenimientoAD+T140.1.1ElectrodomesticosUtilizabanParaEntretenimientoADTeleviso+T140.1.2ElectrodomesticosUtilizabanParaEntretenimientoADEquipoDs+T140.1.3ElectrodomesticosUtilizabanParaEntretenimientoADCelulari+T140.1.4ElectrodomesticosUtilizabanParaEntretenimientoADCelularb+T141.1CómoObteníaElectrodomesticosAD+T142.1DóndeAdquirióLosELectrodomesticosAD,family= poisson(link = "log"))

TADsummary<-summary(TLAD)
expTAD<-exp(TADsummary$coefficients)-1
lrtestTAD<-lrtest(expTAD)
#Model ASEDD-------------------------------
ASEAD<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~ASE33.1VinculoFamiliarTieneMayorIngresoHogarAD+ASE33.1.1FamiliarTieneMayorIngresoHogarAD+
             ASE33.1.2FamiliarTieneMayorIngresoHogarAD+ASE33.1.3FamiliarTieneMayorIngresoHogarAD+ASE33.1.4FamiliarTieneMayorIngresoHogarAD+
             ASE34.1QuienAportaMásDineroCasaAD+ASE34.1.1QuienAportaMásDineroCasaAntesPadreAD+ASE34.1.6QuienAportaMásDineroCasaAntesYoAD+
             ASE36.1FuenteIngresosAD+ASE37.1IngresosMensualesCasaDolaresAD+ASE38.1.2PersonaEncargadaDeFamiliaADMadre+
             ASE38.1.3PersonaEncargadaDeFamiliaADHijos+ASE38.1.4PersonaEncargadaDeFamiliaADHermanos+ASE38.1.5PersonaEncargadaDeFamiliaADYo+
             ASE38.1.6PersonaEncargadaDeFamiliaADOtro+ASE39.1PersonasDependenUstedAD+ASE34.1.3QuienAportaMásDineroCasaAntesHijosAD+
             ASE34.1.4QuienAportaMásDineroCasaAntesHermanosAD+ASE32.1CuantasPersonasTieneIngresoHogarAD+
             ASE34.1.1QuienAportaMásDineroCasaAntesPadreAD+ASE34.1.2QuienAportaMásDineroCasaAntesMadreAD+ASE38.1PersonaEncargadaDeFamiliaAD+
             ASE38.1.1PersonaEncargadaDeFamiliaADPadre,
           family= poisson(link = "log"))

#Model RAD-----------------------
RAD<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~R121.1LugaresDondeGustabaDivertirseAD+R121.1.1LugaresDondeGustabaDivertirseADParque+
           R121.1.2LugaresDondeGustabaDivertirseADCasa+R121.1.3LugaresDondeGustabaDivertirseADPueblo+
           R121.1.4LugaresDondeGustabaDivertirseADCasaAmigosFamiliares+R121.1.5LugaresDondeGustabaDivertirseADMar+
           R122.1Activ.RealizabaMásFrecuenciaParaDivertirseAD+R122.1ActivRealizabaParaDivertirseADHablarAmigos+
           R122.1.1ActivRealizabaParaDivertirseADCaminarFamilia+R122.1.3ActivRealizabaParaDivertirseADAsistirMesquitaMisaCultosR+
           R122.1.4ActivRealizabaParaDivertirseADOtro+R122.2Activ.RealizáParaDivertirseAD+R124.1AlgúnfamiliarconsumíaDrogaAD+
           R127.1CuantasPropiedadesTeniaAD+R128.1TipoDPropiedadesAD+R122.2.1Activ.RealizáParaDivertirseADHablarAmigos+
           R122.2.2Activ.RealizáParaDivertirseADJugarFútbol+R122.2.3Activ.RealizáParaDivertirseADCaminarFamilia+
           R122.2.4Activ.RealizáParaDivertirseADArreglarLimpiar+R122.2.5ActivRealizabaParaDivertirseADAsistirMesquitaMisaCultosR+
           R123.1HaConsumidoAgunaDrogaAD+R128.1.1TipoDPropiedadesADCasa+R128.1.2TipoDPropiedadesADApartamento+
           R125.1CuántoDineroGastabaEnRecreaciónMesAD+R126.1TeníaPropiedadesAD+R128.1.3TipoDPropiedadesADCasa+
           R130.1CómoObtuvoPropiedadADTeniaPapelesDPropiedadAD+R129.1CómoObtuvoPropiedadAD+R131.1UstedEsUnicoDueñoDePropiedadAD+
           R132.1TuvoNecesidadDAbandaonarSuPropiedadAD,
         family= poisson(link = "log"))


AR122.1.2ActivRealizabaParaDivertirseADrreglarLimpiar

EHAD<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~EH113.1AlgúnMomentoHijosDejaronEstudiarPortrabajarAD+EH114.1PagaEducaciónHijosAD+EH114.1.1CuantoAD+
            EH115.1PagoEducaciónEsUnaCargaEconómicaAD+EH116.1HijosTenianMarcasPreferidasDÚtilesAD+
            EH117.1HacíaTruequeObtenerÚtilesEscolaresAD+EH118.1ComprabanÚtilesEscolaresAntesHijosIniciaranClaseAD+
            EH119.1FirmóDocumentoFormalizarInscripciónColegioAD+EH120.1NecesitabaUtilizarUniformeAD+
            EH98.1AQuéSistemaEducativoPerteneciaAD+EH99.1LosNiñosRecibiánEducaciónAD+EH100.1ComoObteniaLosÚtilesEscolaresAD+
            EH101.1QuienLeDonabaORegalabaÚtilesAD+EH102.1DemorabaMásDeUnaHoraLlegarAlColegioAD+EH104.1LeCobrabanEducaciónHijosAD+
            EH105.1HijosHanReprobadoAlgunAñoAD+EH106.1PorqueReprobaronAD+EH107.1FrecuenciaCompraÚtilesEscolaresAD+
            EH103.1MedioTrasporteUtilizabaParAllegarEscuelaAD+EH95.1ViviaConHijosAD+EH108.1CuantoGastabaCompraÚtilesAD+
            EH109.1NivelEducativOMásAltoAlcanzadoHijosAD+EH96.1SusHijosEstudiabanAD+EH97.1HijosEranBuenosEstudiantesAD+
            EH110.1NivelAcadémicoMásBajoAlcanzadoHijosAD+EH111.1TipoDConstrucciónEscuelaAD+EH112.1AsisteReunionesDPadresMadresDFamiliaAD,
          family= poisson(link = "log"))
#Modelo LVAD---------------------
LVAD<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~LV53.1TechoAD+LV53.1.1TechoADTejaDBarro+LV53.1.2TechoADConcreto+
            LV53.1.3TechoADLata+LV53.2TechoAD+LV53.2.1TechoADZinc+LV53.2.2TechoADPrefabricado+LV53.2.3TechoADConcreto+
            LV53.2.4TechoADLata+LV53.2.5TechoADMadera+LV53.2.6TechoADTela+LV54.1ParedesAD+LV54.1.1ParedesADPlastico+
            LV54.1.2ParedesADConcreto+LV54.1.3ParedesADLata+
            LV54.1.4ParedesADTela+LV54.1.5ParedesADBloque+LV54.1.6ParedesADPiedra+
            LV54.1.6ParedesADLadrillo+LV54.1.6ParedesADBloque+LV54.1.6ParedesADPiedra+LV55.1PisosAD+
            LV55.1.1PisosADAlfombra+LV55.1.2PisosADMármol+LV55.1.3PisosADCemento+
            LV55.1.4PisosADLadrillo+LV55.1.5PisosADBloque+LV55.1.6PisosADPiedra+
            LV55.1.6PisosADOtro+LV56.1TamañoCasaAD+LV60.1PagoDArrendamientoADUSD+
            LV57.1TamañoCasaMetrosCuadradosAD+LV58.1TipoViviendaHabitabaAD+
            LV62.1PersonasVivenSuHogarMenoresD18AñosAD+
            LV61.1PersonasVivenSuHogarMayoresD65AñosAD,
          family= poisson(link = "log"))


ANAD<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~AN87.1MercadosDonadosAD+AN88.1SubsidioParaCompraDAlimentosAD+
            AN89.1ComidaConsumdaDiariamenteAD+AN89.1.1ComidaConsumdaDiariamenteADDesayuno+
            AN89.1.2ComidaConsumdaDiariamenteADMediasNueves+AN89.1.3ComidaConsumdaDiariamenteADAlmuerzo+
            AN89.1.4ComidaConsumdaDiariamenteADComida+AN89.1.5ComidaConsumdaDiariamenteADOnces+
            AN89.2.4ComidaConsumdaDiariamenteADComida+AN90.1FrecuenciaConsumoCarneDResAD+AN94.1FrecuenciaConsumoComidasRapidasAD+
            AN81.1FrecuenciaCompraAlimentosAlMesAD+AN84.1CultivandoAD
          AN82.1LugarDCompADAlimentosAD
          AN85.1IntercambioAlimentosUObjetosAD
          AN86.1TomandolosDLaCanecaAD
          AN89.2.1ComidaConsumdaDiariamenteADDesayuno	AN89.2.2ComidaConsumdaDiariamenteADMediasNueves	AN89.2.3ComidaConsumdaDiariamenteADAlmuerzo
          AN89.2.5ComidaConsumdaDiariamenteADOnces
          AN91.1FrecuenciaConsumoCarneDCorderoAD
          AN92.1FrecuenciaConsumoPescadoAD+AN83.1PescaOCazaAD
          AN93.1FrecuenciaConsumoPollOAD,
          family= poisson(link = "log"))


ADAD<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~AD15Motivosdesplazamiento+AD16TipoDelDesplazamiento+AD17AQueGobernaturaLlego+AD18ElLugarDondeLlegoAVivir+
            AD19DesplazamientoMejoróCalidadVida+AD20CuantasVecesDesplazado+AD21DondeViveActualmente+AD22SeDesplazoPorNoConseguirEmpleo+
            AD23SeDesplazoPorLugarDondeViviaEraPeligroso+AD24SeDesplazoPorVecinosNoEstabanConformes+AD25AñoUltimoDesplazamiento+
            AD26ActorDesplazamiento+AD14CiudadViviaAD,
          family= poisson(link = "log"))

V1AD<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~V135.1TienePreferenciAPorAlgunaMarcaAD+V134.1CadaCuantoAdquiriaLaRopaAD+V133.1CómoAdquiriaRopaAD+
            V136.1TienePreferenciaAlgunaMarcaDCalzadoAD
          
          SAD<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~S72.1SusHijosNacieroEnAD+S72.1.1SusHijosNacieroEnADHospital+S72.2SusHijosNacieroEnADCasa+S72.1.3SusHijosNacieroEnADOtro+S78.1NoRecibioServiciosAFaltaDEquiposOProfesionalesAD+
                     S79.1AlSolicitarServicioDSaludAtendíanInmediatamenteAD+S80.1ConsiderabaLaPrestaciónDelServicioDSAludAD+S63.1KilogramosAD+S63.1AñosAD+S64.1EstadoDSaludAD+S66.2VecesAsistíaAlMedicoEnElAñoAD+S66.1VecesAsistíaAlMedicoEnElAñoAD+S67.1NúmeroVecesEnfermaAñoAD+S68.1TipoDEnfermedadesAD+S68.1.2TipoDEnfermedadesADRespiratorias+S68.1.3TipoDEnfermedadesADTransm.Sexual+S68.1.4TipoDEnfermedadesADCardiovascular+S68.1.5TipoDEnfermedadesADInfecciones+S68.1.6TipoDEnfermedadesADOtra+S68.2.1TipoDEnfermedadesADDigestivas+S68.2.2TipoDEnfermedadesADRespiratorias+S68.2.3TipoDEnfermedadesADCardiovascular+S68.2.4TipoDEnfermedadesADInfecciones+S68.2.5TipoDEnfermedadesADOtra	S69.1TipoDEnfermedadesFamiliaAD	S69.1.1TipoDEnfermedadesFamiliaADRespiratorias+S69.1.2TipoDEnfermedadesFamiliaADCardiovascular+S69.1.3TipoDEnfermedadesFamiliaADPsiquiatra+S69.1.4TipoDEnfermedadesFamiliaADInfecciones+S69.1.5TipoDEnfermedadesFamiliaADDermatológicas+S69.1.6TipoDEnfermedadesFamiliaADOtra+S69.2TipoDEnfermedadesFamiliaAD+S69.2.1TipoDEnfermedadesFamiliaADRespiratorias+S69.2.2TipoDEnfermedadesFamiliaADCardiovascular+S69.2.3TipoDEnfermedadesFamiliaADInfecciones+S69.2.4TipoDEnfermedadesFamiliaADDermatológicas+S69.2.5TipoDEnfermedadesFamiliaADOtra+S70.1ServicioDSaludPreferenciaAD+S70.1.1ServicioDSaludPreferenciaADMedicinaTradicional+S70.1.2ServicioDSaludPreferenciaADMedicinaAlternativa+S70.1.3ServicioDSaludPreferenciaADAmbas+S70.1.4ServicioDSaludPreferenciaADCurarseEnCasa+S70.2.1ServicioDSaludPreferenciaADMedicinaTradicional+S70.2.2ServicioDSaludPreferenciaADMedicinaAlternativa+S70.2.3ServicioDSaludPreferenciaADAmbas+S70.2.4ServicioDSaludPreferenciaADCurarseEnCasa+S71.1AdóndeAcudíaCndoEstabaEnfermoAD+S71.1.1AdóndeAcudíaCndoEstabaEnfermoADSobandero+S71.1.2AdóndeAcudíaCndoEstabaEnfermoADSobanderoAux.Farmacia+				
                     
                     AL27.2OcupaciónDD+AL28.2TipoContratoDD+AL29.2SectorTrabajoDD+AL30.2SuTrabajoDD+AL31.2CuántoGanaMensualmenteDD+AL143.2CompraProductosAseoParaCasaDD+AL144.2ProductosDLimpiezAMásUsadosDD+AL144.2.1ProductosDLimpiezAMásUsadosDDJabónbarra+AL144.2.2ProductosDLimpiezAMásUsadosDDBlanqueador+AL144.2.3ProductosDLimpiezAMásUsadosDDJabónCocina+AL144.2.4ProductosDLimpiezAMásUsadosDDLimpiaVidrios+AL144.2.5ProductosDLimpiezAMásUsadosDDTrapero+AL144.2.6ProductosDLimpiezAMásUsadosDDLimpiaPisos+AL144.2.7ProductosDLimpiezAMásUsadosDDJabónpolvo+AL144.2.8ProductosDLimpiezAMásUsadosDDEscoba+AL146.2ProductosAseoPersonalUtilizaDD+AL146.2.1ProductosAseoPersonalUtilizaDDJabónBarra+AL146.2.2ProductosAseoPersonalUtilizaDDShampoo+AL146.2.3ProductosAseoPersonalUtilizaDDJabónintimo+AL146.2.4ProductosAseoPersonalUtilizaDDCremaafeitar+AL146.2.5ProductosAseoPersonalUtilizaDDCremaCuerpo+AL146.2.6ProductosAseoPersonalUtilizaDDAcondicionador+AL146.2.7ProductosAseoPersonalUtilizaDDDesodorante+AL147.2TeníaMarcasPreferidasAseoPersonalDD+AL148.2GastoMensualProductosAseoPersonalDDUSD+AL149.2UstedoFamiliaSeBañanAireLibreDD+AL150.2DondeBañabanRegularmenteDD,
                   family= poisson(link = "log"))				
          
          
          ASE32.1CuantasPersonasTieneIngresoHogarDD+ASE33.2VinculoFamiliarConQuienTieneMayorIngresoHogarDD+ASE33.2.1FamiliarTieneMayorIngresoHogarDD+ASE33.2.2FamiliarTieneMayorIngresoHogarDD+ASE33.2.3FamiliarTieneMayorIngresoHogarDD+ASE33.2.4FamiliarTieneMayorIngresoHogarDD+ASE33.2.5FamiliarTieneMayorIngresoHogarDD+ASE34.1.5QuienAportaMás+ASE34.2QuienAportaMásDineroCasaDD+ASE34.2.1QuienAportaMás DineroCasaAntesPadreDD+ASE34.2.2QuienAportaMásDineroCasaAntesMadreDD+ASE34.2.3QuienAportaMásDineroCasaAntesHijosDD+ASE34.2.4QuienAportaMásDineroCasaAntesHermanosDD+ASE34.2.5QuienAportaMásDineroCasaAntesParejaDD+ASE34.2.6QuienAportaMásDineroCasaAntesYoDD+ASE35.1LosIngresosFamiliaresEranAD+ASE35.2LosIngresosFamiliaresDD+ASE36.1.1FuenteIngresosAntesSubsidio+ASE36.1.2FuenteIngresosAntesPropiedades+ASE36.1.3FuenteIngresosAntesTrabajo+ASE36.2FuenteIngresosDespués+ASE36.2.1FuenteIngresosAntesSubsidio+ASE36.2.2FuenteIngresosAntesPropiedades+ASE36.2.3FuenteIngresosAntesTrabajo+ASE37.2IngresosMensualesCasa(Dolares)DD+ASE38.2PersonaEncargadaDeFamiliaDD+ASE38.2.1PersonaEncargadaDeFamiliaDDPadre+ASE38.2.2PersonaEncargadaDeFamiliaDDMadre+ASE38.2.3PersonaEncargadaDeFamiliaDDHijos+ASE38.2.4PersonaEncargadaDeFamiliaDDHermanos+ASE38.2.5PersonaEncargadaDeFamiliaDDAbuelos+ASE38.2.6PersonaEncargadaDeFamiliaDDYo+ASE38.2.7PersonaEncargadaDeFamiliaDDOtro+ASE39.2PersonasDependenUstedDD,
          family= poisson(link = "log"))		


ASP40.2SuViviendaContabaConServicioGasDD+ASP41.2SuViviendaContabaConServicioENergíaDD+ASP42.2SuViviendaContabaConServicioAguaDD+ASP43.2SuViviendaContabaConServicioAlcantarilladoDD+ASP44.2SuViviendaContabaConServicioRecogidaBasuraDD+ASP45.2SuViviendaContabaConServicioLíneaTelefonicaFijaDD+ASP46.2SuViviendaContabaConServicioTvCablEDD+ASP47.2SuViviendaContabaConServicioInternetDD+ASP48.2SuViviendaContabaConServicioGasPropanoDD+ASP49.2CuálEsElServicioMásCaroDD+ASP49.2.1CuálEsElServicioMásCaroDDAlcantarillado+ASP49.2.2CuálEsElServicioMásCaroDDGas+ASP49.2.3CuálEsElServicioMásCaroDDLuz+ASP49.2.4CuálEsElServicioMásCaroDDAgua+ASP49.2.5CuálEsElServicioMásCaroDDTelefonia+ASP49.2.6CuálEsElServicioMásCaroDDInternet+ASP50.2CostosMensualesServiciosDDUSD+ASP51.2ComoEsLaCalidadDeLosServiciosDD+ASP52.2QueHacíaConLosResiduosDD,
family= poisson(link = "log"))			



LV54.2ParedesDD+LV54.1.1ParedesDDPlástico+LV54.1.2ParedesDDConcreto+LV54.1.3ParedesDDLata+LV54.1.4ParedesDDMadera+LV54.1.5ParedesDDTela+LV55.2PisosDD+LV55.2.1PisosDDTierra+LV55.2.2PisosDDCemento+LV55.2.3PisosDDLadrillo+LV55.2.4PisosDDBloque+LV55.2.5PisosDDPiedra+LV55.2.6PisosDDOtro+LV56.2TamañoCasaDD+LV57.2TamañoCasaMetrosCuadradosDD+LV58.2TipoViviendaHabitabaDD+LV59.1DondeViviaEraAD+LV59.2DondeViveEsDD+LV60.2PagoDArrendamientoDDUSD+LV61.2PersonasVivenSuHogarMayoresD65AñosDD+LV62.1PersonasVivenSuHogarMenoresD18AñosDD,
family= poisson(link = "log"))	

#Model SDD------------------------------------------------  
S63.2KilogramosDD+S63.2AñosDD+S64.2EstadoDSaludDD+S65.2EstadoDSaludFamiliaDD+S67.2NúmeroVecesEnfermaAñoDD+
  S68.1.1TipoDEnfermedadesADDigestivas+S68.2TipoDEnfermedadesDD+S70.1.2ServicioDSaludPreferenciaDD+
  S71.2AdóndeAcudeCndoEstabaEnfermoDD+S71.2.2AdóndeAcudeCndoEstabaEnfermoDDHospital+S71.2.3AdóndeAcudeCndoEstabaEnfermoDDFamilia+
  S71.2.4AdóndeAcudeCndoEstabaEnfermoDDOtro+S72.1.2SusHijosNacieroEnDD+S72.1.2.1SusHijosNacieroEnDDHospital+
  S72.1.2SusHijosNacieroEnDDOtro+S73.2SeEncuentraAfiliadoSistemaSaludDD+S74.1ASolicitadocitaEspecialistaDD+
  S75CómoEraSservicioSalud+S76.1LasInstalacionesDelServicioSaludEranDD+S77.1.1DeCuantoAD+S77.2RealizabaPagoMensualServicioDSaludDD+
  S77.2.1DeCuantoDD+S78.2NoRecibioServiciosAFaltaDEquiposOProfesionalesDD+S79.2AlSolicitarServicioDSaludAtendíanInmediatamenteDD+
  S80.2ConsiderabaLaPrestaciónDelServicioDSAludDD+S71.2.1AdóndeAcudeCndoEstabaEnfermoDDAux.Farmacia,
family= poisson(link = "log"))

#Model ANDD--------------------------------------------------------------------
AN81.2FrecuenciaCompraAlimentosAlMesDD+AN82.2LugarDCompADAlimentosDD+AN83.2PescaOCazaDD+AN84.2CultivandoDD+AN85.2IntercambioAlimentosUObjetosDD+AN86.2TomandolosDLaCanecaDD+AN87.1MercadosDonadosDD+AN88.2SubsidioParaCompraDAlimentosDD+AN89.2ComidaConsumdaDiariamenteDD+AN90.1FrecuenciaConsumoCarneDResDD+AN91.2FrecuenciaConsumoCarneDCorderoDD+AN92.2FrecuenciaConsumoPescadoDD+AN93.2FrecuenciaConsumoPolloDD+AN94.2FrecuenciaConsumoComidasRapidasDD,
family= poisson(link = "log"))		

#Model EHDD----------------------------------------------------------

EH96.2SusHijosEstudianDD+EH97.2HijosSonBuenosEstudiantesDD+EH98.2AQuéSistemaEducativoPerteneDD+EH95.2ViviaConHijosDD+
  EH105.2HijosHanReprobadoAlgunAñoDD+EH99.2LosNiñosRecibenEducaciónDD+EH100.1ComoObtieneLosÚtilesEscolaresDD+
  EH101.2QuienLeDonaORegalaÚtilesDD+EH102.1DemorMásDeUnaHoraLlegarAlColegioDD+EH103.2MedioTrasporteUtilizaParAllegarEscuelaDD+
  EH104.2LeCobranEducaciónHijosDD+EH106.1PorquEReprobaronDD+EH107.2FrecuenciaCompraÚtilesEscolaresDD+
  EH108.2CuantoGastaCompraÚtilesDD+EH109.2NivelEducativOMásAltoAlcanzadoHijosDD+EH110.2NivelAcadémicoMásBajoAlcanzadoHijosDD+
  EH111.2TipoDConstrucciónEscuelaDD+EH112.2AsisteReunionesDPadresMadresDFamiliaDD+
  EH113.2AlgúnMomentoHijosDejaronEstudiarPortrabajarDD+EH114.2PagaEducaciónHijosDD+EH114.2.1CuantoDD+
  EH115.2PagoEducaciónEsUnaCargaEconómicaDD+EH116.2HijosTenianMarcasPreferidasDÚtileSDD+EH117.2HaceTruequeObtenerÚtilesEscolaresDD+
  EH118.2CompraÚtilesEscolaresAntesHijosIniciaranClaseDD+EH119.2FirmóDocumentoFormalizarInscripciónColegioDD+
  EH120.2NecesitabaUtilizarUniformeDD,
family= poisson(link = "log"))		

#Model RDD--------------------------------------------
R121.2LugaresDondeGustabaDivertirseDD+R121.2.1LugaresDondeGustabaDivertirseDDCasa+R121.2.2LugaresDondeGustabaDivertirseDDCasino+R121.2.3LugaresDondeGustabaDivertirseDDCasaAmigosofamiliares+R121.2.4LugaresDondeGustabaDivertirseDDNoSeDivierte+R123.2HaConsumidoAgunaDrogaDD+R124.2AlgúnfamiliarconsumeDrogaDD+R125.2CuántoDineroGastaEnRecreaciónMesDD+R126.1TeníaPropiedadesDD+R127.2CuantasPropiedadesTeniaDD+R128.2TipoDPropiedadesDD+R128.2.1TipoDPropiedadeDDCasa+R128.2.2TipoDPropiedadesDDOtro+R129.2CómoObtuvoPropiedadDD+R130.2CómoObtuvoPropiedadADTeniaPapelesDPropiedadDD+R131.2UstedEsUnicoDueñoDePropiedadDD+R132.2TuvoNecesidadDAbandaonarSuPropiedadDD,
family= poisson(link = "log"))		

#Model VDD---------------------------------------------------
V133.2CómoAdquiriaRopaDD+V134.2CadaCuantoAdquiriaLaRopaDD+V135.2TienePreferenciAPorAlgunaMarcaDD+V136.2TienePreferenciaAlgunaMarcaDCalzadoDD,
family= poisson(link = "log"))	

T137.2QuéElementosTeCnologicosEstanSuHogarDD+T137.2.1QuéElementosTeCnologicosEstanSuHogarDDTelevisor+T137.2.2QuéElementosTeCnologicosEstanSuHogarDDEquipoDSonido+T137.2.3QuéElementosTeCnologicosEstanSuHogarDDNevera+T137.2.4QuéElementosTeCnologicosEstanSuHogarDDLavadora+T137.2.5QuéElementosTeCnologicosEstanSuHogarDDEstufa+T137.2.6QuéElementosTeCnologicosEstanSuHogarDDHornoMicroondas+T137.2.7QuéElementosTeCnologicosEstanSuHogarDDTeléfonoFijo+T137.2.8QuéElementosTeCnologicosEstanSuHogarDDCelular+T137.2.9QuéElementosTeCnologicosEstanSuHogarDDPlancha+T137.2.10QuéElementosTeCnologicosEstanSuHogarDDRadio+T137.2.11QuéElementosTeCnologicosEstanSuHogarDDLicuadora+T138.2ElectrodomesticoMásImportanteSuCasaDD+T139.2ElectrodomesticoPreferidoPorHijosDD+T139.2.1ElectrodomesticoPreferidoPorHijosDDTelevisor+T139.2.2ElectrodomesticoPreferidoPorHijosDDTeléfonofijo+T139.2.3ElectrodomesticoPreferidoPorHijosDDCelular+T140.2ElectrodomesticosUtilizaParaEntretenimientoDD+T140.2.1electrodomesticosUtilizaParaEntretenimientoDDelevisor+T140.2.2ElectrodomesticosUtilizaParaEntretenimientoDDEquipodesonido+T140.2.3ElectrodomesticosUtilizaParaEntretenimientoDDCelularinteligente+T140.2.4ElectrodomesticosUtilizaParaEntretenimientoDDCelularbajagama+T141.2CómoObteníaElectrodomesticosDD+T142.2DóndeAdquirióLosELectrodomesticosDD+TL152.2.1LugarDondePasaTiempoLibreDD+TL152.2.1LugarDondePasaTiempoLibreDDParque+TL152.2.2LugarDondePasaTiempoLibreDDCasa+TL152.2.3LugarDondePasaTiempoLibreDDCasaAmigosFamiliares,
family= poisson(link = "log"))		

#Model TLDD--------------------------------------  
TL153.2Actividades+RealizaEnTiempoLibreDD+TL153.2.1ActividadesRealizaEnTiempoLibreDDTrabajar+TL153.2.2ActividadesRealizaEnTiempoLibreDDRezar+TL153.2.3ActividadesRealizaEnTiempoLibreDDComer+TL153.2.4ActividadesRealizaEnTiempoLibreDDAyudarLaboresDomesticas+TL153.2.5ActividadesRealizaEnTiempoLibreDDDormir+TL153.2.6ActividadesRealizaEnTiempoLibreDDBuscarTrabajo+TL153.2.7ActividadesRealizaEnTiempoLibreDDArreglosmecánicos+TL154.2ElmarotrosLugaresNaturalesPreferidosParaTiempoLibreDD+TL155.2EnTiempoLibreComparteFamiliaDD+TL156.2EnTiempoLibrePrefiereActividadesDomésticasDD+AL145.2GastoMensualProductosLimpiezaUSDDD,
family= poisson(link = "log"))	



C151.2HasolicitadoCréditosoPrestamosDineroDD		


#Variables por revisar------------------------
157DondeViveActualmente	
158SituaciónActualEs






modpoisson1<-glm(ASE32.1CuantasPersonasTieneIngresoHogarAD~,family= poisson(link = "log"))

S1<-summary(a)
exp(S1$coefficients)-1
lrtest(modpoisson1)

Data$S80.1ConsiderabaLaPrestaciónDelServicioDSAludAD
