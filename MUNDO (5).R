salva = ls()

# Bibliotecas -------------------------------------------------------------

pacotes = c("dplyr","readxl","lubridate","tidyr")



for(pacote in pacotes){
  
  if(!is.element(pacote,installed.packages())){install.packages(pacote)}
  
  
  
  
  
  
  
}


library("dplyr")

library("readxl")

library("lubridate")

library("tidyr")



# Fun??es -----------------------------------------------------------------



desacumula = function(x){

  return(c(x[1],diff(x)))

}

traduz = function(paises){

  return(paste(traducao[paises,"pt"]))

}

getPOP = function(paises){return(pop[paises])}

getDENS = function(paises){return(dens[paises])}



# Importa??o --------------------------------------------------------------



LINK = "https://sites.google.com/site/portaldadosuffcontracovid/home"

FILE = "GET.UFF.READ.RData"

download.file(url = paste0(LINK,"/",FILE), destfile=FILE)

load(FILE)  

GET.UFF.READ(c("CASOS.CONFIRMADOS.MUNDO.R","OBITOS.MUNDO.R","RECUPERACOES.MUNDO.R",

               "POPULACAO.MUNDO.R","DENSIDADE.MUNDO.R"))



traducao = read.csv("https://sites.google.com/site/portaldadosuffcontracovid/home/data_bakcup/traducaoJHU.csv?attredirects=0&d=1",sep=";")

rownames(traducao) = traducao[,"en"]



# Adicionando casos, ?bitos e recupera??es na base grande --------------------------------------------------------------



paises = rownames(CASOS.CONFIRMADOS.MUNDO)



CASOS = CASOS.CONFIRMADOS.MUNDO %>%

  as.data.frame() %>%

  mutate(pais=paises) %>%

  select(pais,everything()) %>%

  pivot_longer(-pais,names_to="datas",values_to="casos")



OBITOS = OBITOS.MUNDO %>%

  as.data.frame() %>%

  mutate(pais=paises) %>%

  select(pais,everything()) %>%

  pivot_longer(-pais,names_to="datas",values_to="casos")



RECUPERACOES = RECUPERACOES.MUNDO %>%

  as.data.frame() %>%

  mutate(pais=paises) %>%

  select(pais,everything()) %>%

  pivot_longer(-pais,names_to="datas",values_to="casos") 



baseAcumulados = inner_join(CASOS,OBITOS,by=c("pais"="pais","datas"="datas")) %>%

  inner_join(RECUPERACOES,by=c("pais"="pais","datas"="datas"))



colnames(baseAcumulados)[3:5] = c("casosAcumulado","obitosAcumulado","recuperacoesAcumulado")



baseNovos = baseAcumulados %>% # BIZU  

  group_by(pais) %>%

  summarise(casosNovos = list(desacumula(casosAcumulado)),obitosNovos = list(desacumula(obitosAcumulado)),

            recuperacoesNovos = list(desacumula(recuperacoesAcumulado))) %>%

  unnest() %>%

  select(-pais)



baseFinal = cbind(baseAcumulados,baseNovos)



# Adicionando popula??o e densidade ---------------------------------------



dic = as.data.frame(DICIONARIO.MUNDO) %>%

  select(OBITOS.MUNDO,POPULACAO.MUNDO) %>%

  arrange(OBITOS.MUNDO) %>%

  filter(!is.na(OBITOS.MUNDO))



tradPOP = paste(na.omit(dic$POPULACAO.MUNDO))

dic2 = filter(dic,!is.na(POPULACAO.MUNDO))

pop = POPULACAO.MUNDO[tradPOP,]

names(pop) = dic2$OBITOS.MUNDO

rownames(DENSIDADE.MUNDO)=DENSIDADE.MUNDO[,"X"]

dens = DENSIDADE.MUNDO[tradPOP,"Population.density"]

names(dens)= dic2$OBITOS.MUNDO



baseFinal = baseFinal %>%

  mutate(populacao=getPOP(pais),densidade=getDENS(pais))



BaseFinalMUNDO = baseFinal

colnames(BaseFinalMUNDO) = c("Paises","Data","CasosAcumulados","ObitosAcumulados"

  ,"RecuperadosAcumulados", "CasosNovos", "ObitosNovos"

  ,"RecuperadosNovos", "Populacao", "Densidade")



BaseFinalMUNDO=full_join(BaseFinalMUNDO,traducao,by=c("Paises"="en"))

BaseFinalMUNDO=rename(BaseFinalMUNDO,PaisesPT = pt, PaisesEN= Paises) %>% select(PaisesEN,PaisesPT,everything())



# Tirando outras vari?veis ------------------------------------------------

rm(list=ls()[!is.element(ls(),c(salva,"BaseFinalMUNDO"))])

