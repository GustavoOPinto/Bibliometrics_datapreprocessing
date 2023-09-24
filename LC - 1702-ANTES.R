## Carregar Pacotes #####
c(library(bibliometrix),library(tidyverse),library(openxlsx))

## Setar pasta de trabalho #####
setwd("C:/Users/") #Complementar com caminho da pasta

## Imputar e converter Base de Dados ####
M <- convert2df("scopus 1702 - 07.22.bib", dbsource = "scopus", format = "bibtex")

write.xlsx(M, file = "M1702v07_22.xlsx")

## Criar uma base Auxiliar #####
baseAux<-M

## Bibliometrix - biblionAnalysis ####
results <- biblioAnalysis(M, sep = ";")
options(width=100) #Ano / Autor / Source
S <- summary(object = results, k = 3000, pause = FALSE)
plot(x = results, k = 10, pause = FALSE)

## Estatísticas gerais da amostra #####

#Análise dos Idiomas
Idiomas<-
  baseAux %>%
  count(LA, sort = TRUE) %>%
  mutate(prop = n / sum(n), prop = scales::percent(prop))
Idiomas

Quantidade_Linguas<- rep(0,nrow(Idiomas))

i=1
for (i in 1:nrow(Idiomas)) {
  Quantidade_Linguas[i] <- length(unlist(strsplit(Idiomas$LA,",")[i]))
}

Quantidade_Linguas

Sintese_Linguas<-cbind(Idiomas,Quantidade_Linguas)
Sintese_Linguas

which(baseAux$LA==Sintese_Linguas[c(which(Quantidade_Linguas>1)),1])
baseAux$DI[which(baseAux$LA==Sintese_Linguas[c(which(Quantidade_Linguas>1)),1])]

baseAux$DI[which(baseAux$LA!="ENGLISH")]
length(which(baseAux$LA!="ENGLISH"))


#Publicações por ano
ProducaoAnual<-(S[["AnnualProduction"]])
write.xlsx(ProducaoAnual, file = "ProducaoAnual.xlsx")


#Fontes mais citadas e com maior quantidade de publicações
SOTC<-as.data.frame(subset(M, select = c("SO", "TC")))

SOTC<-
  group_by(SOTC,SO) %>%
  summarise(Count = n(),
            TotalCitacoes=sum(TC))%>%
  arrange (desc(Count))
SOTC

write.xlsx(SOTC, file = "SOTC.xlsx")

#Documentos mais citados
DocsMaisCitados<-as.data.frame(subset(M, select = c("AU", "TI", "SO", "TC", "PY")))

DocsMaisCitados<-
  arrange (DocsMaisCitados,desc(TC))%>%
  mutate(TCperYear = TC / (2023-PY)) #Editar "2023" para "ano da busca +1"
DocsMaisCitados

write.xlsx(DocsMaisCitados, file = "DocsMaisCitados.xlsx")

## Análises sobre AUTORES #####

## Análise - Quantidade de autores de cada documento ####
baseAU<-baseAux
str_split(baseAU$AU,";")

nAutores=rep(0,nrow(baseAU))

i=1
for (i in 1:nrow(baseAU)) {
  nAutores[i]<-length(unlist(str_split(baseAU$AU,";")[i]))
}
nAutores
sum(nAutores)

#Análise - Documentos com único autor

which(nAutores==1)
length(which(nAutores==1))

str_split(baseAU$AU,";")[which(nAutores==1)]

sort(unlist(str_split(baseAU$AU,";")[which(nAutores==1)]))
Autor <- sort(unlist(str_split(baseAU$AU,";")[which(nAutores==1)]))
ListUnicoAutor<-as.data.frame(Autor)

#Análise sem "NA NA"

which(is.na(baseAU$AU))
which(baseAU$AU=="NA NA")
length(which(baseAU$AU=="NA NA"))
baseAU$DI[which(baseAU$AU=="NA NA")]
baseAU$TI[which(baseAU$AU=="NA NA")]
baseAU$DT[which(baseAU$AU=="NA NA")]

ListUnicoAutorSemNA<-subset(ListUnicoAutor, Autor!="NA NA")


## Autores - Quantidade de publicações e Citações ####
str_split(baseAU$AU,";")
unlist(str_split(baseAU$AU,";"))

nAU=rep(0,nrow(baseAU))

i=1
for (i in 1:nrow(baseAU)) {
  nAU[i]<-length(unlist(str_split(baseAU$AU,";")[i]))
}

ncolunaAU<-max(nAU)

ListaAutores_por_artigo<-matrix(NA, ncol = ncolunaAU, nrow = length(baseAU$AU))

i=1
for (i in 1:length(baseAU$AU)) {
  ListaAutores_por_artigo[i,]<-c(unlist(str_split(baseAU$AU,";")[i]),
                                 rep(NA, ncolunaAU-length(unlist(str_split(baseAU$AU,";")[i])))
  )
}
ListaAutores_por_artigo


TodosAutores<-unique(as.vector(ListaAutores_por_artigo))

q_artigos_Autor<-rep(0,length(TodosAutores))

i=1
for (i in 1:length(TodosAutores)) {
  q_artigos_Autor[i]<-length(which(ListaAutores_por_artigo==TodosAutores[i]))
}
q_artigos_Autor

dfListaAutores_por_artigo<-as.data.frame(ListaAutores_por_artigo)

TC<-baseAux$TC
AutoresTC<-data.frame(dfListaAutores_por_artigo,TC)

TotalAutoresCitacoes=rep(0,length(TodosAutores))

i=1
for (i in 1:length(TodosAutores)) {
  TotalAutoresCitacoes[i]<-
    sum(AutoresTC$TC[which(TodosAutores[i]==ListaAutores_por_artigo,arr.ind = TRUE)[,1]])
}

Autores_Artigo_Citacoes<-data.frame(TodosAutores,q_artigos_Autor,TotalAutoresCitacoes)

Autores_Artigo_Citacoes_Ordenado<-
  Autores_Artigo_Citacoes[order(Autores_Artigo_Citacoes$q_artigos_Autor,decreasing = TRUE),]

View(Autores_Artigo_Citacoes_Ordenado)

write.xlsx(Autores_Artigo_Citacoes_Ordenado, file = "Autores_Artigo_Citacoes_Ordenado.xlsx")


## Análises sobre PAÍSES ####
biblioshiny()

#Autores correspondentes
AutoresCorrespondentes<-as.data.frame(subset(M, select = c("AU", "TI", "TC", "C1", "RP", "DI")))

write.xlsx(AutoresCorrespondentes, file = "AutoresCorrespondentes.xlsx")


-------------------------------------------------------
## A partir daqui são os incrementos desenvolvidos ####

#Palavras-chaves com maior frequência

Summary <- summary(object = results, k = 1885, pause = FALSE) #número de 'k' tem que ser editado para o tamanho da amostra
MostRelKeywords<-(Summary[["MostRelKeywords"]])
write.xlsx(MostRelKeywords, file = "MostRelKeywords.xlsx")

##Criar lista de países#####
Pais1<-unique(codelist_panel$country.name.en)

##Separar, por país, todos os autores de cada documento (coluna C1)####
baseC1<-baseAux
baseC1$Pais<-NA
str_split(baseC1$C1,";")

npaises=rep(0,nrow(baseC1))

i=1
for (i in 1:nrow(baseC1)) {
  npaises[i]<-length(unlist(str_split(baseC1$C1,";")[i]))
}

ncoluna<-max(npaises)

which(npaises==ncoluna)
baseC1[which(npaises==ncoluna),]
baseC1$C1[(npaises==ncoluna)]

#a<-str_split(baseC1$C1,";")[1]
#b<-str_split(unlist(a),",")[[1]][length(str_split(unlist(a),",")[[1]])]
#c<-unique(substr(b, start = 2, stop = nchar(b)))

ListaAutores<-matrix(NA, ncol = ncoluna, nrow = length(baseC1$C1))

i=1
j=1
for (i in 1:length(baseC1$C1)) {
  for (j in 1:npaises[i]) {
    if(
      is.na(
        substr(str_split(unlist(str_split(baseC1$C1,";")[i]),",")[[j]]
               [length(str_split(unlist(str_split(baseC1$C1,";")[i]),",")[[j]])],
               start = 2, stop =
               nchar(str_split(unlist(str_split(baseC1$C1,";")[i]),",")[[j]]
                     [length(str_split(unlist(str_split(baseC1$C1,";")[i]),",")[[j]])]))
      )
    ) {next()}

    ListaAutores[i,j]<-
      substr(str_split(unlist(str_split(baseC1$C1,";")[i]),",")[[j]]
             [length(str_split(unlist(str_split(baseC1$C1,";")[i]),",")[[j]])],
             start = 2, stop =
               nchar(str_split(unlist(str_split(baseC1$C1,";")[i]),",")[[j]]
                     [length(str_split(unlist(str_split(baseC1$C1,";")[i]),",")[[j]])]))

  }
}

ListaAutores

View(as.tibble(ListaAutores))

Lista_paises_por_artigo<-matrix(NA, ncol = ncoluna, nrow = length(baseC1$C1))

i=1
for (i in 1:length(baseC1$C1)) {
  Lista_paises_por_artigo[i,]<-c(unique(na.omit(ListaAutores[i,])), rep(NA,
                                                                        ncoluna-length(unique(na.omit(ListaAutores[i,]))))
  )
}
Lista_paises_por_artigo
View(Lista_paises_por_artigo)

##Inspecionar elementos "paises"#####
Lista_paises_por_artigo
as.data.frame(Lista_paises_por_artigo)

#Caso mais de um autor por país, contabilizar apenas 1 país
vetor_de_paises<-as.vector(Lista_paises_por_artigo)
paises<-sort(unique(as.vector(Lista_paises_por_artigo)))

q_artigos_pais<-rep(0,length(paises))

i=1
for (i in 1:length(paises)) {
  q_artigos_pais[i]<-length(which(vetor_de_paises==paises[i]))
}
q_artigos_pais

data<-as.matrix(cbind(paises,q_artigos_pais))
Paises_1pais_por_artigo<-data[order(as.numeric(data[,2]), decreasing = TRUE),]
View(Paises_1pais_por_artigo)

#Analisar, em conjunto, resultados de "Lista_paises_por_artigo" e "Paises_1pais_por_artigo"
#Comparar resultado de "Paises_1pais_por_artigo" com VOSVIEWER e investigar possíveis discrepâncias

#País "INC."
#baseAux$DI[465]
#Paises_1pais_por_artigo[66]
#Paises_1pais_por_artigo[66]="USA"

#Específico da Amostra - Atribuir PAÍSES a Instituições ou Ajustar a grafia dos países após inspeção manual

Lista2_paises_por_artigo<-Lista_paises_por_artigo

#País "COSAPI SA"
which(str_detect(Lista_paises_por_artigo, pattern = 'COSAPI SA'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "COSAPI SA"]<-NA #Pois COSAPI SA seria atribuído ao Peru

#País "SOUTHLAND INDUSTRIES"
which(str_detect(Lista_paises_por_artigo, pattern = 'SOUTHLAND INDUSTRIES'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "SOUTHLAND INDUSTRIES"]<-NA #Pois SOUTHLAND INDUSTRIES seria atribuído aos United States

#País "DPR CONSTRUCTION"
which(str_detect(Lista_paises_por_artigo, pattern = 'DPR CONSTRUCTION'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "DPR CONSTRUCTION"]<-NA #Pois DPR CONSTRUCTION seria atribuído aos United States

#País "SEATTLE"
which(str_detect(Lista_paises_por_artigo, pattern = 'SEATTLE'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "SEATTLE"]<-"SOUTH KOREA"
Lista2_paises_por_artigo [348,3]<-NA
#Pois SEATTLE seria atribuído aos United States e o país do próximo autor (terceira coluna) seria South Korea

#País "ROBERT GORDON UNIVERSITY" #'COPENHAGEN TECHNICAL ACADEMY,ROBERT GORDON UNIVERSITY'
which(str_detect(Lista_paises_por_artigo, pattern = 'ROBERT GORDON UNIVERSITY'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "ROBERT GORDON UNIVERSITY"]<-"SWEDEN"
Lista2_paises_por_artigo [434,2]<-"UNITED KINGDOM"

#País "NA" no artigo mais citado
Lista2_paises_por_artigo [382,1]<-"UNITED STATES"

#Específico da Amostra - Após alterações, criar uma df "N" (final)
ListaN_paises_por_artigo<-Lista2_paises_por_artigo

##Análise Países (1º autor), Artigos e Citações####
ListaN_paises_por_artigo
as.data.frame(ListaN_paises_por_artigo)

Primeiropais=ListaN_paises_por_artigo[,1]
dfPrimeiropais<-as.data.frame(Primeiropais)
View(dfPrimeiropais)

TC<-baseAux$TC
PaisTC<-data.frame(dfPrimeiropais,TC)

PaisTC_Analise<-
  group_by(PaisTC,Primeiropais) %>%
  summarise(TotalCitacoes=sum(TC),
            Count = n())%>%
  mutate(razao = TotalCitacoes / Count)%>%
  arrange (desc(Count))%>%
  select(Primeiropais,Count,TotalCitacoes,razao)
PaisTC_Analise

PaisTC_Analise_Ordenado<-PaisTC_Analise
View(PaisTC_Analise_Ordenado)
write.xlsx(PaisTC_Analise_Ordenado, file = "PaisTC_Analise_Ordenado.xlsx")

#Outra Opção:
#PaisTC_Analise<-
#  group_by(PaisTC,Primeiropais) %>%
#  summarise(TotalCitacoes=sum(TC),
#            Count = n(),
#            Media = TotalCitacoes/Count)

#PaisTC_Analise_Ordenado<-PaisTC_Analise[order(PaisTC_Analise$Count,decreasing=T),]
#View(PaisTC_Analise_Ordenado)
#write.xlsx(PaisTC_Analise_Ordenado, file = "PaisTC_Analise_Ordenado.xlsx")

##Análise de 1 país por artigo (não contabiliza repetições de países por artigo)####
vetor_de_paises<-as.vector(ListaN_paises_por_artigo)

paises<-sort(unique(as.vector(ListaN_paises_por_artigo)))

q_artigos_pais<-rep(0,length(paises))

i=1
for (i in 1:length(paises)) {
  q_artigos_pais[i]<-length(which(vetor_de_paises==paises[i]))
}
q_artigos_pais

data<-as.matrix(cbind(paises,q_artigos_pais))
Paises_1pais_por_artigo<-data[order(as.numeric(data[,2]), decreasing = TRUE),]
Paises_1pais_por_artigo

View(Paises_1pais_por_artigo)
#Após esse resultado, inspecionar manualmente eventuais falhas.
#Comparar esse resultado com VOSVIEWER e investigar possíveis discrepâncias

#País "INC."
#baseAux$DI[465]
#Paises_1pais_por_artigo[66]
#Paises_1pais_por_artigo[66]="USA"

#Quantidade Artigos e Citações
dfLista_paises_por_artigo<-as.data.frame(ListaN_paises_por_artigo)
TC<-baseAux$TC
PaisesTC<-data.frame(dfLista_paises_por_artigo,TC)

TotalCitacoes=rep(0,nrow(Paises_1pais_por_artigo))

i=1
for (i in 1:nrow(Paises_1pais_por_artigo)) {
  TotalCitacoes[i]<-sum(PaisesTC$TC[which(PaisesTC==Paises_1pais_por_artigo[i,1],arr.ind = TRUE)[,1]])
}

Paises_1pais_por_artigo_Citacoes<-cbind(Paises_1pais_por_artigo,TotalCitacoes)

dfPaises_1pais_por_artigo_Citacoes<-data.frame(Paises_1pais_por_artigo_Citacoes)

dfPaises_1pais_por_artigo_Citacoes$razao=
  (as.numeric(dfPaises_1pais_por_artigo_Citacoes$TotalCitacoes)/as.numeric(dfPaises_1pais_por_artigo_Citacoes$q_artigos_pais))

View(dfPaises_1pais_por_artigo_Citacoes)
write.xlsx(dfPaises_1pais_por_artigo_Citacoes, file = "dfPaises_1pais_por_artigo_Citacoes.xlsx")

#Comparar com dados do VosViewer

##Plotar mapa - googleVis####
dfPaises_1pais_por_artigo<-unfactor(as.data.frame(na.omit(Paises_1pais_por_artigo)))

Geo=gvisGeoChart(dfPaises_1pais_por_artigo, locationvar="paises",
                 colorvar="q_artigos_pais",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)

##Análise Cooperação internacional####
ListaN_paises_por_artigo

Segundopais=ListaN_paises_por_artigo[,2]
dfSegundopais<-as.data.frame(Segundopais)

df2paises<-data.frame(dfPrimeiropais,dfSegundopais)

is.na(df2paises$Segundopais)

ifelse(is.na(df2paises$Segundopais)==TRUE,"ÚNICO","MÚLTIPLO")

dfUnicoMultiplo<-as.data.frame(cbind(Primeiropais,ifelse(is.na(df2paises$Segundopais)==TRUE,"ÚNICO","MÚLTIPLO")))

CoopInternacional<-table(dfUnicoMultiplo$Primeiropais,dfUnicoMultiplo$V2)

total<-rep(0,nrow(CoopInternacional))

i=1
for (i in 1:nrow(CoopInternacional)) {
  total[i]<-sum(CoopInternacional[i,1],CoopInternacional[i,2])
}

CoopInternacional_final<-cbind(CoopInternacional,total)
CoopInternacional_final

dfCoopInternacional_final<-data.frame(CoopInternacional_final)

CoopInternacional_final_Ordenado <-
  dfCoopInternacional_final %>%
  arrange (desc(total))%>%
  select(total,ÚNICO, MÚLTIPLO)
CoopInternacional_final_Ordenado

CoopInternacional_final_Ordenado$PaisUnico=
  scales::percent((CoopInternacional_final_Ordenado$ÚNICO/CoopInternacional_final_Ordenado$total),accuracy=.1)

View(CoopInternacional_final_Ordenado)
write.xlsx(CoopInternacional_final_Ordenado, file = "CoopInternacional_final_Ordenado.xlsx")

#FIM ------------------------------------------------------
