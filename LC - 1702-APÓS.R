## Carregar Pacotes #####
c(library(bibliometrix),library(tidyverse),library(countrycode),library(googleVis),library(varhandle),
  library(dplyr),library(openxlsx),library(readxl),library(readr),library(stringr))


## Setar pasta de trabalho #####
setwd("C:/Users/") #Complementar com caminho da pasta

## Imputar e converter Base de Dados ####
M <- convert2df("scopus 1702 - 07.22.bib", dbsource = "scopus", format = "bibtex")
base <- read_csv("scopus 1702 - 07.22.csv")
base2 <- base
base3<-subset(base2,select = c(Authors,`Author(s) ID`))


## Criar uma base Auxiliar#####
baseAux<-M


## Autores - Individualização dos Nomes e ID #####
NomeAutorBIB<-M$AU
base3BIB<-base3
base3BIB$Authors<-c(NomeAutorBIB) #Sobrescrever a coluna de autores com o padrão .bib (em CAPS e separado por ";")

IDs2<-base3$`Author(s) ID`
IDs3<-str_sub(IDs2,1,nchar(IDs2)-1) #Remover o ";" no final de cada linha
base3_linha<-data.frame(base3BIB$Authors,IDs3) #Criar df com autores e IDs corrigidos

base3_linha$base3BIB.Authors[which(base3_linha$base3BIB.Authors == "NA NA")]<-NA
base3_linha$IDs3[which(base3_linha$IDs3 == "[No author id available")]<-NA

autores2<-unlist(stringr::str_split(base3_linha$base3BIB.Authors,pattern = ";")) #Separar os autores
IDs4<-unlist(stringr::str_split(base3_linha$IDs3,pattern = ";")) #Separar os IDs

base_individualizados<-data.frame(autores2,IDs4) #Criar df com autores e IDs individualizados


## Autores - Normalização das grafias ####
#Nomes distintos e únicos
base_final2<-plyr::count(base_individualizados$IDs4) #Contagem dos IDs

base_individualizados2<-base_individualizados

base_individualizados3<-base_individualizados
base_individualizados3$Nomes_distintos2<-NA #Nova coluna para nomes distintos
base_individualizados3$Nomes_unicos2<-NA #Nova coluna para nomes únicos

#Prencher as duas novas colunas com nomes distintos e únicos
i=1
for(i in 1:nrow(base_individualizados3)){
  base_individualizados3$Nomes_distintos2[i]<-
    str_c(unique(base_individualizados3$autores2[which(base_individualizados3$IDs4==base_individualizados3$IDs4[i])]),collapse ="; ")
  base_individualizados3$Nomes_unicos2[i]<-unique(base_individualizados3$autores2[which(base_individualizados3$IDs4==base_individualizados3$IDs4[i])])[1]
}

#Criar df com autores e IDs individualizados, sem repetição
base_individualizados2<-base_individualizados3[!duplicated(base_individualizados3$IDs4),]

#Identififcar "homônimos" e acrescentar um número sequencial
i=1
for (i in 1:nrow(base_individualizados2)) {
  if(length(which(base_individualizados2$Nomes_unicos2[i]==base_individualizados2$Nomes_unicos2))>1){
    base_individualizados2$Nomes_unicos2[which(base_individualizados2$Nomes_unicos2[i]==base_individualizados2$Nomes_unicos2)]<-
      paste0(base_individualizados2$Nomes_unicos2[which(base_individualizados2$Nomes_unicos2[i]==base_individualizados2$Nomes_unicos2)],
             c(1:length(which(base_individualizados2$Nomes_unicos2[i]==base_individualizados2$Nomes_unicos2))))
  }
}

# Imputar a grafia "normalizada" dos autores, na "base original"
base2$AutorNormalizado<-NA

base2$Authors[which(base2$Authors== "[No author name available]")]<-NA

#Preencher nova coluna "$AutorNormalizado", na "base original", com o nome dos autores normalizados (sem variações de grafia)
i=1
j=1
for(i in 1:nrow(base2)){
  Autores3<-rep(NA,length(unlist(strsplit(base2$`Author(s) ID`, ";")[i])))
  if(
    is.na(base2$Authors[i])){
    base2$AutorNormalizado[i]<-NA
    i<-i+1
  }
  else{
    j=1
    for (j in 1:length( Autores3)){
      Autores3[j]<-base_individualizados2$Nomes_unicos2[which(base_individualizados2$IDs4==unlist(strsplit(base2$`Author(s) ID`, ";")[i])[j])]
    }
    base2$AutorNormalizado[i]<-
      str_c(Autores3,collapse = "; ")
  }
}

## Edições nas bases para exportação ####

baseExportarCSV<-base
baseExportarCSV$Authors<-gsub(";",".,",base2$AutorNormalizado)
baseExportarCSV$Authors<-str_pad(string=baseExportarCSV$Authors,side = "right", width = nchar(baseExportarCSV$Authors)+1, pad = ".")

write.csv(baseExportarCSV, file = "scopus 1702 - FINAL-NORMALIZADO.csv")



## Análises sobre AUTORES #####

## Análise - Quantidade de autores de cada documento ####
baseAux$AU<-gsub("; ",";",base2$AutorNormalizado)


results2 <- biblioAnalysis(baseAux, sep = ";")
options(width=100) #Ano / Autor / Source
S2 <- summary(object = results2, k = 3000, pause = FALSE)
plot(x = results, k = 10, pause = FALSE)


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

write.xlsx(Autores_Artigo_Citacoes_Ordenado, file = "Autores_Artigo_Citacoes_Ordenado-FINAL.xlsx")



## Análises sobre PAÍSES #####

## Bibliometrix - biblioshiny ####
biblioshiny()

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


#Específico da Amostra - Atribuir PAÍSES a Instituições ou Ajustar a grafia dos países após inspeção manual

Lista2_paises_por_artigo<-Lista_paises_por_artigo

#País "14 IDT" e "CAREY CONSTRUCTION"
which(str_detect(Lista_paises_por_artigo, pattern = '14 IDT'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "14 IDT"]<-"UNITED KINGDOM"
which(str_detect(Lista_paises_por_artigo, pattern = 'CAREY CONSTRUCTION'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "CAREY CONSTRUCTION"]<-NA #Pois CAREY CONSTRUCTION seria atribuído à UNITED KINGDOM

#País "ADDRESS"
which(str_detect(Lista_paises_por_artigo, pattern = 'ADDRESS'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "ADDRESS"]<-NA #Pois ADDRESS seria atribuído à INDIA

#País "BMW AG" e "VL BAUCONSULT GMBH"
which(str_detect(Lista_paises_por_artigo, pattern = 'VL BAUCONSULT GMBH'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "VL BAUCONSULT GMBH"]<-"GERMANY"
which(str_detect(Lista_paises_por_artigo, pattern = 'BMW AG'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "BMW AG"]<-NA #Pois BMW AG seria atribuído à GERMANY
Lista2_paises_por_artigo [1414,3]<-NA

#País "CC"
which(str_detect(Lista_paises_por_artigo, pattern = 'CC'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "CC"]<-"UNITED KINGDOM"

#País "COMPUTATIONAL DESIGN", "CONSTRUCTION MANAGEMENT", "INFORMATION SYSTEMS AND TECHNOLOGY MANAGEMENT"
which(str_detect(Lista_paises_por_artigo, pattern = 'COMPUTATIONAL DESIGN'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "COMPUTATIONAL DESIGN"]<-"AUSTRALIA"
which(str_detect(Lista_paises_por_artigo, pattern = 'CONSTRUCTION MANAGEMENT'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "CONSTRUCTION MANAGEMENT"]<-NA #Pois CONSTRUCTION MANAGEMENT seria atribuído à AUSTRALIA
which(str_detect(Lista_paises_por_artigo, pattern = 'INFORMATION SYSTEMS AND TECHNOLOGY MANAGEMENT'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "INFORMATION SYSTEMS AND TECHNOLOGY MANAGEMENT"]<-NA #Pois INFORMATION SYSTEMS AND TECHNOLOGY MANAGEMENT seria atribuído à AUSTRALIA
Lista2_paises_por_artigo [986,2]<-NA

#País "COSAPI SA"
which(str_detect(Lista_paises_por_artigo, pattern = 'COSAPI SA'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "COSAPI SA"]<-NA #Pois COSAPI SA seria atribuído ao PERU

#País "COWI" e "COLLABDECISIONS"
which(str_detect(Lista_paises_por_artigo, pattern = 'COWI'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "COWI"]<-"DENMARK"
which(str_detect(Lista_paises_por_artigo, pattern = 'COLLABDECISIONS'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "COLLABDECISIONS"]<-NA #Pois COLLABDECISIONS seria atribuído à GERMANY

#País "EPARTMENT OF CIVIL ENGINEERING"
which(str_detect(Lista_paises_por_artigo, pattern = 'EPARTMENT OF CIVIL ENGINEERING'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "EPARTMENT OF CIVIL ENGINEERING"]<-"FINLAND"
Lista2_paises_por_artigo [509,2]<-NA

#País "GRAA Y MONTERO"
which(str_detect(Lista_paises_por_artigo, pattern = 'GRAA Y MONTERO'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "GRAA Y MONTERO"]<-NA #Pois GRAA Y MONTERO seria atribuído ao PERU

#País "GRADUATE SCHOOL OF BUSINESS (GSB)"
which(str_detect(Lista_paises_por_artigo, pattern = 'GRADUATE SCHOOL OF BUSINESS (GSB)'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "GRADUATE SCHOOL OF BUSINESS (GSB)"]<-"SOUTH AFRICA"

#País "IVIL AND ENVIR. ENGINEERING. DEPT."
which(str_detect(Lista_paises_por_artigo, pattern = 'IVIL AND ENVIR. ENGINEERING. DEPT.'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "IVIL AND ENVIR. ENGINEERING. DEPT."]<-"COLOMBIA"
Lista2_paises_por_artigo [767,2]<-NA

#País "UNIVERSITY OF FLORIDA" e "IL"
which(str_detect(Lista_paises_por_artigo, pattern = 'UNIVERSITY OF FLORIDA'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "UNIVERSITY OF FLORIDA"]<-"UNITED STATES"
which(str_detect(Lista_paises_por_artigo, pattern = 'IL'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "IL"]<-NA #Pois IL seria atribuído aos UNITED STATES

#País "INNOVATIVE AND INDUSTRIAL CONSTRUCTION"
which(str_detect(Lista_paises_por_artigo, pattern = 'INNOVATIVE AND INDUSTRIAL CONSTRUCTION'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "INNOVATIVE AND INDUSTRIAL CONSTRUCTION"]<-NA #Pois INNOVATIVE AND INDUSTRIAL CONSTRUCTION seria atribuído à SWITZERLAND

#País "LD CONSULTING" e "UFRGS"
which(str_detect(Lista_paises_por_artigo, pattern = 'LD CONSULTING'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "LD CONSULTING"]<-NA #Pois ADDRESS seria atribuído ao BRAZIL
which(str_detect(Lista_paises_por_artigo, pattern = 'UFRGS'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "UFRGS"]<-NA #Pois ADDRESS seria atribuído ao BRAZIL
Lista2_paises_por_artigo [1144,2]<-"UNITED STATES"
Lista2_paises_por_artigo [1144,3]<-"UNITED KINGDOM"
Lista2_paises_por_artigo [1144,4]<-NA

#País "NORWEGIAN UNIVERSITY OF SCIENCE AND TECHNOLOGY (NTNU)"
which(str_detect(Lista_paises_por_artigo, pattern = 'NORWEGIAN UNIVERSITY OF SCIENCE AND TECHNOLOGY (NTNU)'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "NORWEGIAN UNIVERSITY OF SCIENCE AND TECHNOLOGY (NTNU)"]<-"NORWAY"
Lista2_paises_por_artigo [1481,2]<-NA

#País "NOTTINGHAM TRENT UNIVERSITY"
which(str_detect(Lista_paises_por_artigo, pattern = 'NOTTINGHAM TRENT UNIVERSITY'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "NOTTINGHAM TRENT UNIVERSITY"]<-"UNITED KINGDOM"

#País "ONSTRUCTION MACE GROUP"
which(str_detect(Lista_paises_por_artigo, pattern = 'ONSTRUCTION MACE GROUP'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "ONSTRUCTION MACE GROUP"]<-"UNITED KINGDOM"
Lista2_paises_por_artigo [1395,2]<-NA

#País "PONTIFICAL CATHOLIC UNIVERSITY OF PERU"
which(str_detect(Lista_paises_por_artigo, pattern = 'PONTIFICAL CATHOLIC UNIVERSITY OF PERU'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "PONTIFICAL CATHOLIC UNIVERSITY OF PERU"]<-"PERU"

#País "PONTIFICIA UNIVERSIDAD JAVERIANA"
which(str_detect(Lista_paises_por_artigo, pattern = 'PONTIFICIA UNIVERSIDAD JAVERIANA'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "PONTIFICIA UNIVERSIDAD JAVERIANA"]<-"COLOMBIA"
Lista2_paises_por_artigo [1167,3]<-NA

#País "RAN"
which(str_detect(Lista_paises_por_artigo, pattern = 'RAN'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "RAN"]<-"IRAN"

#País "SAN DIEGO STATE UNIVERSITY"
which(str_detect(Lista_paises_por_artigo, pattern = 'SAN DIEGO STATE UNIVERSITY'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "SAN DIEGO STATE UNIVERSITY"]<-"UNITED STATES"

#País "SUDESTE PR-FABRICADOS LTDA" e "UNIVERSITY OF CAMPINAS (UNICAMP)"
which(str_detect(Lista_paises_por_artigo, pattern = 'UNIVERSITY OF CAMPINAS (UNICAMP)'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "UNIVERSITY OF CAMPINAS (UNICAMP)"]<-"BRAZIL"
which(str_detect(Lista_paises_por_artigo, pattern = 'SUDESTE PR-FABRICADOS LTDA'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "SUDESTE PR-FABRICADOS LTDA"]<-NA #Pois SUDESTE PR-FABRICADOS LTDA seria atribuído ao BRAZIL
Lista2_paises_por_artigo [1479,3]<-NA

#País "TECHNION - IIT"
which(str_detect(Lista_paises_por_artigo, pattern = 'TECHNION - IIT'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "TECHNION - IIT"]<-"ISRAEL"

#País "TR. INNOV. IN PROJ. AND PROD. MGMT."
which(str_detect(Lista_paises_por_artigo, pattern = 'TR. INNOV. IN PROJ. AND PROD. MGMT.'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "TR. INNOV. IN PROJ. AND PROD. MGMT."]<-"UNITED STATES"

#País "TURKEY (E-MAIL: DEMIRKESEN@GTU.EDU.TR)." e "TURKEY (E-MAIL: HGBAYHAN@SAKARYA.EDU.TR)."
which(str_detect(Lista_paises_por_artigo, pattern = 'TURKEY (E-MAIL: DEMIRKESEN@GTU.EDU.TR).'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "TURKEY (E-MAIL: DEMIRKESEN@GTU.EDU.TR)."]<-"TURKEY"
which(str_detect(Lista_paises_por_artigo, pattern = 'TURKEY (E-MAIL: HGBAYHAN@SAKARYA.EDU.TR).'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "TURKEY (E-MAIL: HGBAYHAN@SAKARYA.EDU.TR)."]<-NA #Pois TURKEY (E-MAIL: HGBAYHAN@SAKARYA.EDU.TR). seria atribuído à TURKEY

#País "UNIVERSIDADE DE SO PAULO"
which(str_detect(Lista_paises_por_artigo, pattern = 'UNIVERSIDADE DE SO PAULO'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "UNIVERSIDADE DE SO PAULO"]<-NA #Pois UNIVERSIDADE DE SO PAULO seria atribuído ao BRAZIL

#País "UNIVERSIDADE DO ESTADO DO RIO DE JANEIRO" e "UNIVERSIDADE DE TAUBAT"
which(str_detect(Lista_paises_por_artigo, pattern = 'UNIVERSIDADE DO ESTADO DO RIO DE JANEIRO'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "UNIVERSIDADE DO ESTADO DO RIO DE JANEIRO"]<-"BRAZIL"
which(str_detect(Lista_paises_por_artigo, pattern = 'UNIVERSIDADE DE TAUBAT'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "UNIVERSIDADE DE TAUBAT"]<-NA #Pois UNIVERSIDADE DE TAUBAT seria atribuído ao BRAZIL
Lista2_paises_por_artigo [1679,2]<-NA

#País "YV BRAVO CONSTRUCTION GROUP"
which(str_detect(Lista_paises_por_artigo, pattern = 'YV BRAVO CONSTRUCTION GROUP'))
Lista2_paises_por_artigo[Lista2_paises_por_artigo == "YV BRAVO CONSTRUCTION GROUP"]<-"PERU"
Lista2_paises_por_artigo [1425,2]<-NA


#Específico da Amostra - Após alterações, criar uma df "N" (final)
ListaN_paises_por_artigo<-Lista2_paises_por_artigo

## Análise Países (1º autor), Artigos e Citações ####
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


## Análise de 1 país por artigo (não contabiliza repetições de países por artigo) ####
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

## Análise Cooperação internacional ####
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
