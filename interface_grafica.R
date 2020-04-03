library(rgdal)
library(mapview)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(png)


####################### AJUSTE DAS CAMADAS DO SHAPEFILE #######################

# Leitura das bases georreferenciadas
## myshp = linhas. myshp2 = estações
myshp <- readOGR(dsn=path.expand("shp_atualizado"),
                 layer="dbo_tblLinhaEstacao_spatial_linestring", stringsAsFactors = FALSE)
myshp2 <- readOGR(dsn=path.expand("shp_atualizado"),
                  layer="dbo_tblEstacao_spatial_point", stringsAsFactors = FALSE, encoding = "UTF-8")

# Substituição do CodigoFerr, antes numérico, agora string, com o nome das estações
tblFerrovia <- readxl::read_excel("Dados/tblFerrovia.xlsx")
tblFerrovia$CodigoFerr <- tblFerrovia$CodigoFerrovia
myshp@data <- plyr::join(myshp@data,
                         tblFerrovia,
                         by='CodigoFerr')
myshp2@data <- plyr::join(myshp2@data,
                          tblFerrovia,
                          by='CodigoFerr')

# Substituição dos códigos de linha pelo nome completo
tblLinha <- readxl::read_excel("Dados/tblLinha.xlsx")
tblLinha$CodigoLi00 <- tblLinha$CodigoLinha
myshp@data <- plyr::join(myshp@data,
                         tblLinha,
                         by='CodigoLi00')

# Substituição dos códigos de estação pelo código de três letras
tblEstacao <- readxl::read_excel("Dados/tblEstacao.xlsx")
tblEstacao$CodigoEsta <- tblEstacao$CodigoEstacao
myshp@data <- plyr::join(myshp@data,
                         tblEstacao,
                         by='CodigoEsta')
DR_2020 <- readxl::read_excel("Dados/dr2020_original.xlsx")
DR_2020$linesta <- paste(DR_2020$Linha, DR_2020$B, sep='!')
myshp@data$linesta <- paste(myshp@data$NomeLinha, myshp@data$CodigoTresLetrasEstacao, sep='!')
myshp@data <- plyr::join(myshp@data,
                         DR_2020,
                         by='linesta')

# Substituição dos códigos de bitola pela sua classificação real
{myshp@data[myshp@data[["CodigoBito"]] == '1', 'CodigoBito'] <- 'Métrica'
  myshp@data[myshp@data[["CodigoBito"]] == '3', 'CodigoBito'] <- 'Larga'
  myshp@data[myshp@data[["CodigoBito"]] == '5', 'CodigoBito'] <- 'Mista'}


# Drop das colunas desnecessarias
myshp@data <- myshp@data[ , !names(myshp@data) %in% c("CodigoLinh", "CodigoLi00",
                                                      "CodigoEsta", "CodigoBito",
                                                      "NumeroSequ", "IndicadorC",
                                                      "IndicadorE", "CodigoLinha",
                                                      "CodigoTresC",
                                                      "CodigoLinh", "CodigoLi00",
                                                      "CodigoEsta", "CodigoFerr",
                                                      "CodigoFerrovia", "SiglaFerrovia",
                                                      "LogotipoFerrovia", "DataExclusao",
                                                      "IndicadorObrigatorioDesempenhoProducao",
                                                      "CodigoLinha", "CodigoEstacao",
                                                      "linesta", "Ferrovia", "Linha",
                                                      "X", "Ferrovia", "Linha",
                                                      "x", "CodigoTresLetrasEstacao",
                                                      "ExtensÃ.o..km.")]

myshp2@data <- myshp2@data[ , !names(myshp2@data) %in% c("CodigoEsta","CodigoFerr", 
                                                         "CodigoMuni", "DataExclus",
                                                         "IndicadorP", "CodigoPort",
                                                         "CodigoEsca", "CodigoArqu",
                                                         "IndicadorT", "IndicadorF",
                                                         "CodigoFerrovia", "LogotipoFerrovia",
                                                         "DataExclusao", 
                                                         "IndicadorObrigatorioDesempenhoProducao")]

colnames(myshp@data)[which(names(myshp@data) == "NumeroQuil")] <- "Marco Quilométrico"
colnames(myshp@data)[which(names(myshp@data) == "NumeroExte")] <- "Extensão do Entre Pátio (km)"
colnames(myshp@data)[which(names(myshp@data) == "NomeFerrovia")] <- "Ferrovia"
colnames(myshp@data)[which(names(myshp@data) == "NomeReduzidoFerrovia")] <- "Sigla-Ferrovia"
colnames(myshp@data)[which(names(myshp@data) == "NomeLinha")] <- "Linha"
colnames(myshp2@data)[which(names(myshp2@data) == "NomeEstaca")] <- "Nome Estação"
colnames(myshp2@data)[which(names(myshp2@data) == "CodigoTres")] <- "Código Estação"
colnames(myshp2@data)[which(names(myshp2@data) == "NomeFerrovia")] <- "Ferrovia"
colnames(myshp2@data)[which(names(myshp2@data) == "NomeReduzidoFerrovia")] <- "Sigla-Ferrovia"

df_estac <- tibble(myshp2@data$`Código Estação`, myshp2@data$`Nome Estação`)
colnames(df_estac) <- c('A','NomeA')
myshp@data <- plyr::join(myshp@data,
                         df_estac,
                         by='A')
colnames(df_estac) <- c('B','NomeB')
myshp@data <- plyr::join(myshp@data,
                         df_estac,
                         by='B')

myshp@data$`Nome Estação A` <- paste(myshp@data$NomeA, " (", myshp@data$A,")", sep="")
myshp@data$`Nome Estação B` <- paste(myshp@data$NomeB, " (", myshp@data$B,")", sep="")
myshp2@data$`Código Estação` <- paste(myshp2@data$`Nome Estação`, " (", myshp2@data$`Código Estação`,")", sep="")
myshp@data <- myshp@data[ , !names(myshp@data) %in% c("A", "B")]

library(leaflet.extras)

n <- 14
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
white = colorRampPalette(c('white', 'gray'))
img <- "Imgs/Logo Vertical - colorida sem fundo.png"

m1 <- mapview(myshp, zcol='Ferrovia',
              legend = TRUE,
              layer.name = "Ferrovia",
              color = col_vector) %>%
  leafem::addLogo(img, src = "local", width = 200, height = 120, url = "http://www.antt.gov.br/") %>%
  
  addMarkers(data = myshp2, lng = myshp2@coords[,1],
             lat=myshp2@coords[,2],
             popup = ~`Código Estação`,
             label=~`Código Estação`,
             group='Código Estação') %>%
  
  addSearchFeatures(targetGroups = 'Código Estação',
                    options = searchFeaturesOptions(
                      zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
                      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )) %>%
  
  addLayersControl(overlayGroups = c("Código Estação", "Ferrovia"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  hideGroup("Código Estação")

  
  
  
  mapshot(m1, url = "mapa_SFF.html", selfcontained = FALSE)























