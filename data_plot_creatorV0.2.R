library(stringr)
library(data.table)
library(highcharter)
library(dplyr)
library(pracma)
library(highcharter)
library(dplyr)
library(httr)
library(purrr)
library(jsonlite)

################### DOWNLOAD AND UNZIP FILE ################### 

setwd(dir = "/home/biagio/R/")
download.file(url = "https://github.com/pcm-dpc/COVID-19/archive/master.zip", destfile = "COVID-19-master.zip")

outDir<-"/home/biagio/R/"
unzip("/home/biagio/R/COVID-19-master.zip",exdir=outDir)

################### CREATE NATIONAL TABLE ################### 

filename <- "/home/biagio/R/COVID-19-master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
table_naz <- read.csv(file= filename, stringsAsFactors = FALSE)

filename <- "/home/biagio/R/COVID-19-master/dati-regioni/dpc-covid19-ita-regioni.csv"
table_reg <- read.csv(file= filename, stringsAsFactors = FALSE)

filename <- "/home/biagio/R/COVID-19-master/dati-province/dpc-covid19-ita-province.csv"
table_prov <- read.csv(file= filename, stringsAsFactors = FALSE)


################### TABELLA DATI INFORMATIVI GENERALI ################### 

#FORMAT DATA FRAME TO CREATE BAR AND PIE PLOTS IN INTRO
bar_plot_intro <- data.frame(
  data_name = character(),
  perc = numeric(),
  stringsAsFactors = FALSE
)

bar_plot_intro[1,1] <- "Deceduti"
bar_plot_intro[2,1] <- "Positivi"
bar_plot_intro[3,1] <- "Guariti"

bar_plot_intro[1,2] <- (tail(table_naz[,"deceduti"],1)/tail(table_naz[,"totale_casi"],1))*100
bar_plot_intro[2,2] <- (tail(table_naz[,"totale_positivi"],1)/tail(table_naz[,"totale_casi"],1))*100
bar_plot_intro[3,2] <- (tail(table_naz[,"dimessi_guariti"],1)/tail(table_naz[,"totale_casi"],1))*100

bar_plot_intro[,2] <- signif(bar_plot_intro[,2], digits=4)

path_out <- "/home/biagio/R/data_plot/"
write.csv(bar_plot_intro,paste(path_out,'intro_plot.csv',sep = ''))

################### TABELLA DATI NAZIONALI ################### 

dati_nazionali <- table_naz[,c(1,3,4,5,7, 9, 10, 11, 12, 13)]
dati_nazionali$data <- substr(dati_nazionali$data,1,nchar(dati_nazionali$data)-9)

#------- dati giornalieri

dati_nazionali <- dati_nazionali %>% 
  mutate(ricoverati_con_sintomi_giorn = ricoverati_con_sintomi - shift(ricoverati_con_sintomi))

dati_nazionali <- dati_nazionali %>% 
  mutate(terapia_intensiva_giorn = terapia_intensiva - shift(terapia_intensiva))

dati_nazionali <- dati_nazionali %>% 
  mutate(totale_ospedalizzati_giorn = totale_ospedalizzati - shift(totale_ospedalizzati))

dati_nazionali <- dati_nazionali %>% 
  mutate(dimessi_giorn = dimessi_guariti - shift(dimessi_guariti))

dati_nazionali <- dati_nazionali %>% 
  mutate(deceduti_giorn = deceduti - shift(deceduti))

dati_nazionali <- dati_nazionali %>% 
  mutate(casi_giorn = totale_casi - shift(totale_casi))

dati_nazionali <- dati_nazionali %>% 
  mutate(tamponi_giorn = tamponi - shift(tamponi))

dati_nazionali <- dati_nazionali %>% 
  mutate(zero_line = 0) 

#------- assetto dati

dati_nazionali <- dati_nazionali[-1,]

#------- dati media mobile 7 giorni

dati_nazionali <- dati_nazionali %>%
  mutate(ricoverati_con_sintomi_mm = movavg(dati_nazionali[,"ricoverati_con_sintomi_giorn"], 7))

dati_nazionali <- dati_nazionali %>%
  mutate(terapia_intensiva_mm = movavg(dati_nazionali[,"terapia_intensiva_giorn"], 7))

dati_nazionali <- dati_nazionali %>%
  mutate(totale_ospedalizzati_mm = movavg(dati_nazionali[,"totale_ospedalizzati_giorn"], 7))

dati_nazionali <- dati_nazionali %>%
 mutate(nuovi_positivi_mm = movavg(dati_nazionali[,"nuovi_positivi"], 7))

dati_nazionali <- dati_nazionali %>%
  mutate(dimessi_giorn_mm = movavg(dati_nazionali[,"dimessi_giorn"], 7))

dati_nazionali <- dati_nazionali %>%
  mutate(deceduti_giorn_mm = movavg(dati_nazionali[,"deceduti_giorn"], 7))

dati_nazionali <- dati_nazionali %>%
  mutate(casi_giorn_mm = movavg(dati_nazionali[,"casi_giorn"], 7))

dati_nazionali <- dati_nazionali %>%
  mutate(tamponi_mm = movavg(dati_nazionali[,"tamponi_giorn"], 7))

#------- nuovi casi al netto di guariti e deceduti

dati_nazionali <- dati_nazionali %>% 
  mutate(casi_netti = nuovi_positivi - deceduti_giorn - dimessi_giorn)

dati_nazionali <- dati_nazionali %>%
  mutate(casi_netti_mm = movavg(dati_nazionali[,"casi_netti"], 7))

path_out <- "/home/biagio/R/data_plot/"
write.csv(dati_nazionali,paste(path_out,'dati_nazionali.csv',sep = ''))

################### TABELLA DATI INFOBOX ################### 

#FORMAT DATA FRAME TO CREATE INFO BOX
info_box <- data.frame(
  deceduti = numeric(),
  positivi = numeric(),
  guariti = numeric(),
  totale_casi = numeric(),
  stringsAsFactors = FALSE
)

info_box[1,1] <- tail(dati_nazionali[,"deceduti"],1)
info_box[1,2] <- tail(dati_nazionali[,"totale_positivi"],1)
info_box[1,3] <- tail(dati_nazionali[,"dimessi_guariti"],1)
info_box[1,4] <- tail(dati_nazionali[,"totale_casi"],1)


path_out <- "/home/biagio/R/data_plot/"
write.csv(info_box,paste(path_out,'info_box.csv',sep = ''))

################### TABELLA DATI REGIONALI ################### 

#CREO CSV GLOBALE DELLE REGIONI
dati_regionali <- table_reg[,c(1,4,7,8,9,11, 13, 14, 15, 16, 17)]
dati_regionali$data <- substr(dati_regionali$data,1,nchar(dati_regionali$data)-9)

dati_regionali<-dati_regionali[order(dati_regionali$denominazione_regione, decreasing = TRUE), ]

#------- dati giornalieri

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(ricoverati_con_sintomi_giorn = ricoverati_con_sintomi - shift(ricoverati_con_sintomi))

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(terapia_intensiva_giorn = terapia_intensiva - shift(terapia_intensiva))

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(totale_ospedalizzati_giorn = totale_ospedalizzati - shift(totale_ospedalizzati))

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(dimessi_giorn = dimessi_guariti - shift(dimessi_guariti))

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(deceduti_giorn = deceduti - shift(deceduti))

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(casi_giorn = totale_casi - shift(totale_casi))

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(tamponi_giorn = tamponi - shift(tamponi))

dati_regionali <- dati_regionali %>% 
  mutate(zero_line = 0) 

#------- dati media mobile 7 giorni

dati_regionali[is.na(dati_regionali)] = 0

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(ricoverati_con_sintomi_mm = movavg(ricoverati_con_sintomi_giorn, 7))

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(terapia_intensiva_mm = movavg(terapia_intensiva_giorn, 7))

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(totale_ospedalizzati_mm = movavg(totale_ospedalizzati_giorn, 7))

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(nuovi_positivi_mm = movavg(nuovi_positivi, 7))

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(dimessi_giorn_mm = movavg(dimessi_giorn, 7))

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(deceduti_giorn_mm = movavg(deceduti_giorn, 7))

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(casi_giorn_mm = movavg(casi_giorn, 7))

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(tamponi_mm = movavg(tamponi_giorn, 7))

#------- nuovi casi al netto di guariti e deceduti

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(casi_netti = nuovi_positivi - deceduti_giorn - dimessi_giorn)

dati_regionali <- dati_regionali %>% group_by(denominazione_regione) %>%
  mutate(casi_netti_mm = movavg(casi_netti, 7))

path_out <- "/home/biagio/R/data_plot/"
write.csv(dati_regionali,paste(path_out,'dati_regionali.csv',sep = ''))


#CREO CSV PER SINGOLA REGIONE

regioni <- unique(dati_regionali$denominazione_regione)
count <- length(regioni)

i<-1
while(i<=count){
  
  dati_regionali_filtered <- dati_regionali %>% 
    filter(denominazione_regione == regioni[i])
  
  path_out <- "/home/biagio/R/data_plot/regioni/"
  file_name <- paste(regioni[i], ".csv", sep="")
  write.csv(dati_regionali_filtered,paste(path_out,file_name,sep = ''))
  
  i <- i+1
  
}

#grafico group by region
#hchart(dati_regionali, "line", hcaes(x = data, y = dimessi_giorn, group = "denominazione_regione"))%>% group_by(Puglia)

################### TABELLA DATI PROVINCIALI ################### 

#CREO CSV GLOBALE DELLE PROVINCE
dati_provinciali <- table_prov[,c(1,4,6,10)]
dati_provinciali$data <- substr(dati_provinciali$data,1,nchar(dati_provinciali$data)-9)

dati_provinciali<-dati_provinciali[order(dati_provinciali$denominazione_regione, decreasing = TRUE), ]

# hchart(dati_provinciali_filtered, "line", hcaes(x = data, y = totale_casi, group = "denominazione_provincia"))

path_out <- "/home/biagio/R/data_plot/"
write.csv(dati_provinciali,paste(path_out,'dati_provinciali.csv',sep = ''))


#CREO CSV PER SINGOLA REGIONE

regioni <- unique(dati_provinciali$denominazione_regione)
count <- length(regioni)

i<-1
while(i<=count){
  
  dati_provinciali_filtered <- dati_provinciali %>% 
    filter(denominazione_regione == regioni[i])
  
  path_out <- "/home/biagio/R/data_plot/province/"
  file_name <- paste(regioni[i],".csv", sep="")
  write.csv(dati_provinciali_filtered,paste(path_out,file_name,sep = ''))
  
  i <- i+1
  
}

################### TABELLA MAPPE PROVINCIALI ################### 
  
  ita <- read_json("/home/biagio/R/data_plot/json_prov.json")
  x <- ita$features[[1]]
  
  txt <- read.csv(file="/home/biagio/R/COVID-19-master/dati-province/dpc-covid19-ita-province-latest.csv", stringsAsFactors = FALSE )
  
  dfita2 <-  ita$features %>% 
    map_df(function(x){
      data_frame(hasc = x$properties$hasc, name = x$properties$name)
    }) 
  
  txt <- txt %>%
    mutate(hasc = paste("IT.", sigla_provincia, sep=""))
  
  dfita2 <- inner_join(dfita2, txt, by = "hasc")
  
  dfita2 <- dfita2[-c(3,4,5,6,7,9,10,11,13,14)]

  path_out <- "/home/biagio/R/data_plot/"
  write.csv(dfita2,paste(path_out,'mappa_prov.csv',sep = ''))

################### TABELLA MAPPE PROVINCIALI - BUBBLE ################### 

  







