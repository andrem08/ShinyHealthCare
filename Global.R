#Bibliotecas
library(DT)
library(shiny)
library(openxlsx)
library(ggplot2)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(knitr)
library(elliptic)
library(rmarkdown)
library(stringr)
library(ggvis)
library(rvest)
library(dplyr)
library(writexl)


#Opçõa de codificação
options(encoding = 'UTF-8')
options(warn=-1)
rm(list = ls())

#Variaveis globais que recebem tabelas
# e guardam os dados dos arquivos .xlsx
healthInfoTable <<- NULL
month_table <<- NULL
heart_pa_table <<- NULL
sleep_data_statistics <<- NULL
heart_plot <<- NULL


#Estilos e peronalizações
table_opt <<- list(
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#2c3e50', 'color': '#fff'});",
    "}"
  )
)
primary_color <<- '#2c3e50'
secondary_color <<- '#ecf0f1'
stylePanel <<- "background-color: #ecf0f1; padding:15px;border-radius:5px;font-size: 18px"
smallStylePanel <<- "background-color: #ecf0f1; padding:6px;border-radius:5px;"

