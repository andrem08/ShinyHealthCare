#Carregando as bibliotecas utilizadas
library(shiny)
library(openxlsx)
library(ggplot2)
source('FunctionsQuiSq.R')
source('FunctionsFisher.R')

options(encoding = 'UTF-8')

function (input, output, session){

  #Seleção para teste Qui-Quadrado
  #Carregar um dos arquicos selecionados
  observeEvent(input$selectFile, observeSelectQuiSq(input, output, session))

  #Construir estatisticas para o teste Qui-Quadrado
  #Observando o botão
  observeEvent(input$button, observeButtonQuiSq(input, output, session))

  #Seleção para teste de Fisher
  #Carregar um dos arquicos selecionados
  observeEvent(input$selectFileFis, observeSelectFisher(input, output, session))

  # Construir estatisticas para o teste de Fisher
  # Observando o botão
  observeEvent(input$buttonFis, observeButtonFisher(input, output, session))
}
