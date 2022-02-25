source('FunctionsQuiSq.R')
source('FunctionsFisher.R')

server <- function (input, output, session){

  #Seleção para teste Qui-Quadrado
  #Carregar um dos arquicos selecionados
  observeEvent(input$file, observeSelectQuiSq(input, output, session))

  #Construir estatisticas para o teste Qui-Quadrado
  #Observando o botão
  observeEvent(input$button, observeButtonQuiSq(input, output, session))

  #Seleção para teste de Fisher
  #Carregar um dos arquicos selecionados
  observeEvent(input$fileF, observeSelectFisher(input, output, session))

  # Construir estatisticas para o teste de Fisher
  # Observando o botão
  observeEvent(input$buttonFis, observeButtonFisher(input, output, session))
}
