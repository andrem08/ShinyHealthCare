
quiSqInterface <- function (){
  tabPanel(
    h5('Teste Qui-Quadrado de Pearson'),
    fluidRow(
      column(3,
             #Inputs para selecionar o arquivo, definir os graus de liberdade e
             # carregar os gráficos
             wellPanel(
                h3('Filtro'),
                fileInput('file', 'Escolha um xlsx file', accept = '.xlsx', buttonLabel = "Escolha!", placeholder = "Nenhum arquivo escolhido"),
                sliderInput('sliderCol', h5('Número de Colunas'),
                          2 , 20, 5, 1),
                sliderInput('sliderLin', h5('Número de Linhas'),
                          2 , 20, 5, 1),
                actionButton('button', 'Carregar Dados', icon("sync"),
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
             ),
      ),
      #Dois gráficos para as duas primeiras linhas
      column(9,
             mainPanel(
               tabsetPanel(type = 'tabs', id = 'plotPanel',
                           tabPanel('Tabela de Contingência', DTOutput('tabCont')),
                           tabPanel('Nova Tabela de Contingência', DTOutput('newTabCont')),
                           #Outputs para as mensagens contendo as estatísticas do teste Qui-Quadrado
                           tabPanel('Estatísticas', verbatimTextOutput('textEstat')),

                           tabPanel(title = uiOutput('firstPlot', ), plotOutput('plotOut',  width = "100%")),
                           tabPanel(title = uiOutput('secondPlot'), plotOutput('plotOut2',  width = "100%")),

               )
             )
      )
    )
  )
}