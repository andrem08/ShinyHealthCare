fisherInterface <- function (){
  tabPanel(
     h5('Teste exato de Fisher'),
     fluidRow(
       column(3,
              #Inputs para selecionar o arquivo, definir os graus de liberdade e
              # carregar os gráficos
              wellPanel(
                 h3('Filtro'),
                 fileInput('fileF', 'Escolha um xlsx file', accept = '.xlsx', buttonLabel = "Escolha!", placeholder = "Nenhum arquivo escolhido"),
                 sliderInput('sliderColFis', h5('Número de Colunas'),
                             2 , 20, 5, 1),
                 sliderInput('sliderLinFis', h5('Número de Linhas'),
                             2 , 20, 5, 1),
                 column(5,
                        numericInput('confLevel', h5('Nivel de confiança: '),
                                     value = 0.95, step = 0.05)),
                 column(5,
                        selectInput('selectRR', h5('Hipótese de risco relativo: '),
                                    choices = c(' > 1', 'Igual à 1', ' < 1'))),
                 br(),br(),
                 actionButton('buttonFis', 'Carregar Dados', icon("sync"),
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              )
       ),
       #Dois gráficos para as duas primeiras linhas
       column(9,
              tabsetPanel(type = 'tabs', id = 'plotPanel2',
                          #Outputs para as mensagens contendo as estatísticas do teste Qui-Quadrado
                          tabPanel('Tabela de Contingência', DTOutput('tabContFis')),
                          tabPanel('Nova Tabela de Contingência', DTOutput('newTabContFis')),
                          tabPanel('Estatísticas', verbatimTextOutput('textEstatFis'))
              )
       )
     )
  )
}