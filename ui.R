library(shinythemes)
library(shinyWidgets)

shinyUI(fluidPage(
  theme = shinytheme('flatly'),
  # setBackgroundColor(
  #   color = c('ghostwhite', 'lightblue'),
  #   gradient = 'linear',
  #   direction = 'top'
  # ),
  # setBackgroundColor(
  #   color = '#ECF0F1'
  # ),
  titlePanel(h2('Testes de Significância')),

  navbarPage(h5('MyShinyStatistics'),
             tabPanel(
                h5(icon('home')),
                fluidRow(
                  column(
                    p(strong('Seja bem Vindo,'), style = 'color: black; font-size: xx-large; font-family: "Old English Text MT"'),
                    br(),
                    p(strong('Testes de significância: '), br(), 'Testes de significância, testes de hipótese ou teste estatístico a é um processo estatístico objetivo
                    que auxilia o pesquisador a utilizá-lo para se tirar uma conclusão, tomar uma decisão do tipo sim ou não sobre uma ou mais populações previamente definidas,
                    a partir de uma ou mais amostras oriundas dessas populações.', br(), br(), 'Em um teste de significância devemos construir duas hipóteses uma hipótese nula H0
                     (há uma associação estatisticamente significativa entre as variáveis) e uma hipótese alternativa Há (não há evidências suficientes para concluir que
                     as variáveis estão associadas) .'

                      ,style="color:black;background-color: #ECF0F1; padding:15px;border-radius:5px;font-size: 15px"),
                    width = 9,
                  )
                )
             ),
             tabPanel(
               h5('Como utilizar')

             ),
             #Inputs e Outputs para os testes de Qui-Quadrado
             tabPanel(
              h5('Teste Qui-Quadrado de Pearson'),
              fluidRow(
                column(3,
                       #Inputs para selecionar o arquivo, definir os graus de liberdade e
                       # carregar os gráficos
                       wellPanel(
                         h3('Filtro'),
                        selectInput('selectFile', h5('Selecione o arquivo: '),
                                    choices = c('Diabetes', 'Mortalidade Covid', 'Câncer',
                                                'Dados', 'Empregos', 'Avaliação ao Cliente', 'Frequência de Enjoo em Movimento')),
                        sliderInput('sliderCol', h5('Número de Colunas'),
                                    2 , 20, 5, 1),
                        sliderInput('sliderLin', h5('Número de Linhas'),
                                    2 , 20, 5, 1),
                        actionButton('button', 'Carregar Dados', icon("sync"),
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       ),
                       #Outputs para as mensagens contendo as estatísticas do teste Qui-Quadrado
                       wellPanel(
                           span(h5('Estatísticas: '),
                                verbatimTextOutput('textEstat')
                           )
                       )
                ),

                #Dois gráficos para as duas primeiras linhas
                column(9,
                       mainPanel(
                         tabsetPanel(type = 'tabs', id = 'plotPanel',
                                     tabPanel('Tabela de Contingência', tableOutput('tabCont')),
                                     tabPanel('Nova Tabela de Contingência', tableOutput('newTabCont')),
                                     tabPanel(title = uiOutput('firstPlot', ), plotOutput('plotOut',  width = "100%")),
                                     tabPanel(title = uiOutput('secondPlot'), plotOutput('plotOut2',  width = "100%"))

                         )
                       )
                )
              )
             ),
             #Inputs e Outputs para os testes exatos de Fisher / Fisher-Freeman-Halton
            tabPanel(
              h5('Teste exato de Fisher'),
                  fluidRow(
                    column(3,
                       #Inputs para selecionar o arquivo, definir os graus de liberdade e
                       # carregar os gráficos
                       wellPanel(
                         h3('Filtro'),
                        selectInput('selectFileFis', h5('Selecione o arquivo: '),
                                    choices = c('Diabetes', 'Câncer',
                                                'Empregos', 'Avaliação ao Cliente', 'Frequência de Enjoo em Movimento')),
                        sliderInput('sliderColFis', h5('Número de Colunas'),
                                    2 , 20, 5, 1),
                        sliderInput('sliderLinFis', h5('Número de Linhas'),
                                    2 , 20, 5, 1),
                         numericInput('confLevel', h5('Nivel de confiança ( Para tabelas de contingência 2x2 )'),
                                      value = 0.95, min = 0.0000001, max = 0.9999999, step = 0.05),
                         selectInput('selectRR', h5('Hipótese de risco relativo: '),
                                    choices = c('Maior do que 1', 'Igual a 1',
                          'Menor do que 1')),
                        actionButton('buttonFis', 'Carregar Dados', icon("sync"),
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                         ),
                      wellPanel(
                           span(h5('Estatísticas: '),
                                verbatimTextOutput('textEstatFis')
                           )
                       )
                    ),
                    #Dois gráficos para as duas primeiras linhas
                    column(9,
                           tabsetPanel(type = 'tabs', id = 'plotPanel2',
                                       #Outputs para as mensagens contendo as estatísticas do teste Qui-Quadrado
                                       tabPanel('Tabela de Contingência', tableOutput('tabContFis')),
                                       tabPanel('Nova Tabela de Contingência', tableOutput('newTabContFis'))
                                       )
                           )
                    )
            )
          )
))