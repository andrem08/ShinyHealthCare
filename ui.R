

source('Interface/welcomePage.R')
source('Interface/HowToUse.R')
source('Interface/QuiSqInterface.R')
source('Interface/FisherInterface.R')

shinyUI(fluidPage(
  theme = shinytheme('flatly'),
  setBackgroundColor(
  ),
  titlePanel(h2('Testes de Significância')),

  navbarPage(h5('MyShinyStatistics'),
             #Apresentação do aplicativo, as estatísticas e sobre o autor
             welcome_page(),
             #Explicação de como utilizar o aplicativo
             howToUse(),
             #Inputs e Outputs para os testes de Qui-Quadrado
             quiSqInterface(),
             #Inputs e Outputs para os testes exatos de Fisher / Fisher-Freeman-Halton
             fisherInterface()
          )
))