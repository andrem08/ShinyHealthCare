howToUse <- function (){
  tabPanel(
     h5('Como utilizar'),
       fluidRow(
         column(
           p('Inicialmente você deve selecionar um arquivo xlsx, definir um tamaho para a sua tabela', style = stylePanel),
           p(strong('Teste do Qui-Quadrado: '), br(), 'Para o teste Qui-Quadrado', style = stylePanel),
           p(strong('Teste de Fisher: '), br(), 'Para o teste de Fisher', style = stylePanel),
           p(strong('Teste T: '), br(), 'Para o teste T', style = stylePanel),

           width = 9
         )
       )
  )
}