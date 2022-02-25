welcome_page <<- function (){

  # ns <- NS(id)

  tabPanel(h5(icon('home')),
  fluidRow(
    column(
      p(strong('Seja bem Vindo,'), style = 'color: black; font-size: xx-large; font-family: "Old English Text MT"'),
      br(),
      p(strong('Testes de significância: '), br(), 'Testes de significância, testes de hipótese ou teste estatístico a é um processo estatístico objetivo
      que auxilia o pesquisador a utilizá-lo para se tirar uma conclusão, tomar uma decisão do tipo sim ou não sobre uma ou mais populações previamente definidas,
      a partir de uma ou mais amostras oriundas dessas populações.', br(), br(), 'Em um teste de significância devemos construir duas hipóteses uma hipótese nula H0
       (há uma associação estatisticamente significativa entre as variáveis) e uma hipótese alternativa Há (não há evidências suficientes para concluir que
       as variáveis estão associadas) .'

        ,style= stylePanel),
      width = 9,
    )
  ))
}