howToUse <- function (){
  navbarMenu(
    title = ('Manual and aditional informations'),
    icon = icon('question'),
    tabPanel(
      title = h5('How to use'),
      fluidRow(
         column(
           p(strong(h3('Manual on how to use the application.')),
             br(),br(),
             strong(h3('Index:')),
             br(),'Vitamins',
             br(),'Calculate your BMI',
             br(),'Sleep data'
             , style = stylePanel),
           width = 12
         )
      )
    ),
    tabPanel(
      title = h5('Informations'),
      fluidRow(
        column(
            p(strong(h3('Here are some extra informations about the application, its uses and informations about WHO: '))
            , style = stylePanel),
            width = 12
        )
      )
    ),
    tabPanel(
      title = h5('References'),
      fluidRow(
        column(
          p(strong(h3('Here are the references and where all the data is gathered:'))
          , style = stylePanel),
          width = 12
        )
      )
    )
  )
}