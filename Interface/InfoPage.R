howToUse <- function (){
  navbarMenu(
    title = ('Manual and aditional informations'),
    icon = icon('question'),
    tabPanel(
      title = h5('How to use'),
      fluidRow(
         column(12,
                h3(strong('Manual on how to use the application.')),
                   wellPanel(
                      h3(strong('Index:')),
                      br(),h3('Nutritional Info'),
                      br(),h3('Calculate your BMI'),
                      br(),h3('Sleep data'),
                      br(),h3('Heartbeat Frequency')
                )
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