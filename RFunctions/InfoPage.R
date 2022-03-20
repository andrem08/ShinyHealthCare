howToUse <- function (){
  navbarMenu(
    title = ('Manual and aditional informations'),
    icon = icon('question'),
    tabPanel(
      title = h5('How to use'),
      fluidRow(
         column(12,
                h2(strong('Manual on how to use the application.')),
                   wellPanel(
                      h3(strong('Index:')),
                      br(),h3('1. Nutritional Info'),
                      br(),h3('2. Calculate your BMI'),
                      br(),h3('3. Sleep data'),
                      br(),h3('4. Heartbeat Frequency'),

                      br(), br(), br(), br(),
                      h2(strong('1. Nutritional Info: ')),
                      h3(strong('  1.1 Vitamins: ')),
                      p('You can select the type of the vitamin, or choose all the vitamins, and it will
                      display a table that contains informations, about each one. In the end you can download
                      the table in a pdf file.', style = stylePanel), br(), br(),

                      h3(strong('  1.2 Nutritional info: ')),
                      p('You can select the type of the type of food, and it will
                      display a table that contains informations, about several foods derivatives
                      from that type. In the end you can download
                      the table in a pdf file.', style = stylePanel), br(), br(),

                      h2(strong('2. Calculate your BMI: ')),
                      p('To calculate teh BMI ( body mass index ), you need your heght and your weight.
                      You can choose between the available systems. In the end you can download
                      the results, and the table in a pdf file.', style = stylePanel), br(), br(),

                      h2(strong('3. Sleep data:')),
                      h3(strong('  3.1 Your sleeping schedule:')),
                      p('', style = stylePanel)
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