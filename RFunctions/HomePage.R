welcome_page <<- function (){
  tabPanel(title = 'Home', icon = icon('home'),tags$style(
      "li a {
        font-size: 18px;
        font-weight: 100;
      }
    "
    ),
  headerPanel(
    div(h1(strong('Shiny Health Care: The shiny app for your health'), style = 'font-size: 50px; text-align: center; font-family: "Helvetica"'))),
  fluidRow(
    column(width = 9,
           br(),
    ),
    column( width = 12,
        withSpinner(imageOutput("myImage"), type = 6, color = '#0000ff')
    ),
    column(
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
      p(strong(h3('Welcome,')), br(), ),
      p('The Shiny Health Care application was created to help you take care of your health more easily,
        organize everything you need to know about your daily health, easy to understand, and everything in one app.',
        br(), br(),
        'The application measures important statistics about your daily life, including some statistics,
        infographics, your sleep schedule and more. The more informed you are about your health, the more empowered
         you are to take action.', br(), br(),
      style = stylePanel),
      h3(strong('Summary: ')),
        p('In the first panel, you can see the most important vitamins, and it\'s benefits and the diseases of your lack.
        You can also see a nutritional table that contains several types of food, each type with different foods, calories,
         proteins and other information.', br(), br(),
        'In the second panel, you can measure your bmi (body mass index) with a detailed description of your situation', br(),br(),
        'In the first part of the third panel, you can check your sleep chart, verify that it is consistent and appropriate
         for your age. You can also add and remove values from your table or create a new one if you don\'t have one.', br(),
        'In the second part of the third panel, you can see a table from the World Health Organization that shows the ideal
        time you need to sleep, according to your age.',br(),br(),
        'In the fourth panel you can check your blood pressure according to your age', br(),br(),
        'In the fifth panel you can check more information. In the first part of the panel you can see the description of certain
        health subjects, and in the second panel you can search on reddit about',
        'If you have any difficulty using any of the panels, see the manual on the last panel.'
        ,style= stylePanel ),
      width = 12,
    )
  ))
}