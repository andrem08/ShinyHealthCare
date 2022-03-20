welcome_page <<- function (){
  tabPanel(title = 'Home', icon = icon('home'),tags$style(
      "li a {
        font-size: 18px;
        font-weight: 100;
      }
    "
    ),
  headerPanel(p(strong('Shiny Health Care,'), style = 'font-size: xxx-large; font-family: "Baskerville Old Face"')),
  fluidRow(
    column(width = 9,
           br(),br()
    ),
    column( width = 12,
        imageOutput("myImage")
    ),
    column(
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
      p(strong(h4('Welcome,')), br()),
      p(strong('The Shiny Health Care application was created to help you take care of your health more easily,
        organize everything you need to know about your daily health, easy to understand, and everything in one app.',
        br(), br(),
        'The application measure important statistics about your daily life, including some statistics,
        infographics, your sleep schedule and more. The more informed you are about your health, the more empowered
        you are to take action. Everything for free.')
        ,style= stylePanel),
      width = 12,
    )
  ))
}