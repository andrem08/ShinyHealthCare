getSleepTable <- function (){
  link <- 'https://www.cdc.gov/sleep/about_sleep/how_much_sleep.html'
  page <- read_html(link)

  ages <- page %>% html_nodes('td:nth-child(2)') %>% html_text()
  hours <- page %>% html_nodes('td~ td+ td') %>% html_text()

  sleepTable <- data.frame(ages = ages, hours = hours)
  return(sleepTable)
}

sleepFunction <- function (){
  navbarMenu(
    title = ('Sleep data'),
    icon = (icon('moon')),
    tabPanel(h5('Sleep hours according to WHO')
       ,
         fluidRow(
           column(
             p('Here you can see if you\'re sleeping well, according to WHO ( World Health Organization ).
             The ideal time can change based on your age'
               , style = stylePanel),
             width = 9
           )),
             fluidRow(
               column(3,
                      wellPanel(
                         h3('Filter'),
                         selectInput('selectAge', 'Select your age', choices = getSleepTable()[['ages']])
                      )
                  )
             )
    ),
    tabPanel(
      h5('Your sleeping schedule'),
      fluidRow(
        column(
          p('Here you can insert your sleeping schedule according to the image below, and then you can
          see the data of your sleeping schedule and recomendations'
            , style = stylePanel),
          width = 9
        )),
      fluidRow(
        column(3,
                  wellPanel(
                    h3('Filter'),
                    selectInput('selectTypeFile', 'Select the type of your file', choices = c('csv', 'xlsx')),
                    fileInput('fileInput', 'Insert the file with your sleep schedule')
                  )
           )
      )
    )
  )
}