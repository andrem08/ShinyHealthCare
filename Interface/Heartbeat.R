getHeartRateTable <- function (){
  link <- 'https://www.topendsports.com/testing/heart-rate-resting-chart.htm'
  link <- url(link, 'rb')
  page <- read_html(link)

  table <- page %>% html_nodes('table') %>% html_table()
  description <- page %>% html_nodes('h1 + p') %>% html_text()

  close(link)

  table <- data.frame(table)
  title_female <- table[[9]][1]
  title_male <- table[[1]][1]

  table <- table[-1, ]
  rownames(table) <- table[[1]]
  table <- table[-c(1, 8)]

  colnames(table) <- table[1,]

}

heartbeatInterface <- function (){
    tabPanel(title = 'Heartbeat frequency',
         fluidRow(
           column(
             p(strong(h3('Heartbeat Frequency:'))),
             p('Here you can monitor the heart rate. Digit your heart rate or a spreadsheet of the results
             to see if your pressure is normal. If you have any problems, consult the manual.'
               , style = stylePanel), br(),
             p(strong(h3('How can you know your heart rate?'))),
             br(),

             p('At the wrist, lightly press the index and middle fingers of one hand on the opposite wrist,
             just below the base of the thumb.',br(), 'At the neck, lightly press the side of the neck,
             just below your jawbone', br(), 'Count the number of beats in 15 seconds, and multiply by four.
              That\'s your heart rate.',br()
               , style = stylePanel),
             width = 9
           )),
             fluidRow(
               column(3,
                      wellPanel(
                         h3(strong('Filter:')),
                         numericInput('heart_rate_age', 'Digit your age', value = 25, min = 0),
                         selectInput('heart_rate_gender', 'Choose your gender', choices = c('Female', 'Male')),
                         numericInput('heart_rate_numeric', 'Digit the result of your heart frequency per minute', value = 80, min = 0),
                         actionButton('heart_rate_numeric_button', 'Calculate'),
                      ),
                      wellPanel(
                         h4(strong('Downloads: ')),
                         downloadButton('download_heart_beat', 'Download the table:')
                      ),
                  ),
               column(9,

               )
             )
    )
}