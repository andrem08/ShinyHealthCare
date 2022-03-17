getSleepTable <- function (){
  link <- 'https://www.cdc.gov/sleep/about_sleep/how_much_sleep.html'
  link <- url(link, 'rb')
  page <- read_html(link)

  ages <- page %>% html_nodes('td:nth-child(2)') %>% html_text()
  hours <- page %>% html_nodes('td~ td+ td') %>% html_text()
  close(link)

  sleepTable <- data.frame(Ages = ages, Hours = hours)

  #Remove o último caracter da string de horas (é um redirecionamento do site)
  for(i in seq(length(sleepTable$Hours))){
    sleepTable$Hours[[i]] <- substring(sleepTable$Hours[[i]], 1, nchar(sleepTable$Hours[[i]])-1)
  }

  return(sleepTable)
}
sleep_table <- getSleepTable()

observeSelectSleepWho <- function (input, output, session){
  if(input$sleep_select_age_who == 'All'){
    output$sleep_table_who <- renderDataTable(
      DT::datatable(sleep_table)
    )
  }else{
    output$sleep_table_who <- renderDataTable(
      DT::datatable(
        sleep_table [which( sleep_table$Ages == input$sleep_select_age_who ), ]
      )
    )
  }
}

sleepFunction <- function (){
  navbarMenu(
    title = ('Sleep data'),
    icon = (icon('moon')),
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
                 selectInput('sleep_select_age', 'Select your age:', choices = sleep_table[['Ages']]),
                 selectInput('sleep_choose_file', 'Select the type of your file: ', choices = c('.csv', '.xlsx')),
                 fileInput('sleep_file', 'Insert your file: '),
                 actionButton('sleep_button_file', 'Calculate: ')
               )
        ),
        column(9,
               tabsetPanel(type = 'tabs', id = 'plotPanel',
                           tabPanel('Your sleep table', DTOutput('user_sleep_dt')),
                           tabPanel('Your sleep graph', DTOutput('user_sleep_graph'))
               )
        )
      )
    ),
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
                         h3(strong('Filter:')),
                         selectInput('sleep_select_age_who', 'Select your age:', choices = c('All', sleep_table[['Ages']])),
                         numericInput('sleep_number', 'Insert your sleep time in hours:', value = 7),
                         actionButton('sleep_button_number', 'Calculate: '),
                      )
                  ),
               column(9,
                      h3(strong('Sleep table according to World Health Organization: ')),
                      dataTableOutput('sleep_table_who')
               )
             )
    )
  )
}