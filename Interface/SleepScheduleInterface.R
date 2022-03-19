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

correctFile <- function(input, output){
  if(is.null(input$sleep_file) || nrow(read.xlsx(input$sleep_file$datapath)) <= 0){
    output$user_sleep_summary <- renderUI({
      p(h4('Error on the file!'))
    })
    month_table <<- NULL
    return(FALSE)

  }else{
    month_table <<- data.frame(read.xlsx(input$sleep_file$datapath))
  }
  if(typeof(month_table[[1]]) != 'double'){
    output$user_sleep_summary <- renderUI({
      p(h4('Error on the file!'))
    })

    month_table <<- NULL
    return(FALSE)
  }
  else{
    month_table <<- data.frame(Hours = month_table[[1]], Days = seq(nrow(month_table)))
    return(TRUE)
  }

}

observeSleepButton <- function (input, output, session){

  if(!is.data.frame(month_table)) {
    if(!correctFile(input, output))
      return()
  }

  selected_age <- which(sleep_table$Ages == input$sleep_select_age)
  ideal_hours <- switch(
    selected_age,
    c(14, 17),
    c(12, 16),
    c(11, 14),
    c(10, 13),
    c(9, 12),
    c(8, 10),
    c(7, 10),
    c(7, 9),
    c(7, 8),
  )
  betw <- ideal_hours[2] - ideal_hours[1]

  output$user_sleep_dt <- DT::renderDataTable(month_table)
  if(nrow(month_table) > 0){
  output$user_sleep_graph <- renderPlot(

    ggplot()+
      geom_rect(aes(ymin = ideal_hours[1] + 2*betw, ymax = 24, xmax = Inf, xmin = -Inf, fill = '1. Too much sleep'), alpha = .2) +
      geom_rect(aes(ymin = ideal_hours[1] + betw, ymax = ideal_hours[1] + 2*betw, xmax = Inf, xmin = -Inf, fill = '2. Oversleep'), alpha = .2) +
      geom_rect(aes(ymin = ideal_hours[1], ymax = ideal_hours[1] + betw, xmax = Inf, xmin = -Inf, fill = '3. Perfect sleep range'), alpha = .2) +
      geom_rect(aes(ymin = ideal_hours[1] - betw, ymax = ideal_hours[1], xmax = Inf, xmin = -Inf, fill = '4. Low sleep time'), alpha = .2) +
      geom_rect(aes(ymin = -Inf, ymax = ideal_hours[1] - betw, xmax = Inf, xmin = -Inf, fill = '5. Lack of sleep'), alpha = .2) +
      geom_line(data = subset(month_table), aes(x = Days, y = Hours, group = 1), size = 2)+
      geom_point(data = subset(month_table), aes(x = Days, y = Hours, group = 1), size = 4) +
      scale_fill_brewer(palette = 'Set1', name = 'Sleep classification: ')+
    theme_bw()
  )
    output$user_sleep_statistics <- renderUI(
      tagList(
        wellPanel(
          uiOutput('sleep_statistics_1'),
          uiOutput('sleep_statistics_2'),
          uiOutput('sleep_statistics_3')
        )
      )
    )
    output$sleep_statistics_1 <- renderUI({p(h4('The Mean of your sleep schedule is:', format(round(mean(month_table$Hours), 3), nsmall = 3)))})
    output$sleep_statistics_2 <- renderUI({p(h4('The Variance of your sleep schedule is:', format(round(var(month_table$Hours), 3), nsmall = 3)))})
    output$sleep_statistics_3 <- renderUI({p(h4('The Standard Deviation of your sleep schedule is:', format(round(sqrt(var(month_table$Hours)), 3), nsmall = 3)))})

    output$user_sleep_summary <- renderUI({
      tagList(
        wellPanel(
          uiOutput('sleep_summary_1'),
          uiOutput('sleep_summary_2'),
          uiOutput('sleep_summary_3'),
        )
      )
    })

  }
  else {
    output$user_sleep_graph <- renderPlot(ggplot())
    output$user_sleep_statistics <- renderUI(p(h4('There\'s no elements on the table.')))
    output$user_sleep_summary <- renderUI(p(h4('There\'s no elements on the table.')))
  }
}

newFile <- function (input, output, session){
  month_table <<- data.frame()
  observeSleepButton(input, output, session)
}

addHour <- function (input, output, session){
  if(!is.data.frame(month_table)) {
    if(!correctFile(input, output))
      return()
  }
  if(nrow(month_table) == 0){
    newHour <- input$sleep_numeric_add
    month_table <<- data.frame(c(Hours = list(newHour), Days = seq(1)))
    observeSleepButton(input, output, session)
    return()
  }
  newHour <- input$sleep_numeric_add
  month_table[nrow(month_table) + 1,] <- c(Hours = newHour, Days = nrow(month_table) + 1)
  month_table <<- month_table
  observeSleepButton(input, output, session)
}

removeHour <- function (input, output, session){
  if(!is.data.frame(month_table)) {
    if(!correctFile(input, output))
      return()
  }

  month_table <<- month_table[-nrow(month_table),]
  observeSleepButton(input, output, session)
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
        ),
           column(3,
                  wellPanel(
                    p('In case you don\'t have a exel spreadsheet: '),
                    actionButton('create_sleep_file', 'Create sleep spreadsheet: ')
                  )
           )),
      fluidRow(
        column(4,
               wellPanel(
                 h3(strong('Filter:')),
                 selectInput('sleep_select_age', 'Select your age:', choices = sleep_table[['Ages']]),
                 fileInput('sleep_file', 'Insert your .xlsx file: '),
                 actionButton('sleep_button_file', 'Calculate: '),
               ),
               column(12, wellPanel(
                      h4(strong('Add and remove days of your .xlsx file:')),
               column(6,
                   p(strong('Add the sleep time of the new day.:')),
                   column(5, actionButton('sleep_button_add', 'Add: ')),
                   column(7, numericInput('sleep_numeric_add', NULL, value = 7)),
               ),
               column(6,
                   p(strong('Remove last hour of your .xlsx file.:')),
                   actionButton('sleep_button_remove', 'Remove: '), p(br()),
               ), style = smallStylePanel
               )),
               wellPanel(
                 h4(strong('Donwloads: ')),
                 downloadButton('sleep_download_xlsx', 'Download the update .xlsx file.'),p(br()),
                 downloadButton('sleep_download_pdf', 'Download all the tables and the statistics:')
               )
        ),
        column(8,
               tabsetPanel(type = 'tabs', id = 'tabset_sleep_statistics',
                           tabPanel('Summary:', uiOutput('user_sleep_summary')),
                           tabPanel('Statistics:', uiOutput('user_sleep_statistics')),
                           tabPanel('Your sleep table:', DTOutput('user_sleep_dt')),
                           tabPanel('Your sleep graph:', plotOutput('user_sleep_graph')),

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
           )
         ),
             fluidRow(
               column(3,
                      wellPanel(
                         h3(strong('Filter:')),
                         selectInput('sleep_select_age_who', 'Select your age:', choices = c('All', sleep_table[['Ages']])),
                         numericInput('sleep_number', 'Insert your sleep time in hours:', value = 7),
                         actionButton('sleep_button_number', 'Calculate: '),
                      ),
                      wellPanel(
                         h4(strong('Downloads: ')),
                         downloadButton('download_sleep_who', 'Download the table:')
                      ),
                  ),
               column(9,
                      h3(strong('Sleep table according to World Health Organization: ')),
                      dataTableOutput('sleep_table_who')
               )
             )
    )
  )
}