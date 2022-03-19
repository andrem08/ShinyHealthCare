library(stringr)
getHeartRateTable <- function (){
  link <- 'https://www.topendsports.com/testing/heart-rate-resting-chart.htm'
  link <- url(link, open = 'rb')
  page <- read_html(link)

  table <- page %>% html_nodes('table') %>% html_table()
  description <- page %>% html_nodes('h1 + p') %>% html_text()

  close(link)

  table <- data.frame(table)
  titles <- c(table[[1]][1],table[[9]][1])

  table <- table[-1, ]
  rownames(table) <- table[[1]] %>%
  str_replace_all('\n', ' ')

  table <- table[-c(1, 8)]

  male_table <- table [][1:6]
  female_table <- table [][7:12]

  colnames(male_table) <- male_table[1,]
  colnames(female_table) <- female_table[1,]
  male_table <- male_table[-1,]
  female_table <- female_table [-1,]

  male_table <- data.frame(t(male_table))
  female_table <- data.frame(t(female_table))

  tables <- list(titles, description, male_table, female_table)
  return(tables)
}

heart_tables <- getHeartRateTable()

calculateHeartFrequency <- function (input, output, session){
  if(input$heart_rate_gender == 'Male'){
    table <- heart_tables[[3]]
    title <- heart_tables[1][[1]][1]
  }else{
    table <- heart_tables[[4]]
    title <- heart_tables[1][[1]][2]
  }
  if (input$heart_rate_age < 18){
    return()
  }
  colnames(table) <- colnames(table) %>%
    str_replace_all('\\.', ' ')
  index <- case_when(
    input$heart_rate_age <= 25 ~ 1,
    26 <= input$heart_rate_age & input$heart_rate_age <= 35 ~ 2,
    36 <= input$heart_rate_age & input$heart_rate_age <= 45 ~ 3,
    46 <= input$heart_rate_age & input$heart_rate_age <= 55 ~ 4,
    56 <= input$heart_rate_age & input$heart_rate_age <= 65 ~ 5,
    65 < input$heart_rate_age  ~ 6
  )

  freq <- do.call(paste, c(table[index, ], sep = "-")) %>%
  str_replace_all('-', ' ') %>%
  str_remove('\\+') %>%
  str_split(' ')

  freq <- as.numeric(freq[[1]])

  if(input$heart_rate_numeric < freq[1])
    return()

  output$heart_table_graph <- renderUI(
    tagList(
      wellPanel(
      uiOutput('heart_table_title'),
      DTOutput('heart_table_1')
    ))
  )
  output$heart_table_title <- renderUI(p(h3(strong(title, ':'))))
  output$heart_table_1 <- DT::renderDataTable(
    table,
    selection = list(selected = index, target = 'row')
  )
  category <- NULL
  for (i in seq(ncol(table) - 1)){
    if(freq[2 * (i - 1) + 1] <= input$heart_rate_numeric & input$heart_rate_numeric <= freq[2 * (i - 1) + 2]){
      category <- colnames(table)[i]
      break
    }
  }
  if (is.null(category))
    category <- colnames(table)[6]
    output$heart_summary <- renderUI(
      tagList(
        wellPanel(
          uiOutput('heart_statistics_1'),
          uiOutput('heart_statistics_3')
        )
      )
    )
  output$heart_statistics_1 <- renderUI (p(h3('Your category is: ', strong(category))))
  output$heart_statistics_3 <- renderUI (p(br(), h4(heart_tables[[2]])))
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
                         numericInput('heart_rate_numeric', 'Digit the result of your heart frequency per minute', value = 70, min = 0),
                         actionButton('heart_rate_numeric_button', 'Calculate'),
                      ),
                      wellPanel(
                         h4(strong('Downloads: ')),
                         downloadButton('download_heart_beat', 'Download the table:')
                      ),
                  ),
               column(9,
                      tabsetPanel(type = 'tabs', id = 'tabset_sleep_statistics',
                                  tabPanel('Resting Heart table:', uiOutput('heart_table_graph')),
                                  tabPanel('Summary:', uiOutput('heart_summary')),
                      )
               )
             )
    )
}