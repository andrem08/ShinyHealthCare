library(stringr)
library(ggvis)
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
    25 < input$heart_rate_age & input$heart_rate_age <= 35 ~ 2,
    35 < input$heart_rate_age & input$heart_rate_age <= 45 ~ 3,
    45 < input$heart_rate_age & input$heart_rate_age <= 55 ~ 4,
    55 < input$heart_rate_age & input$heart_rate_age <= 65 ~ 5,
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
        withSpinner(DTOutput('heart_table_1'), type = 6, color = '#0000ff')
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

getHeartPATable <- function (){
  male <- do.call(paste, c(heart_tables[[3]][5,], sep = "-")) %>%
  str_replace_all('-', ' ') %>%
  str_remove('\\+') %>%
  str_split(' ')

  male <- as.numeric(male[[1]])
  aux <- vector(mode = 'double')
  for (i in seq(floor(length(male)/2))){
    aux[i] <- ((male[1 + 2*(i - 1)] + male[2 + 2*(i - 1)])/2)
  }
  male <- aux

  female <- do.call(paste, c(heart_tables[[4]][5,], sep = "-")) %>%
  str_replace_all('-', ' ') %>%
  str_remove('\\+') %>%
  str_split(' ')

  female <- as.numeric(female[[1]])
  aux <- vector(mode = 'double')
  for (i in seq(floor(length(female)/2))){
    aux[i] <- ((female[1 + 2*(i - 1)] + female[2 + 2*(i - 1)])/2)
  }
  female <- aux

  heart_pa_table <- data.frame(male = male, female = female)
  rownames(heart_pa_table) <- rownames(heart_tables[[3]])
  heart_pa_table <<- heart_pa_table
}

correctHeartPAFile <- function(input, output){
  if(is.null(input$heart_pa_file) || is.null(read.xlsx(input$heart_pa_file$datapath)) || nrow(read.xlsx(input$heart_pa_file$datapath)) != 6){
    output$user_heart_pa_summary <- renderUI({
      p(h4('Error on the file!'))
    })
    heart_pa_table <<- NULL
    return(FALSE)

  }else{
    heart_pa_table_user <- data.frame(read.xlsx(input$heart_pa_file$datapath))
  }
  if(typeof(heart_pa_table_user[[1]]) != 'double'){
    output$user_heart_pa_summary <- renderUI({
      p(h4('Error on the file!'))
    })

    heart_pa_table <<- NULL
    return(FALSE)
  }
  else{
    getHeartPATable()
    heart_pa_table$user <- heart_pa_table_user[[1]]
    heart_pa_table <<- heart_pa_table
    return(TRUE)
  }
}

displayVisPlot <- function (){
  getHeartPATable()

  tooltipGraph2 <- function (x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)

    if (nrow(female[female$id == x$id, ]) > 0){
      rate <- female[female$id == x$id, ]
      paste0('<b>', 'Woman from ages: ',rate$years, '</b><br>',
             'Heart rate: ', rate$rate, "<br>"
      )
    }
    else if(nrow(male[male$id == x$id, ]) > 0){
      rate <- male[male$id == x$id, ]
      paste0('<b>', 'Men from ages: ',rate$years, '</b><br>',
             'Heart rate: ', rate$rate, "<br>"
      )
    }
  }

  n <- nrow(heart_pa_table)

  male <- data.frame(years = rownames(heart_pa_table), rate = heart_pa_table$male, id = seq(n))
  female <- data.frame(years = rownames(heart_pa_table), rate = heart_pa_table$female, id = seq(n+1,2*n))

  size <- 50
  size_hover <- 200
  ocupacity <- 0.5
  ocupacity_hover <- 0.8

  heart_plot <<- vis2 <- ggvis(data = NULL, x = ~years, y = ~rate) %>%
    layer_points(fill := "red", data = male, size := size, size.hover := size_hover,
                 fillOpacity := ocupacity, fillOpacity.hover := ocupacity_hover, key := ~id) %>%
    layer_paths(stroke := "red", data = male) %>%
    layer_points(fill := "blue", data = female, size := size, size.hover := size_hover,
                 fillOpacity := ocupacity, fillOpacity.hover := ocupacity_hover, key := ~id) %>%
    add_tooltip(tooltipGraph2, "hover") %>%
    layer_paths(stroke := "blue", data = female) %>%
    add_axis('x', title = 'Interval of years') %>%
    add_axis('y', title = 'Heart rate') %>%
    scale_nominal("stroke", c('Man', 'woman'), range = c('red', 'blue'))

  bind_shiny(vis2, 'user_heart_pa_graph')
}
heartPACalculation <- function (input, output, session){

  if(!is.data.frame(heart_pa_table)) {
    if(!correctHeartPAFile(input, output))
      return()
  }

  if(is.null(heart_pa_table$user)){
    return()
  }

  tooltipGraph <- function (x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)

    if (nrow(female[female$id == x$id, ]) > 0){
      rate <- female[female$id == x$id, ]
      paste0('<b>', 'Woman from ages: ',rate$years, '</b><br>',
             'Heart rate: ', rate$rate, "<br>"
      )
    }
    else if(nrow(male[male$id == x$id, ]) > 0){
      rate <- male[male$id == x$id, ]
      paste0('<b>', 'Men from ages: ',rate$years, '</b><br>',
             'Heart rate: ', rate$rate, "<br>"
      )
    }
    else if(nrow(user[user$id == x$id, ]) > 0){
      rate <- user[user$id == x$id, ]
      paste0('<b>', 'User from ages: ',rate$years, '</b><br>',
             'Heart rate: ', rate$rate, "<br>"
      )
    }
  }


  n <- nrow(heart_pa_table)

  male <- data.frame(years = rownames(heart_pa_table), rate = heart_pa_table$male, id = seq(n))
  female <- data.frame(years = rownames(heart_pa_table), rate = heart_pa_table$female, id = seq(n+1,2*n))
  user <- data.frame(years = rownames(heart_pa_table), rate = heart_pa_table$user, id = seq(2*n+1,3*n))

  size <- 50
  size_hover <- 200
  ocupacity <- 0.5
  ocupacity_hover <- 0.8


  heart_plot <<- vis <- ggvis(data = NULL, x = ~years, y = ~rate) %>%
    layer_points(fill := "red", data = male, size := size, size.hover := size_hover,
                 fillOpacity := ocupacity, fillOpacity.hover := ocupacity_hover, key := ~id) %>%
    layer_paths(stroke := "red", data = male) %>%
    layer_points(fill := "blue", data = female, size := size, size.hover := size_hover,
                 fillOpacity := ocupacity, fillOpacity.hover := ocupacity_hover, key := ~id) %>%
    layer_paths(stroke := "blue", data = female) %>%
    layer_points(fill := "green", data = user, size := size, size.hover := size_hover,
                 fillOpacity := ocupacity, fillOpacity.hover := ocupacity_hover, key := ~id) %>%
    add_tooltip(tooltipGraph, "hover") %>%
    layer_paths(stroke := "green", data = user)%>%
    layer_paths(stroke := "blue", data = female) %>%
    add_axis('x', title = 'Interval of years') %>%
    add_axis('y', title = 'Heart rate') %>%
    scale_nominal("stroke", c('Man', 'Woman', 'User'), range = c('red', 'blue', 'green'))

  bind_shiny(vis, 'user_heart_pa_graph')
  
}

heartbeatInterface <- function (){
  navbarMenu(
    title = 'Heart rates',
    tabPanel(title = 'Heartbeat frequency',
         fluidRow(
           column(
             h3(strong('Heartbeat Frequency')),
             p('Heart rate is the speed of the heartbeat measured by the number of contractions (beats)
             of the heart per minute (bpm). The heart rate can vary according to the body\'s physical needs,
             including the need to absorb oxygen and excrete carbon dioxide, but is also modulated by numerous
              factors, including, but not limited to, genetics, physical fitness, stress or psychological status,
             diet, drugs, hormonal status, environment, and disease/illness as well as the interaction between
             and among these factors.', br(), br(),
             'Here you can monitor the heart rate. Enter your heart rate or a spreadsheet of the results
             to see if your pressure is normal. If you have any problems, consult the manual.'
               , style = stylePanel), br(),
             p(strong(h3('How can you know your heart rate?'))),
             br(),

             p('At the wrist, lightly press the index and middle fingers of one hand on the opposite wrist,
             just below the base of the thumb.',br(), 'At the neck, lightly press the side of the neck,
             just below your jawbone', br(), 'Count the number of beats in 15 seconds, and multiply by four.
              That\'s your heart rate.',br()
               , style = stylePanel),
             width = 11
           )),
             fluidRow(
               column(3,
                      wellPanel(
                         h3(strong('Filter:')),
                         numericInput('heart_rate_age', 'Enter your age:', value = 25, min = 0),
                         selectInput('heart_rate_gender', 'Choose your gender:', choices = c('Female', 'Male')),
                         numericInput('heart_rate_numeric', 'Enter your heart rate per minute result: ', value = 70, min = 0),
                         actionButton('heart_rate_numeric_button', 'Calculate'),
                      ),
                      wellPanel(
                         h4(strong('Downloads: ')),
                         downloadButton('download_heart_beat', 'Download the table:')
                      ),
                  ),
               column(9,
                      tabsetPanel(type = 'tabs', id = 'user_heart_pa_summary',
                                  tabPanel('Resting Heart table:', uiOutput('heart_table_graph')),
                                  tabPanel('Summary:', uiOutput('heart_summary')),
                      )
               )
             )
    ),
    tabPanel(
      title ='Heartbeat plot',
      fluidRow(
        column(
          h3(strong('Heartbeat plot ')),

          p('Heart rate is the speed of the heartbeat measured by the number of contractions (beats)
             of the heart per minute (bpm). The heart rate can vary according to the body\'s physical needs,
             including the need to absorb oxygen and excrete carbon dioxide, but is also modulated by numerous
             factors, including, but not limited to, genetics, physical fitness, stress or psychological status,
             diet, drugs, hormonal status, environment, and disease/illness as well as the interaction between
             and among these factors.', br(), br(),
             'You can insert your usual heart rate for each of the age intervals anc compare with the average american
             heart rata:'
               , style = stylePanel), br(),
             p(strong(h3('How can you know your heart rate?'))),
             br(),

             p('At the wrist, lightly press the index and middle fingers of one hand on the opposite wrist,
             just below the base of the thumb.',br(), 'At the neck, lightly press the side of the neck,
             just below your jawbone', br(), 'Count the number of beats in 15 seconds, and multiply by four.
              That\'s your heart rate.',br()
               , style = stylePanel),
          width = 11
        ),
      ),
      fluidRow(
        column(4,
               wellPanel(
                 h3(strong('Filter:')),
                 fileInput('heart_pa_file', 'Insert your .xlsx file: '),
                 actionButton('heart_pa_button_file', 'Calculate: '),
               ),
               wellPanel(
                 h4(strong('Donwloads: ')),
                 downloadButton('heart_pa_download_pdf', 'Download the interactive plot in html:')
               )
        ),
        column(7,
               wellPanel(
                 ggvisOutput('user_heart_pa_graph')
               )
        )
      )
    )
  )
}