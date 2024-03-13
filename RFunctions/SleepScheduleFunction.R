# library(shinycssloaders)
# getSleepTable <- function (){
#   link <- 'https://www.cdc.gov/sleep/about_sleep/how_much_sleep.html'
#   link <- url(link, 'rb')
#   page <- read_html(link)
#
#   ages <- page %>% html_nodes('td:nth-child(2)') %>% html_text()
#   hours <- page %>% html_nodes('td~ td+ td') %>% html_text()
#   close(link)
#
#   sleepTable <- data.frame(Ages = ages, Hours = hours)
#
#   #Remove o último caracter da string de horas (é um redirecionamento do site)
#   for(i in seq(length(sleepTable$Hours))){
#     sleepTable$Hours[[i]] <- substring(sleepTable$Hours[[i]], 1, nchar(sleepTable$Hours[[i]])-1)
#   }
#
#   return(sleepTable)
# }
# sleep_table <- getSleepTable()
#
# observeSelectSleepWho <- function (input, output, session){
#   if(input$sleep_select_age_who == 'All'){
#     output$sleep_table_who <- renderDataTable(
#       DT::datatable(sleep_table)
#     )
#   }else{
#     output$sleep_table_who <- renderDataTable(
#       DT::datatable(
#         sleep_table [which( sleep_table$Ages == input$sleep_select_age_who ), ]
#       )
#     )
#   }
# }
#
# correctSleepFile <- function(input, output){
#   if(is.null(input$sleep_file) || nrow(read.xlsx(input$sleep_file$datapath)) <= 0){
#     output$user_sleep_summary <- renderUI({
#       p(h4('Error on the file!'))
#     })
#     month_table <<- NULL
#     return(FALSE)
#
#   }else{
#     month_table <<- data.frame(read.xlsx(input$sleep_file$datapath))
#   }
#   if(typeof(month_table[[1]]) != 'double'){
#     output$user_sleep_summary <- renderUI({
#       p(h4('Error on the file!'))
#     })
#
#     month_table <<- NULL
#     return(FALSE)
#   }
#   else{
#     month_table <<- data.frame(Hours = month_table[[1]], Days = seq(nrow(month_table)))
#     return(TRUE)
#   }
#
# }
#
# observeSleepButton <- function (input, output, session){
#
#   if(!is.data.frame(month_table)) {
#     if(!correctSleepFile(input, output))
#       return()
#   }
#
#   selected_age <- which(sleep_table$Ages == input$sleep_select_age)
#   ideal_hours <- switch(
#     selected_age,
#     c(14, 17),
#     c(12, 16),
#     c(11, 14),
#     c(10, 13),
#     c(9, 12),
#     c(8, 10),
#     c(7, 10),
#     c(7, 9),
#     c(7, 8),
#   )
#   betw <- ideal_hours[2] - ideal_hours[1]
#
#   output$user_sleep_dt <- DT::renderDataTable(month_table)
#   if(nrow(month_table) > 0){
#     user_sleep_plot <- (
#       ggplot()+
#       geom_rect(aes(ymin = ideal_hours[1] + 2*betw, ymax = 24, xmax = Inf, xmin = -Inf, fill = '1. Too much sleep'), alpha = .2) +
#       geom_rect(aes(ymin = ideal_hours[1] + betw, ymax = ideal_hours[1] + 2*betw, xmax = Inf, xmin = -Inf, fill = '2. Oversleep'), alpha = .2) +
#       geom_rect(aes(ymin = ideal_hours[1], ymax = ideal_hours[1] + betw, xmax = Inf, xmin = -Inf, fill = '3. Perfect sleep range'), alpha = .2) +
#       geom_rect(aes(ymin = ideal_hours[1] - betw, ymax = ideal_hours[1], xmax = Inf, xmin = -Inf, fill = '4. Low sleep time'), alpha = .2) +
#       geom_rect(aes(ymin = -Inf, ymax = ideal_hours[1] - betw, xmax = Inf, xmin = -Inf, fill = '5. Lack of sleep'), alpha = .2) +
#       geom_line(data = subset(month_table), aes(x = Days, y = Hours, group = 1), size = 2)+
#       geom_point(data = subset(month_table), aes(x = Days, y = Hours, group = 1), size = 4) +
#       scale_fill_brewer(palette = 'Set1', name = 'Sleep classification: ') +
#       labs('Sleep graph') +
#       theme_bw()
#     )
#     output$user_sleep_graph <- renderUI(shinycssloaders::withSpinner(plotOutput('user_sleep_graph_complete'), type = 6, color = '#0000ff'))
#
#     output$user_sleep_graph_complete <- renderPlot(user_sleep_plot)
#     mean <- as.numeric(format(round(mean(month_table$Hours), 3), nsmall = 3))
#     var <- as.numeric(format(round(var(month_table$Hours), 3), nsmall = 3))
#     standartDev <- as.numeric(format(round(sqrt(var(month_table$Hours)), 3), nsmall = 3))
#     varCoef <- as.numeric(format(round(standartDev/mean, 3), nsmall = 3))
#
#     output$user_sleep_statistics <- renderUI(
#       tagList(
#         wellPanel(
#           uiOutput('sleep_statistics_1'),
#           uiOutput('sleep_statistics_2'),
#           uiOutput('sleep_statistics_3')
#         )
#       )
#     )
#     output$sleep_statistics_1 <- renderUI(p(h4('The Mean of your sleep schedule is:', mean)))
#     output$sleep_statistics_2 <- renderUI(p(h4('The Standard Deviation of your sleep schedule is:', standartDev)))
#     output$sleep_statistics_3 <- renderUI(p(h4('The Variance Coefficient of your sleep schedule is:', varCoef, 'or', varCoef*100, '%')))
#
#     output$user_sleep_summary <- renderUI(
#       tagList(
#         wellPanel(
#           uiOutput('sleep_summary_1'),
#           uiOutput('sleep_summary_2')
#         )
#       )
#     )
#     output$sleep_summary_1 <- renderUI(p(h3(case_when(
#       nrow(month_table) < 4 ~ 'Sleep consistency: ( recommended with 4 or more data )',
#       nrow(month_table) >= 4 ~ 'Sleep consistency: ',
#
#     ), br())))
#     output$sleep_summary_2 <- renderUI(p(h4(case_when(
#
#             - 0.15 <= varCoef & varCoef <= 0.15 ~ 'You have been sleeping regularly.',
#             - 0.3 <= varCoef & varCoef <= 0.3 ~ 'You\re sleep schedule has been inconsistent.',
#             - 0.3 < varCoef | varCoef < 0.3 ~ 'You\re sleep schedule has been very inconsistent!'
#           ))))
#     sleep_data_statistics <<- list(c(mean, standartDev, varCoef), month_table, user_sleep_plot)
#   }
#   else {
#     output$user_sleep_graph <- renderUI(shinycssloaders::withSpinner(plotOutput('user_sleep_graph_complete'), type = 6, color = '#0000ff'))
#     output$user_sleep_graph_complete <- renderPlot(ggplot())
#     output$user_sleep_statistics <- renderUI(p(h4('There\'s no elements on the table.')))
#     output$user_sleep_summary <- renderUI(p(h4('There\'s no elements on the table.')))
#   }
# }
#
# newSleepFile <- function (input, output, session){
#   month_table <<- data.frame()
#   observeSleepButton(input, output, session)
# }
#
# addHour <- function (input, output, session){
#   if(!is.data.frame(month_table)) {
#     if(!correctSleepFile(input, output))
#       return()
#   }
#   if(nrow(month_table) == 0){
#     newHour <- input$sleep_numeric_add
#     month_table <<- data.frame(c(Hours = list(newHour), Days = seq(1)))
#     observeSleepButton(input, output, session)
#     return()
#   }
#   newHour <- input$sleep_numeric_add
#   month_table[nrow(month_table) + 1,] <- c(Hours = newHour, Days = nrow(month_table) + 1)
#   month_table <<- month_table
#   observeSleepButton(input, output, session)
# }
#
# removeHour <- function (input, output, session){
#   if(!is.data.frame(month_table)) {
#     if(!correctSleepFile(input, output))
#       return()
#   }
#
#   month_table <<- month_table[-nrow(month_table),]
#   observeSleepButton(input, output, session)
# }
#
# sleepFunction <- function (){
#   navbarMenu(
#     title = ('Sleep data'),
#     icon = (icon('moon')),
#     tabPanel(
#       ('Your sleeping schedule'),
#       fluidRow(
#         column(
#
#           h3(strong('The importance of sleeping well ')),
#           p('Sleep is an essential function that allows your body and mind to recharge, leaving you refreshed and
#           alert when you wake up. Healthy sleep also helps the body remain healthy and stave off diseases.
#           Without enough sleep, the brain cannot function properly. This can impair your abilities to concentrate,
#           think clearly, and process memories.',
#           style = stylePanel),
#
#           h3(strong('Sleep Statistics ')),
#           p('Here you can insert your sleeping schedule if you have one, or, in case you don\'t have one,
#           click on the button create sleep spreadsheet. You can see if your sleep schedule is suitable for your age.', br(), br(),
#           'You can also see some statistics and the summary will tell if your sleep schedule is it regular or not.'
#             , style = stylePanel),
#
#           width = 9
#         ),
#            column(3,
#
#                   wellPanel(
#                     p('In case you don\'t have a exel spreadsheet: '),
#                     actionButton('create_sleep_file', 'Create sleep spreadsheet: ')
#                   , style = stylePanel)
#
#            )),
#       fluidRow(
#         column(4,
#
#                wellPanel(
#                  h3(strong('Filter:')),
#                  selectInput('sleep_select_age', 'Select your age:',
#                              choices = sleep_table[['Ages']],
#                              selected = sleep_table[['Ages']][7]),
#                  fileInput('sleep_file', 'Insert your .xlsx file: '),
#                  actionButton('sleep_button_file', 'Calculate: '),
#                ),
#
#                column(12, wellPanel(
#                  h4(strong('Add and remove days of your .xlsx file:')),
#
#                  column(6,
#                         p(strong('Add the sleep time of the new day.:')),
#                         column(5, actionButton('sleep_button_add', 'Add: ')),
#                         column(7, numericInput('sleep_numeric_add', NULL, value = 8, min = 0)),
#                  ),
#
#                column(6,
#                       p(strong('Remove last hour of your .xlsx file.:')),
#                       actionButton('sleep_button_remove', 'Remove: '), p(br()),
#                ), style = smallStylePanel
#
#                )),
#
#                wellPanel(
#                  h4(strong('Donwloads: ')),
#                  downloadButton('sleep_download_xlsx', 'Download the update .xlsx file.'),p(br()),
#                  downloadButton('sleep_download_pdf', 'Download all the tables and the statistics:')
#                )
#
#         ),
#         column(8,
#                tabsetPanel(type = 'tabs', id = 'tabset_sleep_statistics',
#                            tabPanel('Summary:',
#                                     (uiOutput('user_sleep_summary'))),
#                            tabPanel('Statistics:', uiOutput('user_sleep_statistics')),
#                            tabPanel('Your sleep table:', DTOutput('user_sleep_dt')),
#                            tabPanel('Your sleep graph:',
#                                     uiOutput('user_sleep_graph')),
#
#                )
#         )
#       ),
#       fluidRow(
#         column(11,
#                h3(strong('Improve Your Sleep Today: Make Sleep a Priority!')),
#                p('Start by making sleep a priority in your schedule. This means budgeting for the hours
#                you need so that work or social activities don’t trade off with sleep. While cutting sleep
#                short may be tempting in the moment, it doesn’t pay off because sleep is essential to being
#                at your best both mentally and physically.',br(),br(),
#                'Improving your sleep hygiene, which includes your bedroom setting and sleep-related habits,
#                is an established way to get better rest. Examples of sleep hygiene improvements include:',br(),br(),
#                '- Sticking to the same sleep schedule every day, even on weekends.',br(),
#                '- Practicing a relaxing pre-bed routine to make it easier to fall asleep quickly.',br(),
#                '- Choosing the best mattress that is supportive and comfortable, and outfitting it with the best
#                pillows and bedding.',br(),
#                '- Minimizing potential disruptions from light and sound while optimizing your bedroom temperature and aroma.',br(),
#                '- Disconnecting from electronic devices like mobile phones and laptops for a half-hour or more before bed.',br(),
#                '- Carefully monitoring your intake of caffeine and alcohol and trying to avoid consuming them in the hours before bed.',
#
#                style = stylePanel)
#         )
#       )
#     ),
#     tabPanel(
#       ('Sleep hours according to WHO'),
#          fluidRow(
#            column(
#              h3(strong('Recommended Sleep Times By Age Group')),
#              p('Scientific research makes clear that sleep is essential at any age. Sleep powers the mind, restores
#              the body, and fortifies virtually every system in the body. But how much sleep do we really need in
#              order to get these benefits?',br(),
#              'Recommended sleep times are broken down into nine age groups. In each group, the guidelines
#              present a recommended range of nightly sleep duration for healthy individuals. In some cases,
#              sleeping an hour more or less than the general range may be acceptable based on a person’s circumstances.',
#                style = stylePanel),
#              h3(strong('Sleeping table ')),
#              p('Here you can see if you\'re sleeping well, according to WHO ( World Health Organization ).
#              The ideal time can change based on your age:'
#                , style = stylePanel),
#              width = 9
#            )
#          ),
#              fluidRow(
#                column(3,
#                       wellPanel(
#                          h3(strong('Filter:')),
#                          selectInput('sleep_select_age_who', 'Select your age:',
#                                      choices = c('All', sleep_table[['Ages']]))
#                       ),
#                       wellPanel(
#                          h4(strong('Downloads: ')),
#                          downloadButton('download_sleep_who', 'Download the table:')
#                       ),
#                   ),
#                column(9,
#                       h3(strong('Sleep table according to World Health Organization: ')),
#                       p('Recommended sleep times are broken down into nine age groups.
#                       In each group, the guidelines present a recommended range of nightly sleep
#                       duration for healthy individuals. In some cases, sleeping an hour more or
#                       less than the general range may be acceptable based on a person’s circumstances.'),
#                       withSpinner(dataTableOutput('sleep_table_who'), type = 6, color = '#0000ff')
#                )
#              )
#     )
#   )
# }