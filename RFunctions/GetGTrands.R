
#Biblioteca para acessar o google trends
library(gtrendsR)

#Imprime os graficos recebidos pelo google trends
gtrands_plots <- function (input, output, session)
{
  text <- input$input_text_search
  if (text == '')
    return()

  dt_gtrends <- gtrends(text, gprop = 'web')

  output$output_gtrands <- renderUI(
    wellPanel(
      tabsetPanel(
        type = 'tabs', id = 'tab_set_gtrand',
        tabPanel('Frequency per data',
          h3(strong('Plot with the frequency per data:')),
          plotOutput('plot_gtrends_iot')
        ),
        tabPanel('Frequency per contry',
                 h3(strong('Plot with the frequency per contry:')),
                 plotOutput('plot_gtrends_fpc')
        )
      )
    )
  )
  iot <- dt_gtrends$interest_over_time
  #remove a ultima porque geralmente nao tem data
  iot <- iot[-nrow(iot),]

  output$plot_gtrends_iot <- renderPlot(
    ggplot(data = iot, aes(date, hits)) +
      geom_line(, color = 'steelblue') +
      theme_minimal()

  )
  fpc <- dt_gtrends$interest_by_country
  fpc <- fpc[which(!is.na(fpc$hits) & fpc$hits != '' & fpc$hits != '<1'),]
  fpc$hits <- as.numeric(fpc$hits)
  fpc <- fpc[order(fpc$hits, decreasing = TRUE),]
  fpc <- head(fpc, 8)

  output$plot_gtrends_fpc <- renderPlot(
    ggplot(data = fpc, aes(location, hits)) +
      geom_bar(stat="identity", fill="steelblue") +
      theme_minimal()
  )

}

#Interface do Search statistics
get_gtrands <- function (){
  tabPanel(
    title = 'Search statistics',
    fluidRow(
        column(3,
               wellPanel(
                 h3(strong('Search extra info:')),
                 textInput('input_text_search', 'Enter the desire word:'),
                 actionButton('input_text_search_button', 'Calculate'),
               ),
        ),
        column(9,
             uiOutput('output_gtrands')
        )
    )
  )
}