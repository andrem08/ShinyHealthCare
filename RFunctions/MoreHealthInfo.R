getHealthInformations <- function (){
  link <- 'https://www.euro.who.int/en/health-topics'
  link <- url(link, open = 'rb')
  while (TRUE){
    tryCatch({
    url <- read_html(link)

    names <- url %>%
      html_nodes('#category-view a') %>%
      html_text()

    all_urls <- url %>%
      html_nodes('#category-view a') %>%
      html_attr('href')

    close(link)
    url_table <- data.frame(Names = names, urls = all_urls)
      break
  }, error = function(e){
        message("Test Read Timeout")
        message("Retrying. .")
    }
    )}
  url_table <- url_table[-which(url_table$Names == 'Home'),]
  url_table <- url_table[order(url_table$Names),]
  rownames(url_table) <- seq(nrow(url_table))

  return(url_table)
}
healthInfoTable <<- getHealthInformations()

mountData <- function (input, output, session){
    selected_row <- input$health_info_table_rows_selected

    link <- (paste0(healthInfoTable$urls[selected_row]))
    # while (TRUE){
    link <- url(link, open = 'rb')
    selected_row_url <- read_html(link)

    description <- try(
        selected_row_url %>%
          html_nodes('.intro-box') %>%
          html_text()
    )

    if(length(description) == 0){
      description <- try(
        selected_row_url %>%
          html_nodes('p:nth-child(1) , p:nth-child(3)') %>%
          html_text()
      )
      description <- description[4:5]

      close(link)

      output$health_info_description <- renderUI(
        tagList(
          wellPanel(
            h3(healthInfoTable$Names[selected_row]),
            h4(description[1]),br(),
            h4(description[2]),br(),
            a('Read more:',
              href = paste0(healthInfoTable[[2]][selected_row]),
              target="_blank"
            ))
        )
      )
    }
    else{
      close(link)
      description <- str_split(description, '[\r\n]')
      description <- subset(description[[1]], !(description[[1]] %in% ''))
      description[2] <- gsub('Read More', '', description[2])


      output$health_info_description <- renderUI(
        tagList(
          wellPanel(
          h3(description[1],':'),
          h4(description[2]),br(),
          a('Read more:',
            href = paste0(healthInfoTable[[2]][selected_row]),
            target="_blank"
          )
          )
        )
      )
    }
  }

healthInformationInterface <- function (){
  tabPanel(title = 'Extra Health Information',
    fluidRow(
      column(4,
             wellPanel(
               h2(strong('Search the topics you want:'), style = 'font-family: "Helvetica"'),br(),
               dataTableOutput('health_info_table')
             )
      ),
      column(8,
             wellPanel(
               uiOutput('health_info_description')
             )
      )
    )
  )
}