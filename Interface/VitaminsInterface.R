library(rvest)
library(dplyr)

getVit <- function (){
    linkVitaminas <- 'https://jumk.de/bmi/vitamin-table.php'
    page <- read_html(linkVitaminas)

    vitaminas <- page %>% html_nodes(".vit")  %>% html_text()

    vitTable <- data.frame(matrix(ncol = 8, nrow = 17))
    index <- 1
    for (i in seq(17)){
        for (j in seq(8)){
          vitTable[[j]][i] <- vitaminas[index]
          index <- index + 1
        }
    }
    colnames(vitTable) <- vitaminas[1:8]
    vitTable <- vitTable [-c(1, 5, 9, 13), ]
    rownames(vitTable) <- seq(13)
  return(vitTable)
}

vitDF <- getVit()

observeSelectizeVit <- function (input, output, session){
  output$tableVit <- renderDataTable(vitDF)
}
vitaminsInterface <- function (){
  tabPanel(
    ('Vitamins'),
    icon = icon('pills'),
    fluidRow(
      column(12,
             p(h3(strong('Vitamins'))),
             p('Vitamins, together with minerals are considered essential nutrients—because acting in concert, they perform hundreds
             of roles in the body. They help shore up bones, heal wounds, and bolster your immune system. They also convert food
             into energy, and repair cellular damage.', style = stylePanel), br(),
             p('\nHere you can find a detailed table of the most important vitamins. The table shows the name of the vitamin,
             where you can find it, your effectiveness in the human body, the problems with the lack of it, overdosing and others.
             There\'s also the daily dose needed for an avarage adult.'
               , style = stylePanel)
      )
    ),
    fluidRow(
      column(3,
             wellPanel(
                 h3(strong('Filter: ')),
                 selectizeInput('selectVit', h5('Escolha uma vitamina'),
                                    choices = vitDF[['Name']]),
                                    multiple = TRUE,
             )
      ),
      column(9,
             dataTableOutput('tableVit')
      )
    )
  )

}