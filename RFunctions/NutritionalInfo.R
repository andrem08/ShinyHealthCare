#Essa função vai pegar as tabelas do site jumk.de
# e vai retornar uma data frame mais organizada
getVit <- function (){
  linkVitaminas <- 'https://jumk.de/bmi/vitamin-table.php'
  linkVitaminas <- url(linkVitaminas, 'rb')
  page <- read_html(linkVitaminas)

  vitaminas <- page %>% html_nodes(".vit")  %>% html_text()
  close(linkVitaminas)

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

#Essa função vai pegar as tabelas do site da wikipedia
# e retornar a tabela mais agradável e compreensivel
# ao usuario
getFoodNutrients <- function (){
  link <- 'https://en.wikipedia.org/wiki/Table_of_food_nutrients'
  link <- url(link, 'rb')

  page <- read_html(link)
  nutritional_table <- page %>% html_nodes('table') %>% html_table()

  close(link)

  tableNames <- list()
  for (i in seq(length(nutritional_table))){
    #Pega uma das tabelas da tabela nutricional
    table <- data.frame(nutritional_table[[i]])

    #A primeira linha da tabela se torna o título das colunas.
    tableNames[i] <- colnames(table)[1]
    colnames(table) <- table[1,]
    table <- table[-1, ]

    #Este loop vai remover as linhas em branco.
    j <- 1
    while (j <= nrow(table)){
      if(table[j,][2] == '' || is.na(table[j,][2])){
        table$Food[j + 1] <- paste(table$Food[j], table$Food[j + 1], sep = ' ')
        table <- table[-j, ]
        j <- j - 1
      }
      j <- j + 1
    }
    #Atualiza o título das linhas.
    rownames(table) <- seq(nrow(table))

    #Este loop vai transformar e formatar todas as strings de números
    # em doubles
    for (j in seq(3, 9)){
      table[[j]] <- as.numeric(gsub(",", "", table[[j]]))
    }
    nutritional_table[[i]] <- table
  }
  tableNames <- gsub('\\.', ' ', tableNames)
+   return(c(list(tableNames), nutritional_table))
}

#guarda as tabelas
vit_DF <- getVit()
nutritional_DF <- getFoodNutrients()

observeSelectizeVit <- function (input, output, session){
  if(input$select_vit == 'All'){
    output$table_vit <- renderDataTable(
      DT::datatable(vit_DF,
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15, 20, 50)
        ))
    )
  }
  else{
    output$table_vit <- renderDataTable(
      DT::datatable(
        vit_DF [which( vit_DF$Name == input$select_vit ), ],
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15, 20, 50)
        )
      )
    )
  }
}

#Download Buttons
downloadVitamins <- function (input, output, session){
  output$download_vit <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', 'pdf')
    },

    content = function(file) {
      src <- normalizePath('reportVita.Rmd')

      #Constroi um arquivo temporario caso nao tenha permissao para modificação
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)

      out <- render('report.Rmd', pdf_document())
      file.rename(out, file)
    }
  )
}

vitaminsInterface <- function (){
  navbarMenu(
    title = 'Nutritional Info',
    icon = icon('pills'),
  tabPanel(
    ('Vitamins'),
    fluidRow(
      column(11,
             p(h3(strong('Vitamins'))),
             p('Vitamins, together with minerals, are considered essential nutrients—because acting in concert, they perform hundreds
              of roles in the body. They help shore up bones, heal wounds, and bolster your immune system. They also convert food
              into energy and repair cellular damage.', style = stylePanel), br(),
             p('Here you can find a detailed table of the most important vitamins. The table shows the name of the vitamin,
             where you can find it, your effectiveness in the human body, the problems with the lack of it, overdosing and others.
             There\'s also the daily dose needed for an average adult.'
               , style = stylePanel)
      )
    ),
    fluidRow(
      column(3,
             wellPanel(
                 h3(strong('Filter: ')),
                 selectizeInput('select_vit', h5('Select a vitamin: '),
                                    choices = c('All', vit_DF[['Name']])),
                                    multiple = TRUE,
             ),
             wellPanel(
               h4(strong('Downloads: ')),
               downloadButton('download_vit', 'Download the table:', icon = icon('download'))
             )
      ),
      column(9,
             shinycssloaders::withSpinner(dataTableOutput('table_vit'), type = 6, color = '#0000ff')
      )
    )
  ),
    tabPanel(
      title = 'Nutritional Table',
      fluidRow(
        column(11,
             p(h3(strong('Nutritional Table'))),
               p('The tables below include tabular lists for selected basic foods, compiled from United States Dept.
               of Agriculture (USDA) sources. Included for each food is its weight in grams, its calories, and
               (also in grams,) the amount of protein, carbohydrates, dietary fiber, fat, and saturated fat.',br(),'
               As foods vary by brands and stores, the figures should only be considered estimates, with more exact
                figures often included on product labels. ', br(),br(),'For precise details about vitamins and mineral contents, the USDA
               source can be used.',
                 style = stylePanel)
        ),
        column(3,
               wellPanel(
               h3(strong('Filter:')),
               selectInput('select_nutritional_food', 'Select the type of your food:',
                           choices = nutritional_DF[[1]]),
               h3(strong('Download the table in pdf: ')),
               ),
               wellPanel(
                 h4(strong('Downloads: ')),
                 downloadButton('download_nut', 'Download the table:')
               ),
        ),
        column(9,
               shinycssloaders::withSpinner(dataTableOutput('nutritional_food_table'), type = 6, color = '#0000ff')
        )
      )
    )
  )
}