source('RFunctions/NutritionalInfo.R')
source('RFunctions/BmiFunction.R')
# source('RFunctions/SleepScheduleFunction.R')
source('RFunctions/Heartbeat.R')
source('RFunctions/MoreHealthInfo.R')
source('RFunctions/GetReddit.R')

source('RFunctions/GetGTrands.R')

server <- function (input, output, session){

  #Remove os warnings
  options(warn=-1)

  #Primeiro Painel
  output$myImage <- renderImage({
    list(src = 'RFunctions/www/Shiny_Health_Care_Image.png', width = '600px', height = '600px', style="display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)

  #Segundo Painel
  observeEvent(input$select_vit, observeSelectizeVit(input, output, session))
    #Inicia a tabela nutricional
  user_nutritional_food <- reactive({
    nutritional_DF [[which(nutritional_DF[[1]] == input$select_nutritional_food) + 1]]
  })
  output$nutritional_food_table <- renderDataTable(user_nutritional_food())

  #Terceiro Painel
  bmiTableStart(input, output, session)
  observeEvent(input$BMI_button, bmiFunction(input, output, session))

  #Quarto Painel
  observeEvent(input$sleep_button_file, observeSleepButton(input, output, session))
  observeEvent(input$sleep_button_add, addHour(input, output, session))
  observeEvent(input$sleep_button_remove, removeHour(input, output, session))
  observeEvent(input$create_sleep_file, newSleepFile(input, output, session))
  observeEvent(input$sleep_file, correctSleepFile(input, output))

  #Quarto Painel, segunda parte
  observeEvent(input$sleep_select_age_who, observeSelectSleepWho(input, output, session))

  #Quinto Painel
  observeEvent(input$heart_pa_file, correctHeartPAFile(input, output))
  observeEvent(input$heart_rate_numeric_button, calculateHeartFrequency(input, output, session))


  observeEvent(input$heart_pa_button_file, heartPACalculation(input, output, session))
  displayVisPlot()

  #Sexto Painel
  #Tabela do arq More Health Info
  output$health_info_table <- DT::renderDataTable(
    healthInfoTable[1],
    selection = 'single'
  )
  observeEvent(input$health_info_table_rows_selected,mountData(input, output, session))
  output$health_info_description <- renderUI(h4(strong('Click on one of those topics on the left table to know more about it')))

  #Sexto Painel parte dois
  #Search using a username

  observeEvent(input$user_name_reddit_button, getRedditUser(input, output, session))
  #Observe the created table

  #Search using a keyword
  observeEvent(input$find_subreddits_button, findSubreddits(input, output, session))

  #Advanced search
  observeEvent(input$advanced_search_button, advancedSearch(input, output, session))

  #Descrição antes de calcular qualquer das funções do Reddit
  output$reddit_description <- renderUI(
     h4(strong('Search for any important information and the results will be
     displayed here:  ')))

  #Sexto painel parte tres
  observeEvent(input$input_text_search_button, gtrands_plots(input, output, session))
  output$output_gtrands <- renderUI(wellPanel(h4(strong('Enter the keyword on the right panel to show the statistics:'))))

  #Download statistics
  downloadFilesPdf <- function (path){
    return (
      content = function(file) {
        src <- normalizePath(path)
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, path, overwrite = TRUE)

        out <- render(path, switch(
          'PDF',
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
    }

    )
  }

  #Download the files
  output$download_vit <- downloadHandler(
    filename = function() { paste('vitamins_table', sep = '.', 'pdf')},
    downloadFilesPdf('vitamins_statistics.Rmd'))

  output$download_nut <- downloadHandler(
    filename = function() { paste('nutritional_table', sep = '.', 'pdf')},
    downloadFilesPdf('nutritional_table_statistics.Rmd'))

  output$sleep_download_pdf <- downloadHandler(
    filename = function() { paste('sleep_schedule', sep = '.', 'pdf')},
    downloadFilesPdf('your_sleep_schedule.rmd'))

  output$download_sleep_who <- downloadHandler(
    filename = function() { paste('sleep_schedule_who', sep = '.', 'pdf')},
    downloadFilesPdf('sleep_schedule.rmd'))

  output$download_heart_beat <- downloadHandler(
    filename = function() { paste('heart_rate', sep = '.', 'pdf')},
    downloadFilesPdf('heart_rate.rmd'))

  output$heart_pa_download_pdf <- downloadHandler(
    filename = "heart_rate_plot.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "heartbeat_plot.rmd")
      file.copy("heartbeat_plot.rmd", tempReport, overwrite = TRUE)

      params <- list(n = input$slider)
      out <- render(tempReport, output_file = file, params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )

  output$download_bmi <- downloadHandler(
    filename = function() { paste('bmi_statistics', sep = '.', 'pdf')},
    downloadFilesPdf('bmi_statistics.rmd'))

  #Download sleep table in excel
  output$sleep_download_xlsx <- downloadHandler(
    filename = function() { "sleep_table.xlsx"},
    content = function(file) {write_xlsx(month_table, path = file)}
  )

  #Quit button
  # observe({
  #   if (input$navbar == "stop")
  #       stopApp()
  # })
}

#END