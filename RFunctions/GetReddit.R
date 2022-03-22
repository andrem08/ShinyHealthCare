#Biblioteca API que facilita a manipulação no R
library(RedditExtractoR)

#Interface do Quinto painel, parte dois.
#Interface do Reddit
dt_reddit <- NULL

getRedditUser <- function (input, output, session) {

    usr <- input$user_name_reddit
    username <- tolower(gsub(' ', '', input$user_name_reddit))

    tryCatch({
      dt_reddit <<- RedditExtractoR::get_user_content(username)[[1]]

      output$reddit_description <- renderUI(tagList(
        wellPanel(
        tabsetPanel(type = 'tabs', id = 'tabset_sleep_reddit',
          tabPanel('Description', uiOutput('description_reddit_usr')),
          tabPanel('Comments',DT::dataTableOutput('comments_reddit')),
          tabPanel('Threads',DT::dataTableOutput('threads_reddit'))
        )
      )))
      output$description_reddit_usr <- renderUI(tagList(
         h3(strong('Informations: ')),

         lapply(seq(length(dt_reddit$about)), function(i) {
          uiOutput(paste0('reddit_description', i))
        }),
        h4('Frequency of the comments: ', nrow(dt_reddit$comments)),
        h4('Frequency of the thread: ', nrow(dt_reddit$threads))
      ))
      lapply(seq(length(dt_reddit$about)), function(i) {
        output[[paste0('reddit_description', i)]] <- renderUI({
          h4(colnames(as.data.frame(dt_reddit$about))[i], ': ', dt_reddit$about[i])
        })
      })
      comments_dt <- data.frame(
        title = dt_reddit$comments$thread_title,
        subreddit = dt_reddit$comments$subreddit,
        comment = dt_reddit$comments$comment,
        date = dt_reddit$comments$date_utc
      )
      output$comments_reddit <- DT::renderDataTable(
        comments_dt,
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15, 20, 50),
          selection = 'single'
        )
      )
      threads_dt <- data.frame(
        title = dt_reddit$threads$title,
        subreddit = dt_reddit$threads$subreddit,
        date = dt_reddit$threads$date_utc
      )
      output$threads_reddit <- DT::renderDataTable(
        threads_dt,
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15, 20, 50),
          selection = 'single'
        )
      )
    }, error = function (e) {
      output$reddit_description <- renderUI(
        tagList(
          h3(strong('Error:')),
          h4('No results found for the username: ',usr)
        )
      )
    }
    )
  }

findSubreddits <- function (input, output, session) {

    subreddits <- input$find_subreddits
    tryCatch({
      dt_reddit <<- RedditExtractoR::find_subreddits(subreddits)

      output$reddit_description <- renderUI(tagList(
        wellPanel(
          h3('Resuls for the keywors: ',input$find_subreddits_button),
          DT::dataTableOutput('subreddits_table'),
          br(),h3('Frequency of the search: ',nrow(dt_reddit))
        )
      ))
      subreddits_dt <- data.frame(
        title = dt_reddit$title,
        subreddit = dt_reddit$subreddit,
        comment = dt_reddit$description,
        comment = dt_reddit$subscribers,
        date = dt_reddit$date_utc
      )
      output$subreddits_table <- DT::renderDataTable(
        subreddits_dt,
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15, 20, 50),
          selection = 'single'
        )
      )
    }, error = function (e) {
      output$reddit_description <- renderUI(
        tagList(
          h3(strong('Error:')),
          h4('No results for the keywords: ', subreddits)
        )
      )
    }
    )
  }

advancedSearch <- function (input, output, session) {

    keyword <- if( input$search_keyword_adv != 'None') input$search_keyword_adv else NA
    subreddits <- if( input$search_subreddit_adv != 'None') input$search_subreddit_adv else NA
    if ((is.na(keyword) & is.na(subreddits))){
      output$reddit_description <- renderUI(
        tagList(
           h3(strong('Error:')),
           h4('Error in one or more inputs.')
        )
      )
      return()
    }

    sort <- input$sort_adv
    period <- input$period_adv

    tryCatch({
      dt_reddit <<- RedditExtractoR::find_thread_urls(keywords = keyword, sort_by = sort, subreddit = subreddits, period = period)

      output$reddit_description <- renderUI(tagList(
        wellPanel(
          h3('Resuls for the inputs: ',input$find_subreddits_button),
          DT::dataTableOutput('advanced_table'),
          br(),h4('Frequency of the search: ',nrow(dt_reddit))
        )
      ))
      advanced_dt <- data.frame(
        title = dt_reddit$title,
        subreddit = dt_reddit$subreddit,
        nComments = dt_reddit$comments,
        text = dt_reddit$text,
        date = dt_reddit$date_utc
      )
      output$advanced_table <- DT::renderDataTable(
        advanced_dt,
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15, 20, 50),
          selection = 'single'
        )
      )
    }, error = function (e) {
      output$reddit_description <- renderUI(
        tagList(
          h3(strong('Error:')),
          h4('Error in one or more inputs.')
        )
      )
    }
    )
  }

getRedditInterface <- function (){
  tabPanel(
    title = 'Search on reddit for more info',
          fluidRow(
            column(12,
                   h3(strong('Search aditional informations on the reddit page:'))
            ),
        column(3,
               #Filters
               wellPanel(
                  h3(strong('Filter: ')),
                  textInput('user_name_reddit', 'Search for a username:'),
                  actionButton('user_name_reddit_button', 'Search:'),
                  textInput('find_subreddits', 'Search using a keyword:'),
                  actionButton('find_subreddits_button', 'Search:')
               ),
               #Painel avançado
               wellPanel(
                 h3(strong('Advanced search:')),
                 h4('One of these must be written:'),
                 textInput('search_keyword_adv', 'Search for a keyword(s) (opt)', 'None'),
                 textInput('search_subreddit_adv', 'Search for a subreddit (opt)', 'None'),
                 selectInput('sort_adv', 'Select the sorting', choices = c('hot', 'new', 'top'), selected = 'hot'),
                 selectInput('period_adv', 'Select the search period', choices = c('hour', 'day', 'month', 'year', 'all'), selected = 'day'),
                 actionButton('advanced_search_button', 'Search:')
               )
        ),
        column(9,
               #Descrição dos resultados
               #Outputs
               wellPanel(
                 uiOutput('reddit_description')
               ),
               uiOutput('redit_extra_description')
        )
  )

  )
}