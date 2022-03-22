#Biblioteca API que facilita a manipulação no R
library(RedditExtractoR)

#Interface do Quinto painel, parte dois.
#Interface do Reddit
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
                 h3('Advanced search:'),
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