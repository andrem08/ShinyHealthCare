howToUse <- function (){
  navbarMenu(
    title = ('Manual and aditional informations'),
    icon = icon('question'),
    tabPanel(
      title = ('How to use'),
      fluidRow(
         column(12,
                #Summary
                h2(strong('Manual on how to use the application.')),
                wellPanel(
                  h4(strong('Index:')),
                  h4('1. Nutritional Info'),
                  h4('2. Calculate your BMI'),
                  h4('3. Sleep data'),
                  h4('4. Heartbeat Frequency'),
                  h4('5. More Information'),

                  #Body Information
                  br(), br(), br(),
                  h2(strong('1. Nutritional Info: ')),
                  h3(strong('  1.1 Vitamins: ')),
                  p('You can select the type of the vitamin, or choose all the vitamins, and it will
                     display a table that contains information about each one. In the end, you can download
                     the table to a pdf file.', style = stylePanel), br(),

                  h3(strong('  1.2 Nutritional info: ')),
                  p('You can select the type of the type of food, and it will
                     display a table that contains information, about several food derivatives
                     of that type. In the end, you can download
                     the table in a pdf file.', style = stylePanel), br(),

                  h2(strong('2. Calculate your BMI: ')),
                  p('To calculate the BMI ( body mass index ), you\'ll need your height and your weight.
                     You can choose between the available metric systems. Then it will display the row
                     you\'re currently in and a description. In the end, you can download
                     the results and the table in a pdf file.', style = stylePanel), br(),

                  h2(strong('3. Sleep data:')),
                  h3(strong('  3.1 Your sleeping schedule:')),
                  p('To check your sleep schedule, you first need a .xslx table. Click on the
                     \"Create sleep spreadsheet\" button. After that, you can add or remove how much you\'ve
                     slept over the days. Then save your .xslx file on your computer for when you need to use it again.
                     If you already have a sleep schedule, click on the \"Insert your .xlsx file\" button and then
                     select your sleep schedule spreadsheet.',br(),
                    'In the end, you can download the results and the table in a pdf file.',
                    style = stylePanel), br(),

                  h3(strong(' 3.2 Sleep hours according to WHO')),
                  p('Here you can see a sleep table with the ideal hours of sleep per age. You can also filter
                     which age interval you want to display or display all of them. In the end, you can download the
                     results and the table in a pdf file.',
                    style = stylePanel
                  ),br(),

                  h2(strong('4 Heartbeat Frequency')),
                  h3(strong('4.1 Heart rates')),
                  p('In this panel you can enter your age, your gender and, after measuring your heart rate, enter
                     on the numeric input. In case you don\'t know how to measure your heart frequency, there\'s a panel
                     that contains how you can do it.',br(),
                    'Then press calculate to see a table with ages per classification, and in the Summary,
                     you can see your category.', br(), 'In the end, you can
                     download the results and the table in a pdf file.',
                    style = stylePanel
                  ), br(),

                  h3(strong('4.2 Heartbeat plot')),
                  p('Here you can see a plot of the average of the contractions per minute for men
                   and women. You can insert an exel table with 6 values containing your bpm
                   from every age. You need to write a column with your name and 6 values. In
                   case you don\'t have data for any of those intervals, just digit 0 in its place.',
                  br(),'In the end, you can download the results and the table in a pdf file.',
                    style = stylePanel),br(),

                  h2(strong('5 More Information')),
                  h3(strong('5.1 Extra Health Information')),
                  p('Here you can select any of the topics of the table and it will give you an short
                   description. You can also click on, read more to open the html link',
                  style = stylePanel),br(),
                  h3(strong('5.2 Search on reddit')),
                  p('Here you can search on the reddit for additional information. You can search per user,
                   or a keyword, and at the end you have an advanced search.', style = stylePanel),br(),
                h3(strong('5.3 Search statistics')),
                p('Here you can search a keyword to show it\'s frequency, comparing to the date, and the conty
                that it was used.', style = stylePanel)
                ),br()
         )
      )
    ),
    tabPanel(
      title = ('References'),
      fluidRow(
        column(
          p(strong(h2('References: '))),
          wellPanel(
            p('1.1 Vitamins informations: ',a('https://jumk.de/bmi/vitamin-table.php',
                                             href = 'https://jumk.de/bmi/vitamin-table.php', target="_blank"), br(),
            '1.2 Nutritional table informations: ', a('https://en.wikipedia.org/wiki/Table_of_food_nutrients',
                                                     href = 'https://en.wikipedia.org/wiki/Table_of_food_nutrients', target="_blank"),br(),
            '2.1 About BMI: ', a('https://en.wikipedia.org/wiki/Body_mass_index',
                                 href = 'https://en.wikipedia.org/wiki/Body_mass_index', target="_blank"),br(),
            '2.2 BMI table: ', a('https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/english_bmi_calculator/bmi_calculator.html',
                                 href = 'https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/english_bmi_calculator/bmi_calculator.html', target="_blank"),br(),
            '3.1 Sleep table: ', a('https://www.cdc.gov/sleep/about_sleep/how_much_sleep.html',
                                  href = 'https://www.cdc.gov/sleep/about_sleep/how_much_sleep.html', target="_blank"),br(),
            '3.2 Importance of sleep: ', a('https://www.sleepfoundation.org/how-sleep-works/why-do-we-need-sleep',
                                           href = 'https://www.sleepfoundation.org/how-sleep-works/why-do-we-need-sleep', target="_blank"),br(),
            '3.3 How much sleep do we really need: ', a('https://www.sleepfoundation.org/how-sleep-works/how-much-sleep-do-we-really-need',
                                                        href = 'https://www.sleepfoundation.org/how-sleep-works/how-much-sleep-do-we-really-need', target="_blank"), br(),
            '4. Heart rate table: ', a('https://www.topendsports.com/testing/heart-rate-resting-chart.htm',
                                      href = 'https://www.topendsports.com/testing/heart-rate-resting-chart.htm', target="_blank"),br(),
            '5.1 More Health information: ', a('https://www.euro.who.int/en/health-topics',
                                      href = 'https://www.euro.who.int/en/health-topics', target="_blank"),br(),
            '5.2 Reddit: ', a('https://www.reddit.com/',
                                      href = 'https://www.reddit.com/', target="_blank"),br(),
          style = stylePanel),
          ),
          width = 11
        )
      )
    )
  )
}