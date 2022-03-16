source('Interface/HomePage.R')
source('Interface/InfoPage.R')
source('Interface/NutritionalInfo.R')
source('Interface/BmiInterface.R')
source('Interface/SleepScheduleInterface.R')
source('Interface/Heartbeat.R')


ui <<- fluidPage(
  theme = shinytheme('flatly'),
  setBackgroundColor(
  ),
  navbarPage(('MyShinyStatistics'),
             #Apresentação do aplicativo, as estatísticas e sobre o autor
             welcome_page(),
             vitaminsInterface(),
             bmiInterface(),
             sleepFunction(),
             heartbeatInterface(),
             #Informaçoes e ajuda
             howToUse(),
          )
)