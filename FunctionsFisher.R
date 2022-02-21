observeSelectFisher <- function (input, output, session){

    #Arquivos que o usuario escolhe
    #Adicionar os arquivos aqui!!
    fileFisher <- switch(input$selectFileFis, 'Câncer' = 'Dados/CancerDeMamaMortalidade.xlsx',
                    'Diabetes' = 'Dados/Diabetes.xlsx',
                    'Empregos' = 'Dados/Empregos.xlsx',
                    'Avaliação ao Cliente' = 'Dados/AvaliacaoAoCliente.xlsx',
                    'Frequência de Enjoo em Movimento' = 'Dados/FrequenciaEnjoo.xlsx')

    #Carregar o arquivo selecionado, nomear as linhas e
    # remover a primeira coluna, que contem descrições
    dadosFisher <<- read.xlsx(fileFisher, 1)
    rownames(dadosFisher) <<- dadosFisher[, 1]
    dadosFisher <<- dadosFisher[-1]

    #Ordena linhas e colunas em ordem alfabetica
    dadosFisher <<- dadosFisher[order(rownames(dadosFisher)),]
    dadosFisher <<- dadosFisher[order(colnames(dadosFisher))]

    #Atualizar os comprimentos dos sliders conforme o tamanho da tabela
    updateSliderInput(session = session, inputId = 'sliderColFis' ,max = ncol(dadosFisher ), value = ncol(dadosFisher ))
    updateSliderInput(session = session, inputId = 'sliderLinFis' ,max = nrow(dadosFisher), value = nrow(dadosFisher))

    #Constroi a tabela de contingência
    output$tabContFis <- renderTable(dadosFisher, rownames = TRUE, height = 300, width = 600)

    #Mostra a mesma opção no teste de fisher
    updateSelectInput(session = session, inputId = 'selectFile', selected = input$selectFileFis)
  }

observeButtonFisher <- function (input, output, session){
    dadosFisherApp <- dadosFisher

    #Remover aleatoriamente linhas e colunas conforme os sliders dado pelo usuário
    if(input$sliderColFis < ncol(dadosFisherApp)) dadosFisherApp <- dadosFisherApp[-sample(seq_len(ncol(dadosFisherApp)), ncol(dadosFisherApp) - input$sliderColFis, replace = F)]
    if(input$sliderLinFis < nrow(dadosFisherApp)) dadosFisherApp <- dadosFisherApp[-sample(seq_len(nrow(dadosFisherApp)), nrow(dadosFisherApp) - input$sliderLinFis, replace = F), ]

    #Calcula o teste de Fisher
    #Output imprime o valor de p, o intervalo de confiança
    # e a estimativa
    fisher <- fisher.test(dadosFisherApp, alternative = 'greater', conf.int = TRUE, conf.level = input$confLevel)
    if (nrow(dadosFisherApp) == ncol(dadosFisherApp) & ncol(dadosFisherApp) == 2)output$textEstatFis <- renderPrint(cat('Valor de p: ', fisher$p.value, '\n',
                              'Intervalo de confianca de: ',as.double(fisher$conf.int)[1],' a ',as.double(fisher$conf.int)[2],  '.\n',
                              'Estimativa : ',as.double(fisher$estimate) , sep = ''))
    else output$textEstatFis <- renderPrint(cat('Valor de p: ', fisher$p.value, sep = ''))
   }