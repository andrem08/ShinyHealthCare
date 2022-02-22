
#Ao selecionar um dos files, esta função carrega os dados
# na variavel global dadosFisher, atualiza os sliders input e
# atualiza os nomes dos paineis de gráfico
observeSelectFisher <- function (input, output, session){

    #Arquivos que o usuario escolhe
    #Adicionar os arquivos aqui!!
    file <- switch(input$selectFileFis, 'Câncer' = 'Dados/CancerMortalidade.xlsx',
                    'Diabetes' = 'Dados/Diabetes.xlsx',
                    'Empregos' = 'Dados/Empregos.xlsx',
                    'Avaliação ao Cliente' = 'Dados/AvaliacaoAoCliente.xlsx',
                    'Frequência de Enjoo em Movimento' = 'Dados/FrequenciaEnjoo.xlsx')

    #Carregar o arquivo selecionado, nomear as linhas e
    # remover a primeira coluna, que contem descrições
    dadosFisher <<- read.xlsx(file, 1)
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
    output$newTabContFis <- renderTable(dadosFisher, rownames = TRUE, height = 300, width = 600)

    #Mostra a mesma opção no teste de fisher
    updateSelectInput(session = session, inputId = 'selectFile', selected = input$selectFileFis)
  }

observeButtonFisher <- function (input, output, session){
    dadosApp <- dadosFisher

    #Remover aleatoriamente linhas e colunas conforme os sliders dado pelo usuário
    if(input$sliderColFis < ncol(dadosApp)) dadosApp <- dadosApp[-sample(seq_len(ncol(dadosApp)), ncol(dadosApp) - input$sliderColFis, replace = F)]
    if(input$sliderLinFis < nrow(dadosApp)) dadosApp <- dadosApp[-sample(seq_len(nrow(dadosApp)), nrow(dadosApp) - input$sliderLinFis, replace = F), ]

    #Atualiza com a nova tabela de contingência
    output$newTabContFis <- renderTable(dadosApp, rownames = TRUE, height = 300, width = 600)

    #Calcula o teste de Fisher e imprime o valor de p,
    # o intervalo de confiança e o risco relativo.
    # Caso a tabela seja diferente de 2x2
    # o algritmo fisher.test() aplica o algoritmo fisher-Freeman-Halton
    # e não são mostrados os intervalos de confiança e a estimativa de fisher
    fisher <- fisher.test(dadosApp, conf.int = TRUE, conf.level = input$confLevel,
                          alternative = (switch(input$selectRR, 'Maior do que 1' = 'greater',
                                                'Igual a 1' = 'two.sided', 'Menor do que 1' = 'less') ))
    if (nrow(dadosApp) == ncol(dadosApp) & ncol(dadosApp) == 2)output$textEstatFis <- renderPrint(cat('Valor de p: ', fisher$p.value, '\n',
                              'Intervalo de confianca de: ',as.double(fisher$conf.int)[1],' a ',as.double(fisher$conf.int)[2],  '.\n',
                              'Risco relativo : ',as.double(fisher$estimate) , sep = ''))
    else output$textEstatFis <- renderPrint(cat('Valor de p: ', fisher$p.value, sep = ''))
}
