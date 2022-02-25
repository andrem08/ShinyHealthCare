
#Ao selecionar um dos files, esta função carrega os dados
# na variavel global dadosFisher, atualiza os sliders input e
# atualiza os nomes dos paineis de gráfico
observeSelectFisher <- function (input, output, session){

    #Os arquivos que o usuario baixar
    # Caso ele nao tenha nenhum, o programa vai enviar um de exemplo
    file <- input$fileF

    if(is.null(file)){
        dados <<- read.xlsx('Dados/FrequenciaEnjoo.xlsx', 1)
        rownames(dados) <<- dados[, 1]
        dados <<- dados[-1]
    }
    else{
        dados <<- read.xlsx(file$datapath, 1)
        rownames(dados) <<- dados[, 1]
        dados <<- dados[-1]
    }

    #Atualizar os comprimentos dos sliders conforme o tamanho da tabela
    updateSliderInput(session = session, inputId = 'sliderColFis' ,max = ncol(dados), value = ncol(dados))
    updateSliderInput(session = session, inputId = 'sliderLinFis' ,max = nrow(dados), value = nrow(dados))

    #Constroi a tabela de contingência
    output$tabContFis <- DT::renderDT(datatable(dados, options = table_opt))
    output$newTabContFis <- DT::renderDT(datatable(dados, options = table_opt))

  }

observeButtonFisher <- function (input, output, session){
    dadosApp <- dados

    #Remover aleatoriamente linhas e colunas conforme os sliders dado pelo usuário
    if(input$sliderColFis < ncol(dadosApp)) dadosApp <- dadosApp[-sample(seq_len(ncol(dadosApp)), ncol(dadosApp) - input$sliderColFis, replace = F)]
    if(input$sliderLinFis < nrow(dadosApp)) dadosApp <- dadosApp[-sample(seq_len(nrow(dadosApp)), nrow(dadosApp) - input$sliderLinFis, replace = F), ]

    #Atualiza com a nova tabela de contingência
    output$newTabContFis <- DT::renderDT(datatable(dadosApp, options = table_opt))

    #Calcula o teste de Fisher e imprime o valor de p,
    # o intervalo de confiança e o risco relativo.
    # Caso a tabela seja diferente de 2x2
    # o algritmo fisher.test() aplica o algoritmo fisher-Freeman-Halton
    # e não são mostrados os intervalos de confiança e a estimativa de fisher
    if(0 >= input$confLevel || input$confLevel >= 1 ){
        output$textEstatFis <- renderPrint(cat('Nivel de confiança para fora do intervalo aberto 0 - 1'))
        return()
    }

    fisher <- fisher.test(dadosApp, conf.int = TRUE, conf.level = input$confLevel,
                          alternative = (switch(input$selectRR, ' > 1' = 'greater',
                                                'Igual à 1' = 'two.sided', ' < 1' = 'less') ))
    if (nrow(dadosApp) == ncol(dadosApp) & ncol(dadosApp) == 2)output$textEstatFis <- renderPrint(cat('Valor de p: ', fisher$p.value, '\n',
                              'Intervalo de confianca de: ',as.double(fisher$conf.int)[1],' a ',as.double(fisher$conf.int)[2],  '.\n',
                              'Risco relativo : ',as.double(fisher$estimate) , sep = ''))
    else output$textEstatFis <- renderPrint(cat('Valor de p: ', fisher$p.value, sep = ''))
}
