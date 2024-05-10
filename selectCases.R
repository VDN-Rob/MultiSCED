selectCases <- function(input, output, session, choices){
  
  output$selectCases = renderUI({
    ns = session$ns
    selectInput(ns('selectCases'),
                'Cases',
                choices = choices(),
                selected = unlist(choices()),
                multiple = T,
                selectize = T)
  })
  
  observeEvent(input$toggleCases,{
    if(input$toggleCases %% 2 == 0){
      updateSelectInput(session,
                        'selectCases',
                        choices = choices(),
                        selected = unlist(choices()))
    }
    else{
      updateSelectInput(session,
                        'selectCases',
                        choices = choices(),
                        selected = NULL)
    }
  })
  
  observeEvent(input$toggleCases,{
    if(input$toggleCases %% 2 == 0){
      output$state = renderPrint({
        unlist(choices())
      })
    }
    else{
      output$state = renderPrint({
        NULL
      })
    }
  })
  
  return(list('selectCases' = reactive({input$selectCases})))
}