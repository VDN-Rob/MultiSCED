radioStan <- function(input, output, session, checkStan){
  
  output$check = reactive({checkStan()})
  
  # outputOptions(output, "check", suspendWhenHidden = FALSE)
  
  radio.reuseStanData <- reactive({input[[ "radio.reuseStanData" ]] })
  
  # output$reuseStanData = renderUI({
  #   ns = session$ns
  #   conditionalPanel(
  #     condition = 'input.check',
  #     radioButtons(ns('radio.reuseStanData'),
  #                  'Results',
  #                  choices = c('Use standardized data','Use unstandardized data'))
  #   )
  # })
  
  output$reuseStanData = renderUI({
    ns = session$ns
    radioButtons(ns('radio.reuseStanData'),
                 'Results',
                 choices = c('Use standardized data','Use unstandardized data'))
  })
  
  return(radio.reuseStanData)
}