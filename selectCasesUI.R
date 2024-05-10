selectCasesUI <- function(id){
  ns <- NS(id)
  # column(12,
  #        fluidRow(uiOutput(ns('selectCases'))),
  #        fluidRow(actionButton(ns('toggleCases'), label = "(De)select all"))
  #        )
  wellPanel(
    uiOutput(ns('selectCases')),
    actionButton(ns('toggleCases'), label = "(De)select all")
  )
}