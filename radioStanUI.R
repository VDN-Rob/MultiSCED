radioStanUI <- function(id){
  ns <- NS(id)
  wellPanel(uiOutput(ns('reuseStanData')))
}