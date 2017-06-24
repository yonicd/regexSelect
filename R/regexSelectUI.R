#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param id id of shiny module used in regexSelect
#' @param label character, label of the selectize object
#' @param choices List of values to select from. If elements of the list are named, then that name rather than 
#' the value is displayed to the user. This can also be a named list whose elements are (either named or unnamed) 
#' lists or vectors. If this is the case, the outermost names will be used as the "optgroup" label for the elements 
#' in the respective sublist. This allows you to group and label similar choices.
#' @param checkbox.selected character,options of the checkbox to set as TRUE, see details, Default: c("enable", "ignore.case")
#' @param checkbox.inline boolean, render the checkbox choices inline (i.e. horizontally), Default: TRUE
#' @details checkbox.selected is used as a proxy for ellipses to pass arguments to a grep(selectize value,selectize choices,value=TRUE,...).
#' This makes the options in checkbox.selected the same as the arguments that pass to grep: ignore.case, perl,fixed and invert. In addition there
#' are two more arguments that the user can set enable which toggles the grep functionality to return it to regular selectize with 
#' options multiple=TRUE and create=TRUE. The other argument is retain, this lets the user control if the search terms are added to the selectize choices
#' or to keep it as originally entered, there by converting the selectize into a search field. 
#' @return A list of HTML elements that can be added to a UI definition.
#' @examples 
#' if(interactive()){
#'ui <- shiny::fluidPage(
#'regexSelectUI(id = "a", label = "Variable:",choices = names(iris)),
#'shiny::tableOutput("data")
#')
#'
#'server <- function(input, output, session) {
#'  curr_cols<-callModule(regexSelect, "a",shiny::reactive(iris))
#'  
#'  observeEvent(curr_cols(),{
#'  cols_now<-curr_cols()
#'  if(length(cols_now)==0)  cols_now<-names(data())
#'  output$data <- shiny::renderTable({iris[,cols_now , drop = FALSE]}, rownames = TRUE)
#'  })
#'}
#'
#'shiny::shinyApp(ui, server)
#'
#'  }
#' @rdname regexSelectUI
#' @export 
#' @import shiny
regexSelectUI <- function(id, label, choices,checkbox.selected=c('enable','ignore.case'),checkbox.inline=TRUE) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::selectizeInput(inputId = ns("variable"),
                          label = label,
                          choices = choices,
                          options=list(multiple=TRUE,create = TRUE)),
    shiny::checkboxGroupInput(inputId = ns('grep'),label = 'regex options',
                              choices = c('Enable'='enable',
                                          'Retain Searches'='retain',
                                          'Ignore Case'='ignore.case',
                                          'Perl'='perl',
                                          'Fixed'='fixed',
                                          'Invert'='invert'),
                              selected = checkbox.selected,
                              inline  = checkbox.inline)
  )
}