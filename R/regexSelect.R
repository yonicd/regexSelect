#' @title Create a selectize list input control with regular expression capabilities
#' @description Create a selectize list that can be used to choose a single or multiple 
#' items from a list of values with extension for regular expression.
#' @param input The input slot that will be used to access the value.
#' @param output The output variable to read the list of values returned be regex query
#' @param session The session of the shiny application
#' @param data reactive element contains a character vector where matches are sought, 
#' or an object which can be coerced by as.character to a character vector
#' @return reactive character vector
#' @examples 
#' if(interactive()){
#'ui <- shiny::fluidPage(
#'regexSelectUI(id = "a", label = "Variable:",choices = names(iris)),
#'shiny::tableOutput("data")
#')
#' 
#' 
#'ui.show <- shiny::fluidPage(
#'regexSelectUI(id = "a", label = "Variable:",choices = names(iris),checkbox.show = TRUE),
#'shiny::tableOutput("data")
#')
#'
#'server <- function(input, output, session) {
#'  curr_cols<-shiny::callModule(regexSelect, "a",shiny::reactive(names(iris)))
#'  
#'  shiny::observeEvent(curr_cols(),{
#'  cols_now<-curr_cols()
#'  if(length(cols_now)==0)  cols_now<-names(data())
#'  output$data <- shiny::renderTable({iris[,cols_now , drop = FALSE]}, rownames = TRUE)
#'  })
#'}
#'
#'#do not show regex checkboxes
#'shiny::shinyApp(ui, server)
#'
#'#show regex checkboxes
#'shiny::shinyApp(ui.show, server)
#'  }
#' @rdname regexSelect
#' @export 
#' @import shiny
#' @importFrom shinyjs hide
regexSelect <- function(input, output, session, data) {

  current_cols<-shiny::eventReactive(input$variable,{
    
    if('enable'%in%input$grep){
      curr_cols<-switch((nchar(input$variable)==0)+1,
                        grep(input$variable,data(),
                             value=TRUE,
                             ignore.case = 'ignore.case'%in%input$grep,
                             perl = 'perl'%in%input$grep,
                             fixed='fixed'%in%input$grep,
                             invert='invert'%in%input$grep),
                        NULL)
    }else{
      curr_cols<-switch((input$variable%in%data())+1,data(),input$variable)
    }
    
    curr_cols
  })
  
  shiny::observe({
    if(!'retain'%in%input$grep){
      shiny::updateSelectizeInput(session=session,
                           inputId = 'variable',
                           choices = data(),
                           selected = input$variable,
                           options = list(multiple=TRUE,create=TRUE))
    }
  })

  return(current_cols)
  }