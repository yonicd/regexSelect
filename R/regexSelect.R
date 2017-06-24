#' @title Create a selectize list input control with regular expression capabilities
#' @description Create a selectize list that can be used to choose a single or multiple items from a list of values
#' with extension for regular expression.
#' @param input The input slot that will be used to access the value.
#' @param output The output variable to read the list of values returned be regex query
#' @param session The session of the shiny application
#' @param data reactive element contains data 
#' @return reactive character vector
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
#' @rdname regexSelect
#' @export 
#' @import shiny
regexSelect <- function(input, output, session, data) {
  
  if(!inherits(data,'reactive')) data<-shiny::reactive(data)
  
  current_cols<-shiny::eventReactive(input$variable,{
    
    if('enable'%in%input$grep){
      curr_cols<-switch((nchar(input$variable)==0)+1,
                        grep(input$variable,names(data()),
                             value=TRUE,
                             ignore.case = 'ignore.case'%in%input$grep,
                             perl = 'perl'%in%input$grep,
                             fixed='fixed'%in%input$grep,
                             invert='invert'%in%input$grep),
                        NULL)
    }else{
      curr_cols<-switch((input$variable%in%names(data()))+1,names(data()),input$variable)
    }
    
    curr_cols
  })
  
  shiny::observe({
    if(!'retain'%in%input$grep){
      shiny::updateSelectizeInput(session=session,
                           inputId = 'variable',
                           choices = names(data()),
                           selected = input$variable,
                           options = list(multiple=TRUE,create=TRUE))
    }
  })
  
  return(current_cols)
  }