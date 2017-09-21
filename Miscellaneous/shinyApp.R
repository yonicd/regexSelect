library(shiny)
shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    selectizeInput(inputId = "variable",
                   label = "Variable:",
                   choices = unique(diamonds$clarity),
                   options=list(multiple=TRUE,create = TRUE)),
    checkboxGroupInput(inputId = 'grep',label = 'regex options',
                       choices = c('Enable'='enbl',
                                   'Retain Searches'='ret',
                                   'ignore.case'='ignore',
                                   'perl'='perl','fixed'='fixed','invert'='invert'),
                       selected = c('enbl','ignore'),
                       inline  = TRUE),
    plotOutput("data")
  ),
  server = function(input, output,session) {
    shinyjs::hide('grep')
    observe({
      if(!'ret'%in%input$grep){
        updateSelectizeInput(session=session,inputId = 'variable',choices = unique(diamonds$clarity),selected = input$variable,options = list(multiple=TRUE,create=TRUE))
      }
    })
    
    observeEvent(input$variable,{
      
      if('enbl'%in%input$grep){
        curr_cols=switch((nchar(input$variable)==0)+1,
                         grep(input$variable,unique(diamonds$clarity),
                              value=TRUE,
                              ignore.case = 'ignore'%in%input$grep,
                              perl = 'perl'%in%input$grep,
                              fixed='fixed'%in%input$grep,
                              invert='invert'%in%input$grep),
                         NULL)
      }else{
        curr_cols=switch((input$variable%in%unique(diamonds$clarity))+1,unique(diamonds$clarity),input$variable)
      }
      
      
      
      if(length(curr_cols)>0){
        if(all(curr_cols%in%unique(diamonds$clarity))){
          output$data <- renderPlot({
            ggplot(diamonds[diamonds$clarity%in%curr_cols,],aes(x=table,y=carat,colour=clarity))+geom_point()
          })
        }}
      
    })
    })

shinyApp(ui, server)

ui <- fluidPage(
  selectInput('var','Choose Variable',
              choices = names(diamonds)[sapply(diamonds,function(x) inherits(x,c('character','factor')))],
              selected = 'clarity'),
  uiOutput('regexchoose'),
  plotOutput("data")
  )

server <- function(input, output, session) {
  
  observeEvent(input$var,{ 
    output$regexchoose<-shiny::renderUI({
      regexSelectUI(id = "a", label = input$var,choices = unique(diamonds[[input$var]]))
    })
  })
  
    curr_cols<-callModule(regexSelect, "a",shiny::reactive(unique(diamonds[[input$var]])))
    
    observeEvent(curr_cols(),{
      cols_now<-curr_cols()
      output$data <- shiny::renderPlot({
        ggplot(diamonds[diamonds[[input$var]]%in%cols_now,],aes_string(x='table',y='carat',colour=input$var))+geom_point()
      })
  })
 
}
  
shinyApp(ui, server)
