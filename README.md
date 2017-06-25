# regexSelect
R package to enable regular expression searches within a shiny selectize object.

## install
```r
devtools::install_github('yonicd/regexSelect')
```

## Examples

### renderTable

```r

require(shiny)

if(interactive()){
ui <- shiny::fluidPage(
regexSelectUI(id = "a", label = "Variable:",choices = names(iris)),
shiny::tableOutput("data")
)

server <- function(input, output, session) {
  curr_cols<-callModule(regexSelect, "a",shiny::reactive(names(iris)))
  
  observeEvent(curr_cols(),{
  cols_now<-curr_cols()
  if(length(cols_now)==0)  cols_now<-names(iris)
  output$data <- shiny::renderTable({iris[,cols_now , drop = FALSE]}, rownames = TRUE)
  })
}

shiny::shinyApp(ui, server)

}
```

### renderPlot

```r

library(ggplot2)
ui <- fluidPage(
  selectInput('var','Choose Variable',
              choices = names(diamonds)[sapply(diamonds,function(x) inherits(x,c('character','factor')))],
              selected = 'clarity'),
  uiOutput('regexchoose'),
  plotOutput("data")
  )
  
server <- function(input, output, session) {
  
  output$regexchoose<-shiny::renderUI({
    regexSelectUI(id = "a", label = input$var,choices = unique(diamonds[[input$var]]))
  })
  
  observeEvent(input$var,{
    curr_cols<-callModule(regexSelect, "a",shiny::reactive(unique(diamonds[[input$var]])))
    
    observeEvent(curr_cols(),{
      cols_now<-curr_cols()
      output$data <- shiny::renderPlot({
        ggplot(diamonds[diamonds[[input$var]]%in%cols_now,],aes_string(x='table',y='carat',colour=input$var))+geom_point()
      })
    })    
  })
  
}
  
shinyApp(ui, server)
```