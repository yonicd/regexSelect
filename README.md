# regexSelect
R package to enable regular expression searches within a shiny selectize object.

## install
```r
devtools::install_github('yonicd/regexSelect')
```

## Example

```r

require(shiny)

if(interactive()){
ui <- shiny::fluidPage(
regexSelectUI(id = "a", label = "Variable:",choices = names(iris)),
shiny::tableOutput("data")
)

server <- function(input, output, session) {
  curr_cols<-callModule(regexSelect, "a",shiny::reactive(iris))
  
  observeEvent(curr_cols(),{
  cols_now<-curr_cols()
  if(length(cols_now)==0)  cols_now<-names(data())
  output$data <- shiny::renderTable({iris[,cols_now , drop = FALSE]}, rownames = TRUE)
  })
}

shiny::shinyApp(ui, server)

}
```