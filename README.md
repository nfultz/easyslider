# easyslider: Easy sliders for shiny.

Quickly add UI components by aesthetic mapping

  It can be frustrating to add a slider in shiny, which typically required changing
code in three places: the uiOutput in `ui.R`,
a `renderUI` in `server.R` and also wiring it
up to a plot by reading from `input$thing`.

  Instead, we can build simple UIs more conviniently by
generating the components from aesthetics. In your server
function, pipe data through some filters, then generate
plots and tables appropriately.

# Example:

Pipe your data through a filter and use it in a plot:

```r
 #server.R

 require(dplyr)
 require(ggplot2)

 library(shiny)
 library(easyslider)


 shinyServer(function(input, output) {

   df <- diamonds %>%
     slider2Filter(aes(depth)) %>%
     dropdownFilter(aes(clarity))

   output$distPlot <- renderPlot({
       df() %>% ggplot() + aes(x=carat, y=price, color=cut) + geom_point()
     })

 })

```

And a simple UI, which doesn't need to be updated as you change filters:


```r
 #ui.R

 library(shiny)


 shinyUI(fluidPage(

   # Application title
   titlePanel("Easy Slider Diamond Demo"),

   # Sidebar with easyslider controls
   sidebarLayout(
     sidebarPanel(
       easySliderUIOutput()
     ),

     # Show a plot of the filtered data
     mainPanel(
       plotOutput("distPlot")
     )
   )

 ))



```
