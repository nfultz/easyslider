#' easyslider: Easy sliders for shiny.
#'
#' Quickly add UI components by aesthetic mapping
#'
#' This package was written because it was frustrating to
#' add a slider in shiny, which typically required changing
#' code in three places: the placeholder in \code{ui.R},
#' a \code{renderUI} in \code{server.R} and also wiring it
#' up to a plot by using \code{input$thing}.
#'
#' Instead, we can build simple UIs more conviniently by
#' generating the components from aesthetics. In your server
#' function, pipe data through some filters, then generate
#' plots and tables appropriately.
#'
#' @import shiny
#' @docType package
#' @name easyslider
#' @rdname easyslider
#' @examples \dontrun{
#'
#' #ui.R
#'
#' library(shiny)
#'
#'
#' shinyUI(fluidPage(
#'
#'   # Application title
#'   titlePanel("Easy Slider Diamond Demo"),
#'
#'   # Sidebar with easyslider controls
#'   sidebarLayout(
#'     sidebarPanel(
#'       easySliderUIOutput()
#'     ),
#'
#'     # Show a plot of the filtered data
#'     mainPanel(
#'       plotOutput("distPlot")
#'     )
#'   )
#'
#' ))
#'
#' #server.R
#'
#' require(dplyr)
#' require(ggplot2)
#'
#' library(shiny)
#' library(easyslider)
#'
#'
#' shinyServer(function(input, output) {
#'
#'   df <- diamonds %>%
#'     slider2Filter(aes(depth)) %>%
#'     dropdownFilter(aes(clarity))
#'
#'   output$distPlot <- renderPlot({
#'       df() %>% ggplot() + aes(x=carat, y=price, color=cut) + geom_point()
#'     })
#'
#' })
#'
#'
#' }
NULL

tl <- new.env(parent = emptyenv())
# tl[['.aes']] <- c()


#' Easy slider functions
#'
#' @param df a data.frame
#' @param aes an aesthic to map a column to the filter
#' @param label the label to use on the filter
#' @return a reactive, filtered data.frame
#' @export
#' @rdname easyslider_functions
sliderFilter <- function(df, aes, label, ...){

  input <- dynGet("input", NULL)
  output <- dynGet("output", NULL)
  df_ <- if(is.data.frame(df)) reactive(df) else if (is.reactive(df)) df
  aes <- as.character(aes$x)
  if(missing(label)) label <- aes

  render_aes_ui(output, aes, reactive({
      df <- df_()
      r <- c(min(df[[aes]]), max(df[[aes]]))
      sliderInput(aes, label, r[1], r[2], input[[aes]])
    })
  )

  reactive({
    df <- df_()
    subset(df, df[[aes]] == input[[aes]])
  })

}

#' @export
#' @rdname easyslider_functions
slider2Filter <- function(df, aes, label, ...){

  input <- dynGet("input", NULL)
  output <- dynGet("output", NULL)
  df_ <- if(is.data.frame(df)) reactive(df) else if (is.reactive(df)) df
  aes <- as.character(aes$x)
  if(missing(label)) label <- aes

  render_aes_ui(output, aes, reactive({
      df <- df_()
      r <- c(min(df[[aes]]), max(df[[aes]]))
      value <- if(is.null(input[[aes]])) r else input[[aes]]
      sliderInput(aes, label, r[1], r[2], value)
    })
  )

  reactive({
    df <- df_()
    subset(df, input[[aes]][1] <= df[[aes]] & df[[aes]] <= input[[aes]][2])
  })

}

#' @param sfn a function that returns the dropdown items you want to display, given the unique values
#'     present in the column specified by the aesthetic (eg sort, sample, etc)
#' @export
#' @rdname easyslider_functions
dropdownFilter <- function(df, aes, label, ..., sfn=I){

  input <- dynGet("input", NULL)
  output <- dynGet("output", NULL)
  df_ <- if(is.data.frame(df)) reactive(df) else if (is.reactive(df)) df
  aes <- as.character(aes$x)
  if(missing(label)) label <- aes

  render_aes_ui(output, aes, reactive({
    df <- df_()
    r <-sfn(unique(df[[aes]]))
    selectInput(aes, label, r, input[[aes]])
  }))

  reactive({
    df <- df_()
    subset(df, df[[aes]] == input[[aes]])
  })

}

#' @export
#' @rdname easyslider_functions
easySliderUIOutput <- function(){
  uiOutput("AES")
}



render_aes_ui <- function(output, aes, r) {

  # this should preserve insertion order for us...
  if(!aes %in% tl[['.aes']]) tl[['.aes']] <- c(tl[['.aes']], aes)

  tl[[aes]] <- r

  to_render <- mget(tl[['.aes']], tl)

  output[["AES"]] <- renderUI({
      do.call(tagList, lapply(to_render, do.call, list()))
    })
}
