#' Easy sliders for shiny.
#'
#' This package was written because it was frustrating to
#' add a slider in shiny, which typically required changing
#' code in three places.
#'
#' @name easyslider
#' @import shiny
#' @import rlang
NULL

tl <- new.env(parent = emptyenv())

#' @export
#' @rdname easyslider
sliderFilter <- function(df, aes, ..., input, output, p=parent.frame()){

  df_ <- if(is.data.frame(df)) reactive(df) else if (is.reactive(df)) df

  aes <- as.character(aes$x)

  tl[[aes]] <-  reactive({
    df <- df_()
    r <- c(min(df[[aes]]), max(df[[aes]]))
    sliderInput(aes, aes, r[1], r[2], input[[aes]])
  })

  output[["AES"]] <- renderUI({
      do.call(tagList, lapply(tl, do.call, list()))
    })

  reactive({
    df <- df_()
    subset(df, df[[aes]] == input[[aes]])
  })

}

#' @export
#' @rdname easyslider
slider2Filter <- function(df, aes, ..., input, output){

  df_ <- if(is.data.frame(df)) reactive(df) else if (is.reactive(df)) df

  aes <- as.character(aes$x)

  tl[[aes]] <-  reactive({
    df <- df_()
    r <- c(min(df[[aes]]), max(df[[aes]]))
    value <- if(is.null(input[[aes]])) r else input[[aes]]
    sliderInput(aes, aes, r[1], r[2], value)
  })

  output[["AES"]] <- renderUI({
      do.call(tagList, lapply(tl, do.call, list()))
    })

  reactive({
    df <- df_()
    subset(df, input[[aes]][1] <= df[[aes]] & df[[aes]] <= input[[aes]][2])
  })

}

#'
#' @export
#' @rdname easyslider
dropdownFilter <- function(df, aes, ..., input=input, output=output){

  df_ <- if(is.data.frame(df)) reactive(df) else if (is.reactive(df)) df

  aes <- as.character(aes$x)


  tl[[aes]] <-  reactive({
    df <- df_()
    r <-unique(df[[aes]])
    selectInput(aes, aes, r, input[[aes]])
  })

  output[["AES"]] <- renderUI({
      do.call(tagList, lapply(tl, do.call, list()))
    })

  reactive({
    df <- df_()
    subset(df, df[[aes]] == input[[aes]])
  })

}