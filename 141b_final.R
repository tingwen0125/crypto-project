library(httr)
library(testit)
library(jsonlite)
library(shiny)
library(ggplot2)
library(tidyverse)
library(anytime)
library(rvest)
library(shinythemes)

json <- fromJSON("https://api.coingecko.com/api/v3/exchange_rates")
names <- c()
units <- c()
rates <- c()
types <- c()

coins <- fromJSON("https://api.coingecko.com/api/v3/coins")
c.name <- c()
c.symbol <- c()
c.id <- c()
for (id in coins$id) {
  c.id <- append(c.id, id)
}
for (n in coins$name) {
  c.name <- append(c.name, n)
}
for (sym in coins$symbol) {
  c.symbol <- append(c.symbol, sym)
}
coin_data <- tibble(id = c.id, name = c.name, symbol = c.symbol)

for (c in json$rates) {
  names <- append(names, c$name)
  units <- append(units, c$unit)
  rates <- append(rates, c$value)
  types <- append(types, c$type)
}
btc_db <- tibble(name = names, unit = units, rate = rates, type = types)

ui <- fluidPage(
  navbarPage("All About Cryptocurrency",
    theme = shinytheme("darkly"),
    tabPanel("Crypto Info",
      fluid = TRUE, icon = icon("star"),
      sidebarLayout(
        sidebarPanel(
          selectInput("cid", "Crypto Name", choices = coin_data$id %>% set_names(coin_data$name)),
          textInput(inputId = "day", label = "Crypto data up to number of days ago"),
          helpText("Day Example:1, 365, to max")
        ),
        mainPanel(
          uiOutput("img"),
          br(),
          textOutput("coin_description"),
          uiOutput("link"),
          uiOutput("forum_link"),
          br(),
          textOutput("last_updated"),
          hr(),
          fluidRow(
            column(6,
                   plotOutput("price_plot"),
                   textOutput("summary_title"),
                   verbatimTextOutput("summary")
                ),
            column(6,
                   plotOutput("market_cap_plot"),
                   textOutput("m.summary_title"),
                   verbatimTextOutput("m.summary")
                )
          )
        )
      )
    ),
    tabPanel("Converting Bitcoin",
      fluid = TRUE, icon = icon("bitcoin"),
      sidebarLayout(
        sidebarPanel(
          selectInput("type", "Bitcoin to other", choices = btc_db$type),
          selectInput("name", "Name", choices = "-")
        ),
        mainPanel(
          textOutput("exchange_title"),
          tableOutput("rate_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  type_data <- reactive({
    btc_db %>% filter(type == input$type)
  })
  name_data <- reactive({
    type_data() %>% filter(name == input$name)
  })
  market_chart <- reactive({
    req(input$cid)
    endpoint <- str_glue("https://api.coingecko.com/api/v3/coins/{coin}/market_chart?vs_currency=usd&days={day}&interval=daily",
      coin = input$cid, day = input$day
    )
    a <- fromJSON(endpoint)
    return(tibble(day = anytime(a$prices[, 1] / 1000), price = a$prices[, 2], market_caps = a$market_caps[, 2], total_volumes = a$total_volumes[, 2]))
  })
  coin_info <- reactive({
    req(input$cid)
    b <- fromJSON(str_glue("https://api.coingecko.com/api/v3/coins/{coin}?localization=false&tickers=false&market_data=false&community_data=false&developer_data=false&sparkline=false",
      coin = input$cid
    ))
    return(b)
  })

  ### update select input name
  observeEvent(input$type, {
    updateSelectInput(session, "name", choices = c("-", type_data()$name))
  })
  ### output coin img######
  output$img <- renderUI({
    tags$img(src = coin_info()$image$small)
  })
  ###output coin link####
  output$link <-renderUI({
    tags$a(href=coin_info()$links$homepage[1],paste(str_to_title(input$cid),"Homepage"),target="_blank")
  })
  ###output forum link###
  output$forum_link<-renderUI({
    tags$a(href=coin_info()$links$official_forum_url[1],paste(str_to_title(input$cid)),"Official Forum",target="_blank")
  })

  ### description##
  output$coin_description <- renderText({
    if(has_error(read_html(coin_info()$description$en))){
      coin_info()$description$en
    }
    else
      html_text(read_html(coin_info()$description$en))
  })
  output$exchange_title <- renderText({
    req(input$type)
    req(input$name == "-")
    paste0("Rate Table of Bitcoin to other ", input$type)
  })
  ###last updated###
  output$last_updated <- renderText({
    paste0("Last Updated: ",anytime(coin_info()$last_updated,tz="America/Los_Angeles"))
  })
  ### Produce rate table
  output$rate_table <- renderTable({
    req(input$type)
    if (input$name == "-") {
      type_data() %>% select(name, unit, rate)
    } else {
      name_data() %>% select(name, unit, rate)
    }
  })
  ### Plot crypto price
  output$price_plot <- renderPlot({
    req(input$day)
    ggplot(market_chart(), aes(x = day, y = price)) +
      geom_line() +
      ggtitle(paste(str_to_title(input$cid), "Price in USD vs Time")) +
      xlab("Time") +
      ylab("Price")
  })
  #### Price Summary Output
  output$summary_title <- renderText({
    req(input$day)
    paste("Summary Statistics for the Price of", str_to_title(input$cid), "in the Last", input$day, "days")
  })
  output$summary <- renderPrint({
    req(input$day)
    summary(market_chart()$price)
  })
  ###Plot market cap
  output$market_cap_plot <- renderPlot({
    req(input$day)
    ggplot(market_chart(), aes(x = day, y = total_volumes)) +
      geom_line() +
      ggtitle(paste(str_to_title(input$cid), "Total Volume in USD vs Time")) +
      xlab("Time") +
      ylab("Total Volume")
  })
  ###Market cap Summary Output
  output$m.summary_title <- renderText({
    req(input$day)
    paste("Summary Statistics for the Market Cap of", str_to_title(input$cid), "in the Last", input$day, "days")
  })
  output$m.summary <- renderPrint({
    req(input$day)
    summary(market_chart()$market_caps)
  })
}

shinyApp(ui, server)
