library(dplyr)
library(shiny)
library(bslib)
library(qrcode)
library(digest)
library(shinyTime)

cards <- list(
  card(
    tags$b('QR code content:'),
    htmlOutput('t')
  ),
  card(
    plotOutput("p")
  )
)

ui <- page_sidebar(
  title = "zinter - generate and print QR codes",
  sidebar = sidebar(
    title = "Controls",
    selectizeInput(
      "qrtype", "Select QR type", choices = c('code', 'event', 'location', 'sepa', 'vcard', 'wifi')
    ),
    conditionalPanel(
      condition = ("input.qrtype == 'code'"),
      textInput('code_text', 'QR content', placeholder = 'text')
    ),
    conditionalPanel(
      condition = ("input.qrtype == 'event'"),
      textInput('event_title', '', placeholder = 'Event title'),
      dateInput('event_start', 'Event start'),
      dateInput('event_end', 'Event end')
    )
  ),
  layout_columns(cards[[1]], cards[[2]], col_widths = c(4,8))
)

server <- function(input, output) {
  output$t <- renderText({
    case_when(
      input$qrtype == 'code' ~ input$code_text,
      input$qrtype == 'event' ~ paste0(
        'Event title: ', input$event_title, '<br>',
        'Event start: ', input$event_start, '<br>', 
        'Event end: ', input$event_end)
    )
  })
  
  qr <- reactive({
    
    if(input$qrtype == 'event') {
      qr_event(
        as.POSIXct(input$event_start), 
        as.POSIXct(input$event_end), 
        input$event_title
      )
    } else if (input$qrtype == 'code') {
      qr_code(input$code_text)
    } else if (input$qrtype == 'location') {
      qr_location(
        latitude = 40.730610, 
        longitude = -73.935242
      ) 
    } else if (input$qrtype == 'sepa') {
      qr_sepa(
        iban = 'XXX', 
        beneficiary = 'AA', 
        amount = 1
      )
    } else if (input$qrtype == 'vcard') {
      qr_vcard(
        given = '', 
        family = '', 
        address = c(street_nr = '', city = '', region ='', postal_code = '', country = ''),
        email = ''
      )
    } else if (input$qrtype == 'wifi') {
      qr_wifi(
        ssid = ''
      )
    }
  })
  
  output$p <- renderPlot({
    req(input$qrtype)
    plot(qr())
  })
 
 
}

shinyApp(ui, server)