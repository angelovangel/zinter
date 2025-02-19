library(dplyr)
library(shiny)
library(bslib)
library(qrcode)
library(digest)
library(shinyTime)
library(rhandsontable)
library(reactable)
library(knitr)

#make qr and png on the fly and do 64-bit encoding of the image
make_qr <- function(x) {
 q <- qr_code(x)
 qname <- paste0('www/', digest(runif(1), algo = 'crc32'), '.png')
 png(filename = qname)
 plot(q) 
 dev.off()
 qr_img <- knitr::image_uri(qname)
 unlink(qname)
 qr_img
}

example_table <- tibble(
  code = sapply(LETTERS[1:6], digest, algo = 'crc32'),
  label = LETTERS[1:6]
)

cards <- list(
  card(
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
    ),
    conditionalPanel(
      condition = "input.qrtype == 'location'",
      numericInput('loc_lat', 'Latidute', value = 40.730610),
      numericInput('loc_lon', 'Longitude', value = -73.935242)
    ),
    conditionalPanel(
      condition = "input.qrtype == 'sepa'",
      textInput('sepa_iban', 'IBAN'),
      textInput('sepa_benef', 'Beneficiary'),
      numericInput('sepa_amount', 'Amount', value = 1),
      textInput('sepa_ref', 'Transfer reference')
    )
  ),
  card(
    tags$b('QR code content:'),
    htmlOutput('t')
  ),
  card(
    plotOutput("p")
  )
)

cards2 <- list(
  card(
    rHandsontableOutput('hot')
  ),
  card(
    reactableOutput('rtable')
  )
)
ui <- page_navbar(
  nav_panel(
    title = "One QR code",
    layout_columns(cards[[1]], cards[[2]], cards[[3]], col_widths = c(4, 4, 4))
  ),
  nav_panel(
    title = 'Table with QR codes', 
    layout_columns(cards2[[1]], cards2[[2]], col_widths = c(4, 8))
  )
)

server <- function(input, output) {
  
  output$t <- renderText({
    case_when(
      input$qrtype == 'code' ~ input$code_text,
      input$qrtype == 'event' ~ paste0(
        'Event title: ', input$event_title, '<br>',
        'Event start: ', input$event_start, '<br>', 
        'Event end: ', input$event_end),
      input$qrtype == 'location' ~ paste0(
        'Latidite/Longitude: <code>', 
        input$loc_lat, '<br>',input$loc_lon
      ),
      input$qrtype == 'sepa' ~ paste0(
        'Iban: ', input$sepa_iban, '<br>',
        'Beneficiary: ', input$sepa_benef, '<br>',
        'Amount: ', input$sepa_amount, ' EUR<br>',
        'Reference: ', input$sepa_ref
      ),
      input$qrtype == 'vcard' ~ paste0(
        'vcard'
      ),
      input$qrtype == 'wifi' ~ paste0(
        'wifi'
      )
    )
  })
  
  ### START REACTIVES 
  
  # single QR
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
        latitude = input$loc_lat, 
        longitude = input$loc_lon
      ) 
    } else if (input$qrtype == 'sepa') {
      qr_sepa(
        iban = input$sepa_iban, 
        beneficiary = input$sepa_benef, 
        amount = input$sepa_amount, 
        unstructured_reference = input$sepa_ref
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
  
  # Table QRs reactive
  qr_table <- reactive({
    if (!is.null(input$hot)) {
      as_tibble(hot_to_r(input$hot))
    }
  })
  ### END REACTIVES 
  
  output$p <- renderPlot({
    req(input$qrtype)
    plot(qr())
  })
  
  output$hot <- renderRHandsontable({
    rhandsontable(example_table, rowHeaders = NULL, stretchH = 'all')
  })
  
  output$rtable <- renderReactable({
    df <- qr_table()
    if (!is.null(df)) {
      reactable(
        df, 
        columns = list(
          code = colDef(cell = function(value) {
            img_src <- make_qr(value)
            image <- img(src = img_src, style = "height: 64px;", alt = value)
            tagList(
              div(style = "display: inline-block; width: 96px", image),
              value
            )
          })
        )
      )
    }
  })
 
 
}

shinyApp(ui, server)