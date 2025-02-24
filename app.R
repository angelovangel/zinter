library(dplyr)
library(shiny)
library(bslib)
library(qrcode)
library(digest)
library(shinyTime)
library(shinyvalidate)
library(rhandsontable)
library(reactable)
library(htmlwidgets)
library(knitr)
library(svglite)
library(tidygeocoder)
library(leaflet)


IBAN_REGEX <-  "[a-zA-Z]{2}\\s*[0-9]{2}[a-zA-Z0-9]{4}[0-9]{7}([a-zA-Z0-9]?){0,16}"

#make qr and png on the fly and do 64-bit encoding of the image
make_qr <- function(x) {
 q <- qr_code(x)
 #qname <- paste0('www/', digest(runif(1), algo = 'crc32'), '.png')
 qname <- tempfile(fileext = ".png")
 png(filename = qname)
 plot(q) 
 dev.off()
 qr_img <- knitr::image_uri(qname)
 unlink(qname)
 qr_img
}

make_table <- function(rows, cols) {
  total <- rows * cols
  codes <- sapply(runif(1:total), digest, algo = 'crc32')
  m <- matrix(codes, nrow = rows, ncol = cols)
  as.data.frame(m)
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
      radioButtons(
        'loc_search', 
        label = 'Choose location entry mode', 
        choices = c('Enter lat/lon' = 'latlon', 'Address search' = 'address'), 
        inline = T),
      conditionalPanel(condition = "input.loc_search == 'latlon'",
        numericInput('loc_lat', 'Latidute', value = 40.730610),
        numericInput('loc_lon', 'Longitude', value = -73.935242)
      ),
      conditionalPanel(condition = "input.loc_search == 'address'",
        textInput('address_search', 'Enter address', placeholder = 'Street, city, country ...', width = "100%"),
        actionButton('go_search', 'Search address', icon = icon('map-pin'))
      )
    ),
    conditionalPanel(
      condition = "input.qrtype == 'sepa'",
      textInput('sepa_iban', 'IBAN'),
      textInput('sepa_benef', 'Beneficiary'),
      numericInput('sepa_amount', 'Amount', value = 1),
      textInput('sepa_ref', 'Transfer reference')
    ),
    conditionalPanel(
      condition = "input.qrtype == 'vcard'",
      textInput('vcard_firstname', 'First name'),
      textInput('vcard_familyname', 'Family name'),
      textInput('vcard_email', 'email')
    )
  ),
  card(
    tags$b('QR code content:'),
    conditionalPanel(condition = "input.qrtype == 'location'", leafletOutput('mymap1')),
    uiOutput('t')
  ),
  card(
    plotOutput("p"),
    radioButtons('format', label = '', choices = c('png', 'svg'), selected = 'png', inline = T),
    uiOutput('download_qr')
  )
)

cards2 <- list(
  card(
    tags$p('QR codes values'),
    rHandsontableOutput('hot')
  ),
  card(
    reactableOutput('rtable')
  )
)
ui <- page_navbar(
  
  nav_panel(
    title = "One QR code",
    layout_columns(cards[[1]], cards[[2]], cards[[3]], col_widths = c(3, 6, 3))
  ),
  nav_panel(
    title = 'Table with QR codes',
    fluidRow(
      column(width = 2, 
        textInput('qr_table_text', 'QR code text', value = "KAUST-BCL")
      ),
      column(width = 2,
        numericInput('nrows', 'Number of rows', value = 3, max = 12, min = 1)
      ),
      column(width = 2,
        numericInput('ncols', 'Number of columns', value = 3, max = 6, min = 1)
      ),
      column(width = 2, 
        selectizeInput('qr_size', 'QR size', choices = c(32, 48, 64, 72, 96, 124), selected = 64)
      ),
      column(width = 2, actionButton('reset', 'Reset codes', width = '100%', style = 'margin-top:25px', icon = icon('rotate-right'))
      ),
      column(width = 2, downloadButton('download', 'Download table', style = 'margin-top:25px', icon = icon('chevron-down'))
      )
    ),
    layout_columns(cards2[[1]], cards2[[2]], col_widths = c(4, 8))
  )
)

server <- function(input, output, session) {
  
  iv <- InputValidator$new()
  iv$add_rule('vcard_email', sv_optional())
  iv$add_rule('vcard_email', sv_email())
  iv$add_rule('loc_lat', sv_between(-90, 90))
  iv$add_rule('loc_lon', sv_between(-180, 180))
  
  #iv$add_rule('sepa_iban', sv_regex(IBAN_REGEX, message = "Not a valid IBAN"))
  iv$enable()
  
  # 'code', 'event', 'location', 'sepa', 'vcard', 'wifi'
  output$mymap1 <- renderLeaflet({
    req(iv$is_valid())
    
    leaflet() %>% 
      addTiles() %>%
      addScaleBar(position = 'topright') %>%
      addPopups(
        lng = isolate(input$loc_lon), 
        lat = isolate(input$loc_lat), 
        popup = HTML("Selected location:<br>Lat: ", input$loc_lat, "<br>Lon: ",input$loc_lon)) %>%
      addCircleMarkers(
      lng=isolate(input$loc_lon), 
      lat=isolate(input$loc_lat), 
      label = "Selected location", 
      #icon = awesomeIcons('location-crosshairs', library = 'fa') 
      stroke = FALSE, 
      radius = 6, fillOpacity = 0.8
      ) %>%
      mapOptions(zoomToLimits = 'first')
  })
  
  output$t <- renderUI({
    req(iv$is_valid())
    
    if(input$qrtype == 'code') {
      tags$a(input$code_text)
    } else if(input$qrtype == 'event') {
      HTML(
        'Event title: ', input$event_title, '<br>',
        'Event start: ', input$event_start, '<br>', 
        'Event end: ', input$event_end
      )
    } else if(input$qrtype == 'location') {
      tagList(
        tags$code("Lat: ", input$loc_lat, "Lon: ", input$loc_lon)
      )
    }
  })
    
  #     input$qrtype == 'event' ~ htmlOutput(
  #       'Event title: ', input$event_title, '<br>',
  #       'Event start: ', input$event_start, '<br>', 
  #       'Event end: ', input$event_end),
  #     input$qrtype == 'location' ~ paste0(
  #       'Latidite/Longitude: <code>', 
  #       input$loc_lat, '<br>',input$loc_lon
  #     ),
  #     input$qrtype == 'sepa' ~ paste0(
  #       'Iban: ', input$sepa_iban, '<br>',
  #       'Beneficiary: ', input$sepa_benef, '<br>',
  #       'Amount: ', input$sepa_amount, ' EUR<br>',
  #       'Reference: ', input$sepa_ref
  #     ),
  #     input$qrtype == 'vcard' ~ paste0(
  #       'vcard'
  #     ),
  #     input$qrtype == 'wifi' ~ paste0(
  #       'wifi'
  #     )
  #   )
  # })
  observeEvent(input$mymap1_click, {
    proxy <- leafletProxy('mymap1', deferUntilFlush = F)
    req(input$mymap1_click)
    
    updateNumericInput('loc_lat', value = input$mymap1_click$lat, session = session)
    updateNumericInput('loc_lon', value = input$mymap1_click$lng, session = session)
    proxy %>% 
      setView(
        lng = input$mymap1_click$lng, 
        lat = input$mymap1_click$lat, 
        zoom = input$mymap1_zoom)
  })
 
  observeEvent(input$go_search, {
    req(input$address_search)
    #req(iv$is_valid())
    hit <- tidygeocoder::geo(address = input$address_search)
    updateNumericInput('loc_lat', value = hit$lat, session = session)
    updateNumericInput('loc_lon', value = hit$long, session = session)
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
    req(iv$is_valid())
    plot(qr())
  })
  
  output$download_qr <- renderUI({
    if (input$format == 'png') {
      downloadButton('download1', 'Download QR as PNG')
    } else {
      downloadButton('download1', 'Download QR as SVG')
    }
  })
  
  ### Table tab
  # store the table in reactive so that it can be downloaded as pdf
  qr_reactive <- reactiveValues(table = NULL, outfile = NULL)
  
  output$hot <- renderRHandsontable({
    # clever way to reset code
    # see the action button help why it works
    req(input$nrows)
    req(input$ncols)
    input$reset # takes dependency on reset
    rhandsontable(
      make_table(rows = input$nrows, cols = input$ncols), 
      rowHeaders = NULL, stretchH = 'all', colHeaders = NULL
    )
  })
  
  
  
  output$rtable <- renderReactable({
    df <- qr_table()
    if (!is.null(df)) {
      qr_reactive$table <- reactable(
        df, 
        pagination = FALSE, 
        
        #borderless = T,
        defaultColDef = colDef(
          headerStyle = "display: none;", 
          style = "margin: 0;",
          align = 'center',
          cell = function(value) {
            img_src <- make_qr(value)
            image <- img(
              src = img_src, 
              # qr size with input
              style = paste0("height: ", input$qr_size, "px;"), 
              alt = value
            )
            tagList(
              div(
                style = "white-space: pre; font-family: monospace, monospace; font-size: 10px;", 
                paste0(" ", input$qr_table_text)
              ),
              div(style = "display: inline-block; width: 64px;", image),
              div(
                style = "white-space: pre; vertical-align: top; font-family: monospace, monospace; font-size: 11px;", 
                paste0(" ", value)
              )
            )
          }
        )
      )
      qr_reactive$table
    }
  })
  
  # prepare file for download
  observeEvent(qr_reactive$table, {
    out <- tempfile(fileext = '.html')
    htmlwidgets::saveWidget(qr_reactive$table, file = out)
    if (file.exists(out)) {
      qr_reactive$outfile <- out
    } else {
      qr_reactive$outfile <- NULL
    }
  })
  
  output$download <- downloadHandler(
    filename = paste0(Sys.Date(),'-qr-table.html'),
    content = function(file) {
      file.copy(
        from = qr_reactive$outfile, 
        to = file
      )
      #htmlwidgets::saveWidget(qr_reactive$table, file, selfcontained = T)
    }
  )
  
  output$download1 <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), 'qr-code.', input$format)
    },
    content = function(file) {
      if(input$format == 'png') {
        png(filename = file)
      } else {
        svglite(filename = file)  
      }
      plot(qr())
      dev.off()
    }
  )
 
 
}

shinyApp(ui, server)