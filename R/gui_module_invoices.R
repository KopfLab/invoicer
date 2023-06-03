# test invoices module
test_module_invoices <- function() {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("Invoices Module Test"),
        module_invoices_ui(id = "invoices")
      ),
      server = function(input, output) {
        invoices <-
          callModule(module_invoices_server, id = "invoices")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}

# invoices server ----
module_invoices_server <- function(input, output, session) {


  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    data = NULL
  )

  # use test data ----
  observeEvent(input$test_data, {
    module_message(ns, "debug", "invoices test data button pressed")
  })

}

# invoices user interface ------
module_invoices_ui <- function(id) {


  # namespace
  ns <- NS(id)

  # page
  fluidPage(

    checkboxInput(ns("test_data"), "Use test data", value = FALSE)

  )
}
