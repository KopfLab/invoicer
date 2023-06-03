# sever
server <- function(user_name) {

  shinyServer(function(input, output, session) {

    gui_message("info", "starting invoicer GUI")

    # navigation
    observeEvent(input$nav, gui_message("debug", "menu selected: ", input$nav))

    # invoices module
    invoices <- callModule(module_invoices_server, id = "invoices")

    output$user_name <- renderText({ user_name })
    output$user_role <- renderText({ "test" })

  })

}
