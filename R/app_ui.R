# user interface
ui <- function() {

  # constants
  app_color <- "blue"
  app_title <- "InvoiceR"
  app_box_default <- "#2c3b41"

  # options
  options(spinner.color = app_color)

  # header
  header <- shinydashboard::dashboardHeader(title = app_title)

  # sidebar
  sidebar <- shinydashboard::dashboardSidebar(
    # headers
    collapsed = FALSE, disable = FALSE,
    shinyjs::useShinyjs(), # enable shinyjs
    shinytoastr::useToastr(), # enable toaster
    prompter::use_prompt(), # enable prompter
    tags$head( # css headers
      # custom
      tags$style(
        type="text/css",
        HTML(paste(
          # body top padding
          ".box-body {padding-top: 5px; padding-bottom: 0px}",
          # custom background box
          sprintf(".box.box-solid.box-info>.box-header{color:#fff; background: %s; background-color: %s;}", app_box_default, app_box_default),
          sprintf(".box.box-solid.box-info{border:1px solid %s;}", app_box_default),
          sep="\n"))
      )
    ),
    # menu
    uiOutput("nav")
  )

  # body
  body <- shinydashboard::dashboardBody(
    h1("Welcome to the invoice generator"),
    h2(htmlOutput("account")),
    h4(
      id = "picker",
      fileInput("order_file", "Select stratocore order export:", accept = ".csv") |> add_tooltip("Select the .csv file from a stratocore order 'exported as spreadsheet'")
    ) |> shinyjs::hidden(),
    h4(
      id = "generator",
      htmlOutput("order_info"),
      textAreaInput("service_summary", label = "Enter a short Service Summary:"),
      downloadButton("generate_invoice", label = "Generate Invoice", icon = icon("file-word")),
      downloadButton("generate_je", label = "Generate Journal Entry", icon = icon("file-excel"))
    ) |> shinyjs::hidden()
  )

  # dashboard page
  shinydashboard::dashboardPage(
    title = app_title, # tab title
    skin = app_color, # styling
    header = header,
    sidebar = sidebar,
    body = body
  )

}
