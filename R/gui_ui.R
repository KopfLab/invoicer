# user interface
ui <- function() {

  # constants
  app_color <- "blue"
  app_title <- "GEOM/CUBES RBSA"
  app_box_default <- "#2c3b41"

  # options
  options(spinner.color = app_color)

  # header
  header <- shinydashboard::dashboardHeader(title = app_title)

  # sidebar
  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarUserPanel(textOutput("user_name"), textOutput("user_role")),
    shinydashboard::sidebarSearchForm(label = "Quicksearch", "quicksearch_text", "quicksearch_button"),
    shinydashboard::sidebarMenu(
      id = "nav",
      shinydashboard::menuItem("Invoices", tabName = "invoices", icon = icon("dashboard")),
      shinydashboard::menuItem("Quotes", tabName = "quotes", icon = icon("poo-storm"))
    ),
    shinyjs::useShinyjs(), # enable shinyjs
    tags$head( # css headers
      tags$style(
        type="text/css",
        HTML(paste(
          # error validation output
          ".shiny-output-error-validation { color: red; font-size: 20px; padding: 20px; }", # do we want this red?
          ".shiny-output-error-info { color: black; font-size: 20px; padding: 20px; }",
          # body top padding
          ".box-body {padding-top: 5px; padding-bottom: 0px}",
          # pads on shiny items
          ".form-group, .selectize-control {margin-bottom: 7px;}", #Padding in input widgets
          ".form-group, .selectize-control {margin-top: 2px;}",
          # custom background box
          sprintf(".box.box-solid.box-info>.box-header{color:#fff; background: %s; background-color: %s;}", app_box_default, app_box_default),
          sprintf(".box.box-solid.box-info{border:1px solid %s;}", app_box_default),
          sep="\n"))
      )
    )
  )

  # body
  body <- shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        "invoices",
        h4(icon("dashboard"), "Invoices"),
        module_invoices_ui(id = "invoices")
      ),
      shinydashboard::tabItem(
        "quotes",
        h4(icon("poo-storm"), "Quotes")
      )
    )
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
