# sever
server <- function(data_folder) {

  # start up message
  log_info("\n\n========================================================")
  log_info("starting invoicer GUI with data_folder '", data_folder, "'", if(is_dev_mode()) " in DEV mode")

  # load accounts
  accounts <- readxl::read_excel(file.path(data_folder, "accounts.xlsx"))
  accounting_contact <- "Marilynn Bender (<a href='mailto:geoacct@colorado.edu'>geoacct@colorado.edu</a>)"

  # server function
  shinyServer(function(input, output, session) {

    # instance message
    log_info("connecting to invoicer GUI server")

    # reactive values
    values <- reactiveValues(
      account = tibble::tibble(),
      order = NULL
    )

    # create nav bar based on accounts
    output$nav <- renderUI({
      shinydashboard::sidebarMenu(
        id = "account_id",
        purrr::pmap(list(accounts$account_id, accounts$account, accounts$icon),
                    function(id, label, icon) shinydashboard::menuItem(label, tabName = id, icon = icon(icon))
        ) |> tagList()
      )
    })

    # account selection
    observeEvent(input$account_id, {
      log_debug("account selected: ", input$account_id)
      values$account <- accounts[accounts$account_id == input$account_id,]
      values$order <- NULL
      shinyjs::show("picker")
      shinyjs::hide("generator")
    })

    # account info
    output$account <- renderUI({
      if (nrow(values$account) == 0L)
        "Please select an account from the menu."
      else
        HTML(sprintf("Account: %s %s", values$account$account, icon(values$account$icon)))
    })

    # order file upload
    observeEvent(input$order_file, {
      shinyjs::hide("generator")
      log_info("loading file ", input$order_file$name, user_msg = "Reading order file...")
      values$order <-
        tryCatch(
          read_stratocore_order_export(input$order_file$datapath),
          error = function(e) {
            log_error(user_msg = sprintf("There was an issue reading '%s'", input$order_file$name), error = e)
            NULL
          }
        )
      if (!is.null(values$order)) {
        log_success(user_msg = "Complete")
        shinyjs::toggle("generate_je", condition = values$order$internal)
        updateTextAreaInput(inputId = "service_summary", value = values$order$purpose_draft)
        shinyjs::show("generator")
      }
    })

    # order info
    output$order_info <- renderText({
      req(values$order)
      HTML(
        sprintf(
          "<p><strong>Order #%s looks to be an %s order from %s (%s) for a total of $%s.</strong></p><ol><li>If this is correct, please enter a short 'Service summary' below (the draft text is the concatenation of all user and staff comments on the order, if there are any). <u>This service summary is what usually shows up in accounting systems</u>.</li><li>Then proceed to generate the %s.</li><li><u>Make sure to save a copy of all sent PDFs in the lab records!</u></li></ol>",
          values$order$order_ref, if (values$order$internal) { "internal" } else { "external" },
          paste(values$order$customer_first, values$order$customer_last), values$order$customer_group,
          values$order$total,
          if (values$order$internal) {
            sprintf(
              "internal invoice and journal entry.</li><li>Check them both for accuracy (if you make manual changes, make sure they are in both!), save them as PDFs and send the invoice PDF to %s (<a href='mailto:%s'>%s</a>) and the journal entry PDF to %s",
              values$order$customer_first, values$order$customer_email, values$order$customer_email,
              accounting_contact
            )
          } else {
            sprintf(
              "external invoice.</li><li>Check it for accuracy (make manual changes if needed), save as PDF and send the PDF to %s (<a href='mailto:%s'>%s</a>) and, if the customer wants to pay by cheque or wire transfer, also to %s",
              values$order$customer_first, values$order$customer_email, values$order$customer_email,
              accounting_contact
            )
          }
        )
      )
    })

    # check if service summary is entered
    observeEvent(input$service_summary, {
      shinyjs::toggleState("generate_invoice", condition = !is.null(input$service_summary) && nchar(input$service_summary) > 0)
      shinyjs::toggleState("generate_je", condition = !is.null(input$service_summary) && nchar(input$service_summary) > 0)
    }, ignoreNULL = FALSE)

    # generate invoice
    output$generate_invoice <- downloadHandler(
      filename = function() {
        create_invoice_doc_output_path(
          ref_no = values$order$order_ref,
          account_id = values$account$account_id,
          customer_last_name = values$order$customer_last,
          amount = values$order$total,
          internal = values$order$internal,
          ext = "docx"
        )
      },
      content = function(file) {
        # generate invoice
        log_info("generating invoice", user_msg = "Generating invoice...")
        path <- tryCatch({
          if (values$order$internal) {
            # internal invoice
            create_invoice_internal(
              template = file.path(data_folder, values$account$int_invoice_template),
              reference = file.path(data_folder, values$account$int_invoice_reference),
              ref_no = values$order$order_ref,
              account_id = values$account$account_id,
              invoice_date = Sys.Date(),
              purpose = input$service_summary,
              st_from = values$order$account_nr,
              customer_first_name = values$order$customer_first,
              customer_last_name = values$order$customer_last,
              customer_affiliation = values$order$customer_group,
              customer_email = values$order$customer_email,
              customer_phone = values$order$customer_phone,
              invoice_contact = values$account$contact,
              invoice_contact_email = values$account$contact_email,
              services = values$order$items[[1]],
              acknowledgements = values$account$acknowledgements
            ) |> render_invoice()
          } else {
            # external invoice
            create_invoice_external(
              template = file.path(data_folder, values$account$ext_invoice_template),
              reference = file.path(data_folder, values$account$ext_invoice_reference),
              ref_no = values$order$order_ref,
              account_id = values$account$account_id,
              invoice_date = Sys.Date(),
              purpose = input$service_summary,
              st_to = values$account$speedtype,
              customer_first_name = values$order$customer_first,
              customer_last_name = values$order$customer_last,
              customer_affiliation = values$order$customer_group,
              customer_email = values$order$customer_email,
              customer_phone = values$order$customer_phone,
              service_lab = values$account$lab,
              store_url = values$account$store,
              invoice_contact = values$account$contact,
              invoice_contact_email = values$account$contact_email,
              services = values$order$items[[1]],
              acknowledgements = values$account$acknowledgements
            ) |> render_invoice()
          }
        },
        error = function(e) {
          log_error(user_msg = "There was an issue generating this invoice (an empty file will download)", error = e)
          NULL
        })

        # check if it worked
        if (!is.null(path)) {
          log_success(user_msg = "Complete")
          file.copy(from = path, to = file)
        } else {
          file.copy(from = file.path(data_folder, values$account$int_invoice_reference), to = file)
        }
      }
    )

    # generate journal entry
    output$generate_je <- downloadHandler(
      filename = function() {
        create_invoice_je_output_path(
          ref_no = values$order$order_ref,
          account_id = values$account$account_id,
          customer_last_name = values$order$customer_last,
          amount = values$order$total
        )
      },
      content = function(file) {
        # generate invoice
        log_info("generating journal entry", user_msg = "Generating journal entry...")
        path <- tryCatch({
          create_invoice_je(
            template = file.path(data_folder, values$account$je_invoice_template),
            ref_no = values$order$order_ref,
            account_id = values$account$account_id,
            invoice_date = Sys.Date(),
            purpose = input$service_summary,
            st_from = values$order$account_nr,
            st_to = values$account$speedtype,
            customer_first_name = values$order$customer_first,
            customer_last_name = values$order$customer_last,
            customer_affiliation = values$order$customer_group,
            completed_by = values$account$contact,
            description = values$order$je_summary,
            services = values$order$items[[1]]
          )
        },
        error = function(e) {
          log_error(user_msg = "There was an issue generating this journal entry (an empty file will download)", error = e)
          NULL
        })

        # check if it worked
        if (!is.null(path)) {
          log_success(user_msg = "Complete")
          file.copy(from = path, to = file)
        } else {
          file.copy(from = file.path(data_folder, values$account$je_invoice_template), to = file)
        }
      }
    )
  })

}
