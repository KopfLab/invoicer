# general journal entry ============

# create a journal entry file
create_journal_entry <- function(values, fields, template_filepath, output_filepath) {

  # safety checks
  stopifnot(
    "need a template file" = !missing(template_filepath) && file.exists(template_filepath)
  )

  # add values to fields
  fields <- fields |>
    dplyr::left_join(
      dplyr::tibble(col = LETTERS, col_idx = seq_along(LETTERS)),
      by = "col"
    ) |>
    dplyr::inner_join(
      values |> unlist() |> tibble::enframe(),
      by = c("variable" = "name")
    )

  # write workbook
  wb <- template_filepath |> openxlsx::loadWorkbook()

  # update fields
  for (i in seq_along(fields$variable)) {
    wb |>
      openxlsx::writeData(
        sheet = 1,
        x = fields$value[i],
        xy = c(fields$col_idx[i], fields$row[i])
      )
  }

  # save workbook
  wb |> openxlsx::saveWorkbook(output_filepath, overwrite = TRUE)
  return(invisible(fields))
}

# specific journal entries ==============

# standard journal entry fields
get_journal_entry_fields <- function() {
  dplyr::tribble(
    ~variable,            ~col, ~row,
    "order_dep", 	        "D", 	5L,
    "order_date", 	      "O", 	5L,
    "reference_no", 	    "R", 	5L,
    "purpose", 	          "D", 	6L,
    "expense_st", 	      "C", 	14L,
    "expense_account",    "D", 	14L,
    "expense_fund", 	    "F", 	14L,
    "expense_qty", 	      "L", 	14L,
    "expense_unit_price", "N", 	14L,
    "expense_desc", 	    "O", 	14L,
    "expense_total", 	    "S", 	14L,
    "service_st", 	      "C", 	28L,
    "service_account", 	  "D", 	28L,
    "service_fund", 	    "F", 	28L,
    "service_total", 	    "S", 	28L,
    "completed_by", 	    "P", 	36L,
    "completed_date", 	  "S", 	36L
  )
}

# transfer journal entry
create_transfer_je <- function(template, ref_no, transfer_date, purpose, st_from, st_to, amount, completed_by) {

  # safety checks
  stopifnot(
    "`template` required" = !missing(template),
    "`ref_no` required" = !missing(ref_no),
    "`transfer_date` required" = !missing(transfer_date),
    "`purpose` required" = !missing(purpose),
    "`st_from` required" = !missing(st_from),
    "`st_to` required" = !missing(st_to),
    "`amount` required" = !missing(amount),
    "`completed_by` required" = !missing(completed_by)
  )

  # set values
  values <- list(
    purpose = purpose,
    order_date = transfer_date |> format("%m/%d/%Y"),
    reference_no = ref_no,
    expense_st = st_from,
    expense_total = amount |> scales::label_dollar()(),
    service_st = st_to,
    service_total = amount |> scales::label_dollar()(),
    completed_by = completed_by,
    completed_date = transfer_date |> format("%m/%d/%Y")
  )

  # journal entry
  output_filepath <- create_transfer_je_output_path(ref_no, st_from, st_to, amount)
  fields <- get_journal_entry_fields()
  create_journal_entry(
    values = values, fields = fields, template_filepath = template, output_filepath = output_filepath
  )

  return(output_filepath)

}

# for invoicing
create_invoice_je <- function(
    template, ref_no, invoice_date, purpose,
    st_from, st_to, customer_name, customer_affiliation, completed_by,
    description, total
    #items_description, items_quantity, items_unit_price, items_unit_size
) {

  # safety checks
  stopifnot(
    "`template` required" = !missing(template),
    "`ref_no` required" = !missing(ref_no),
    "`invoice_date` required" = !missing(invoice_date),
    "`purpose` required" = !missing(purpose),
    "`st_from` required" = !missing(st_from),
    "`st_to` required" = !missing(st_to),
    "`customer_name` required" = !missing(customer_name),
    "`customer_affiliation` required" = !missing(customer_affiliation),
    "`completed_by` required" = !missing(completed_by),
    "`description` required" = !missing(description),
    "`total` required" = !missing(total)
    # "`items_description` required" = !missing(items_description),
    # "`items_quantity` required" = !missing(items_quantity),
    # "`items_unit_price` required" = !missing(items_unit_price),
    # "`items_unit_size` required" = !missing(items_unit_size)
  )

  # FIXME: cannot yet handle multiple items in invoice
  #stopifnot(length(items_description) == 1)

  # set values
  #total <- sum(items_quantity * items_unit_price)
  expense_fund <- stringr::str_match(st_from, "1(\\d\\d)")[1,2]
  service_fund <- stringr::str_match(st_to, "1(\\d\\d)")[1,2]
  values <- list(
    purpose = purpose,
    order_date = invoice_date |> format("%m/%d/%Y"),
    order_dep = sprintf("%s / %s", customer_affiliation, customer_name),
    reference_no = ref_no,
    expense_st = st_from,
    # as per Marilynn's instructions - NOTE: this might be different for Bert (fund 29), check!!
    expense_account = if (expense_fund == "30") " 530102" else "530100",
    expense_fund = expense_fund,
    #expense_qty = items_quantity,
    #expense_unit_price = sprintf("$%s/%s", items_unit_price, items_unit_size),
    expense_desc = description,
    expense_total = total |> scales::label_dollar()(),
    service_st = st_to,
    # as per Marilynn's instructions - NOTE: this might be different for Bert (fund 29), check!!
    service_account = if (expense_fund == "30") "380101" else "380100",
    service_fund = service_fund,
    service_total = total |> scales::label_dollar()(),
    completed_by = completed_by,
    completed_date = invoice_date |> format("%m/%d/%Y")
  )

  # journal entry
  output_filepath <- create_invoice_je_output_path(ref_no, customer_name, customer_affiliation, total)
  fields <- get_journal_entry_fields()
  create_journal_entry(
    values = values, fields = fields, template_filepath = template, output_filepath = output_filepath
  )

  return(output_filepath)

}


# general invoice =====

# create an invoice rmd file
create_invoice <- function(values = list(), template_filepath, output_filepath) {

  # safety checks
  stopifnot(
    "need a template file" = !missing(template_filepath) && file.exists(template_filepath)
  )

  # copy template
  values |>
    glue::glue_data(
      readr::read_lines(template_filepath) |> paste(collapse = "\n"),
      .open = "${",
      .null = ""
    ) |>
    cat(file = output_filepath)

}

# render invoice rmd file
render_invoice <- function(file_path) {
  rmarkdown::render(file_path)
}

# external invoice ============

create_invoice_external <- function(
    template, reference, ref_no,
    invoice_date, purpose, st_to,
    customer_name, customer_affiliation, customer_email, customer_phone,
    service_lab, store_url,
    invoice_contact, invoice_contact_email,
    services
) {

  # safety checks
  stopifnot(
    "`template` required" = !missing(template),
    "`reference` required" = !missing(reference),
    "`ref_no` required" = !missing(ref_no),
    "`invoice_date` required" = !missing(invoice_date),
    "`purpose` required" = !missing(purpose),
    "`st_to` required" = !missing(st_to),
    "`service_lab` required" = !missing(service_lab),
    "`store_url` required" = !missing(store_url),
    "`customer_name` required" = !missing(customer_name),
    "`customer_affiliation` required" = !missing(customer_name),
    "`customer_email` required" = !missing(customer_email),
    "`customer_phone` required" = !missing(customer_phone),
    "`invoice_contact` required" = !missing(invoice_contact),
    "`invoice_contact_email` required" = !missing(invoice_contact_email),
    "`services` data frame with columns `category`, `item`, `unit_price`, `quantity`, `price` required" = !missing(services) && is.data.frame(services) && all(c("category", "item", "unit_price", "quantity", "price") %in% names(services))
  )

  # set values
  total <- sum(services$price)
  values <- list(
    reference = reference,
    service_lab = service_lab,
    customer_name = customer_name,
    customer_affiliation = customer_affiliation,
    customer_email = customer_email,
    customer_phone = customer_phone,
    ref_no = ref_no,
    invoice_date = invoice_date |> format("%m/%d/%Y"),
    st_to = st_to,
    store_url = store_url,
    purpose = purpose,
    invoice_contact = invoice_contact,
    invoice_contact_email = invoice_contact_email,
    items_category = services$category,
    items_description = services$item,
    items_quantity = services$quantity,
    items_unit_price = services$unit_price,
    items_price = services$price
  )

  # journal entry
  output_filepath <- create_invoice_ext_output_path(ref_no, customer_name, customer_affiliation, total)
  create_invoice(
    values = values, template_filepath = template, output_filepath = output_filepath
  )

  return(output_filepath)

}

create_invoice_internal <- function(
  template, reference, ref_no,
  invoice_date, purpose, st_from,
  customer_name, customer_affiliation, customer_email, customer_phone,
  invoice_contact, invoice_contact_email,
  services
) {

  # safety checks
  stopifnot(
    "`template` required" = !missing(template),
    "`reference` required" = !missing(reference),
    "`ref_no` required" = !missing(ref_no),
    "`invoice_date` required" = !missing(invoice_date),
    "`purpose` required" = !missing(purpose),
    "`st_from` required" = !missing(st_from),
    "`customer_name` required" = !missing(customer_name),
    "`customer_affiliation` required" = !missing(customer_name),
    "`customer_email` required" = !missing(customer_email),
    "`customer_phone` required" = !missing(customer_phone),
    "`invoice_contact` required" = !missing(invoice_contact),
    "`invoice_contact_email` required" = !missing(invoice_contact_email),
    "`services` data frame with columns `category`, `item`, `unit_price`, `quantity`, `price` required" = !missing(services) && is.data.frame(services) && all(c("category", "item", "unit_price", "quantity", "price") %in% names(services))
  )

  # set values
  total <- sum(services$price)
  values <- list(
    reference = reference,
    customer_name = customer_name,
    customer_affiliation = customer_affiliation,
    customer_email = customer_email,
    ref_no = ref_no,
    invoice_date = invoice_date |> format("%m/%d/%Y"),
    st_from = st_from,
    purpose = purpose,
    invoice_contact = invoice_contact,
    invoice_contact_email = invoice_contact_email,
    customer_phone = customer_phone,
    items_category = services$category,
    items_description = services$item,
    items_quantity = services$quantity,
    items_unit_price = services$unit_price,
    items_price = services$price
  )

  # journal entry
  output_filepath <- create_invoice_int_output_path(ref_no, customer_name, customer_affiliation, total)
  create_invoice(
    values = values, template_filepath = template, output_filepath = output_filepath
  )

  return(output_filepath)
}

# path helpers =========

create_transfer_je_output_path <- function(ref_no, st_from, st_to, amount) {
  sprintf("%s_transfer_ST%s_to_ST%s_%sUSD.xlsx", ref_no, st_from, st_to, paste(amount))
}

create_invoice_je_output_path <- function(ref_no, customer_name, customer_affiliation, amount) {
  sprintf("%s_int_%s_%s_%sUSD.xlsx", ref_no,
          stringr::str_replace_all(customer_name, "[ ,/.]+", "_"),
          stringr::str_replace_all(customer_affiliation, "[ ,/.]+", "_"),
          paste(amount))
}

create_invoice_ext_output_path <- function(ref_no, customer_name, customer_affiliation, amount) {
  sprintf("%s_ext_%s_%s_%sUSD.Rmd", ref_no,
          stringr::str_replace_all(customer_name, "[ ,/.]+", "_"),
          stringr::str_replace_all(customer_affiliation, "[ ,/.]+", "_"),
          paste(amount))
}

create_invoice_int_output_path <- function(ref_no, customer_name, customer_affiliation, amount) {
  sprintf("%s_int_%s_%s_%sUSD.Rmd", ref_no,
          stringr::str_replace_all(customer_name, "[ ,/.]+", "_"),
          stringr::str_replace_all(customer_affiliation, "[ ,/.]+", "_"),
          paste(amount))
}

