# read in items tables from stratocore order export
read_stratocore_order_export_items <- function(header, end, lines) {
  items <- readr::read_csv(I(lines[(header+1L):end]), show_col_types = FALSE, col_names = FALSE)
  headers <- lines[header] |> stringr::str_split_1(stringr::fixed(","))
  category <- headers[1]
  unit_price_col <- headers |> stringr::str_detect("^\"Unit price") |> which()
  price_col <- headers |> stringr::str_detect("^\"Price") |> which()

  # check
  stopifnot(
    "must have an 'Unit price' column" = !is_empty(unit_price_col),
    "must have an 'Price' column" = !is_empty(price_col)
  )

  # return data
  items |> dplyr::select(item = 2, unit_price = !!unit_price_col, price = !!price_col) |>
    dplyr::mutate(category = stringr::str_remove_all(!!category, "\""), .before = 1L) |>
    dplyr::mutate(quantity = .data$price / .data$unit_price, .before = "price") |>
    dplyr::filter(.data$price > 0)
}

# read stratocore order export
read_stratocore_order_export <- function(file_path) {
  # safety checks
  stopifnot("file must exist" = file.exists(file_path))

  # info
  sprintf("Reading %s", basename(file_path)) |> message()

  # read file lines
  lines <- readr::read_lines(file_path)
  order_ref <- lines |> stringr::str_subset("^\"Order ref")
  order_for <- lines |> stringr::str_subset("^\"Order for")
  account_nr <- lines |> stringr::str_subset("^\"Account number to use")
  affiliation <- lines |> stringr::str_subset("^\"Affiliation")

  # check
  stopifnot(
    "must have an 'Order ref' entry" = !is_empty(order_ref),
    "must have an 'Order for' entry" = !is_empty(order_for),
    "must have an 'Account number to use' entry" = !is_empty(account_nr),
    "must have an 'Affiliation' entry" = !is_empty(affiliation)
  )

  # skip staff and user comment notes
  # FIXME: do we want to do something else with these?
  comment_lines <- lines |> stringr::str_subset("^,*\"(User|Staff) comments")
  comments <-
    tibble::tibble(
      type = comment_lines |> stringr::str_extract("User|Staff"),
      comment = comment_lines |> stringr::str_remove("^,*\"(User|Staff) comments: ?") |>
        stringr::str_remove("\"$")
    )
  data_lines <- lines |> stringr::str_subset("^,*\"(User|Staff) comments", negate = TRUE)
  is_line_item <- data_lines |> stringr::str_detect("^\"#")
  header_lines <- which(diff(is_line_item) == 1)
  last_lines <- which(diff(is_line_item) == -1)

  # get items
  items <- purrr::map2(header_lines, last_lines, read_stratocore_order_export_items, lines = data_lines) |>
    dplyr::bind_rows()

  # return
  data <- tibble::tibble(
    order_ref = stringr::str_extract(order_ref, "\\d+"),
    order_for = order_for,
    internal = stringr::str_detect(affiliation, "CU-Boulder"),
    customer = stringr::str_extract(order_for, "(?<=Order for ).*(?=,? email: )") |> stringr::str_remove(",$"),
    customer_first = stringr::str_extract(customer, "(?<= ).*"),
    customer_last = stringr::str_extract(customer, "^[^ ]+"),
    customer_email = stringr::str_extract(order_for, "(?<=email: )[^, ]*"),
    customer_phone_and_group = stringr::str_extract(order_for, "(?<=phone: )[^,]*"),
    customer_group = stringr::str_extract(customer_phone_and_group, "(?<=\\()[^)]*(?=\\)\"$)"),
    customer_phone = customer_phone_and_group |> stringr::str_remove(stringr::fixed(sprintf(" (%s)\"", customer_group))),
    account_nr = stringr::str_extract(account_nr, "(?<=: ).*") |> stringr::str_remove("\\.?\"$"),
    categories = items$category |> unique() |> paste(collapse = ", "),
    total = sum(items$price),
    items = list(items),
    comments = list(comments),
    purpose_draft = purrr::map_chr(.data$comments, ~.x$comment[nchar(.x$comment) > 0] |> paste(collapse = " "))
  ) |> dplyr::select(-"order_for", -"customer_phone_and_group", -"customer")

  return(data)
}
