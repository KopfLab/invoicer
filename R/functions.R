

create_invoice <- function() {

  # system.file(package = "tbltools", "extdata", "evaluation_template.Rmd") %>%
  #   read_lines() %>%
  #   glue::glue_collapse(sep = "\n") %>%
  #   str_interp(list(data_gs_title = data_gs_title)) %>%
  #   cat(file = file.path(folder, "evaluation.Rmd"))


}

render_invoice <- function() {

  markdown::mark("testing.Rmd")

}


create_journal_entry <- function() {

  # use template and then use this

  # library(openxlsx)
  #
  # # Create a workbook
  # wb <- createWorkbook()
  # addWorksheet(wb, "mysheet")
  #
  # # Write one value to B2
  # writeData(wb, sheet = "mysheet", data.frame(value ="My Value"),
  #           startCol = "B", startRow = 2, colNames = FALSE)
  # # Add some styles
  # addStyle(wb, sheet = "mysheet", rows = 2, cols = "B",
  #          style = createStyle(textDecoration = "Bold", fgFill = "yellow"))
  # saveWorkbook(wb, "myxl.xlsx", overwrite = TRUE)
  #
  # # Reopen the xl file and write a new value to B2 to check that formatting is retained
  # wb <- loadWorkbook("myxl.xlsx")
  # writeData(wb, sheet = "mysheet", data.frame(value = "My new Value"),
  #           startRow = 2, startCol = "B", colNames = FALSE)
  # saveWorkbook(wb, "myxl1.xlsx", overwrite = TRUE)

}
