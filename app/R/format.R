
format_as_datatable <- function(
  df,
  row_names = FALSE,
  escape = TRUE,
  filter = 'none',
  page_length = 20,
  scroll_x = TRUE,
  column_defs = NULL
) {
  df %>% 
    datatable(
      rownames = row_names,
      filter = filter,
      escape = escape,
      options = list(
        pageLength = page_length,
        scrollX = scroll_x,
        columnDefs = column_defs
      )
    )
}