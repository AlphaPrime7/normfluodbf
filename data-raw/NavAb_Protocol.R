# file_path <- "data-raw/NavAb_Protocol.xlsx"
# navab_protocol = read_all_sheets("data-raw/NavAb_Protocol.xlsx")
# sheet_names <- excel_sheets(file_path)
# names(navab_protocol) <- sheet_names
write_xlsx(navab_protocol, path = "data-raw/NavAb_Protocol.xlsx")
