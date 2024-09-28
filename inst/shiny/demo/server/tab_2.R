output$raw <- renderTable({
  if (input$table_type == 'default') {
    print(file_input())
  }
})

output$raw_dt <- DT::renderDataTable({
  if (input$table_type == 'dt') {
    dt <- print(file_input())
    dt <- DT::datatable(dt,
                        filter = 'top',
                        class = 'cell-border stripe', #c('display', 'compact', 'cell-border', 'stripe')
                        editable = 'cell',
                        options = list(scrollX = TRUE,
                                       scrollY = TRUE,
                                       fixedHeader = TRUE,
                                       fixedColumns = list(leftColumns = 1),
                                       pageLength = 15),
                        rownames = TRUE,
                        callback = JS("table.on('edit', function(e, dt, old_value) {
                                      Shiny.setInputValue('cell_edit', {
                                        row: e.target.parents('tr').index(),
                                        col: e.target.cellIndex,
                                        value: e.target.innerHTML
                                      });
                                    })"))
    all_columns <- colnames(dt)

    for (col in all_columns) {
      dt <- dt %>%
        DT::formatStyle(
          col,
          color = c('black')
        )
    }

    dt
  }
})


output$download_data <- downloadHandler(
  filename = function() {
    paste0(input$dbfordat, ".csv")},
  content = function(file){
    write.csv(editable_data(), file)})



