observe(print(strftime(input$time1, "%T")))

wells_used <- reactiveValues(dat_data = NULL)

all_inputs_ready <- reactive({
  req(input$dbfordat)
  req(input$dbfordat$datapath)
  req(input$tnp)
  req(input$cycles)
  req(input$ru)
  TRUE
})

observeEvent(input$confirm_rows, {
  req(all_inputs_ready())
  ext <- tools::file_ext(input$dbfordat$name)
  if (ext == 'dat') {
    if (input$data_view == 'raw') {
      wells_used$dat_data <- normfluodbf::normfluodat(
        input$dbfordat$datapath,
        tnp = input$tnp,
        cycles = input$cycles,
        rows_used = c(input$ru),
        norm_scale = 'raw'
      )
    }
    else if (input$data_view == 'normalized') {
      wells_used$dat_data <- normfluodbf::normfluodat(
        input$dbfordat$datapath,
        tnp = input$tnp,
        cycles = input$cycles,
        rows_used = c(input$ru),
        norm_scale = 'one'
      )
    }
    else if (input$data_view == 'normalized_100') {
      wells_used$dat_data <- normfluodbf::normfluodat(
        input$dbfordat$datapath,
        tnp = input$tnp,
        cycles = input$cycles,
        rows_used = c(input$ru),
        norm_scale = 'hundred'
      )
    }
    else if (input$data_view == 'normalized_z') {
      wells_used$dat_data <- normfluodbf::normfluodat(
        input$dbfordat$datapath,
        tnp = input$tnp,
        cycles = input$cycles,
        rows_used = c(input$ru),
        norm_scale = 'z-score'
      )
    }
  }
  else {
    #nothing happens
  }


})

observeEvent(input$apply_normalization, {
  req(all_inputs_ready())
  ext <- tools::file_ext(input$dbfordat$name)
  if (ext == 'dat') {
    if (input$data_view == 'raw') {
      wells_used$dat_data <- normfluodbf::normfluodat(
        input$dbfordat$datapath,
        tnp = input$tnp,
        cycles = input$cycles,
        rows_used = c(input$ru),
        norm_scale = 'raw'
      )
    }
    else if (input$data_view == 'normalized') {
      wells_used$dat_data <- normfluodbf::normfluodat(
        input$dbfordat$datapath,
        tnp = input$tnp,
        cycles = input$cycles,
        rows_used = c(input$ru),
        norm_scale = 'one'
      )
    }
    else if (input$data_view == 'normalized_100') {
      wells_used$dat_data <- normfluodbf::normfluodat(
        input$dbfordat$datapath,
        tnp = input$tnp,
        cycles = input$cycles,
        rows_used = c(input$ru),
        norm_scale = 'hundred'
      )
    }
    else if (input$data_view == 'normalized_z') {
      wells_used$dat_data <- normfluodbf::normfluodat(
        input$dbfordat$datapath,
        tnp = input$tnp,
        cycles = input$cycles,
        rows_used = c(input$ru),
        norm_scale = 'z-score'
      )
    }
  }
  else {
    #nothing happens
  }

})


file_input <- reactive({
  req(input$dbfordat$name) #got it fine now!
  ext <- tools::file_ext(input$dbfordat$name)
  tryCatch ({
    switch(ext,
           dbf = if (input$data_view == 'raw')
           {normfluodbf::normfluordbf(input$dbfordat$datapath, norm_scale = 'raw')}
           else if (input$data_view == 'normalized') {
             {normfluodbf::normfluordbf(input$dbfordat$datapath, norm_scale = 'one')}
           }
           else if (input$data_view == 'normalized_100') {
             {normfluodbf::normfluordbf(input$dbfordat$datapath, norm_scale = 'hundred')}
           }
           else if (input$data_view == 'normalized_z') {
             {normfluodbf::normfluordbf(input$dbfordat$datapath, norm_scale = 'z-score')}
           }
             ,
           dat = wells_used$dat_data,
           csv = readr::read_csv(input$dbfordat$datapath,
                           col_types = cols(.default = "c"),
                           na = c("", "NA", "N/A"),
                           trim_ws = TRUE),
           validate("Invalid file; Please upload a .dbf, .csv or .dat file")
    )
  }, error = function(e) {
    validate(paste("Error reading file:", e$message))
  })

})

rendered_data <- reactiveVal()
editable_data <- reactiveVal()

output$assay_header <- renderTable({
  if (input$table_type == 'default') {
    head(file_input(), input$preview_rows)
  }
})

output$assay_data <- DT::renderDataTable({
  if (input$table_type == 'dt') {
    data_top <- head(file_input(), input$preview_rows)
    editable_data(data_top)
    DT:::DT2BSClass('display')
    DT:::DT2BSClass(c('compact', 'cell-border'))
    dt <- DT::datatable(data_top,
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
    all_columns <- colnames(data_top)

    for (col in all_columns) {
      dt <- dt %>%
        DT::formatStyle(
          col,
          backgroundColor = DT::styleInterval(c(0, 0.99), c("#006FEF", "#00FF00", "orange")),
          color = c('black') #DT::styleInterval(c(0, 0.99999), c("white", "black", "black"))
        )
    }

    rendered_data(data_top)

    dt
  }

})

observeEvent(input$cell_edit, {
  edit_info <- input$cell_edit
  updated_data <- editable_data()
  updated_data[edit_info$row + 1, edit_info$col + 1] <- edit_info$value
  editable_data(updated_data)
})

output$download <- downloadHandler(
  filename = function() {
    paste0(input$dbfordat, ".csv")},
  content = function(file){
    write.csv(editable_data(), file)})





