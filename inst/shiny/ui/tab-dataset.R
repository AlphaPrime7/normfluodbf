# normfluodbf R package - Tingwei Adeck 2024
# --- Dataset tab UI --- #

tabPanel(
  title = "Dataset",
  id = "dataset_tab",
  value = "dataset_tab",
  class = "fade in",
  icon = icon("table"),

  tabsetPanel(
    id = "dataset_tabs",
    type = "tabs",

    #user uploaded data---
    tabPanel(
      title = "Upload a new dataset",
      id = "newds",
      h3(strong("Upload data from a Fluostar Liposome Flux Assay"),
         helpPopup("To use this tool, obtain a <i>.dat</i> or <i>.dbf</i> file from Fluostar Liposome FLux Assays")),
      br(),
      fileInput(
        "upload_data",
        div("Data Files",
            helpPopup('The files used here come from Fluostar Liposome Flux Assays'),
            br(),
            downloadLink(
              outputId = "dev_sample_data",
              label = "Example Data File")
            ),
        multiple = TRUE,
        accept = c(".dbf",
                   ".dat",
                   ".csv",
                   ".tsv",
                   ".txt")
      ),
      withBusyIndicator(
        actionButton(
          "upload_dataBtn",
          "Upload data",
          class = "btn-primary"
        )
      )
    ),

    tabPanel(
      title = "Load a Saved Data Set",
      id = "loadds"
    )

  )
)
