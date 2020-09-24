library(jsonlite)
library(ggplot2)
library(DLMtool)
library(hodfr)

source('dlm_mapping.R')
source('dlmtool_glossary.R')

options(shiny.sanitize.errors = FALSE)

dlmtool_methods <- read.csv('dlmtool-methods.csv')

server <- function(input, output, session) {
    # Get names of data.frame inputs from UI
    df_names <- shiny::isolate(names(Filter(is.data.frame, reactiveValuesToList(input))))
    names(df_names) <- df_names

    ##### All plots / output are based on the current table input
    dlm_csv <- reactive({
        utils::capture.output(dataframes_to_csv(
            lapply(df_names, function (x) input[[x]])))
    })

    dlm_doc <- reactive({
        f <- tempfile(fileext = ".csv")
        writeLines(dlm_csv(), con = f)
        d <- DLMtool::XL2Data(f)
        unlink(f)
        return(d)
    })

    ##### File handling
    observeEvent(input$loadCSV, {
        updateTextInput(session, "filename", value = gsub('.\\w+$', '', input$loadCSV$name))
        dfs <- csv_to_dataframes(input$loadCSV$datapath)
        for (n in df_names) {
            updateHodfrInput(session, n, dfs[[n]])
        }
    })

    observeEvent(input$loadDemo, {
        updateTextInput(session, "filename", value = "demo")
        dfs <- csv_to_dataframes("demo.csv")
        for (n in df_names) {
            updateHodfrInput(session, n, dfs[[n]])
        }
    })

    output$saveCSV <- downloadHandler(
        filename = function() {
            paste0(input$filename, ".csv")
        },
        content = function(file) {
            writeLines(dlm_csv(), con = file)
        }
    )

  #### Catch / Abundance index Plot

  catchPlot <- function () {
    d <- dlm_doc()
    summary(d, wait=FALSE, plots=c('TS'))
  }
  output$catchPlot <- renderPlot({ catchPlot() })
  output$catchPlotDownload <- downloadHandler(
      filename = function() { paste(input$document_name, ".catchPlot.png", sep="") },
      content = function(file) { png(file) ; print(catchPlot()) ; dev.off() }
  )

  #### CAA

  caaPlot <- function () {
    d <- dlm_doc()
    summary(d, wait=FALSE, plots=c('CAA'), tplot = 50)
  }
  output$caaPlot <- renderPlot({ caaPlot() })
  output$caaPlotDownload <- downloadHandler(
      filename = function() { paste(input$document_name, ".caaPlot.png", sep="") },
      content = function(file) { png(file) ; print(caaPlot()) ; dev.off() }
  )

  #### CAL

  calPlot <- function () {
    d <- dlm_doc()
    summary(d, wait=FALSE, plots=c('CAL'), tplot = 50)
  }
  output$calPlot <- renderPlot({ calPlot() })
  output$calPlotDownload <- downloadHandler(
      filename = function() { paste(input$document_name, ".calPlot.png", sep="") },
      content = function(file) { png(file) ; print(calPlot()) ; dev.off() }
  )

  #### Parameter Distributions

  parameterDistributionsPlot <- function () {
    d <- dlm_doc()
    summary(d, wait=FALSE, plots=c('PD'))
  }
  output$parameterDistributionsPlot <- renderPlot({ parameterDistributionsPlot() })
  output$parameterDistributionsPlotDownload <- downloadHandler(
      filename = function() { paste(input$document_name, ".parameterDistributionsPlot.png", sep="") },
      content = function(file) { png(file) ; print(parameterDistributionsPlot()) ; dev.off() }
  )

  #### Diagnostics

  output$canTable <- renderTable({
    d <- dlm_doc()
    out <- merge(data.frame(Code = Can(d), stringsAsFactors = FALSE), dlmtool_methods, by = "Code", all.x = TRUE)
    out$Code <- dlmtool_help_link(out$Code)
    out[with(out, order(Direction, Code)), colnames(dlmtool_methods)]
  }, sanitize.text.function = function(x) x)  # NB: Disable HTML escaping for help links

  output$cantTable <- renderTable({
    d <- dlm_doc()
    out <- as.data.frame(Cant(d), stringsAsFactors = FALSE)
    colnames(out) <- c("Code", "Reason")
    out <- merge(out, dlmtool_methods, by = "Code", all.x = TRUE)
    out$Code <- dlmtool_help_link(out$Code)
    out[with(out, order(Direction, Code)), c('Direction', 'Code', 'Name', 'Type', 'Reason')]
  }, sanitize.text.function = function(x) x)  # NB: Disable HTML escaping for help links

  #### TAC plot
  document_mps <- observeEvent(dlm_csv(), {
    # Document changed, so clear legend checkboxes
    updateCheckboxGroupInput(session, "mpLegend", choices = c(''), selected = c())
  })
  dlm_tac <- reactive({
    d <- dlm_doc()

    # Restrict to selected MPs if any selected
    MPs <- input$mpLegend
    if (length(MPs) == 0) MPs <- NA

    return(runMP(d, MPs = MPs, reps=1000, silent = TRUE))
  })
  output$mpBoxPlot <- renderPlot({
    d_tac <- dlm_tac()
    results <- boxplot(d_tac, col = "#237aa5")

    # Update legend with available MPs
    if (length(input$mpLegend) == 0) {
        MPs <- rev(as.character(results$MP))
        updateCheckboxGroupInput(session, "mpLegend",
            choiceValues = MPs,
            choiceNames = lapply(unname(MPs), function (m) {
                span(HTML(dlmtool_help_link(m)),
                    span(dlmtool_method_info(m)['description'], style="float:right;width:calc(100% - 10rem);padding-bottom:1rem"))
            }))
    }
  })
  output$mpBoxPlotDownload <- downloadHandler(
      filename = function() { paste(input$document_name, ".mpBoxPlot.png", sep="") },
      content = function(file) {
          d_tac <- dlm_tac()

          png(file)
          boxplot(d_tac, col = "#237aa5")
          dev.off()
      }
  )
  output$mpResultDownload <- downloadHandler(
      filename = function() { paste(input$document_name, ".mpResult.csv", sep="") },
      content = function(file) {
          d_tac <- dlm_tac()

          pdf(file = NULL)  # NB: Using boxplot for it's result table, not it's plot
          write.csv(boxplot(d_tac), file = file)
          dev.off()
      }
  )
}
