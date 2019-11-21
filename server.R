library(jsonlite)
library(ggplot2)
library(memoise)
library(DLMtool)
library(hodfr)

source('ffdbclient.R')
source('dlmtool_glossary.R')

options(shiny.sanitize.errors = FALSE)

dlmtool_methods <- read.csv('dlmtool-methods.csv')

server <- function(input, output, session) {
    ##### All plots / output are based on the current table input
    ffdb_doc <- reactive({
        list(
            metadata=input$metadata,
            catch=input$catch,
            caa=input$caa,
            cal=input$cal,
            constants=input$constants,
            cv=input$cv)
    })

    dlm_doc <- reactive({
        f <- tempfile(fileext = ".csv")
        ffdbdoc_to_dlmtool_csv(ffdb_doc(), output = f)
        d <- DLMtool::XL2Data(f)
        unlink(f)
        return(d)
    })

    ##### File handling
    observeEvent(input$loadCSV, {
        updateTextInput(session, "filename", value = gsub('.\\w+$', '', input$loadCSV$name))
        d <- dlmtool_csv_to_ffdbdoc(input$loadCSV$datapath)
        updateHodfrInput(session, "metadata", d$metadata)
        updateHodfrInput(session, "catch", d$catch)
        updateHodfrInput(session, "caa", d$caa)
        updateHodfrInput(session, "cal", d$cal)
        updateHodfrInput(session, "constants", d$constants)
        updateHodfrInput(session, "cv", d$cv)
    })

    output$saveCSV <- downloadHandler(
        filename = function() {
            paste0(input$filename, ".csv")
        },
        content = function(file) {
            ffdbdoc_to_dlmtool_csv(ffdb_doc(), output = file)
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
    summary(d, wait=FALSE, plots=c('CAA'))
  }
  output$caaPlot <- renderPlot({ caaPlot() })
  output$caaPlotDownload <- downloadHandler(
      filename = function() { paste(input$document_name, ".caaPlot.png", sep="") },
      content = function(file) { png(file) ; print(caaPlot()) ; dev.off() }
  )

  #### CAL

  calPlot <- function () {
    d <- dlm_doc()
    summary(d, wait=FALSE, plots=c('CAL'))
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

  mpBoxPlot <- function () {
    d <- dlm_doc()
    d_tac <- runMP(d, reps=1000)
    boxplot(d_tac)
  }
  output$mpBoxPlot <- renderPlot({ mpBoxPlot() })
  output$mpBoxPlotDownload <- downloadHandler(
      filename = function() { paste(input$document_name, ".mpBoxPlot.png", sep="") },
      content = function(file) { png(file) ; print(mpBoxPlot()) ; dev.off() }
  )

  output$mpTable <- renderTable({
    d <- dlm_doc()
    # Find all possible output methods
    out <- merge(data.frame(Code = Can(d), stringsAsFactors = FALSE), dlmtool_methods, by = "Code")
    out$Code <- dlmtool_help_link(out$Code)
    out[which(out$Direction == 'output'), colnames(out) != 'Direction']
  }, sanitize.text.function = function(x) x)  # NB: Disable HTML escaping for help links
}
