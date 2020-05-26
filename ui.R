library(shinycssloaders)
library(hodfr)
source('dlm_mapping.R')

navbarPage(id = "nav", windowTitle = "FarFish DLMGui",
                  title = div(
                      span("FarFish DLMGui"),
                      a(icon("github", lib = "font-awesome"),
                          href="https://github.com/farfish/dlmgui",
                          class="navbar-brand",
                          style="position: absolute; top: 0; right: 0")),
                  tabPanel("Edit data",

                      div(class="row",
                          div(class="col-md-3",
                              fileInput('loadCSV', 'Load a DLMtool CSV',
                                  accept = c('text/csv', 'text/comma-separated-values', 'text/tab-separated-values', 'text/plain', '.csv', '.tsv')),
                              div(style = "margin-top: -15px", span("...or"), actionLink("loadDemo", "Load demo data"))),
                          div(class="col-md-3",
                              textInput('filename', NULL, label="Filename to save as")),
                          div(class="col-md-3",
                              downloadButton("saveCSV", "Save data to CSV", style = "margin-top: 25px"))),

                      h3('Data description'),
                      p('Please fill the form with all the data available for your ',
                        'stock, if you are not sure about the value of some category ',
                        "please enter 'NA'. If a field heading is underlined ",
                        "you can hover over for a more detailed description."),
                      p("Based on:"),
                      HTML(format(citation('DLMtool'), style="html")),
                      hodfr(
                          "metadata",
                          fields = dlm_to_hodfr(dlm_metadata),
                          params = list(rowHeaderWidth = 140),
                          orientation = 'vertical'),

                      h3('Catch data'),
                      p('Enter the unit for catch data in the field above, e.g. "Tonnes".'),
                      hodfr(
                          "catch",
                          fields = list(
                              list(name = "catch", title = "Catch")),
                          values = list(type = 'year', min = 2000, max = 2000),
                          params = list(rowHeaderWidth = 170),
                          orientation = 'vertical'),

                      h3('Abundance Index'),
                      p('Enter the unit for abundance index data in the field above, e.g. "Tonnes".'),
                      hodfr(
                          "abundance_index",
                          fields = list(
                              list(name = "abundance_index", title = "Abundance Index")),
                          values = list(type = 'year', min = 2000, max = 2000),
                          params = list(rowHeaderWidth = 170),
                          orientation = 'vertical'),

                      h3('Annual fishing effort'),
                      hodfr(
                          "effort",
                          fields = list(
                              list(name = "effort", title = "Effort")),
                          values = list(type = 'year', min = 2000, max = 2000),
                          params = list(rowHeaderWidth = 100),
                          orientation = 'vertical'),

                      h3('Catch at age'),
                      p('Catch should be in individuals.'),
                      hodfr(
                          "caa",
                          fields = list(type = "bins", max = 10),
                          values = list(type = "year", min = 2000, max = 2000)),

                      h3('Catch at length'),
                      p('Length should be in mm, catch should be in individuals. All bins should have a number in, apart from the final column which is the maximum length. For the final column all years should be NA.'),
                      hodfr(
                          "cal",
                          fields = list(type = "bins", max = 10),
                          values = list(
                              list(name= "Min Length", title = "Min Length"),
                              list(type = "year", min = 2000, max = 2000)),
                          params = list(rowHeaderWidth = 100)),

                      h3('Constants'),
                      hodfr(
                          "constants",
                          fields = dlm_to_hodfr(dlm_constants),
                          values = list(
                              list(name = "value", title = "Value")),
                          orientation = 'vertical',
                          params = list(rowHeaderWidth = 330)),

                      h3('Coefficient of variation'),
                      p("CV is a measure of imprecision, i.e. how imprecise you think this value could be"),
                      hodfr(
                          "cv_constants",
                          fields = dlm_to_hodfr(dlm_cv_constants),
                          values = list(
                              list(name = "value", title = "Value")),
                          orientation = 'vertical',
                          params = list(rowHeaderWidth = 270)),

                      p("")),

                  tabPanel("Catch / Abundance Index Plot",
                      withSpinner(plotOutput("catchPlot", height=600)),
                      downloadButton("catchPlotDownload", label = "Download plot")),
                  tabPanel("CAA",
                      withSpinner(plotOutput("caaPlot", height=600)),
                      downloadButton("caaPlotDownload", label = "Download plot")),
                  tabPanel("CAL",
                      withSpinner(plotOutput("calPlot", height=600)),
                      downloadButton("calPlotDownload", label = "Download plot")),
                  tabPanel("Parameter Distributions",
                      withSpinner(plotOutput("parameterDistributionsPlot", height=600)),
                      downloadButton("parameterDistributionsPlotDownload", label = "Download plot")),
                  tabPanel("Diagnostics",
                      h2("Enough data to produce"),
                      tableOutput("canTable"),
                      h2("Cannot produce"),
                      tableOutput("cantTable")),
                  tabPanel("TAC Plot",
                      p(strong("Please Note:"), "This tool cannot be used for management purposes, it requires previous knowledge or training on stock assessment theory"),
                      withSpinner(plotOutput("mpBoxPlot")),
                      downloadButton("mpBoxPlotDownload", label = "Download plot"),
                      downloadButton("mpResultDownload", label = "Download result table"),
                      checkboxGroupInput("mpLegend", "MPs to show:", inline = TRUE)),
                  footer = includeHTML("footer.html")
)
