install.packages('remotes')

install.packages('readxl')  # Required by DLMtool::XL2Data
remotes::install_github('DLMtool/DLMtool', '635442c')

install.packages("xml2")  # For parsing help output

install.packages("shinycssloaders")
remotes::install_github("shuttlethread/hodfr")
install.packages('shiny')

# Development dependencies
if (nzchar(Sys.getenv('DEVEL_MODE'))) {
    install.packages('unittest')
}
