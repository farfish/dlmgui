library(xml2)
library(DLMtool)

xml_help_file <- function(topic, pkg_ref) {
    # NB: (topic), (pkg_ref) is magical, see help help
    help_file <- tryCatch(utils:::.getHelpFile(help((topic), (pkg_ref))), error = function (e) character())
    if (length(help_file) == 0) {
        return(NULL)
    }
    return(xml2::read_html(paste(capture.output(tools:::Rd2HTML(help_file)), collapse = "\n")))
}

dlmtool_method_info <- function (method_code) {
    doc <- xml_help_file(method_code, 'DLMtool')

    if (is.null(doc)) {
        return(c(code = method_code, name = '', description = ''))
    }
    return(c(
        code = method_code,
        name = xml2::xml_text(xml2::xml_find_first(doc, '/html/body/h2')),
        description = xml2::xml_text(xml2::xml_find_first(doc, "/html/body/h3[text() = 'Description']/following-sibling::p"))))
}

# Decorate character vector of DLMtool (method_codes) with HTML links to their documentation
dlmtool_help_link <- function (method_codes) {
    dlmtool_help_url <- function (method_codes) {
        vapply(method_codes, function (x) {
            out <- as.character(help(x, 'DLMtool', try.all.packages = FALSE, help_type = 'text'))
            if (length(out) > 0) return(paste0(gsub('.*/', 'https://dlmtool.github.io/DLMtool/reference/', out), '.html'))
            return("")
        }, "")
    }

    method_codes <- as.character(method_codes)  # NB: Resolve any factors first
    urls <- dlmtool_help_url(method_codes)
    ifelse(urls == "", method_codes, paste0('<a href="', urls, '" target="_blank">', method_codes, '</a>'))
}

dlmtool_slot_info_fn <- function () {
    doc <- xml_help_file('Data-class', 'DLMtool')

    return(function (slot_name) {
        xml2::xml_text(xml2::xml_find_first(doc, paste0(
            "/html/body/dl/dt[code/text() = '", slot_name, "']/following-sibling::dd")))
    })
}
