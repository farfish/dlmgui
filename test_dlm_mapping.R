library(unittest, quietly = TRUE)
library(DLMtool)

source('dlm_mapping.R')

line_list <- function (lines) {
    structure(
        gsub(',', ' , ', gsub(',*\r?$', '', lines)),
        names = gsub(',.*', '', lines))
}

dlm_loopback <- function (in_file) {
    tmp_file <- tempfile(fileext = ".csv")
    DLMtool::Data2csv(DLMtool::XL2Data(in_file), file = tmp_file)
    out <- line_list(readLines(tmp_file))
    unlink(tmp_file)
    return(out)
}

dfs_to_lines <- function (in_file) {
    dfs <- csv_to_dataframes(in_file)

    out_file <- tempfile(fileext = ".csv")
    capture.output(dataframes_to_csv(dfs), file = out_file)
    out_lines <- dlm_loopback(out_file)
    unlink(out_file)

    return(out_lines)
}

for (file_path in c('Cobia.csv')) {
    ok_group(file_path, {
        out_lines <- dfs_to_lines(file_path)
        cobia_lines <- dlm_loopback(file_path)

        ok(ut_cmp_identical(
            sort(names(out_lines)),
            sort(names(cobia_lines))), "Lists of names are the same")
        for (line in names(cobia_lines)) {
            if (line %in% c('Effort', 'CV Effort', 'CV Catch', 'CV Abundance index', 'Recruitment index')) {
                # Don't do anything, we don't support these (yet)
            } else {
                ok(ut_cmp_identical(
                    out_lines[[line]],
                    cobia_lines[[line]]), line)
            }
        }
    })
}