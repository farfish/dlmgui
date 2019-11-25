source('dlmtool_glossary.R')

# Lifted from R/Data2csv.R and chopped up into sections
dlm_metadata <- t(matrix(c(
      "Name", "Name",
      "Common_Name", "Common Name",
      "Species", "Species",
      "Region", "Region",
      "Units", "Units",

      "Ref","Reference OFL",
      "Ref_type", "Reference OFL type"
      ), nrow=2))

# TODO: Additional tables:
#      "Rec", "Recruitment index",
#      "ML", "Mean length",
#      "Lc", "Modal length (Lc)",
#      "Lbar", "Mean length above Lc",

dlm_constants <- t(matrix(c(
      "Mort","M",
      "vbLinf","Von Bertalanffy Linf parameter",
      "vbK","Von Bertalanffy K parameter",
      "vbt0","Von Bertalanffy t0 parameter",
      "wla","Length-weight parameter a",
      "wlb","Length-weight parameter b",

      "steep","Steepness",
      "sigmaR","sigmaR",
      "L50","Length at 50% maturity",
      "L95","Length at 95% maturity",
      "LFC","Length at first capture",
      "LFS","Length at full selection",
      "Vmaxlen", "Vulnerability at asymptotic length",

      "Dep","Current stock depletion",
      "Abun","Current stock abundance",
      "SpAbun","Current spawning abundance",

      "FMSY_M","FMSY/M",
      "BMSY_B0","BMSY/B0",

      "Cref","Catch Reference",
      "Bref","Biomass Reference",
      "Iref","Index Reference",
      "AvC","Average catch over time t",
      "Dt","Depletion over time t"
      ), nrow=2))

dlm_cv_constants <- t(matrix(c(
      "CV_Mort","CV M",
      "CV_vbLinf","CV von B. Linf parameter",
      "CV_vbK","CV von B. K parameter",
      "CV_vbt0","CV von B. t0 parameter",
      "CV_wla","CV Length-weight parameter a",
      "CV_wlb","CV Length-weight parameter b",

      "CV_steep","CV Steepness",
      "CV_sigmaR", "CV sigmaR",
      "CV_L50","CV Length at 50% maturity",
      "LenCV", "CV of length-at-age",  # i.e. CV_L95
      "CV_LFC","CV Length at first capture",
      "CV_LFS","CV Length at full selection",

      # NB: All these can be d@Year long, but assume wll have the same value
      "CV_Cat","CV Catch",
      "CV_Ind","CV Abundance index",
      "CV_Effort", "CV Effort",
      "CV_Rec","CV Recruitment index",

      "CV_Dep","CV current stock depletion",
      "CV_Abun","CV current stock abundance",
      "CV_SpAbun","CV current spawning abundance",

      "CV_FMSY_M","CV FMSY/M",
      "CV_BMSY_B0","CV BMSY/B0",

      "CV_Cref","CV Catch Reference",
      "CV_Bref","CV Biomass Reference",
      "CV_Iref","CV Index Reference",
      "CV_AvC","CV Average catch over time t",
      "CV_Dt","CV Depletion over time t"
      ), nrow=2))

dlm_to_hodfr <- function (m) {
    get_help_text <- dlmtool_slot_info_fn()
    lapply(seq_len(nrow(m)), function (i) {
        help_text <- get_help_text(m[i, 1])
        if (!is.na(help_text)) {
            title <- paste0('<abbr title="', gsub('"', '&quot;', help_text), '">', m[i, 2], '</abbr>')
        } else {
            title <- m[i, 2]
        }
        list(name = m[i, 1], title = title)
    })
}

csv_to_dataframes <- function (filename) {
    remove_na <- function(tbl) {
        # Strip NA rows from start/end
        non_empty_rows <- which(ncol(tbl) > rowSums(is.na(tbl)))
        if (length(non_empty_rows) == 0) {
            return(tbl)
        }
        tbl[seq(min(non_empty_rows), max(non_empty_rows)), , drop = FALSE]
    }

    const_to_df <- function (m) {
        # Generate list of named values from a dlm_* structure
        dat <- lapply(structure(m[, 1], names = m[, 1]), function (slot_name) { ifelse(length(slot(d, slot_name)) > 0, slot(d, slot_name), NA) })
        dat <- as.data.frame(dat)
        rownames(dat) <- 'value'
        return(dat)
    }

    d <- DLMtool::XL2Data(filename)
    list(
        metadata = const_to_df(dlm_metadata),
        catch = remove_na(data.frame(catch = as.vector(d@Cat), row.names = d@Year)),
        abundance_index = remove_na(data.frame(abundance_index = as.vector(d@Ind), row.names = d@Year)),
        effort = remove_na(data.frame(effort = as.vector(d@Effort), row.names = d@Year)),
        caa = (function () {
            dat <- as.data.frame(d@CAA[1,,])
            rownames(dat) <- d@Year[seq_len(nrow(dat))]  # NB: Should be equal, unless there's no data
            colnames(dat) <- seq(1,dim(dat)[2])
            return(remove_na(dat))
        })(),
        cal = (function () {
            dat <- as.data.frame(cbind(d@CAL[1,,], rep(NA, length(d@CAL_bins) - dim(d@CAL)[3])))
            rownames(dat) <- d@Year[seq_len(nrow(dat))]  # NB: Should be equal, unless there's no data
            dat <- remove_na(dat)

            bins <- as.data.frame(t(d@CAL_bins))
            rownames(bins) <- c("Min Length")
            return(rbind(bins, dat))
        })(),
        constants = const_to_df(dlm_constants),
        cv_constants = const_to_df(dlm_cv_constants))
}

dataframes_to_csv <- function (dfs) {
    null_to_na <- function (x) { ifelse(is.null(x), NA, ifelse(identical(x, "NA"), NA, x)) }

    write_line <- function(line_name, x) {
        cat(paste0(c(line_name, x), collapse = ","), "\r\n", sep="")
    }

    write_values <- function(df, dlm_mapping) {
        lookup <- structure(dlm_mapping[, 2], names = dlm_mapping[, 1])
        for (n in names(df)) {
            write_line(lookup[[n]], null_to_na(as.character(df[1, n])))
        }
    }

    write_values(dfs$metadata, dlm_metadata)
    write_values(dfs$constants, dlm_constants)
    write_values(dfs$cv_constants, dlm_cv_constants)

    # Year scale should cover both catch and abundance
    years <- as.numeric(c(rownames(dfs$catch), rownames(dfs$abundance_index)))
    years <- seq(min(years), max(years))
    write_line("Year", years)
    write_line('Duration t', length(years))
    write_line('LHYear', max(years))

    write_line('Catch', as.numeric(dfs$catch[as.character(years), 1]))
    write_line('Abundance index', as.numeric(dfs$abundance_index[as.character(years), 1]))
    write_line('Effort', as.numeric(dfs$effort[as.character(years), 1]))

    # CAA
    if (!all(is.na(dfs$caa))) {
        for (r in rownames(dfs$caa)) {
            write_line(paste('CAA', r), as.numeric(dfs$caa[r,]))
        }
    }
    write_line('Maximum age', ncol(dfs$caa))

    # CAL
    if (!all(is.na(dfs$cal))) {
        for (r in rownames(dfs$cal)) {
            write_line(ifelse(r == "Min Length", "CAL_bins",paste('CAL', r)), as.numeric(dfs$cal[r,]))
        }
    } else {
        write_line("CAL_bins", "")
    }
}
