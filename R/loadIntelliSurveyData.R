#---------------------------------------------------------------------
# Load IntelliSurvey tripleS data exports:
# - returns data frame with factor variables to match survey answer levels
# - repairs some encoding issues in the data (as the file from intellisurvey is a mix of utf8 and windows-1252)
# - removes non-breaking space characters
# - keeps "dummy" data


# Requires: xml2
#---------------------------------------------------------------------

# Parse the .sss (xml) file into a simple list describing the data layout
read_sss_meta = function(sssFilename, encoding = "UTF-8", sep = "_") {

    doc = xml2::read_xml(sssFilename, encoding = encoding)
    rec = xml2::xml_find_first(doc, ".//record")

    # number of header rows to skip in the csv
    skip = suppressWarnings(as.integer(xml2::xml_attr(rec, "skip")))
    if (is.na(skip)) skip = 0L

    # helper: text of a child node, or NA if the node doesn't exist
    node_text = function(node, xpath) {
        x = xml2::xml_find_first(node, xpath)
        if (inherits(x, "xml_missing")) NA_character_ else xml2::xml_text(x)
    }

    var_nodes = xml2::xml_find_all(rec, "./variable")

    vars = lapply(var_nodes, function(v) {
        pos    = xml2::xml_find_first(v, "./position")
        start  = as.integer(xml2::xml_attr(pos, "start"))
        finish = suppressWarnings(as.integer(xml2::xml_attr(pos, "finish")))
        if (is.na(finish)) finish = start

        name  = node_text(v, "./name")
        width = finish - start + 1L

        # variables spanning several columns (e.g. "multiple") get numbered names
        col_names = if (width == 1L) name else paste(name, seq_len(width), sep = sep)

        val_nodes = xml2::xml_find_all(v, "./values/value")

        label       = node_text(v, "./label")
        codes       = xml2::xml_attr(val_nodes, "code")
        code_labels = xml2::xml_text(val_nodes)

        # replace any non-breaking space characters:
        label       = gsub("&nbsp;?|\u00a0", " ", label)
        code_labels = gsub("&nbsp;?|\u00a0", " ", code_labels)


        list(ident       = xml2::xml_attr(v, "ident"),
             type        = xml2::xml_attr(v, "type"),
             name        = name,
             label       = label,
             start       = start,
             finish      = finish,
             col_names   = col_names,
             codes       = codes,
             code_labels = code_labels
        )
    })

    # build the full vector of csv column names, in column order
    n_cols = max(vapply(vars, function(v) v$finish, integer(1)))
    all_col_names = paste0("V", seq_len(n_cols))
    for (v in vars) {
        all_col_names[v$start:v$finish] = v$col_names
    }

    list(skip = skip, variables = vars, col_names = all_col_names)
}


# Some exports mix encodings: mostly UTF-8, but with stray single-byte
# (windows-1252) characters such as a lone non-breaking space (byte 0xA0).
# This decodes valid UTF-8 as UTF-8 and any other byte as windows-1252, and
# writes a clean UTF-8 copy to a temporary file (also dropping any BOM).
repair_to_utf8 = function(infile) {

    bytes = readBin(infile, what = "raw", n = file.info(infile)$size)
    if (length(bytes) >= 3 && all(bytes[1:3] == as.raw(c(0xEF, 0xBB, 0xBF)))) {
        bytes = bytes[-(1:3)]
    }

    # invalid bytes are replaced by tokens like "<a0>"
    txt = iconv(rawToChar(bytes), from = "UTF-8", to = "UTF-8", sub = "byte")

    # turn each token back into the character it means in windows-1252
    tokens = unique(unlist(regmatches(txt, gregexpr("<[0-9a-fA-F]{2}>", txt))))
    for (tk in tokens) {
        this_byte = as.raw(strtoi(substr(tk, 2, 3), 16L))
        ch = iconv(rawToChar(this_byte), from = "windows-1252", to = "UTF-8")
        if (is.na(ch)) ch = "?"
        txt = gsub(tk, ch, txt, fixed = TRUE)
    }

    outfile = tempfile("utf8_")
    writeBin(charToRaw(enc2utf8(txt)), outfile)
    outfile
}


# Read the csv and use the metadata to convert columns to the right types
read_sss = function(sssFilename, csvFilename, sep = "_") {

    # make clean UTF-8 copies of both files (removed again on exit)
    sss_clean = repair_to_utf8(sssFilename)
    csv_clean = repair_to_utf8(csvFilename)
    on.exit(unlink(c(sss_clean, csv_clean)), add = TRUE)

    meta = read_sss_meta(sss_clean, encoding = "UTF-8", sep = sep)

    # the csv's own column headers (the last skipped line), which may be
    # renamed versions of the .sss names (e.g. "Q0_2" -> "USResident")
    csv_names = meta$col_names
    if (meta$skip >= 1) {
        hdr = read.csv(file = csv_clean, skip = meta$skip - 1, nrows = 1,
                       header = FALSE, colClasses = "character",
                       check.names = FALSE, fileEncoding = "UTF-8")
        hdr = as.character(unlist(hdr[1, ]))
        if (length(hdr) == length(meta$col_names)) {
            blank = is.na(hdr) | !nzchar(trimws(hdr))
            csv_names = ifelse(blank, meta$col_names, hdr)
        } else {
            warning("csv header has ", length(hdr), " fields but .sss describes ",
                    length(meta$col_names), " columns; using .sss names instead")
        }
    }
    if (anyDuplicated(csv_names) > 0) {
        warning("Duplicate column names in csv header: ",
                paste(unique(csv_names[duplicated(csv_names)]), collapse = ", "))
    }

    # read using the (unique) .sss names internally; renamed back at the end
    dat = read.csv(file = csv_clean, skip = meta$skip, header = FALSE,
                   col.names = meta$col_names, colClasses = "character",
                   stringsAsFactors = FALSE, check.names = FALSE,
                   na.strings = character(0), fileEncoding = "UTF-8")

    var_labels  = character(0)
    label_table = list()

    for (v in meta$variables) {

        has_codes = length(v$codes) > 0

        for (cn in v$col_names) {

            if (!(cn %in% names(dat))) next
            x = dat[ ,cn]

            # blank cells are missing for everything except free text
            if (v$type != "character") {
                x[!nzchar(trimws(x))] = NA
            }

            if (has_codes) {
                # factor, with levels in the same order as defined in the .sss
                lvl_labels = v$code_labels
                if (anyDuplicated(lvl_labels) > 0) {
                    # labels not unique, so prefix them with their codes
                    lvl_labels = trimws(paste0(v$codes, " ", lvl_labels))
                }

                # match on numeric value where possible ("01" vs "1"), else on text
                num_codes = suppressWarnings(as.numeric(v$codes))
                if (!anyNA(num_codes)) {
                    idx = match(suppressWarnings(as.numeric(x)), num_codes)
                } else {
                    idx = match(x, v$codes)
                }

                n_bad = sum(!is.na(x) & is.na(idx))
                if (n_bad > 0) {
                    warning(sprintf("Variable '%s': %d value(s) not found in code list; set to NA",
                                    cn, n_bad))
                }

                x = factor(lvl_labels[idx], levels = lvl_labels)
                label_table[[cn]] = setNames(v$codes, lvl_labels)

            } else if (v$type == "quantity") {
                x = suppressWarnings(as.numeric(x))
            }

            dat[ ,cn] = x
            var_labels[cn] = if (is.na(v$label)) "" else v$label
        }
    }

    # mapping table: .sss name, csv column name, question text
    q_text = unname(var_labels[meta$col_names])
    q_text[is.na(q_text)] = ""
    var_map = data.frame(internal_var_name = meta$col_names,
                         var_name = csv_names,
                         label    = q_text,
                         stringsAsFactors = FALSE)

    # restore the original csv column names
    label_names = names(label_table)
    names(dat) = csv_names
    names(label_table) = csv_names[match(label_names, meta$col_names)]

    attr(dat, "var.labels")  = var_map      # internal_var_name / var_name / label
    attr(dat, "label.table") = label_table  # code -> label lookup, named by csv column
    dat
}


# Main entry point: give either a zip file, or an sss file and a csv file
loadIntelliSurveyTripleS = function(zipFilename = NULL,
                                    sssFilename = NULL,
                                    csvFilename = NULL) {

    if (!is.null(zipFilename)) {
        if (!is.null(sssFilename) || !is.null(csvFilename)) {
            stop("If zipFilename is provided, sssFilename and csvFilename should both be omitted")
        }
        if (!file.exists(zipFilename)) {
            stop("Zip file not found: ", zipFilename)
        }

        exdir = tempfile("sss_")
        dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(exdir, recursive = TRUE, force = TRUE), add = TRUE)

        unzip(zipfile = zipFilename, exdir = exdir)
        extracted_files = list.files(exdir, full.names = TRUE, recursive = TRUE)

        sssFilename = extracted_files[grepl("\\.sss$", extracted_files, ignore.case = TRUE)]
        csvFilename = extracted_files[grepl("\\.(asc|csv)$", extracted_files, ignore.case = TRUE)]

        if (length(sssFilename) != 1) {
            stop("Expected exactly one .sss file in the zip, found ", length(sssFilename))
        }
        if (length(csvFilename) != 1) {
            stop("Expected exactly one .asc/.csv file in the zip, found ", length(csvFilename))
        }
    } else if (is.null(sssFilename) || is.null(csvFilename)) {
        stop("If zipFilename is NULL, both sssFilename and csvFilename must be provided")
    }

    read_sss(sssFilename = sssFilename, csvFilename = csvFilename)
}
