# helper_functions.R
# Shared utility functions used across multiple analysis and exploration scripts.

# -- Data loading helpers ------------------------------------------------------

# Map a column_schema.csv "type" value to an Arrow data type.
schema_type_to_arrow <- function(type) {
  switch(type,
         character = utf8(),
         date      = date32(),
         logical   = boolean(),
         integer   = int32(),
         double    = float64(),
         factor    = utf8(),
         stop(sprintf("Unknown schema type: %s", type))
  )
}

# Build an Arrow Schema for one source table from the row-per-column schema
# data frame (Data/column_schema.csv). Returns NULL if the table is not in
# the schema (e.g. reference CSVs under Data/).
make_arrow_schema_csv <- function(col_schema, table_name) {
  specs <- col_schema[col_schema$table == table_name, ]
  if (nrow(specs) == 0) return(NULL)
  fields <- lapply(seq_len(nrow(specs)), function(i) {
    field(specs$column[i], schema_type_to_arrow(specs$type[i]))
  })
  do.call(arrow::schema, fields)
}

# Verify that a file's columns match the schema exactly.
# Stops if any mismatch is found; otherwise prints an OK message.
check_schema <- function(schema, table_name, file_path, na = c("", "NA", "NULL", "null")) {
  file_cols   <- names(read_csv(file_path, n_max = 0, show_col_types = FALSE, na = na))
  schema_cols <- schema[schema$table == table_name, "column"]
  
  extra_in_file   <- setdiff(file_cols,   schema_cols)
  extra_in_schema <- setdiff(schema_cols, file_cols)
  
  msgs <- character(0)
  if (length(extra_in_file) > 0)
    msgs <- c(msgs, sprintf("  In file but missing from schema: %s", paste(extra_in_file,   collapse = ", ")))
  if (length(extra_in_schema) > 0)
    msgs <- c(msgs, sprintf("  In schema but missing from file: %s", paste(extra_in_schema, collapse = ", ")))
  
  if (length(msgs) > 0)
    stop(sprintf("Schema mismatch for '%s':\n%s", table_name, paste(msgs, collapse = "\n")))
  
  message(sprintf("Schema OK: %s (%d columns)", table_name, length(schema_cols)))
  invisible(TRUE)
}

# Verify that a SQL table's columns match the schema exactly.
# Uses SELECT TOP 0 to avoid fetching rows.
check_schema_sql <- function(schema, table_name, conn, sql_table) {
  db_cols     <- names(DBI::dbGetQuery(conn, sprintf("SELECT TOP 0 * FROM %s", sql_table)))
  schema_cols <- schema[schema$table == table_name, "column"]
  
  extra_in_db     <- setdiff(db_cols,     schema_cols)
  extra_in_schema <- setdiff(schema_cols, db_cols)
  
  msgs <- character(0)
  if (length(extra_in_db) > 0)
    msgs <- c(msgs, sprintf("  In DB but missing from schema: %s",     paste(extra_in_db,     collapse = ", ")))
  if (length(extra_in_schema) > 0)
    msgs <- c(msgs, sprintf("  In schema but missing from DB: %s", paste(extra_in_schema, collapse = ", ")))
  
  if (length(msgs) > 0)
    stop(sprintf("Schema mismatch for '%s':\n%s", table_name, paste(msgs, collapse = "\n")))
  
  message(sprintf("Schema OK: %s (%d columns)", table_name, length(schema_cols)))
  invisible(TRUE)
}

# Coerce a data frame's columns to R types defined in the schema.
# character columns have na_strings replaced with NA. Dates are coerced via
# as.Date() which handles character, Date, and POSIXct inputs uniformly.
apply_col_types <- function(data, schema, table_name, na_strings = c("", "NA", "NULL", "null", "*Unspecified")) {
  specs <- schema[schema$table == table_name, ]
  for (i in seq_len(nrow(specs))) {
    col  <- specs$column[i]
    type <- specs$type[i]
    if (!col %in% names(data)) next
    data[[col]] <- switch(type,
                          character = { x <- as.character(data[[col]]); x[x %in% na_strings] <- NA_character_; x },
                          date      = as.Date(data[[col]]),
                          logical   = as.logical(data[[col]]),
                          integer   = as.integer(data[[col]]),
                          double    = as.double(data[[col]])
    )
  }
  data
}

# Detect whether a config$files entry refers to a CSV file path versus a SQL
# object name. A value ending in .csv or containing a path separator is treated
# as a file path; anything else (e.g. "dbo.MedicationTable") is treated as SQL.
is_csv_source <- function(value) {
  grepl("\\.csv$", value, ignore.case = TRUE) || grepl("[/\\\\]", value)
}

# Dispatch schema check to CSV or SQL version per-table based on the value of
# config$files[[table_name]]. SQL sources require a non-NULL conn.
check_schema_table <- function(schema, table_name, config, conn = NULL) {
  source_value <- config$files[[table_name]]
  if (is_csv_source(source_value)) {
    check_schema(schema, table_name, source_value)
  } else {
    if (is.null(conn))
      stop(sprintf("Table '%s' is configured as a SQL source ('%s') but no database connection was provided. Add a 'database' block to config.json or change this entry to a .csv path.",
                   table_name, source_value))
    check_schema_sql(schema, table_name, conn, source_value)
  }
}

# Read a table from either a SQL database or a CSV file per-table based on the
# value of config$files[[table_name]], returning a typed data frame. SQL
# sources require a non-NULL conn. CSV sources are read with data.table::fread
# for speed, then date and factor columns are coerced using the schema.
read_table <- function(config, col_schema, table_name, conn = NULL) {
  source_value <- config$files[[table_name]]
  if (is_csv_source(source_value)) {
    specs <- col_schema[col_schema$table == table_name, ]
    dt <- data.table::fread(
      source_value,
      colClasses   = make_fread_colClasses(col_schema, table_name),
      na.strings   = c("", "NA", "NULL", "null", "*Unspecified"),
      showProgress = TRUE
    )
    for (col in specs$column[specs$type == "date"]) {
      fmt <- specs$format[specs$column == col]
      data.table::set(dt, j = col, value = as.Date(dt[[col]], format = fmt))
    }
    for (col in specs$column[specs$type == "factor"]) {
      data.table::set(dt, j = col, value = as.factor(dt[[col]]))
    }
    # fread refuses to downcast auto-detected int32 (0/1) columns to logical via
    # colClasses, so logical columns are read as integer and coerced here.
    for (col in specs$column[specs$type == "logical"]) {
      data.table::set(dt, j = col, value = as.logical(dt[[col]]))
    }
    tibble::as_tibble(dt)
  } else {
    if (is.null(conn))
      stop(sprintf("Table '%s' is configured as a SQL source ('%s') but no database connection was provided. Add a 'database' block to config.json or change this entry to a .csv path.",
                   table_name, source_value))
    DBI::dbGetQuery(conn, sprintf("SELECT * FROM %s", source_value)) %>%
      apply_col_types(col_schema, table_name)
  }
}

# Build a named list of colClasses for data.table::fread from the schema.
# Dates are read as character so that format strings can be applied afterward.
make_fread_colClasses <- function(schema, table_name) {
  specs <- schema[schema$table == table_name, ]
  type_map <- c(
    character = "character",
    date      = "character",
    logical   = "integer",
    integer   = "integer",
    double    = "numeric",
    factor    = "character"
  )
  cc <- unname(type_map[specs$type])
  names(cc) <- specs$column
  cc
}

# Build a readr cols() spec from a row-per-column schema data frame.
# schema must have columns: table, column, type, format
make_col_types <- function(schema, table_name) {
  specs <- schema[schema$table == table_name, ]
  col_list <- lapply(seq_len(nrow(specs)), function(i) {
    switch(specs$type[i],
           character = col_character(),
           date      = col_date(format = specs$format[i]),
           logical   = col_logical(),
           integer   = col_integer(),
           double    = col_double(),
           factor    = col_factor()
    )
  })
  names(col_list) <- specs$column
  do.call(cols, col_list)
}

# Build a recoded SimpleGenericName column from a row-per-mapping recode data frame.
# mapping must have columns: table, raw_name, canonical_name
apply_recode <- function(data, mapping, table_name) {
  tbl_map <- mapping[mapping$table == table_name, c("raw_name", "canonical_name")]
  data %>%
    left_join(tbl_map, by = c("SimpleGenericName" = "raw_name")) %>%
    mutate(SimpleGenericName = dplyr::coalesce(canonical_name, SimpleGenericName)) %>%
    dplyr::select(-canonical_name)
}

# Warn if any SimpleGenericName in data has no entry in mapping for table_name.
# Returns a data frame with columns (table, name) listing every missing name.
check_recode <- function(data_names, mapping, table_name) {
  map_names  <- mapping %>% filter(table == table_name) %>% pull(raw_name)
  missing    <- sort(setdiff(data_names, map_names))
  if(length(missing) > 0){
    out_filename <- str_glue("OutputData/missing_recodes-{table_name}.csv")
    warning(sprintf("check_recode [%s]: %d name(s) have no recode entry, missing name(s) will be written to \"%s\":\n  %s",
                    table_name, length(missing),
                    out_filename,
                    paste(missing, collapse = "\n  ")))
    missing_recodes <- data.frame(table = table_name, raw_name = missing, stringsAsFactors = FALSE)
    write.csv(missing_recodes, out_filename, row.names = FALSE)
  }
}

# -- Negative binomial regression helpers -------------------------------------

interpret_nb <- function(model, term, outcome, comparison, alpha = 0.05, digits = 2) {
  coefs <- summary(model)$coefficients
  
  if (!term %in% rownames(coefs)) {
    stop(sprintf("Term '%s' not found in model.", term))
  }
  
  est   <- coefs[term, "Estimate"]
  se    <- coefs[term, "Std. Error"]
  ci_level <- 1 - alpha
  z     <- qnorm(1 - alpha/2)
  lo    <- est - z * se
  hi    <- est + z * se
  
  rr    <- exp(est)
  rr_lo <- exp(lo)
  rr_hi <- exp(hi)
  pval  <- coefs[term, "Pr(>|z|)"]
  
  sig_txt <- ifelse(pval < alpha, "This difference is statistically significant", "This difference is not statistically significant")
  
  direction <- ifelse(rr < 1, "lower", "higher")
  pct_diff  <- abs(1 - rr) * 100
  
  cat(
    sprintf("Estimated rate ratio: %0.*f (%d%% CI %0.*f to %0.*f)\n",
            digits, rr, round(ci_level * 100), digits, rr_lo, digits, rr_hi),
    sprintf(
      "Interpretation: Patients in the group with Semaglutide treatment had about %0.*f%% %s rate of %s compared with %s treatment, adjusting for other variables.\n",
      digits,
      pct_diff,
      direction,
      outcome,
      comparison
    ),
    sprintf("p-value: %0.4f. %s at alpha = %s.\n", pval, sig_txt, alpha),
    sep = ""
  )
}

check_od <- function(model, threshold = 0.05) {
  od <- check_overdispersion(model)
  if (od$p_value < threshold)
    cat(sprintf("Overdispersed: ratio = %.2f, p = %.4f\n", od$dispersion_ratio, od$p_value))
  else
    cat(sprintf("Not overdispersed: ratio = %.2f, p = %.4f\n", od$dispersion_ratio, od$p_value))
}

check_mc <- function(model, vif_threshold = 5) {
  cc <- check_collinearity(model)
  max_vif <- max(cc$VIF, na.rm = TRUE)
  if (max_vif >= vif_threshold)
    cat(sprintf("Multicollinearity detected: max VIF = %.2f (%s)\n", max_vif, cc$Term[which.max(cc$VIF)]))
  else
    cat(sprintf("No multicollinearity: max VIF = %.2f\n", max_vif))
}

sig_code <- function(p) {
  dplyr::case_when(
    p <  0.001 ~ "\\*\\*\\*",
    p <  0.01  ~ "\\*\\*",
    p <  0.05  ~ "\\*",
    p <  0.1   ~ ".",
    TRUE       ~ ""
  )
}

interpret_pwp <- function(model, term, outcome, comparison, alpha = 0.05, digits = 2) {
  coefs <- summary(model)$coefficients
  
  if (!term %in% rownames(coefs)) {
    stop(sprintf("Term '%s' not found in model.", term))
  }
  
  hr   <- coefs[term, "exp(coef)"]
  pval <- coefs[term, "Pr(>|z|)"]
  
  # Use robust SE when cluster() was specified, otherwise model-based SE
  se <- if ("robust se" %in% colnames(coefs)) coefs[term, "robust se"] else coefs[term, "se(coef)"]
  
  ci_level <- 1 - alpha
  z_crit   <- qnorm(1 - alpha / 2)
  hr_lo    <- exp(log(hr) - z_crit * se)
  hr_hi    <- exp(log(hr) + z_crit * se)
  
  direction <- ifelse(hr < 1, "lower", "higher")
  pct_diff  <- abs(1 - hr) * 100
  sig_txt   <- ifelse(pval < alpha,
                      "This difference is statistically significant",
                      "This difference is not statistically significant")
  
  cat(
    sprintf("Estimated hazard ratio: %0.*f (%d%% CI %0.*f to %0.*f)\n",
            digits, hr, round(ci_level * 100), digits, hr_lo, digits, hr_hi),
    sprintf(
      "Interpretation: Patients with Semaglutide treatment had about %0.*f%% %s hazard of %s events compared with %s.\n",
      digits, pct_diff, direction, outcome, comparison
    ),
    sprintf("p-value: %0.4f. %s at alpha = %s.\n", pval, sig_txt, alpha),
    sep = ""
  )
}

check_poisson <- function(model, threshold = 1.5) {
  od <- sum(residuals(model, type = "pearson")^2) / model$df.residual
  degree  <- dplyr::case_when(od < 1.5 ~ "minimal", od < 3 ~ "mild", od < 10 ~ "moderate", TRUE ~ "severe")
  verdict <- if(od >= threshold) "NB model recommended" else "Poisson likely sufficient"
  cat(sprintf("Poisson overdispersion: %.2f (%s) - %s\n\n", od, degree, verdict))
}

my_cut <- function(my_value, range, my_min = 1) {
  if(is.na(my_value)){return("NA")}
  if(my_value == 0){return("0")}
  
  # Basic checks
  stopifnot(is.numeric(my_value),
            length(range) == 1,
            is.numeric(range),
            is.finite(range),
            range > 0)
  
  if(my_value > 0){
    lower <- floor((my_value - 1) / range) * range + 1
    upper <- lower + range - 1
    if(my_min != 1 & lower == 1){lower <- my_min}
    
    if(upper == lower){return(as.character(lower))}
    return(paste0(lower, " - ", upper))
  }
  if(my_value < 0){
    upper <- ceiling((my_value + 1) / range) * range - 1
    lower <- upper - range + 1
    if(upper == lower){return(as.character(lower))}
    return(paste0(lower, " - ", upper))
  }
}

# -- Table formatting helpers --------------------------------------------------

# Decide how a Table 1 row's TOTAL / minus / plus counts should be *displayed*
# in the shared (HTML) table so that no patient count under `threshold`
# (i.e. <= 10) is ever revealed, either directly or by back-calculation from
# the row total. The raw counts are still written unmasked to the CSV/RDS
# exports; only the HTML display is masked.
#
# Rules (see my_table1):
#   * TOTAL <= 20: show TOTAL as "<= 20"; each group is "<= 10" if under 11,
#     otherwise "<= 20".
#   * TOTAL > 20 with exactly one group under 11: round TOTAL to the nearest
#     ten and show "~<rounded>"; the small group is "<= 10"; the other group
#     mirrors the TOTAL display ("~<rounded>") so the small count can't be
#     recovered by subtraction.
#   * Otherwise: no masking (returns NULL).
#
# Percents are recomputed from the *displayed* number over that column's
# sample size (`*_N`) and shown as "~<pct>%". `total_N` / `minus_N` / `plus_N`
# are the overall and per-strata sample sizes (from the "n" row).
#
# Returns list(TOTAL = list(count, pct), minus = ..., plus = ...) where `count`
# is a ready-to-display string and `pct` is the numeric percent (NA to suppress
# it), or NULL when the row needs no masking.
mask_table1_row <- function(total_n, minus_n, plus_n,
                            total_N, minus_N, plus_N, threshold = 11) {
  
  minus_small <- minus_n < threshold
  plus_small  <- plus_n  < threshold
  
  # No masking: total clears the small-total ceiling and neither group is small.
  if (total_n > 20 && !minus_small && !plus_small) return(NULL)
  
  # Percent of the displayed number over this column's sample size (1 decimal).
  disp_pct <- function(num, denom) {
    if (is.na(denom) || denom <= 0) return(NA_real_)
    round(num / denom * 100, 1)
  }
  le10 <- function(denom) list(count = "<= 10", pct = disp_pct(10, denom))
  le20 <- function(denom) list(count = "<= 20", pct = disp_pct(20, denom))
  
  if (total_n <= 20) {
    return(list(
      TOTAL = le20(total_N),
      minus = if (minus_small) le10(minus_N) else le20(minus_N),
      plus  = if (plus_small)  le10(plus_N)  else le20(plus_N)
    ))
  }
  
  # TOTAL > 20 with exactly one group under 11: round the total and mirror it
  # into the non-small group; the small group shows "<= 10".
  rounded    <- round(total_n / 10) * 10
  total_disp <- paste0("~", scales::comma(rounded))
  matched    <- function(denom) list(count = total_disp, pct = disp_pct(rounded, denom))
  
  list(
    TOTAL = list(count = total_disp, pct = disp_pct(rounded, total_N)),
    minus = if (minus_small) le10(minus_N) else matched(minus_N),
    plus  = if (plus_small)  le10(plus_N)  else matched(plus_N)
  )
}

number_to_viridis <- function(x) {
  x <- as.numeric(x)
  # Ensure input is numeric
  if (x < 0 | x > 100) {
    stop("Input must be between 0 and 100.")
  }
  
  # Normalize non-NA values to [0, 1]
  normalized <- x / 100
  
  # Create a palette of 256 colors on the viridis scale
  palette <- viridis(256)
  
  # Map normalized values to indices between 1 and 256
  indices <- round(normalized * 225) + 1
  
  # Assign the corresponding colors
  palette[indices]
}

my_table1 <- function(this.data, my_strata, filename, varsToFactor, new_names = NULL, new_titles = NULL, new_colnames = NULL, verbose = FALSE){
  
  csv_filename <- paste0("OutputData/", filename, ".csv")
  frmt_filename <- paste0("OutputData/", filename, "-frmt.rds")
  html_filename <- paste0("html_tables/", filename, ".html")
  png_filename <- paste0("html_tables/", filename, ".png")
  
  tableOne <- CreateTableOne(vars = varsToFactor, strata = my_strata,
                             data = this.data,
                             addOverall = T)
  tab3Mat <- print(tableOne, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
  tab3Mat <- as.data.frame(tab3Mat)
  
  tab3Mat <- tab3Mat %>% mutate(p = replace(p, p == NaN, ""),
                                p = replace(p, p == "NA", ""),
                                p_num = ifelse(p == "<0.001", 0, p))
  
  tab3Mat <- tab3Mat %>% mutate(p_num = as.numeric(p_num))
  
  if(verbose){
    cat("Original row names: \"")
    cat(paste(rownames(tab3Mat), collapse = "\", \""))
    cat("\"")
  }
  
  tab3Mat$nn <- rownames(tab3Mat)
  tab3Mat$title <- rownames(tab3Mat)
  row.names(tab3Mat) <- 1:nrow(tab3Mat)
  colnames(tab3Mat) <- c("TOTAL", "minus", "plus","p","test", "p_num", "n2", "n1")
  
  frmt_dataset <- tab3Mat
  # Format table for readability
  
  # -- Mask small patient counts for the shared (HTML) table -----------------
  # Decide, per row, how the TOTAL/minus/plus counts should be displayed so no
  # count under `mask_threshold` (<= 10) is disclosed. The raw counts are left
  # untouched in tab3Mat (CSV) and frmt_dataset (RDS); only the HTML rendering
  # below uses these decisions. Continuous (mean (SD)) rows are left alone.
  # mask_decisions[[i]] is NULL when row i needs no masking.
  mask_threshold <- 11
  is_continuous  <- grepl("mean\\ \\(SD", tab3Mat$n2)
  extract_count  <- function(x) suppressWarnings(as.numeric(gsub("^$|^(\\d+).*|.*", "\\1", x)))
  
  n_row   <- which(tab3Mat$n2 == "n")
  total_N <- NA_real_; minus_N <- NA_real_; plus_N <- NA_real_
  if (length(n_row) == 1) {
    total_N <- extract_count(tab3Mat$TOTAL[n_row])
    minus_N <- extract_count(tab3Mat$minus[n_row])
    plus_N  <- extract_count(tab3Mat$plus[n_row])
  }
  
  total_n <- extract_count(tab3Mat$TOTAL)
  minus_n <- extract_count(tab3Mat$minus)
  plus_n  <- extract_count(tab3Mat$plus)
  
  mask_decisions <- vector("list", nrow(tab3Mat))
  for (i in seq_len(nrow(tab3Mat))) {
    if (is_continuous[i] || is.na(total_n[i]) || is.na(minus_n[i]) || is.na(plus_n[i])) next
    d <- mask_table1_row(total_n[i], minus_n[i], plus_n[i],
                         total_N, minus_N, plus_N, mask_threshold)
    # Assigning NULL via [[<- would drop the slot and shrink the list; keep the
    # pre-allocated NULL in place for rows that need no masking.
    if (!is.null(d)) mask_decisions[[i]] <- d
  }
  
  # The CSV export keeps the raw counts (tab3Mat is left unmasked); masking is
  # applied only to the HTML rendering built from frmt_masked below.
  
  if(!is.null(new_names)){tab3Mat$nn <- new_names}
  if(!is.null(new_titles)){tab3Mat$title <- new_titles}
  if(!is.null(new_colnames)){colnames(tab3Mat) <- new_colnames}
  
  write.csv(tab3Mat, csv_filename, row.names = F)
  
  # Format for HTML
  max.pval <- 0.2 # Maximum p-value (for color scale)
  for(i in 1:nrow(frmt_dataset)){
    if(is.na(frmt_dataset$p_num[i])){frmt_dataset$p[i] <- "";next}
    if(frmt_dataset$p_num[i] < 0.05){ # if entry is statistically significant
      frmt_dataset$p[i] <- cell_spec(
        format(frmt_dataset$p[i], nsmall = 6),
        bold = T,
        color = "black",
        background = spec_color(frmt_dataset$p_num[i],
                                begin = 0, end = 0.9, option = "magma", direction = -1,
                                scale_from = c(0,max.pval))
      )
    }else{ # if entry is not significant
      frmt_dataset$p[i] <- cell_spec(
        format(frmt_dataset$p[i], nsmall = 6),
        bold = T,
        color = "black"
      )
    }
  }
  
  frmt_dataset <- frmt_dataset %>%
    mutate(across(all_of(c("TOTAL", "minus", "plus")),
                  ~ifelse(grepl("mean\\ \\(SD", n2), gsub("^([0-9.]+).*|.*", "\\1", .), comma(as.numeric(gsub("^$|^(\\d+).*|.*", "\\1", .)))),
                  .names = "{.col}.freq")) %>%
    mutate(across(all_of(c("TOTAL", "minus", "plus")),
                  ~gsub("^$|.*\\(([0-9.]+)\\).*|.*", "\\1", .),
                  .names = "{.col}.rate")) %>%
    rowwise() %>%
    mutate(across(all_of(c("TOTAL.rate", "minus.rate", "plus.rate")),
                  ~ifelse(. == "", .,
                          ifelse(grepl("mean\\ \\(SD", n2),
                                 paste0("(", ., ")"),
                                 cell_spec(paste0(.,"%"), color = "white", background = number_to_viridis(.))
                          )
                  ),
                  .names = "{.col}")) %>%
    ungroup()
  
  # Build a masked copy for the shared HTML table, overwriting the small-count
  # cells with their masked display ("<= 10" / "<= 20" / "~<rounded>") and a
  # recomputed "~<pct>%" (displayed number over the column's sample size).
  # frmt_dataset itself is left unmasked for the RDS export.
  frmt_masked <- frmt_dataset
  masked_rate <- function(pct) {
    if (is.na(pct)) return("")
    cell_spec(paste0("~", format(pct, nsmall = 1), "%"),
              color = "white",
              background = number_to_viridis(min(max(pct, 0), 100)))
  }
  for (i in seq_len(nrow(frmt_masked))) {
    d <- mask_decisions[[i]]
    if (is.null(d)) next
    frmt_masked$TOTAL.freq[i] <- d$TOTAL$count
    frmt_masked$TOTAL.rate[i] <- masked_rate(d$TOTAL$pct)
    frmt_masked$minus.freq[i] <- d$minus$count
    frmt_masked$minus.rate[i] <- masked_rate(d$minus$pct)
    frmt_masked$plus.freq[i]  <- d$plus$count
    frmt_masked$plus.rate[i]  <- masked_rate(d$plus$pct)
  }
  
  # Select the display columns and apply optional relabelling to both the
  # unmasked (RDS) and masked (HTML) versions.
  finalize_frmt <- function(df) {
    df <- df %>%
      dplyr::select(n1, n2, TOTAL.freq, TOTAL.rate, minus.freq, minus.rate, plus.freq, plus.rate, p) %>%
      replace(is.na(.), "")
    if(!is.null(new_names)){df$n2 <- new_names}
    if(!is.null(new_titles)){df$n1 <- new_titles}
    if(!is.null(new_colnames)){colnames(df) <- c(new_colnames[1:3], "", new_colnames[4], "", new_colnames[5], "", new_colnames[6])}
    df
  }
  
  frmt_dataset <- finalize_frmt(frmt_dataset)  # unmasked -> RDS export
  frmt_masked  <- finalize_frmt(frmt_masked)   # masked   -> HTML display
  
  save(frmt_dataset, file = frmt_filename)
  html_text <- kbl(frmt_masked, escape = F, align = "r") %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = TRUE, font_size = 16)
  
  html_text <- as.character(html_text)
  
  system(paste0("rm ", html_filename))
  writeLines(html_text, html_filename)
  
  invisible(html_text)
}

# -- Sensitivity analysis helpers ----------------------------------------------

# Print IRR + 95% CI + p-value for the treatment x modifier interaction term
summarize_interaction <- function(model, mod_label,
                                  trt_term = "treatment_nameSemaglutide",
                                  alpha = 0.05, digits = 2) {
  coefs    <- summary(model)$coefficients
  int_rows <- rownames(coefs)[grepl(paste0("^", trt_term, ":"), rownames(coefs))]
  if (length(int_rows) == 0) {
    cat("  Interaction term not found.\n")
    return(invisible(NULL))
  }
  term <- int_rows[1]
  est  <- coefs[term, "Estimate"]
  se   <- coefs[term, "Std. Error"]
  z    <- qnorm(1 - alpha / 2)
  rr   <- exp(est); lo <- exp(est - z * se); hi <- exp(est + z * se)
  pval <- coefs[term, "Pr(>|z|)"]
  sig  <- if (pval < alpha) " *" else ""
  cat(sprintf("  Interaction (%s): IRR = %.2f (95%% CI %.2f-%.2f), p = %.4f%s\n",
              mod_label, rr, lo, hi, pval, sig))
}

# Print stratum-specific treatment IRRs from an emmeans contrast object
summarize_strata <- function(emm_contrast, mod_var, digits = 2) {
  cs <- summary(emm_contrast, infer = c(TRUE, TRUE))
  for (j in seq_len(nrow(cs))) {
    cat(sprintf("    %s = %-5s  IRR = %.2f (95%% CI %.2f-%.2f), p = %.4f\n",
                mod_var, as.character(cs[[mod_var]][j]),
                cs$ratio[j], cs$asymp.LCL[j], cs$asymp.UCL[j], cs$p.value[j]))
  }
}

stat_smd <- function(var, treat, data) {
  z <- data[[treat]]
  x <- data[[var]]
  xT <- x[z == 1]
  xC <- x[z == 0]
  m0 <- mean(xC)
  m1 <- mean(xT)
  s_pooled <- sqrt((sd(xC)^2 + sd(xT)^2) / 2)
  return((m1 - m0) / s_pooled)
}

stat_pval <- function(var, treat, data) {
  z <- data[[treat]]
  x <- data[[var]]
  xT <- x[z == 1]
  xC <- x[z == 0]
  if (is.numeric(x)) {
    test_obj <- try(t.test(xT, xC), silent = TRUE)
    if (inherits(test_obj, "htest")) test_obj$p.value else NA_real_
  } else {
    tab     <- table(x, z)
    chi_obj <- try(chisq.test(tab, correct = FALSE), silent = TRUE)
    if (inherits(chi_obj, "htest") && all(chi_obj$expected >= 5)) {
      chi_obj$p.value
    } else {
      fish_obj <- try(fisher.test(tab), silent = TRUE)
      if (inherits(fish_obj, "htest")) fish_obj$p.value else NA_real_
    }
  }
}

std_diff <- function(var, treat, data) {
  z <- data[[treat]]
  x <- data[[var]]
  xT <- x[z == 1]
  xC <- x[z == 0]
  if (is.numeric(x)) {
    s_pooled <- sqrt((sd(xT, na.rm = TRUE)^2 + sd(xC, na.rm = TRUE)^2) / 2)
    if (s_pooled == 0) return(NA_real_)
    return((mean(xT, na.rm = TRUE) - mean(xC, na.rm = TRUE)) / s_pooled)
  } else {
    pT     <- mean(as.numeric(xT == levels(x)[2]), na.rm = TRUE)
    pC     <- mean(as.numeric(xC == levels(x)[2]), na.rm = TRUE)
    p_pool <- (pT * length(xT) + pC * length(xC)) / (length(xT) + length(xC))
    denom  <- sqrt(p_pool * (1 - p_pool))
    if (denom == 0) return(NA_real_)
    return((pT - pC) / denom)
  }
}

stat_ks <- function(var, treat, data) {
  z <- data[[treat]]
  x <- data[[var]]
  xT <- x[z == 1]
  xC <- x[z == 0]
  if (is.numeric(x)) {
    ks_obj <- try(suppressWarnings(ks.test(xT, xC)), silent = TRUE)
  } else {
    ks_obj <- try(suppressWarnings(ks.test(as.numeric(factor(xT)),
                                           as.numeric(factor(xC)))), silent = TRUE)
  }
  if (inherits(ks_obj, "htest")) {
    list(ks_stat = as.numeric(ks_obj$statistic), ks_pval = ks_obj$p.value)
  } else {
    list(ks_stat = NA_real_, ks_pval = NA_real_)
  }
}