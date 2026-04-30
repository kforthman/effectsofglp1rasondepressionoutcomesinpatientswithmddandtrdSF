# helper_functions.R
# Shared utility functions used across multiple analysis and exploration scripts.

# -- Data loading helpers ------------------------------------------------------

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
      if (inherits(dt[[col]], "IDate")) next
      fmt <- specs$format[specs$column == col]
      data.table::set(dt, j = col, value = data.table::as.IDate(dt[[col]], format = fmt))
    }
    for (col in specs$column[specs$type == "factor"]) {
      data.table::set(dt, j = col, value = as.factor(dt[[col]]))
    }
    data.table::setDF(dt)
    tibble::as_tibble(dt, .name_repair = "minimal")
  } else {
    if (is.null(conn))
      stop(sprintf("Table '%s' is configured as a SQL source ('%s') but no database connection was provided. Add a 'database' block to config.json or change this entry to a .csv path.",
                   table_name, source_value))
    DBI::dbGetQuery(conn, sprintf("SELECT * FROM %s", source_value)) %>%
      apply_col_types(col_schema, table_name)
  }
}

# Build a colClasses spec for data.table::fread from the schema.
# ISO dates (%Y-%m-%d) are parsed directly into IDate by fread's C parser,
# avoiding the POSIXlt intermediate that as.Date.character allocates
# (~9x the final size). Non-ISO dates fall back to character and are
# converted afterward.
make_fread_colClasses <- function(schema, table_name) {
  specs <- schema[schema$table == table_name, ]
  is_iso_date <- specs$type == "date" & specs$format == "%Y-%m-%d"
  groups <- list(
    IDate     = specs$column[is_iso_date],
    character = specs$column[specs$type %in% c("character", "factor") |
                              (specs$type == "date" & !is_iso_date)],
    logical   = specs$column[specs$type == "logical"],
    integer   = specs$column[specs$type == "integer"],
    numeric   = specs$column[specs$type == "double"]
  )
  groups[lengths(groups) > 0]
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
  tbl_map <- mapping[mapping$table == table_name, ]
  idx <- match(data$SimpleGenericName, tbl_map$raw_name)
  hit <- !is.na(idx)
  if (any(hit)) {
    data$SimpleGenericName[hit] <- tbl_map$canonical_name[idx[hit]]
  }
  data
}

# Warn if any SimpleGenericName in data has no entry in mapping for table_name.
# Returns a data frame with columns (table, name) listing every missing name.
check_recode <- function(data, mapping, table_name) {
  data_names <- data %>% distinct(SimpleGenericName) %>% pull(SimpleGenericName)
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
    ungroup() %>%
    dplyr::select(n1,n2,TOTAL.freq, TOTAL.rate,minus.freq,minus.rate, plus.freq, plus.rate, p) %>%
    replace(is.na(.), "")
  
  if(!is.null(new_names)){frmt_dataset$n2 <- new_names}
  if(!is.null(new_titles)){frmt_dataset$n1 <- new_titles}
  if(!is.null(new_colnames)){colnames(frmt_dataset) <- c(new_colnames[1:3], "", new_colnames[4], "", new_colnames[5], "", new_colnames[6])}
  
  save(frmt_dataset, file = frmt_filename)
  html_text <- kbl(frmt_dataset, escape = F, align = "r") %>%
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
