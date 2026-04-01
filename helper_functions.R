# helper_functions.R
# Shared utility functions used across multiple analysis and exploration scripts.

# ── Negative binomial regression helpers ─────────────────────────────────────

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

check_poisson <- function(model, threshold = 1.5) {
    od <- sum(residuals(model, type = "pearson")^2) / model$df.residual
    degree  <- dplyr::case_when(od < 1.5 ~ "minimal", od < 3 ~ "mild", od < 10 ~ "moderate", TRUE ~ "severe")
    verdict <- if(od >= threshold) "NB model recommended" else "Poisson likely sufficient"
    cat(sprintf("Poisson overdispersion: %.2f (%s) — %s\n\n", od, degree, verdict))
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

# ── Table formatting helpers ──────────────────────────────────────────────────

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

    csv_filename <- paste0(filename, ".csv")
    frmt_filename <- paste0(filename, "-frmt.rds")
    html_filename <- paste0("html_tables/", filename, ".html")
    png_filename <- paste0(filename, ".png")

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
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, font_size = 16)%>%
    scroll_box(width = "100%", height = "1000px")

    html_text <- as.character(html_text)

    system(paste0("rm ", html_filename))
    writeLines(html_text, html_filename)

    invisible(html_text)
}
