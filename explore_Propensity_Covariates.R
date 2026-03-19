# %% [markdown]
# pre-requisites:
# - get_DTE_cohort.ipynb

# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()
Sys.setenv(OPENSSL_CONF="/dev/null")

# %%
load("dte_cohort_wNontreat_data.rds")
dte_cohort_data <- this.data

# %%
drug_info <- c("Semaglutide",
                        "Insulins", 
                        "Metformin", 
                        "DPP4i", 
                        "SGLT2i",
                        "SU", 
                        "TZD", 
                        "GLP1RA",
                        "Nontreatment"
                        ) %>%
matrix(ncol = 1, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("drug"))

# %%
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
                                        scale_from = c(0,max.pval))#,
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
    IRdisplay::display_html(
        html_text
    )

    system(paste0("rm ", html_filename))
    writeLines(html_text, html_filename)
    #webshot::webshot(html_filename, png_filename)

    cat("\n\n")
    cat(html_filename)
    cat("\n\n")
    cat(png_filename)
}

# %%
varsToFactor <- c(
    "age.survey",
    "age_at_index_years",
    "mdd_to_index_years",
    "sex", 
    "race.ethnicity",
    "Persons_with_Potential_Health_Hazards_Related_to_Socioeconomic_and_Psychosocial_Circumstances",
    "Problems_Related_to_Lifestyle",

    "Type_2_Diabetes_Mellitus_with_Complications", 
    "Obese",
    "Bariatric_Surgery",

    "ADHD",
    "Agoraphobia", 
    "Anxiety_Disorder_NOS", 
    "Generalized_Anxiety", 
    "OCD",
    "Panic_Disorder", 
    "PTSD",
    "Social_Anxiety_Disorder",

    "Alcohol_Abuse",
    "Alcohol_Dependence",
    "Cannabis_Abuse", 
    "Cannabis_Dependence", 
    "Cocaine_Abuse",
    "Cocaine_Dependence",
    "Opioid_Abuse",
    "Opioid_Dependence",
    "Sedative_Abuse",
    "Sedative_Dependence", 
    "Tobacco_Use_Disorder", 

    "Hypertension", 
    "Hypercholesterolemia", 
    "Hyperlipidemia", 
    "Heart_Disease", 
    "Stroke", 
    "Diseases_of_the_Arteries_Artrioles_and_Capillaries",

    "Antidepressants",
    "Average_Total_All_Visits_PerYear", 
    "Average_Total_Outpatient_Visits_PerYear", 
    "Average_Total_Psychologic_or_Psychiatric_Procedure_or_Service_PerYear"
)

new_titles <- c("n", 
                "", 
                "",
                "",
                "Sex", 
                "Race/Ethinicity", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "Lifestyle and SES", 
                "", 
                "Problems Related to Type 2 Diabetes", 
                "", 
                "", 
                "Psychiatric Comorbidities", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "Substance Abuse and Dependence", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "Heart Related Disorders", 
                "", 
                "", 
                "", 
                "", 
                "", 
                "Health System Utilization", 
                "", 
                "", 
                "")

new_names <- c("", 
               "Age (mean (SD))", 
               "Age at Index (mean (SD))",
               "MDD to Index Years (mean (SD))",
               "Male", 
               "", 
               "Asian", 
               "Black", 
               "White", 
               "Hispanic or Latino", 
               "Other", 
               "Persons with Potential Health Hazards Related to Socioeconomic and Psychosocial Circumstances", 
               "Problems Related to Lifestyle", 
               "Type 2 Diabetes Mellitus with Complications", 
               "Obese", 
               "Bariatric Surgery", 
               "ADHD", 
               "Agoraphobia", 
               "Anxiety Disorder NOS", 
               "Generalized Anxiety", 
               "OCD", 
               "Panic Disorder", 
               "PTSD", 
               "Social Anxiety Disorder", 
               "Alcohol Abuse", 
               "Alcohol Dependence", 
               "Cannabis Abuse", 
               "Cannabis Dependence", 
               "Cocaine Abuse", 
               "Cocaine Dependence", 
               "Opioid Abuse", 
               "Opioid Dependence", 
               "Sedative Abuse", 
               "Sedative Dependence", 
               "Tobacco Use Disorder", 
               "Hypertension", 
               "Hypercholesterolemia", 
               "Hyperlipidemia", 
               "Heart Disease", 
               "Stroke", 
               "Diseases of the Arteries Artrioles and Capillaries", 
               "Antidepressants",
               "Average Total All Visits Per Year (mean (SD))", 
               "Average Total Outpatient Visits Per Year (mean (SD))", 
               "Average Total Psychologic or Psychiatric Procedure or Service Per Year (mean (SD))")


for(this_drug in drug_info$drug){

    if(this_drug == "Semaglutide"){next}
    display_markdown(paste("# Semaglutide vs ", this_drug))

    Semaglutide_Population_for_Semaglutide_vs_Drug <- paste0("Semaglutide_Population_for_Semaglutide_vs_", this_drug)
    Drug_Population_for_Semaglutide_vs_Drug <- paste0(this_drug, "_Population_for_Semaglutide_vs_", this_drug)

    varname_semaglutide_age_at_index_years <- "Semaglutide_age_at_index_years"
    varname_thisdrug_age_at_index_years <- paste0(this_drug, "_age_at_index_years")

    varname_semaglutide_mdd_to_index_days <- "Semaglutide_mdd_to_index_days"
    varname_thisdrug_mdd_to_index_days <- paste0(this_drug, "_mdd_to_index_days")

    this.data <- dte_cohort_data %>% 
    filter(!!sym(Semaglutide_Population_for_Semaglutide_vs_Drug) | !!sym(Drug_Population_for_Semaglutide_vs_Drug)) %>%
    mutate(treatment = ifelse(!!sym(Semaglutide_Population_for_Semaglutide_vs_Drug) == TRUE, 1, 0)) %>%
    mutate(age_at_index_years = ifelse(treatment, 
                                       !!sym(varname_semaglutide_age_at_index_years), 
                                       !!sym(varname_thisdrug_age_at_index_years))) %>%
    mutate(mdd_to_index_days = ifelse(treatment,
                                      !!sym(varname_semaglutide_mdd_to_index_days), 
                                      !!sym(varname_thisdrug_mdd_to_index_days)),
          mdd_to_index_years = time_length(ddays(mdd_to_index_days), "years"))

    my_table1(this.data = this.data, 
              my_strata = Drug_Population_for_Semaglutide_vs_Drug, 
              filename = paste0("Semaglutide_vs_", this_drug, "-Basic_Demographics"), 
              varsToFactor = varsToFactor,   
              new_colnames = c("Variable", "", paste0("TOTAL Semaglutide vs ", this_drug, " Population"), "Semaglutide Population", paste0(this_drug, " Population"), "p"),
              verbose = FALSE,
              new_titles = new_titles,
              new_names = new_names
              )
}

# %%

# %%
