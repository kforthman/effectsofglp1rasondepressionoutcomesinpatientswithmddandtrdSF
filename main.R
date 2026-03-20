library(rmarkdown)

comparator_drugs <- c("DPP4i", "GLP1RA", "Insulins", "Metformin",
                      "Nontreatment", "SGLT2i", "SU", "TZD")

for (drug in comparator_drugs) {
    message("Rendering report for: ", drug)
    render(
        input       = "report_Propensity_Scoring.Rmd",
        output_file = paste0("report_Propensity_Scoring-SemaglutideVs", drug, ".html"),
        params      = list(
            target_drug     = "Semaglutide",
            comparator_drug = drug,
            cohort_file     = "dte_cohort_wNontreat_data.rds",
            covariates_file = "ps_covariates.csv"
        ),
        envir = new.env()
    )
}
