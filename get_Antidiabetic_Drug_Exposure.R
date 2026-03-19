# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %%
load("ehr_length_noObs_v2.rds")
ehr_length_tab <- ehr_length
#ehr_length_tab %>% head

# %%
all.data.1 <- ehr_length_tab %>% dplyr::select(person_id)

# %%
drug_info <- c("Semaglutide", "Data_Prepped/IDs-Semaglutide-Drug_Exposure.txt",
               "Insulins", "Data_Prepped/IDs-Insulins-Drug_Exposure.txt",
               "Metformin", "Data_Prepped/IDs-Metformin-Drug_Exposure.txt",
               "DPP4i", "Data_Prepped/IDs-DPP_4i-Drug_Exposure.txt",
               "SGLT2i", "Data_Prepped/IDs-SGLT2i-Drug_Exposure.txt",
               "SU", "Data_Prepped/IDs-SU-Drug_Exposure.txt",
               "TZD", "Data_Prepped/IDs-TZD-Drug_Exposure.txt",
               "GLP1RA", "Data_Prepped/IDs-GLP_1RAs_Excluding_Semaglutide-Drug_Exposure.txt"
              ) %>%
matrix(ncol = 2, byrow = TRUE) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
setNames(c("drug", "file_path"))

drug_info

# %%
all.data.2 <- all.data.1

# Create a column to define lifetime antidiabetic drug use
for(i in 1:nrow(drug_info)){
    this_col_name <- paste0(drug_info$drug[i], "_Use")
    
    this_data <- read.csv(drug_info$file_path[i], header = F) %>% 
    rename(person_id = V1) %>% 
    mutate(!!sym(this_col_name) := 1) %>% 
    mutate(person_id = as.character(person_id))
    
    all.data.2 <- all.data.2 %>%
    left_join(this_data, by = "person_id") %>%
    mutate(!!sym(this_col_name) := replace(!!sym(this_col_name), is.na(!!sym(this_col_name)), 0)) %>%
    mutate(!!sym(this_col_name) := as.logical(!!sym(this_col_name)))
    
    var_name <- paste0(drug_info$drug[i], "_id_data")
    paste0("Creating variable ", var_name, "\n") %>% cat
    assign(var_name, this_data)
}

# Create a column to define unique antidiabetic drug use
for(i in 1:nrow(drug_info)){
    this_col_name <- paste0(drug_info$drug[i], "_Use")
    this_col_name_new <- paste0(drug_info$drug[i], "_Use_Only")
    
    other_drugs <- setdiff(paste0(drug_info$drug, "_Use"), this_col_name)
    
    # Build expression: current drug & !(other_drugs OR ...)
    # Example: Semaglutide_Use_Only = Semaglutide_Use & !(Insulins_Use    | Metformin_Use | DPP4i_Use     | SGLT2i_Use | SU_Use     | TZD_Use | GLP1RA_Use)
    all_other_expr <- reduce(map(other_drugs, sym), ~ call("|", .x, .y))  # builds chained OR
    only_expr <- call("&", sym(this_col_name), call("!", all_other_expr))
    
    all.data.2 <- all.data.2 %>%
    mutate(!!sym(this_col_name_new) := !!only_expr)
}

# Create a column to define unique antidiabetic drug use between 2 drugs.
for(i in 1:nrow(drug_info)){
    if(drug_info$drug[i] == "Semaglutide"){next}
    
    other_drug_use_name <- paste0(drug_info$drug[i], "_Use")
    semaglutide_col_name_new <- paste0("Semaglutide_no_", drug_info$drug[i], "_Use")
    other_drug_col_name_new <- paste0(drug_info$drug[i], "_no_Semaglutide_Use")
    
    semaglutide_expr <- call("&", sym("Semaglutide_Use"), call("!", sym(other_drug_use_name)))
    other_drug_expr <- call("&", call("!", sym("Semaglutide_Use")), sym(other_drug_use_name))
    
    all.data.2 <- all.data.2 %>%
    mutate(!!sym(semaglutide_col_name_new) := !!semaglutide_expr,
           !!sym(other_drug_col_name_new) := !!other_drug_expr
           )
    }

# %%
this.data <- all.data.2

# %%
this.data %>% dim
this.data %>% head %>% print_all_cols

# %%
save(this.data, file = "data_DTE_AntidiabeticExposure.rds")

# %%
