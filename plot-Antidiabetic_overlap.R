# %%
source("workbench_functions.txt")
source("easy_delete_copy_R.txt")
source("my_table1.txt")
notebook_setup()

# %%
load("dte_cohort_wNontreat_data.rds", verbose = TRUE)

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
# Loop over each drug to identify which drug comes first
for(i in 1:nrow(drug_info)){
    this_drug_1 <-  drug_info$drug[i]
    for(j in 1:nrow(drug_info)){
        this_drug_2 <-  drug_info$drug[j]
        if(this_drug_1 == this_drug_2){next}
        var_name_timeline <- paste0(this_drug_1, "_index_precedes_", this_drug_2, "_index")
        var_name_first_drug_record <- ifelse(this_drug_1 == "Nontreatment", "Nontreatment_index", 
                                             paste0(this_drug_1, "_first_drug_record"))
        var_name_first_comp_record <- ifelse(this_drug_2 == "Nontreatment", "Nontreatment_index", 
                                             paste0(this_drug_2, "_first_drug_record"))
        
        this.data <- this.data %>%
        mutate(!!sym(var_name_timeline) := !!sym(var_name_first_drug_record) < !!sym(var_name_first_comp_record))
    }
}

# %%
# Prepare your data
use_matrix <- this.data %>% 
filter(MDD_noBDnoSCH & 
       Meets_Custom_Eligibility_Requirements) %>%
  dplyr::select(Semaglutide_Use, Insulins_Use, Metformin_Use, 
                DPP4i_Use, SGLT2i_Use, SU_Use, TZD_Use, GLP1RA_Use, Nontreatment_Population_for_Semaglutide_vs_Nontreatment) %>% 
  as.matrix
use_matrix <- use_matrix * 1
co_occurrence <- t(use_matrix) %*% use_matrix

co_occurrence

# %%
options(repr.plot.width=25, repr.plot.height=20)
par(mar = c(0,0,0,0))

# Create a custom palette; note we use indices 1:150 rather than 0:150.
my_palette <- libr.pal(200, "LIBR_gradient_pink")[1:175]

# Define custom axis titles (corresponding to the columns/rows in your data)
axis_titles <- c("Semaglutide Use", "Insulin Use", "Metformin Use", 
                 "DPP-4i Use", "SGLT-2i Use", "SU Use", "TZD Use", "Other GLP1-RA Use", "Nontreatment Group")

# Set row and column names of the co-occurrence matrix to match these titles
rownames(co_occurrence) <- axis_titles
colnames(co_occurrence) <- axis_titles

# Create the corrplot.
# (We remove cl.lim and rely on the default zlim; if your corrplot version supports zlim, you can try adding it.)
corrplot(co_occurrence,
         is.corr = FALSE,
         method = "color",     # colored squares
         addCoef.col = "transparent",  # hide automatic numbers
         number.cex = 1.2,     # (not used here)
         tl.col = "black",     # text label color
         tl.cex = 1.5,         # increase axis title size
         tl.srt = 45,          # rotate axis labels for readability
         mar = c(2, 0, 0, 0),  # (bottom, left, top, right)
         cl.pos = "n",         # suppress the built-in legend
         col = my_palette,
         family = "Poppins")

# Add the custom title with mtext() to move it lower
mtext("Lifetime Co-occurrence Counts of Antidiabetic Medications", 
      side = 3,  # Position the title on the top (side = 3)
      line = -15,  # Line 1 means the title is slightly below the top margin
      adj = 0.75,
      cex = 2,    # Adjust title size
      col = "black", 
      family = "Poppins")  # Set title color

# Format the counts with commas
formatted_numbers <- apply(co_occurrence, c(1,2), function(x) format(x, big.mark = ",", scientific = FALSE))

# Overlay the formatted numbers.
n <- ncol(co_occurrence)
for (i in 1:n) {
  for (j in 1:n) {
    # Note: corrplot flips the y-axis.
    text(j, n - i + 1, formatted_numbers[i, j], col = "black", cex = 2, family = "Poppins")
  }
}

# Now, add a custom legend.
# Define tick positions (0 to 55,000 with intervals of 5,000) and format with commas.
ticks <- seq(0, 55000, by = 5000)
tick_labels <- format(ticks, big.mark = ",", scientific = FALSE)

# Retrieve the current plotting region dimensions.
usr <- par("usr")
# Adjust these coordinates as needed so the legend appears within the plotting device.
legend_xl <- usr[2] + 0.2   # left x-coordinate (shift further right if needed)
legend_yb <- usr[3] + 0.5     # bottom y-coordinate
legend_xr <- usr[2] + 0.4   # right x-coordinate (adjust width as needed)
legend_yt <- usr[4] - 3.5     # top y-coordinate

# Draw the custom vertical color legend.
color.legend(legend_xl, legend_yb, legend_xr, legend_yt,
             legend = tick_labels,
             rect.col = my_palette,
             gradient = "y",    # vertical gradient
             align = "rb",      # align legend labels to the right-bottom
             cex = 1.2, 
             family = "Poppins")

# %%
use_matrix <- matrix(NA, nrow = nrow(drug_info), ncol = nrow(drug_info))
rownames(use_matrix) <- 1:nrow(drug_info)
colnames(use_matrix) <- 1:nrow(drug_info)

this.data_filt <- this.data %>% 
filter(MDD_noBDnoSCH & 
       Meets_Custom_Eligibility_Requirements)

for(i in 1:nrow(drug_info)){
    this_drug_1 <-  drug_info$drug[i]
    rownames(use_matrix)[i] <- paste0(this_drug_1, " precedes")
    for(j in 1:nrow(drug_info)){
        this_drug_2 <-  drug_info$drug[j]
        colnames(use_matrix)[j] <- this_drug_2
        if(this_drug_1 == this_drug_2){
            this_col <- ifelse(this_drug_1 == "Nontreatment", "Nontreatment_Population_for_Semaglutide_vs_Nontreatment", paste0(this_drug_1, "_Use"))
            use_matrix[i,j] <- this.data_filt %>% pull(!!sym(this_col)) %>% sum(na.rm = T)
            next
        }
        use_matrix[i,j] <- this.data_filt %>% pull(!!sym(paste0(this_drug_1, "_index_precedes_", this_drug_2, "_index"))) %>% sum(na.rm = T)
        
    }
}

use_matrix

# %%
options(repr.plot.width=25, repr.plot.height=20)
par(mar = c(0,0,0,0))

# Create a custom palette; note we use indices 1:150 rather than 0:150.
my_palette <- libr.pal(200, "LIBR_gradient_pink")[1:175]

# Create the corrplot.
# (We remove cl.lim and rely on the default zlim; if your corrplot version supports zlim, you can try adding it.)
corrplot(use_matrix,
         is.corr = FALSE,
         method = "color",     # colored squares
         addCoef.col = "transparent",  # hide automatic numbers
         number.cex = 1.2,     # (not used here)
         tl.col = "black",     # text label color
         tl.cex = 1.5,         # increase axis title size
         tl.srt = 45,          # rotate axis labels for readability
         mar = c(2, 0, 0, 8),  # (bottom, left, top, right)
         cl.pos = "n",         # suppress the built-in legend
         col = my_palette, 
             family = "Poppins")

# Add the custom title with mtext() to move it lower
mtext("Lifetime Co-occurrence Counts of Antidiabetic Medications", 
      side = 3,  # Position the title on the top (side = 3)
      line = -20,  # Line 1 means the title is slightly below the top margin,
      adj = 0.75,
      cex = 2,    # Adjust title size
      col = "black", 
             family = "Poppins")  # Set title color

# Format the counts with commas
formatted_numbers <- apply(use_matrix, c(1,2), function(x) format(x, big.mark = ",", scientific = FALSE))

# Overlay the formatted numbers.
n <- ncol(use_matrix)
for (i in 1:n) {
  for (j in 1:n) {
    # Note: corrplot flips the y-axis.
    text(j, n - i + 1, formatted_numbers[i, j], col = "black", cex = 2, 
             family = "Poppins")
  }
}

# Now, add a custom legend.
# Define tick positions (0 to 55,000 with intervals of 5,000) and format with commas.
ticks <- seq(0, 55000, by = 5000)
tick_labels <- format(ticks, big.mark = ",", scientific = FALSE)

# Retrieve the current plotting region dimensions.
usr <- par("usr")
# Adjust these coordinates as needed so the legend appears within the plotting device.
legend_xl <- usr[2] + 0.2   # left x-coordinate (shift further right if needed)
legend_yb <- usr[3] + 5     # bottom y-coordinate
legend_xr <- usr[2] + 0.4   # right x-coordinate (adjust width as needed)
legend_yt <- usr[4] - 7.5   # top y-coordinate

# Draw the custom vertical color legend.
color.legend(legend_xl, legend_yb, legend_xr, legend_yt,
             legend = tick_labels,
             rect.col = my_palette,
             gradient = "y",    # vertical gradient
             align = "rb",      # align legend labels to the right-bottom
             cex = 1.2, 
             family = "Poppins")

# %%
