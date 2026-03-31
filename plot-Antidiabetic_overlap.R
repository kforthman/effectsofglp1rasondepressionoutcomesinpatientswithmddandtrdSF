this.data <- nontreat_result$dte_cohort_data2
all_groups

# %%
# Loop over each drug to identify which drug comes first
for(i in 1:length(all_groups)){
  this_drug_1 <-  all_groups[i]
  for(j in 1:length(all_groups)){
    this_drug_2 <-  all_groups[j]
    if(this_drug_1 == this_drug_2){next}
    var_name_timeline <- paste0(this_drug_1, "_index_precedes_", this_drug_2, "_index")
    var_name_drug_1_index <- paste0(this_drug_1, "_Index")
    var_name_drug_2_index <- paste0(this_drug_2, "_Index")
    
    this.data <- this.data %>%
      mutate(!!sym(var_name_timeline) := !!sym(var_name_drug_1_index) < !!sym(var_name_drug_2_index))
  }
}

# %%
# Prepare your data
use_matrix <- this.data %>%
  select(paste0(c(all_drugs, nontreatment_group), "_Index")) %>%
  mutate(across(everything(), ~ if_else(is.na(.), 0L, 1L))) %>% 
  as.matrix
co_occurrence <- t(use_matrix) %*% use_matrix

co_occurrence

# %%
options(repr.plot.width=25, repr.plot.height=20)
par(mar = c(0,0,0,0))

# Create a custom palette; note we use indices 1:150 rather than 0:150.
my_palette <- libr.pal(200, "LIBR_gradient_pink")[1:175]

# Define custom axis titles (corresponding to the columns/rows in your data)
axis_titles <- c(paste0(all_drugs, " Use"), paste0(nontreatment_group, " Group"))

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
use_matrix <- matrix(NA, nrow = length(all_groups), ncol = length(all_groups))
rownames(use_matrix) <- 1:length(all_groups)
colnames(use_matrix) <- 1:length(all_groups)

for(i in 1:length(all_groups)){
  this_drug_1 <-  all_groups[i]
  rownames(use_matrix)[i] <- paste0(this_drug_1, " precedes")
  for(j in 1:length(all_groups)){
    this_drug_2 <-  all_groups[j]
    colnames(use_matrix)[j] <- this_drug_2
    if(this_drug_1 == this_drug_2){
      use_matrix[i,j] <- NA
      next
    }
    use_matrix[i,j] <- this.data %>% 
      pull(!!sym(paste0(this_drug_1, "_index_precedes_", this_drug_2, "_index"))) %>% 
      sum(na.rm = T)
    
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
