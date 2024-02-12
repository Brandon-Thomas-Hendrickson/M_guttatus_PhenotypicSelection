# Install the package
install.packages(c("ggplot2","geepack", "readr", "sjPlot", "MASS", "stats","emmeans","wesanderson","ggpubr","purrr"))

# Load the package
library(readr)
library(sjPlot)
library(MASS)
library(stats)
library(emmeans)
library(wesanderson)
library(ggpubr)
library(ggplot2)
library(purrr)

##############################################################
## Phenotypic Selection Analysis of Traits by Final Density ##
##############################################################

# Load dataframe
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/standardized_data.csv")

# Create the trait List
trait_list <- c("flower_number_1_std", "D2Flowering_std", "pollen_avg_std", "ff_height_std", "total_node_number_std", "prop_early_growth_std", "final_height_std", "veg_weight_std", "leaf_area_std")

# Remove NA from yield
data <- data[!is.na(data$yield), ]

#Make Density a Factor
data$Final_Density <- as.factor(data$Final_Density)

# Run Phenotypic Selection Analysis using yield as the fitness measure and the traits in as predictors
psa_list <- list()
for(trait in trait_list){
    psa <- lm(yield ~ data[[trait]], data = data)
    psa_list[[trait]] <- psa
}

# Run Phenotypic Selection Analysis using yield as the fitness measure and the traits in as predictors
psa_list_FD <- list()
for (density in unique(data$Final_Density)) {
    for (trait in trait_list) {
        psa <- lm(yield ~ data[[trait]], data = data)
        psa_list_FD[[paste(density, trait, sep = "_")]] <- psa
    }
}

# Initialize an empty data frame
results <- data.frame(Trait = character(), Density = character(), Estimate = numeric())

for (density in unique(data$Final_Density)) {
    for (trait in trait_list) {
        psa <- lm(data[data$Final_Density == density, "yield"] ~ data[data$Final_Density == density, trait], data = data[data$Final_Density == density, ])
        psa_list_FD[[paste(density, trait, sep = "_")]] <- psa

        # Extract the model estimate for the trait
        estimate <- coef(psa)[2]

        # Add the result into the data frame
        results <- rbind(results, data.frame(Trait = trait, Density = as.character(density), Estimate = estimate))
    }
}

########################################
## Fittest Plant Phenotypic Selection ##
########################################

# Load dataframe
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/standardized_data_F.csv")

# Create the trait List
trait_list <- c("flower_number_1_std", "D2Flowering_std", "pollen_avg_std", "ff_height_std", "total_node_number_std", "prop_early_growth_std", "final_height_std", "veg_weight_std", "leaf_area_std")

# Remove NA from yield
data <- data[!is.na(data$yield), ]

#Make Density a Factor
data$Final_Density <- as.factor(data$Final_Density)

# Run Phenotypic Selection Analysis using yield as the fitness measure and the traits in as predictors
psa_list_FD <- list()
for (density in unique(data$Final_Density)) {
    for (trait in trait_list) {
        psa <- lm(yield ~ data[[trait]], data = data)
        psa_list_FD[[paste(density, trait, sep = "_")]] <- psa
    }
}

# Initialize an empty data frame
results_F <- data.frame(Trait = character(), Density = character(), Estimate_F = numeric())

for (density in unique(data$Final_Density)) {
    for (trait in trait_list) {
        psa <- lm(data[data$Final_Density == density, "yield"] ~ data[data$Final_Density == density, trait], data = data[data$Final_Density == density, ])
        psa_list_FD[[paste(density, trait, sep = "_")]] <- psa

        # Extract the model estimate for the trait
        estimate <- coef(psa)[2]

        # Add the result into the data frame
        results_F <- rbind(results_F, data.frame(Trait = trait, Density = as.character(density), Estimate_F = estimate))
    }
}

##########################################
## Least Fit Plant Phenotypic Selection ##
##########################################

# Load dataframe
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/standardized_data_LF.csv")

# Create the trait List
trait_list <- c("flower_number_1_std", "D2Flowering_std", "pollen_avg_std", "ff_height_std", "total_node_number_std", "prop_early_growth_std", "final_height_std", "veg_weight_std", "leaf_area_std")

# Remove NA from yield
data <- data[!is.na(data$yield), ]

#Make Density a Factor
data$Final_Density <- as.factor(data$Final_Density)

# Run Phenotypic Selection Analysis using yield as the fitness measure and the traits in as predictors
psa_list_LFD <- list()
for (density in unique(data$Final_Density)) {
    for (trait in trait_list) {
        psa <- lm(yield ~ data[[trait]], data = data)
        psa_list_LFD[[paste(density, trait, sep = "_")]] <- psa
    }
}

# Initialize an empty data frame
results_LF <- data.frame(Trait = character(), Density = character(), Estimate_LF = numeric())

for (density in unique(data$Final_Density)) {
    for (trait in trait_list) {
        psa <- lm(data[data$Final_Density == density, "yield"] ~ data[data$Final_Density == density, trait], data = data[data$Final_Density == density, ])
        psa_list_LFD[[paste(density, trait, sep = "_")]] <- psa

        # Extract the model estimate for the trait
        estimate <- coef(psa)[2]

        # Add the result into the data frame
        results_LF <- rbind(results_LF, data.frame(Trait = trait, Density = as.character(density), Estimate_LF = estimate))
    }
}

# Merge the results data frames
results_big <- merge(results, results_F, by = c("Trait", "Density"))
results_big <- merge(results_big, results_LF, by = c("Trait", "Density"))

# Write out the results to a CSV file
write.csv(results_big, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/phenotype_selection.csv", row.names = FALSE)

# Split the results data frame by 'Trait'
results_split <- split(results_big, results_big$Trait)

# Get the number of unique traits
num_traits <- length(unique(results_big$Trait))

#Y labels list 
y_labels <- c("Flowering Date", "Height @ Flower", "Final Height", "# Flowers", "Leaf Area","Pollen Number", "Early Growth", "Node Number","Biomass")
# Create a ggplot for each trait
plots <- lapply(1:num_traits, function(i) {
    data <- results_split[[i]]
    p <- ggplot(data, aes(x = Density)) +
        geom_point(aes(y = Estimate), color = "black") +
        geom_point(aes(y = Estimate_F), color = "red") +
        geom_point(aes(y = Estimate_LF), color = "blue") +
        geom_line(aes(y = Estimate), group = 1, color = "black") +
        geom_line(aes(y = Estimate_F), group = 1, color = "red") +
        geom_line(aes(y = Estimate_LF), group = 1, color = "blue") +
        labs(x = NULL, y = y_labels[i]) +
        theme_light() +
        theme(
            text = element_text(family = "Times New Roman", size = 12),
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)
        ) + 
        scale_y_continuous(labels = function(x) format(round(x, 3)), breaks = function(x) pretty(x, n = 5))
    # Add the x-axis label for the last three plots
    if(i > num_traits - 3) {
        p <- p + labs(x = "Density")
    }
    return(p)
})
# Arrange the plots in one column
combined_plot <- ggarrange(plotlist = plots, ncol = 3, nrow = 3)

# Save the plots as a PDF
ggsave(filename = "phenotype_selection.pdf", plot = combined_plot, device = cairo_pdf, width = 8, height = 6)

############################################################################
## Linear Regression Analysis of Selection Coefficients and Final Density ##
############################################################################

# Load dataframe
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/phenotype_selection.csv")

# Make Density numeric
data$Density <- as.numeric(data$Density)
# Create the trait List
trait_list <- c("flower_number_1_std", "D2Flowering_std", "pollen_avg_std", "ff_height_std", "total_node_number_std", "prop_early_growth_std", "final_height_std", "veg_weight_std", "leaf_area_std")

# Run Linear Regression Analysis using the selection coefficients as the response and the final density as the predictor
results <- data.frame(Trait = character(), Type = character(), Estimate = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

for (trait in trait_list) {
    for (type in c("Estimate", "Estimate_F", "Estimate_LF")) {
        lra <- lm(get(type) ~ Density, data = data[data$Trait == trait,])
        estimate <- coef(lra)[2]
        p_value <- summary(lra)$coefficients[2, 4]
        results <- rbind(results, data.frame(Trait = trait, Type = type, Estimate = estimate, P_Value = p_value, stringsAsFactors = FALSE))
    }
}

# Remove row names
rownames(results) <- NULL

# Write out the results to a CSV file
write.csv(results, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/linear_regression.csv", row.names = FALSE)


