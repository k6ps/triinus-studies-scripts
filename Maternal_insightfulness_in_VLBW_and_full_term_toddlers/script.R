#
# Ver. 4.2, 01.08.2017. Author: Taavi Tänavsuu (tanavst@gmail.com).
#
# This is based on the first article, i.e. copied from and improved upon 
# the model in file ../andmed_r_projekt/tee_graafik_ja_tabel.R
#
# Improvements in ver. 4.2 (01.08.2017):
# -- Changed data file headings to English.
#
# Improvements in ver. 4.1 (20.07.2017):
# -- Added commends for fields used.
# -- Fixed linear regression results table for control group.
# 
# Improvements in ver. 4 (20.07.2017):
# -- Translated comments, function and variable names to English for Esa.
# 
# Improvements in ver. 3 (29.01.2017):
# -- Changed interpretation of EEK-2 AST score. Previously, it was 1 starting from score 8, 
# now it is 1 starting from score 7. As a result of that, interpretation has changed for subjects 5 and 7 in the 
# clinical group in original data file.
# -- The EEK-2 results for control group have been scored and added to the original data file. Unfortunately, 
# data is missing for 7 subjects, which is quite a big share of the entire control group.
#
# Improvements in ver. 2 (21.01.2017):
# -- According to Tiia's advice, removed the entire section of child rearing values data from this study.
# -- Replaced the initial EMK values interpreted by Triinu to values interpreted by Triinu and Kaidi together
# in consensus. A few values got changed.
#

# This is obviously machine-specific.
setwd("C:/Users/Taavi/OneDrive/Vanadest_arvutitest_arhiveeritud/Maternal_insightfulness_in_VLBW_and_full_term_toddlers")

# We need this additional library.
library(psych)

# Let's read the entire original data file
all_data <- read.delim("norm_kliiniline_Taavi_01.08.2017.csv", fileEncoding = "UTF-8")

# Filter out fields that we don't use in this study.
filtered_data <- all_data[,c(
	"code", # a unique code associated with subject
	"group", # group: 1=clinical (preterm) 0=control (normal-term)
	"mothers_age", # mother's age in years
	"relations_with_fathers_parents", # relations with father's parents: 1=close and good, 2=close but problematic, 3=remote and good, 4=remote and non-engaging, 0=dead
	"relations_with_mothers_parents", # relations with mother's parents: 1=close and good, 2=close but problematic, 3=remote and good, 4=remote and non-engaging, 0=dead
	"mothers_education", # mother's education: 4=university, 3=highscool, 2=vocational highschool, 1=secondary school or lower
	"family_status", # family status: 1=single, 2=married, 3=cohabiting, 4=divorced, 5=separate
	"income", # income per family member: 4=more than 4000 EEK/month, 3=2000-4000 EEK/month, 2=1000-2000 EEK/month, 1=less than 1000 EEK/month
	"risk", # health risk of the child (only applicable for clinical group): 1=high, 2=low
	"disability", # disability of the child (data only available for clinical group): 1=no, 2=yes
	"nursing_when_leaving_hospital", # nursing of the child at the time of leaving the hospital (only applicable for clinical group): 1=breastfeeding, 2=artificial babyfood, 3=mixed
	"EMK_consensual", # insightfulness score: 1=positively insightful, 2=one-sided, 3=disengaged
	"EEK_2_DEP", # EST-Q score for maternal depression: 1=higher, 0=lower
	"EEK_2_GAN", # EST-Q score for Maternal General Anxiety: 1=higher, 0=lower
	"EEK_2_APP", # EST-Q score for Maternal Agoraphobia-Panic: 1=higher, 0=lower  
	"EEK_2_SAN", # EST-Q score for Maternal Social Anxiety: 1=higher, 0=lower
	"EEK_2_FAT", # EST-Q score for Maternal Fatique: 1=higher, 0=lower
	"EEK_2_INS" # EST-Q score for Maternal Insomnia: 1=higher, 0=lower
)]

output.results <- function(file_contents, file_name, do.write = TRUE) {
	# That's the subfolder where results are created as .csv files
	# It needs to exist before running this script.
	results_subfolder <- "exceli_failid/01.08.2017_v4.2/"

	print(file_name)
	print(file_contents)
	
	if (do.write) {
		write.csv2(file_contents, paste(results_subfolder, paste(file_name, ".csv", sep=""), sep=""))
	}
}

# ==============================================================================
# Phi-coefficients analysis
# ==============================================================================

# Transform values of each field into two groups.
# First, a helper function:
triinu.sotsem.andmed.transformToDichotomous <- function(given_value, transformer_function) {
	if(is.na(given_value)) NA else transformer_function(given_value)
}

# Second: transforming functions for each field (that has more than two possible values)
triinu.sotsem.andmed.transformMothersAgeToDichotomous <- function(given_value) {
	triinu.sotsem.andmed.transformToDichotomous(given_value, function(x) {if(x >= 30) "older" else "younger"})
}

triinu.sotsem.andmed.transformRelationsWithFathersParentsToDichotomous <- function(given_value) {
	triinu.sotsem.andmed.transformToDichotomous(given_value, function(x) {if(x == 1) "good" else "other"})
}

triinu.sotsem.andmed.transformRelationsWithMothersParentsToDichotomous <- function(given_value) {
	triinu.sotsem.andmed.transformToDichotomous(given_value, function(x) {if(x == 1) "good" else "other"})
}

triinu.sotsem.andmed.transformMothersEducationToDichotomous <- function(given_value) {
	triinu.sotsem.andmed.transformToDichotomous(given_value, function(x) {if(x == 4) "higher" else "other"})
}

triinu.sotsem.andmed.transformFamilyStatusToDichotomous <- function(given_value) {
	triinu.sotsem.andmed.transformToDichotomous(given_value, function(x) {if(x == 2 || x == 3) "together" else "other"})
}

triinu.sotsem.andmed.transformIncomeToDichotomous <- function(given_value) {
	triinu.sotsem.andmed.transformToDichotomous(given_value, function(x) {if(x == 4) "higher" else "lower"})
}

triinu.sotsem.andmed.transformHealthRiskToDichotomous <- function(given_value) {
	triinu.sotsem.andmed.transformToDichotomous(given_value, function(x) {if(x == 1) "high" else "low"})
}

triinu.sotsem.andmed.transformChildFoodToDichotomous <- function(given_value) {
	triinu.sotsem.andmed.transformToDichotomous(given_value, function(x) {if(x == 1 || x == 3) "breastfeeding" else "other"})
}

# Let's run these functions
filtered_data$mothers_age <- sapply(filtered_data$mothers_age,triinu.sotsem.andmed.transformMothersAgeToDichotomous)
filtered_data$relations_with_fathers_parents <- sapply(filtered_data$relations_with_fathers_parents,triinu.sotsem.andmed.transformRelationsWithFathersParentsToDichotomous)
filtered_data$relations_with_mothers_parents <- sapply(filtered_data$relations_with_mothers_parents,triinu.sotsem.andmed.transformRelationsWithMothersParentsToDichotomous)
filtered_data$mothers_education <- sapply(filtered_data$mothers_education,triinu.sotsem.andmed.transformMothersEducationToDichotomous)
filtered_data$family_status <- sapply(filtered_data$family_status,triinu.sotsem.andmed.transformFamilyStatusToDichotomous)
filtered_data$income <- sapply(filtered_data$income,triinu.sotsem.andmed.transformIncomeToDichotomous)
filtered_data$risk <- sapply(filtered_data$risk,triinu.sotsem.andmed.transformHealthRiskToDichotomous)
filtered_data$nursing_when_leaving_hospital <- sapply(filtered_data$nursing_when_leaving_hospital,triinu.sotsem.andmed.transformChildFoodToDichotomous)

# Separate clinical and control group
filtered_data_clinical <- subset(filtered_data, Grupp == 1)
filtered_data_control <- subset(filtered_data, Grupp == 0)

# Let's study the relation of several factors to insightfulness. As all fields have dichotomous values, 
# we get 2 x 2 frequency tables. Then we'll find the absolute values of phi-coefficients to show how strong
# a degree of association they have with insightfulness.
fields_used <- c(
	"mothers_age",
	"relations_with_fathers_parents",
	"relations_with_mothers_parents",
	"mothers_education",
	"family_status",
	"income",
	"risk",
	"disability",
	"nursing_when_leaving_hospital",
	"EEK_2_DEP",
	"EEK_2_GAN",
	"EEK_2_APP",
	"EEK_2_SAN",
	"EEK_2_FAT",
	"EEK_2_INS"
)

triinu.sotsem.phiCoefficientsAnalysis <- function (fields_used, clinical_group, control_group) {
	phiEMK <- function(given_data, given_field)  {
		if (areAllValuesNA(given_data[given_field])) {
			NA
		} else if (areAllValuesEqual(given_data[given_field])) {
			0
		} else {
			frequency_table <- ftable(given_data[c(given_field, "EMK_consensual")])
			abs(phi(frequency_table))
		}
	}

	areAllValuesNA <- function(given_vector) {
		colMeans(is.na(given_vector)) == 1
	}

	areAllValuesEqual <- function(given_vector) {
		x <- NULL
		l <- given_vector[[1]]
		for (i in seq_len(length(l))) {
			if (!is.na(l[i])) {
				if (is.null(x)) {
					x <- l[i]
				} else {
					if (!identical(l[i],x)) {
						return(FALSE)
					}
				}
			}
		}
		return(TRUE)		
	}

	createTableRow <- function(given_field) {
		data.frame(
			Clinical = phiEMK(clinical_group, given_field),
			Control = phiEMK(control_group, given_field),
			row.names = given_field
		)
	}

	results_table <- NULL
	for (i in seq_len(length(fields_used))) {
		results_table_row <- createTableRow(fields_used[i])
		results_table <- rbind(results_table, results_table_row)
	}
	results_table
}

phi_coefficients_results_table <- triinu.sotsem.phiCoefficientsAnalysis(fields_used, filtered_data_clinical, filtered_data_control)

output.results(phi_coefficients_results_table, "phi_coefficients_results_table")

# =======================================================
# Linear regression analysis
# =======================================================
# (Just trying a different method to see if we get a similar result.)
triinu.sotsem.linearRegressionAnalysis = function(fields_used, given_data, multivar=FALSE) {

	dependent_field <- "EMK_consensual"

	# Template function for studying various linear models
	createLinearRegressionModel <- function(data, x, right_side){
	     "%+%" <- function(x,y) paste(x,y,sep="")
	     linear_regression_formula <- x %+% "~" %+% right_side 
	     linear_regression_formula <- as.formula(linear_regression_formula)
	     summary(lm(linear_regression_formula, data=data))
	}

	# Create table row(s) for coefficients. Argument is
	# assumed to be of summary.lm type.
	createCoefficientsRow <- function(x) {
		coefs <- coef(x)
		# Skip the intercept row
		as.data.frame(coefs)[2:NROW(coefs),]
	}

	# Add significancy codes.
	createSignificancyCode <- function(x) {
		print(x)
		if (x < 0.001) "***"
		else if (x < 0.01) "**"
		else if (x < 0.05) "*"
		else if (x < 0.1) "."
		else ""
	}

	results_table <- NULL
	if (multivar) {
		print("= Multivariate linear regression =")
		right_side <- paste(fields_used, collapse=" + ")
		results_table <- coef(createLinearRegressionModel(given_data, dependent_field, right_side))
		# Skip the intercept row
		results_table <- as.data.frame(results_table)[2:NROW(results_table),]
	} else {
		print("= Univariate linear regression =")
		createLapplyFriendlyLinearRegressionModel <- function(right_side) { createLinearRegressionModel(given_data, dependent_field, right_side) }
		# array of summary.lm-s
		results_table <- lapply(fields_used, createLapplyFriendlyLinearRegressionModel)
		results_table <- do.call(rbind,lapply(results_table, createCoefficientsRow))
	}

	# Create significancy codes
	significancy_codes <- sapply(as.numeric(t(results_table[4])), createSignificancyCode) 

	# Give it a somewhat better format
	results_table[1] <- lapply(results_table[1], formatC, format="f", digits=2, mode="double")
	results_table[2] <- format(results_table[2], scientific=FALSE, digits=1, nsmall=1, trim=TRUE)
	results_table[3] <- format(results_table[3], scientific=FALSE, digits=1, nsmall=1, trim=TRUE)
	results_table[4] <- lapply(results_table[4], formatC, format="f", digits=3, mode="double")

	# Add significancy codes
	results_table[4] <- paste(t(results_table[4]), significancy_codes, sep="")

	results_table

}

# Fields that are used in linear regression analyss (the same fields used in phi-coefficient analysis,
# except those for which data is missing or all subjects have the same value.)
fields_used <- c(
	"mothers_age",
	"relations_with_fathers_parents",
	"relations_with_mothers_parents",
	"mothers_education",
	"family_status",
	"income",
	"risk",
	"disability",
	"nursing_when_leaving_hospital",
	"EEK_2_DEP",
	"EEK_2_SAN",
	"EEK_2_FAT",
	"EEK_2_INS"
)
linear_regression_results_table_clinical <- triinu.sotsem.linearRegressionAnalysis(fields_used, filtered_data_clinical)
output.results(linear_regression_results_table_clinical, "linear_regression_results_table_clinical")

fields_used <- c(
	"mothers_age",
	"relations_with_fathers_parents",
	"relations_with_mothers_parents",
	"mothers_education",
	"family_status",
	"income",
	"EEK_2_DEP", # EST-Q score for maternal depression: 1=higher, 0=lower
	"EEK_2_GAN", # EST-Q score for Maternal General Anxiety: 1=higher, 0=lower
	"EEK_2_FAT", # EST-Q score for Maternal Fatique: 1=higher, 0=lower
	"EEK_2_INS" # EST-Q score for Maternal Insomnia: 1=higher, 0=lower
)

linear_regression_results_table_control <- triinu.sotsem.linearRegressionAnalysis(fields_used, filtered_data_control)
output.results(linear_regression_results_table_control, "linear_regression_results_table_control")

fields_used <- c(
	"mothers_age",
	"relations_with_fathers_parents",
	"relations_with_mothers_parents",
	"mothers_education",
	"nursing_when_leaving_hospital"
)
multivariate_linear_regression_results_table_clinical_1 <- triinu.sotsem.linearRegressionAnalysis(fields_used, filtered_data_clinical, multivar=TRUE)
output.results(multivariate_linear_regression_results_table_clinical_1, "multivariate_linear_regression_results_table_clinical_1")

fields_used <- c(
	"mothers_age",
	"relations_with_mothers_parents",
	"nursing_when_leaving_hospital"
)
multivariate_linear_regression_results_table_clinical_2 <- triinu.sotsem.linearRegressionAnalysis(fields_used, filtered_data_clinical, multivar=TRUE)
output.results(multivariate_linear_regression_results_table_clinical_2, "multivariate_linear_regression_results_table_clinical_2")
