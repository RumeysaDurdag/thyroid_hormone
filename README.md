# Thyroid Disease Analysis and Classification

An applied statistical analysis and machine-learning coursework project using a thyroid disease dataset. The project combines data preparation, exploratory data analysis, missing-value imputation, feature encoding, and supervised classification in R.

## Project objectives
- Explore patient and thyroid-related variables.
- Examine missingness and prepare the data for analysis.
- Investigate relationships among thyroid hormone measurements.
- Compare classification approaches for the target class.

## Workflow
1. **Data preparation:** standardize categorical values, convert variable types, remove selected columns, and inspect missing observations.
2. **Exploratory analysis:** review distributions, compare groups with boxplots, visualize relationships, and examine correlations among numeric variables.
3. **Missing-data handling:** apply Multiple Imputation by Chained Equations (MICE) and create an imputed dataset.
4. **Feature engineering:** encode categorical predictors using one-hot encoding.
5. **Modeling:** fit Support Vector Machine (SVM), Artificial Neural Network (ANN), Random Forest, and XGBoost classifiers.
6. **Evaluation:** inspect confusion matrices and classification metrics on training and test partitions.

## Tools and libraries
- **Language:** R
- **Data manipulation:** dplyr, tidyverse, stringr
- **Visualization:** ggplot2, visdat, DataExplorer, ggcorrplot
- **Imputation and preprocessing:** mice, caret
- **Machine learning:** e1071, nnet, randomForest, xgboost

## Repository contents
| File | Description |
|---|---|
| FINAL.R | Data cleaning, exploratory analysis, imputation, preprocessing, and model training |
| thyroid.csv | Input dataset used in the analysis |
| thyroid_imputed.csv | Imputed dataset generated during the workflow |
| FINAL PROJECT.docx | Original coursework report |

## Run locally
1. Install R and the packages listed above.
2. Clone this repository.
3. Open FINAL.R in RStudio.
4. Set the working directory to the repository root, or open the project from that directory.
5. Run the script in order.

The script expects thyroid.csv in the working directory. The original script was developed as coursework and may require package-version adjustments in a current R environment.

## Notes
This is an academic project demonstrating an applied statistical workflow. Model performance should be interpreted using the reported test-set metrics in the analysis; no performance figures are stated here because the workflow has not been independently rerun as part of this repository review.
