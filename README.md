# Alzheimer-Diagnosis
In this analysis we are going to find the relationship between various characteristics of Alzheimer’s diseases and the diagnosis of Alzheimer’s (Demented) or NonAlzheimer’s(Non-Demented) individuals. The goal here is to analyze dataset containing relevant information and provide insights to the relationship between the characteristics. Analysis will be carried in several steps, mentioning specific tasks and which includes performing descriptive statistics, implementing clustering algorithms, fitting a logistic regression to predict the variable and implementing a feature selection method to find important features. Results from the analysis will be presented in tables, graphs and statistical models. This analysis will be explained in detail with justified methods.

## Analysis of Dataset and Data Preprocessing
There are 10 columns for 350 individuals, Group column with being diagnosis and
rest of the columns are supporting factors for diagnosis. (like:M.F, Age, CDR-Clinical
dementia rating, eTIV-Estimated total intracranial volume, nWBV-Normalize
whole brain volume)

### Data processing
- Remove “Converted” class from the dataset
- Convert Group & M.F column to numerical data
- Replace the missing values in SES and MMSE(using median)
- Standardizing the values for eTIV, nWBV, ASF

### Analysis
1. Descriptive Statistics
2. Clustering Algorithm
3. Logistic Regression
4. Feature Selection
