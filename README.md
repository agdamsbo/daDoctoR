# Intro

I try my best to share my basic workarounds in R to more effectively do statistical research. Feel free to be inspired or comment.

AS all functions have been collected in the package daDoctoR, please refer to the documentation there.
Install the package with the following command: devtools::install_github('agdamsbo/daDoctoR')


## Further research

In need of a suitable function to perform the chi-squared test of Hardy-Weinberg-equillibrium in my study poppulation, I ended up writing my own. It also contains a few summarise functions. This is actually the function I am most proud of, as it represents an actual universal test for both bi- and triallelic sustems in non-sexcromosome genes.

### Genotype distribution testing
- hwe_allele.R -- requires input in the form of two vectors with alleles listed
- hwe_geno.R -- requires input as numbers of each genotype (mm, mn, nn for biallelic systems and mm, mn, nn, mo, no, oo for triallelic)
- hwe.sum.R -- summarising tests for genotypes grouped by af factor. Performs HWE test for each group and returns neatly formatted distribution for easy copy-pasting to print. No comparisons of groups. Use the oddsratio or chisq.test.

### Formatting large data frames
- col_fact.R -- formatting columns as factor for names containing text elements of a vector provided. Labels or levels can be provided.
- col_num.R -- formatting columns as numeric for names containing text elements of a vector provided.

### Bivariate logistic regression analyses
- rep_glm.R -- for a stepwise gating regression approach this provides several bivariate logistic regression analyses for columns of a dataframe specified by af vector of the format c(). Use the dput() to obtain names of dataframe in correct format.
- rep_lm -- same as rep_glm, but for linear models
- rep_reg_cie.R -- Analysis of change in estimate approach with specified cut set at 10 % as standard.

### Publishin
- strobe_print.R -- regression analysis table complying with STROBE.

## Research year

Commands for extracting data from cpr-numbers:
- age_calc_function.R
- cpr_check_function.R
- cpr_sex_function.R
- date_convert_function.R
- dob_extract_cpr_function.R

All of these commands are written to work with the Danish Central Person Registry (CPR) numbers of the format ddmmyy-xxxx.
