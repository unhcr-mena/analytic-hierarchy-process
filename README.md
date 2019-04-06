# Analytic Hierarchy Process

Tutorial to use Analytic Hierarchy Process to build a vulnerability formula

This is based on this [tutorial](https://rpubs.com/gluc/ahp)

## Usage notes

1. Enter comparison criteria in `data/criteria.csv`. The file should have exactly two columns:
    * __name__: a short variable name for analysis purposes only.
    * __label__: the long description to be printed on the form.
2. Source `code/1-build-xlsform.R`.
3. Create new KoBo survey based on `out/form.xlsx`, collect data, and download results to `data/survey.xlsx`.  
__NOTE:__ DO ___NOT___ USE GROUP NAMES WHEN EXTRACTING THE DATA.
4. Source `code/2-build-hierarchy.R`.
5. Source `code/3-final-report.R` to run the analysis then check your `out/` folder for the analysis report and case definitions.
