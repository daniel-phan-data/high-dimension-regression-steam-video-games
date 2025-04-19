# MODEL_LIN_STEAM

## Description
This project is a statistical analysis of steam's video games data, scrapped from their API, through the use of linear models, and other regression techniques. You will find here all the code we used, in R.

## Folder structure
Some of the following R scripts were replicated in SAS and Python, hence the python, sas, and r folders.
The files are as follows :

setup: used to load and clean our data before analysis

univariate_tests: script that displays all the univariate tests we needed for the report

univariate_tests_log: more univariate tests with a log transformation for the variables hard to visualize otherwise

correlation_matrix: script that displays the correlation matrix of the selected relevant variables for our analysis

rating: outdated as it has been merged with setup, creates new variable rating to reflect Steam's evaluations

bivariate_tests: script that displays the bivariate tests used for our analysis

univ_rating: script to display statistical tests on the rating variable we created

lm_and_hypotheses: script that can create linear models and test the essential hypotheses

lm_selection: script that can run linear model selection algorithms

classification: script that can create and test a classification model that predicts estimated owners of a given game

report: uses functions from the other scripts to generate the specific models and graphs used in our analysis report

poly: script that tests polynomial models for each predictors and check if it fits data better

## Installation
### R:
We recommend Rstudio and the 4.4.1 version of R.

### Python:
We recommend Python 3.13 or at least 3.xx
1. Navigate to the python directory:
```bash
cd steam_python
```
2. Install or update pip:
```bash
python -m pip install --upgrade pip
```
3. Install the required dependencies using pip:
```bash
pip install -r requirements.txt
```
4. Execute the following command to run the desired script:
```bash
python ./[filename.py]
```

## Authors and acknowledgment
PHAN Daniel
MOUILLET Antonin
OUSALEM Nathan
NDAYIZYE Perle M.S
GODJE Ibrahim

## Project status
Finished.