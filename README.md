# DectechR

Collection of functions useful for Dectech analysts, such as extracting survey data from Questback output, and getting consistent coefficient tables from regressions.


### Installation

You can install DectechR from github with:

```r
# install.packages("devtools")
devtools::install_github("Dectech/DectechR")
```

### Main functions

The main functions are:

* getOutput (extract regression output)
* getScreePlot
* getFactorLoadingsTable
* cc (copy to clipboard)
* dectechXmlToDataframe (convert Questback output to a dataframe)
* unpackUserVariable (Split a custom variable (e.g. "1\*3\*5\*7") into multiple columns)
* addMobile (to identify mobile users)
* runUnivariate (run a series of univariate models)

### Style guide

When adding functions to this library, we should mostly stick to the following guide: http://adv-r.had.co.nz/Style.html

The key points being:

* Use descriptive meaningful variable/function names
* Generally, variable names should be nouns, and function names should be verbs
* Use the "<-" assignment operator, rather than "="
* leave space between operators e.g. "(x - 3) / 2" rather than "(x-3)/2"

One difference from the above is that for functions we should use camelCase, as this is statistically the most common style across R packages on CRAN. 
For variables we can use lowercase, with an underscore to seperate words (as suggested in the style guide) 
    e.g. some_result <- getResult(some_input)


