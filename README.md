# DectechR

Collection of functions useful for Dectech analysts, such as extracting survey data from Questback output, and getting consistent coefficient tables from regressions.


### Installation / Updating

You can install or update DectechR using the following:

```r
#-- if DectechR is already loaded, then unload it using:
detach(package:DectechR, unload = TRUE)

# install.packages("devtools")
devtools::install_github("Dectech/DectechR")
```

If you get errors about "Error in fetch(key) : lazy-load database..." try restarting RStudio.

### Main functions

Opening XML data:

* dectechXmlToDataframe: for getting EFS data in XML format (recommended over csv!)

Getting data to clipboard:

* cc: copy anything to clipboard
* cc_varlist: format a variable list with quotations etc. for pasting into a script

Regression modelling:

* getOutput: extract regression output to clipboad
* getUnivariate: run a series of univariates on a model (and copy to clipboad)

Factor analysis:

* getScreePlot: get scree plot before running factor analysis (table copied to clipboard)
* getFactorLoadingsTable: nicely format factor analysis results (and copy to clipboard)

Data recoding:

* unpackUserVariable: split a custom variable (e.g. "1\*3\*5\*7") into multiple columns
* addMobile: identify mobile users from their browser


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


### Major changes in latest update (09-Jan-2024)

#### getUnivariate() / runUnivariate():
- can now handle formula entry i.e. if you want to run a univariate based on a formula rather than a fitted model

#### getOuput()
- for multilogit models the beta and p-values are now outputted in a wide format, for easy comparison

#### cc()
- added cc_varlist() for easily pasting lists of variable names
- changed the output encoding to avoid strange artefacts when pasting to excel (e.g. previous "£" was getting outputted as "Â£")
