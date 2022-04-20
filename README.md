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

The main functions are:

* getOutput (extract regression output)
* getScreePlot
* getFactorLoadingsTable
* cc (copy to clipboard)
* cc_varlist (format a character string, for pasting into a script)
* dectechXmlToDataframe (convert Questback output to a dataframe)
* unpackUserVariable (Split a custom variable (e.g. "1\*3\*5\*7") into multiple columns)
* addMobile (to identify mobile users)
* getUnivariate (run a series of univariates on a model)

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


