Exploration of breakeven points for cost-benefit analysis
=========================================================

R package 

Quick start example using built-in NPV and ranges
```
library(cost.benefit.breakeven)
data(ranges)
runShiny()
```

Detailed instructions
======================
Setup
------
1.	Create a directory in which the analysis will be held, using a systematic naming structure
    1.	e.g. prefix_date_description – “CBA 20140502 initial analysis”
2.	In that directory, create a file named NPV.R
    -	In that file, define a function:
        1.	 named NPV
        2.	with first argument scen, a character string giving the name of each scenario
        3.	that returns the NPV for scen
        4.	with other arguments all the numeric values that are used in the analysis
        5.	For an example, see https://github.com/josephguillaume/cost_benefit_breakeven/tree/master/R/NPV.R
3.	Inside R, install the cost.benefit.breakeven R package 
    1.	Packages-Install package(s) from local zip files
    2.	Select the zip file, e.g. cost.benefit.breakeven_0.2-4.zip
4.	In R, move to the directory in which the analysis is held

        setwd("X:/path/to/CBA 20140502 initial analysis")
    
5.	In R, type the following to load the package and the NPV function

        library(cost.benefit.breakeven)
        source("NPV.R ")

6.	Create a file csv file named (for e.g.) “cba_parameter_ranges.csv”
    1.	It should have columns: Variable, Modeled,Min,Max
    2.	You can create a template by calling:

            write.csv(getRanges(),"cba_parameter_ranges.csv",row.names=FALSE)

    3.	Ensure the file only includes the variables of interest
    4.	Ensure the Min and Max values for the analysis make sense
7.	In R, type the following to start the interface. 

        runShiny()

Running the analysis
--------------------

Assuming the analysis has been setup, the following code will load  everything and start the interface

    ## Set the directory of the analysis
    setwd("X:/path/to/CBA 20140502 initial analysis")
    ## Load the package and the NPV function
    library(cost.benefit.breakeven)
    source("NPV.R")
    ## Load the ranges of the analysis
    ranges <- read.csv("cba_parameter_ranges.csv",stringsAsFactors=FALSE)
     ## Start the interface
    runShiny()
