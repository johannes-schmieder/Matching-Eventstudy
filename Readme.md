# Code Example for Matching Event Study

This repository contains example code for estimating an event study design on a sample of treated individuals who are matched to a control group. 

The matching uses a combination of exact matching and propsensity score matching.  To achieve this the code defines 'cells' of observations (e.g. event year and industry). Then the code loops over the cells. Within each cell the code estimates the propensity score for being treated and matches a control individidual with the closest propensity score without replacement. 

Finally the code stacks all the 'sub-experiments' corresponding to each cell. Note that individuals (treated or control) can show up in multiple cells. For this reasons the code creates a new person id variable that is unique across cells. This new person ID should then be used in the subsequent analysis.

The code first simulates data and then estimates the event study on the simulated data. The context for this example is a job displacement analysis, where displaced workers are matched to non-displaced workers, however the same strategy can be used for many other types of analysis.

The code is based on the following paper:
Schmieder, Johannes F., Till von Wachter, and Jörg Heining. “The Costs of Job Displacement over the Business Cycle and Its Sources: Evidence from Germany.” American Economic Review 113, no. 5 (May 2023): 1208–54. https://doi.org/10.1257/aer.20200252.


## Requirements
The code relies on two stata tools:
- cellgraph 
- latexlog 

For installation instructions see:
https://github.com/johannes-schmieder/cellgraph
https://github.com/johannes-schmieder/latexlog


