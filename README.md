# SWMMLIDOPT
### SWMMLIDopt: a tool for optimization of low impact development (LID) measures using the SWMM model

The SWMMLIDOPT utilizes the Storm Water Management Model (SWMM) and provide a user-friendly environment for conducting multi-objective optimization of the selection and placement of LID measures in urban catchments.  

The SWMMLIDopt operates from a simplified user interface that prepares the optimization input, executes the multi-objective optimization, based on the total cost and reduction of peak drainage flow rates, and visualizes the optimization results. SWMMLIDopt is created using several R packages such as Shiny (creates interactive user interface with R), swmmr (prepares, executes, and reads results of SWMM models from R environment) and mco (solves multi-objective optimization problems using the non-dominated sorting genetic algorithm (NSGA-II)). 

To operate SWMMLIDOPT in your computer, the following must be installed: 

1. R and Rstudio: follow the instruction in this link (https://posit.co/download/rstudio-desktop/)
2. The Storm Water Management Model (SWMM): install the (x64) version in this link (https://www.epa.gov/water-research/storm-water-management-model-swmm)
3. Rtools: install the version that is compatible with your R version from this link (https://cran.r-project.org/bin/windows/Rtools/) 
  
      To check your R version, run this command in the R terminal 
    ```r
    R --version
    ```
    Alternatively, check the version of R in R studio>Tools>Global options ...>General 










## License

[MIT](https://choosealicense.com/licenses/mit/)
