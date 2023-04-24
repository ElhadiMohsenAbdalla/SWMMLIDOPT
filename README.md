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


### How to use SWMMLIDopt

The following example illustrates the use of SWMMLIDopt tool for oprimization of LID measures. In this example we use NTNU_case_noLID.inp which can be downloaded from the example folder in this repository. Open the example NTNU_case_noLID.inp in your SWMM software to make sure that it is working. Next, open Rstudio and follow these steps 

2. DOWNLOAD and open the SWMMLIDopt.R.
![alt text](https://github.com/ElhadiMohsenAbdalla/SWMMLIDOPT/tree/main/Figures/fig0.jpg?raw=true) 
4. Click Run App bottom. 
5. The following window should appear.
6. Choose the inp file (i.e., the model created by SWMM software). In this exaple we use NTNU_case_noLID.inp. It is important that the inp file has no LID measures. 
7. The SWMMLIDopt will excute the model, plot the catchment, simulation results, and print the catchment characteristics as shown in the following figure
8. In the box (Subcatchment tags), assign the land types of each catchment to one of the standard types in SWMMLIDOPT ('Parking', 'Roof','Vegetation','Road','Walkway') as shown in the following figure. Make sure that your .inp file contains tags for each catchment similar to the example provided here. It is possible to assign the same tag to many catchment in the inp file
9. The tab "LID creation tool" is used to create inp file with LID measures that are assigned based on user input. For example, As in the the following figure, four LID measures were selected: BR(Bioretenetion cell), GR(Green roof), PP (permeable pavement) and VS (vegetated swales). BR will be implemented in the land types 'Parking' and 'Vegetation', while GR will only be implemented in the land type 'Roof'. Click the bottom "Create LIDs and Export results" to export the .inp file with LID measures
10. The LID measures parameters have the default values from the SWMM manual. It is possible to open the exported SWMM inp file and modify the parameters of the LID measures, as shown in the following figure
11. In the box (Optimization), you can set the Optimization constrains and run the Optimization. First, choose the inp file with LID measures. In the "Optimization constrains" tab, you can set the maximum LID measure for the entire catchment. In addition, you can set an upper limit for the density of each LID measure and the unit cost of each LID measure (euro/m2)
12. In the tab "Run optimization", you can run the multi-objective. After the optimization is finished, the text "Optimization is finished. Please export the results" will be printed in the screen. Then you can export the optimization results (*.json*)
13. The tab "Optimization results" can be used to visualize the results and compare between pareto optimal results. You first choose the results file (*.json*) and the SWMM file with LID measures (.inp). The pareto front will be plotted as follows: 
14. you can choose a pareto optimal scenario as shown the following figure. You can then export the LID measure scenario as inp file and .csv file
15. In the tab "Compare pareto solutions", you can select and compare different pareto optimal solutions 
16. In the tab "Details pareto solutions comparsion ", you can compare the desnity of specific LID measures between two pareto optimal solutions         








## License

[MIT](https://choosealicense.com/licenses/mit/)
