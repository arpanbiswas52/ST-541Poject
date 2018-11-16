To Create a Milestone for project

$Week 5: Due 10/25/2018$

Research study on the optimization algorithm in R of the project and answer the following questions:

Can we use any nonlinear constraint algorithm to build the optimization model?

If not, can we use some other technique/workaround to show the optimization results of the project?

If not, need to review the scope of the project and find some other ways to answer the primary research objectives proposed in the project.

Update: 
We could find some Non-linear constraints algorithm package in R suitable for our project. We will use package "nloptr" for solving non-linear constrained optimization problem. 
Specifically we will use Sequential Quadratic Programming algorithm "slsqp" which solve nonlinearly constrained, gradient-based optimization, supporting both equality and inequality constraints. This classical method is known to be one of the powerful optimization algorithm due to fast convergence and therefore is the best approach in this project as one of our primary research objective is reducing the computational effort. 

$Week 6: Due 11/01/2018$

Based on what we decide on week 5, start coding. 

<Update> Status: In Progress

Create the data file with all the data required. 
<Update> Status: Complete
All the input data required for this project has been stored in the DATA folder.

In our project, we have 3 input data for Inflows of GCL, inflows of LWG and Price.

The input data has uncertainities.

The input data are the realizations of Inflows, Price for 14 days where each column represents a realization and each row represent day.


Create all the required folders in the project to store appropiate function .rmd files. 

<Update> Status: Complete
Created folders "data", "results", "simulations", "constraints", "objective function"
Folder 'data' will contain all the input data that will be required in the project like Price data, Inflows data. 
Folder 'results' will contain all the results found in this project
Folder'simulations' will contain all the functions written in .rmd to run the simulations. This will be the most significant for our project.
Folder 'constraints' will contain .rmd files where all the constraints for our problem will be written.
Folder 'objective function' will contain .rmd files where the objective function for our problem will be written.



$Week 7: Due 11/08/2018$

To start the integral part of coding. Writing several functions as required.

Status: In Progress:
In sim.rmd (Inside Simulations folder), all the fundamental functions required have been coded. The functions are to quantify Storage or reservoirs, Forebay Elevation, Tailwater, Head and Energy generation.
Therefore, milestone for this week has been reached already.

However, we kept the status in Progress to do analysis regarding the reduction of runtime on the simulation which has been scheduled to complete in next week and will start coding on the constraints and the objective function of the optimization model.

$Week 8: Due 11/15/2018$

Continue coding. 

Status: In Progress

Complete the simulation part. 

Status: In progress. 
The main coding part for simulation is completed but the analysis of simulations using different variance reduction/ minimal computational cost techniques is still ongoing and will be done next week.

Start the coding for optimization by writing objective functions and constraints(Can confirm this part after completing Week 5 milestone). 

Status: In progress. 
The codes for constraint validation is done. In next week, we will complete the codes for function evaluation.


$Week 9: Due 11/22/2018$

Complete the optimization part. (Can confirm this part after completing Week 5 milestone). 

Start report writing with the results found. 

$Week 10: Due 11/29/2018$

Complete Report and Presentation slides for project presentation