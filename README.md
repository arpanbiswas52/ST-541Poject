
# An approach to Robust Optimization of Large Scale Complex River System

Author: “Arpan Biswas”

# Research Objective:

Renewable energy, such as hydroenergy, is one of the major sources for
electricity generation for energy sectors. Over many decades, there has
been continuous development of water resource management for the
economic benefit of electricity industries. However, in real world,
these energy sectors deal with different sources of uncertainties like
Inflows, Market Prices and Market Demand of electricity which
significantly impacts their operations and therefore in generating
revenue. Therefore, the objective in this project is to generate a
Robust Optimization framework for maximizing the Net Revenue and thereby
provide a robust solution of optimal operation control. However, RO
model is too expensive due to it’s high simulation and function
evaluation costs during qunatification and propagation of uncertainity
through the large scale complex system. This can make the model
inefficient since the operators need to run the model at regular
interval of time (hourly). Therefore, in this project, another research
objective is to investigate on increasing the efficiency of the model in
terms of reducing computational cost and/or providing better robust
optimal solutions.

The report in pdf format can be found
[here](https://github.com/ST541-Fall2018/arpanbiswas52-project-ComplexRiverSystem/blob/Master/Report.pdf).

The slides from the presetation can be found
[here](https://github.com/ST541-Fall2018/arpanbiswas52-project-ComplexRiverSystem/blob/Master/ProjectST541_Presentation.pdf).

List of package required to run the project: `tidyverse`, `nloptr`,
`tictoc` and `readr`

nloptr is a special package for optimization algorithm for nonlinear
constraint problems. I have used Sequential Quadractic function
(slsqp()) in the model as my problem is highly nonlinear. The function
will return a list containing the optimal decision, objective function
at the optimal solution, number of iterations to converge, and
convergence message. For more details, you can check at `?slsqp`.
