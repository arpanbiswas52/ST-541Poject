
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

# Computational Efforts for this project

I have created 16 functions for the project which are in R folder:

1\.**samples.R**: In this file, 2 functions have been created-
`get_samples_antithetic()` and `get_samples()`.
`get_samples_antithetic()` generates samples for Antithetic variable
approach and `get_samples()` generates samples for Monte Carlo approach.

2\.**simulation.R**: In this file, 7 functions have been created-
`Storage_realizations()`, `Forebay_realizations()`,
`Tailwater_realizations()`, `Tailwater_realizations()`,
`Head_realizations()`, `Energy_realizations()`,
`mean_sd_sim_antithetic()` and `mean_sd_sim()`. `Storage_realizations()`
simulates the storage of the reservoirs, `Forebay_realizations()`
simulates the forebay elevation of the reservoirs,
`Tailwater_realizations()` simulates the tailwater of the reservoirs,
`Head_realizations()` simulates the head of the reservoirs,
`Energy_realizations()` simulates the energy generation of the
reservoirs, `mean_sd_sim_antithetic()` and `mean_sd_sim()` calculate the
means and the standard deviations of the simulated data obtained from
all the previous mentioned functions in this file using Antithetic
Variables and Monte Carlo approaches.

3\.**objfuncrevenue.R**: In this file, 2 functions have been created-
`Total_Revenue_cal()` and `obj_Func_eval()`. `Total_Revenue_cal()`
simulates and calculates the mean and the standard deviation of total
Revenue of the \(t=14\) days optimization period. `obj_Func_eval()`
evaluates the robust objective function which is to maximize expected
total revenue

4\.**constraints\_validation.r**: In this file, 5 functions have been
created- `Storage_constraints()`, `Forebay_constraints()`,
`Energy_constraints()`, `ForebayEndofPeriod_constraints()`,
`Constraints_validation()`. `Storage_constraints()` validates all the
constraints of Storage of the reservoirs, `Forebay_constraints()`
validates all the constraints of Froebay elevation of the reservoirs,
`Energy_constraints()` validates the constraints on the energy
generation by Turbines. `ForebayEndofPeriod_constraints()` is special
constraints applied to Grand Coulee only to stabilize the forebay
elevation at the end of period of optimization. Grand Coulle has the
largest storage and take a longer time to fill. Therefore, we need to be
sure the optimal result doesnot provide solution to empty the whole
reservoir at the end of optimization period and should be within a
certain level of forebay at the start of next optimization
period.`Constraints_validation()` calls all the above functions in this
file and returns a single large matrix to the optimization program by
concatenating all the validated results.slsqp() do not allow any
additional arguments to this constraint functions
`Constraints_validation()`, therefore I have to rewrite all the initial
conditions, inflow data etc. again inside the functions as I cannot pass
those arguments. Therfore, the code looks bit repeatative inside this
function but it has been deliberately done due to the said reason.

I have documented as taught in the lecture for all the functions in
samples.R, simulation.R and objfuncrevenue.R, which comprised of 11
functions. Since one of my prime focus is the computational efficiency
of the model, I have always attempted to code the RO model efficiently
by extensive applications of operations of large data in vectors and
matrices, using `rowMeans()`, `colMeans()` etc., building several small
functions to avoid redundant execution of codes multiple times, avoiding
loops in large simulations. However, we had to use loops in calculating
some quantities like Storage in each days (timesteps) since we need to
know the value of storage at pervious day to calculate the storage at
the current day. Therefore calculating the value of storage of 14 days
together was not possible using operations in vectors. However, this
will not significantly increase the runtime as we need to calculate for
14 days only (14 iterations).
