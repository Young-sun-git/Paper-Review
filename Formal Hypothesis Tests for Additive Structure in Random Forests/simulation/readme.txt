Readme for Simulation Code related to
JCGS-16-022 - Formal Hypothesis Tests for Additive Structure in Random Forests
Lucas Mentch & Giles Hooker
09-28-2016

There are 6 folders contained in this .zip file:

1.  SimsNoRP:  (3) .R files
The code used to generate the test statistics (then used to calculate alpha level and power)
for the linear model simulation results in Table 1 can be found in "LM_pval.R".  The Tests of total and
partial additivity in Table 2 that do not utilize random projections may also be carried out using the
files "Table_1_2_Total.R” and "Table_2_Partial.R”, respectively, though updated, parallelized versions of
this code now appear in the noRP2 folder.

2.  noRP2:  (3) .sh files; (2) .R files
The 3 .sh files entitled “IntTestAll***.sh” carry out the simulation results for tests not involving 
random projections in Table 2 where the additional noise has standard deviation *.**.  These simulations were conducted
in R using a cluster so the initial lines in these files contain only cluster-specific information and can
be disregarded. One R file, “power.R” extracts these results and calculates the alpha level and power.  The other
.R file (EffectSizes.R) calculates the effect size on the x-axis of Figure 2 and creates the plot.

3/4/5.  SimsRPv.**:  (14) .sh files; (1) .R file in each
Each of these folders contain 14 .sh files that correspond to the 14 simulation results incorporating random projections in 
Table 2, where the ** correspond to the column with added noise std deviation 0.**.  In each folder, the file named
"RPsim_m1.sh”, for example, produces the results corresponding to model 1 in Table 2.  These simulations were conducted
in R using a cluster so the initial lines in these files contain only cluster-specific information and can be disregarded. 
The single .R file "RPsim_PostCluster.R" is a brief .R file used to extract the results and could equivalently be added
to the end of each .sh file.

6.  FinalPowerTable:  (4) .sh files; (2) .R files
Two of these .sh files (“RPsim_intv01.sh” and “RPsim_intv10.sh”) were used to calculate the results necessary to 
produce the first two plots in Figure 3, where the additional noise has standard deviation 0.1 or 1.0, respectively. 
The results were extracted and the power calculated using PowerTable.R.  The remaining two .sh files 
(“RPsim_vimp01.sh” and “RPsim_vimp10.sh”) were used to calculate the results for the variable importance plot (right) 
in Figure 3.  The files “PowerTable.R” and “PowerTablevimp.R” were used to extract the results and calculate the power for the
tests of additivity and importance, respectively.

			