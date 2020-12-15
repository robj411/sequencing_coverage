Sequencing coverage and weights
================

## Coverage

We compute and present the coverage of sequence sampling in local
authorities (LAs) in the UK. We define the coverage as the number of
sequences per case. We calculate one value per day per LA. In order to
smooth the effects of small numbers, the number of sequences and cases
given to a day is the sum of sequences or cases over the preceding two
weeks.

Sequences are counted from the COG-UK server, and cases are accessed
from the ONS API (<https://api.coronavirus.data.gov.uk>).

## Weights

The coverages lend themselves to the definition of sample weights, which
can be used to reweight sequences in analyses. The inverse of the
coverage is the number of cases
(![c](https://latex.codecogs.com/png.latex?c "c")) per sequence
(![s](https://latex.codecogs.com/png.latex?s "s")), which we denote
![v\_{i,j}](https://latex.codecogs.com/png.latex?v_%7Bi%2Cj%7D
"v_{i,j}") for LA ![i](https://latex.codecogs.com/png.latex?i "i") and
day ![j](https://latex.codecogs.com/png.latex?j "j"). Then, to rebalance
all LAs at a single day ![j](https://latex.codecogs.com/png.latex?j
"j"), we define the weights   
![w\_{i,j} =
v\_{i,j}/v\_j^\*](https://latex.codecogs.com/png.latex?w_%7Bi%2Cj%7D%20%3D%20v_%7Bi%2Cj%7D%2Fv_j%5E%2A
"w_{i,j} = v_{i,j}/v_j^*")  
where
![v\_j^\*=\\max\_i(v\_{i,j})](https://latex.codecogs.com/png.latex?v_j%5E%2A%3D%5Cmax_i%28v_%7Bi%2Cj%7D%29
"v_j^*=\\max_i(v_{i,j})").

Likewise, to consider a timeseries over a single LA (or a larger,
aggregated geography), we would define   
![w\_{i,j} =
v\_{i,j}/v\_i^\*](https://latex.codecogs.com/png.latex?w_%7Bi%2Cj%7D%20%3D%20v_%7Bi%2Cj%7D%2Fv_i%5E%2A
"w_{i,j} = v_{i,j}/v_i^*")  
where
![v\_i^\*=\\max\_j(v\_{i,j})](https://latex.codecogs.com/png.latex?v_i%5E%2A%3D%5Cmax_j%28v_%7Bi%2Cj%7D%29
"v_i^*=\\max_j(v_{i,j})"). The consequence of these reweightings is that
the most representative sequence retains a weight of 1, and all other
sequences are downweighted relatively.

From these, we can determine the effective sample size (ESS) of e.g. the
set of sequences sampled on a given day
![j](https://latex.codecogs.com/png.latex?j "j"):

  
![ESS\_j =
\\sum\_iw\_{i,j}s\_{i,j}.](https://latex.codecogs.com/png.latex?ESS_j%20%3D%20%5Csum_iw_%7Bi%2Cj%7Ds_%7Bi%2Cj%7D.
"ESS_j = \\sum_iw_{i,j}s_{i,j}.")  

Using these weight definitions, the ESS is the total number of cases
scaled down by the smallest coverage (sequences per case) among all LAs.
