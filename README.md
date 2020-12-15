# Sequencing coverage and weights

## Coverage

We compute and present the coverage of sequence sampling in local authorities (LAs) in the UK. 
We define the coverage as the number of sequences per case. We calculate one value per day per LA. 
In order to smooth the effects of small numbers, the number of sequences and cases given to a day is 
the sum of sequences or cases over the preceding two weeks.

Sequences are counted from the COG-UK server, and cases are accessed from the ONS API (https://api.coronavirus.data.gov.uk).


## Weights

The coverages lend themselves to the definition of sample weights, which can be used to reweight sequences in analyses. The inverse of the coverage is the number of cases per sequence, which we denote 
$v_{i,j}$ for LA $i$ and day $j$.
