For this Challenge #4 submission, we used the randomForest R package.
Processing steps were as follows:

(1) The 3 samples with manually assigned labels for a dataset
    are loaded into one data matrix.
(2) This data matrix is split into two random samples.
(3) The first half of the random samples is used to optimize the
    parameter 'mtry'.
(4) The second half of the random samples is used to tune the classifier
    (a tree ensemble), using the optimized value of 'mtry'.
(5) The tuned classifier is then used to classify the entire data set of
    predictor variables.

The resulting output can be seen in the directory named "Results080410".
The R script is "Challenge4-v6-08-04-10.r".
The Bourne shell "driver" script for the R script is "Challenge4.sh".

Note that the randomForest R package can give probabilities of
class assignments; these are the output files with names like
"Probabilities.001.csv".  These can be turned into "hard"
classifications by simply taking the class with the highest
predicted probability; these are the output files with names
like "001.csv".

The computing environment was
1GHz AMD Opteron(tm) Processor 250
Total Memory: 7857 KB
Swap: 8511 KB
1024 KB cache size
TLB size: 1088 4K pages

