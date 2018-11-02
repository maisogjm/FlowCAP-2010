METHODOLOGY
Our core classification method is based on the two-dimensional version of a method for clustering flow cytomety data, called curvHDR.  Given a data set with two dimensions or variables, 2D curvHDR defines N possibly overlapping clusters, where a cluster is defined by a many-sided polygon approximating an enclosed curve.  The value of N is determined in a data-driven manner; so N will vary from data set to data set.  The reference for curvHDR is: Naumann, U., Luta, G. and Wand, M.P. (2010).  The curvHDR method for gating flow cytometry samples.  Bioinformatics, 11:44, 1-13.

In our R code, the subfunction ClassifyNonoverlapRow is used to assign data points in non-overlapping areas to a cluster.  On the other hand, the subfunction ResolveOverlaps is used to assign data points in overlapping areas to the cluster whose centroid is nearest.  Data points which fall outside any clusters are assigned to cluster #0.  Ultimately, the main R function ClassifyUsing2DCurvHDR outputs a total of N+1 classifications: N well-defined clusters, and one class for data points falling outside any of the N well-defined clusters.

In order to use the 2D version of curvHDR, the dimensionality of the data must be reduced to 2.  This could have been done, e.g., by performing a PCA or ICA and then keeping only the first two components.  However, we thought it might be interesting to try non-negative matrix factorization (NMF), with the desired number of factors / components set to 2.  The reference for NMF is: Lee DD, Seung HS. Learning the parts of objects by non-negative matrix factorization. Nature. 1999 Oct 21;401(6755):788-91.

The NMF method requires the data to be non-negative.  However, the NDD data set is based on difference data, and therefore has negative values.  To enforce non-negativity so that NMF can be used, the absolute values of the NDD were taken.  Then, the variances within each column were set to unity, and NMF was performed.  Then, the two-column ‘W’ matrix output from NMF was transformed using the asinh function, and then fed to curvHDR for classification.

abs() --> Variance Normalization --> NMF --> asinh() --> curvHDR

For the other four data sets, if any negative values were detected, the minimum was subtracted from all values to force non-negativity; if there were no negative values, then minimum subtraction was not performed.  Thus, subtracting the minimum was conditional on the presence of negative values.  (The CFSE data has negative values, although it wasn’t described as a data set of differences.)  The processing stream for the other four data sets was as follows:

Variance Normalization --> Conditional minimum subtraction --> NMF --> asinh() --> curvHDR

A 3D version of curvHDR is under development.

____________________________________________________________

 
IMPLEMENTATION
The R package implementing NMF that we used can be obtained here:
http://cran.r-project.org/web/packages/NMF/index.html
A very new version of the NMF package is available, version 0.4.8.  While processing data for flowCAP, we discovered a bug in this package which prevented processing from going to completion.  We notified the maintainer of this package, who fixed the bug and made the amended package available on June 28.  (We have been frantically processing data since then.)  Without this bug fix, NMF will crash on some data sets.

An R package implementing curvHDR can be obtained here:
http://www.uow.edu.au/~mwand/Rpacks.html
A vignette illustrating the use of curvHDR is available at that web page.  If you have trouble installing curvHDR using the ZIP file available at that page (a colleague found it problematic), you can instead try installing curvHDR from the ZIP file included with this Cover Letter in the same folder.  Note that the ZIP file provided with this Cover Letter is a different build than the one on the website, although it is the same version number.

curvHDR has some dependencies.  You will need to install the flowCore package, available from Bioconductor here:
http://bioconductor.org/packages/2.4/bioc/html/flowCore.html

You will also need to install the misc3d, geometry, and hdrcde packages.
http://cran.r-project.org/web/packages/geometry/index.html
http://cran.r-project.org/web/packages/hdrcde/index.html
http://cran.r-project.org/web/packages/misc3d/index.html

Without these other libraries, you will get an error message when you attempt to actually load the curvHDR library into R.

Finally, the ClassifyUsing2DCurvHDR function in our script has an option to output scatter plots, although those scatter plots have been suppressed in the R code as provided.  These plots require the following two packages: geneplotter (Bioconductor) and RColorBrewer (CRAN).
http://bioconductor.org/packages/2.4/bioc/html/geneplotter.html
http://cran.r-project.org/web/packages/RColorBrewer/index.html
If you’d rather not install these two packages, comment out the lines loading these two libraries in the R code.

____________________________________________________________

COMPUTING ENVIRONMENTS
To distribute the processing load, different computers were used to process each data set.  (It was found that the data sets with larger samples, viz. NDD and CFSE, could be processed in a reasonable amount of time only on a computer with lots of memory.  Indeed, at the time of this writing I am not sure that the NDD data will be finished in time for the deadline!)  The specific computing environment used to process each data set is described in a plain text file in the data set’s folder.

Windows XP PCs were run to process this data.  However, Bourne shell scripts have been provided as illustrated in the flowCAP submission example.
http://www.terryfoxlab.ca/flowsite/download/flowCAP/Example.pdf

_______________

Computing Environment for the Lymph, GvHD, and StemCell data sets



These were relatively small data sets that my laptop could handle.

Approximate times required to process these data sets
Lymph:
GvHD:
StemCell:

_______________

NDD Computing Environment

Windows XP Professional x64 Edition

Intel Xeon CPU
5140 @ 2.33 GHz
8 GB RAM

These were larger samples that required a more powerful computer to process in a reasonable amount of time.

The first two samples took about one hour each to run.  Upon finishing these first two samples, I realized that at that rate there wasn't enough time to process all 30 samples.  Then I realized that I was probably under-utilizing the power of this four-core computer with lots of memory.  So, I started three more R sessions, and had four R sessions going simultaneously at one time, each processing one subset of the NDD data set.  I relied on Windows to distribute the processing load across the four cores.  The entire process took about ten hours, and unfortunately I wasn't able to finish processing one file (022.csv) in time.

It occurs to me that this strategy of having multiple concurrent R sessions may cause the random number generator to be in different states than it would have been if the process had been done in one long session.  This implies that the results obtained from a subsequent run (such as one that the flowCAP folks might do to test this R code) may differ slightly from the results I have obtained here.

_______________

CFSE Computing Environment

Processor: Intel Core2 Duo CPU T6400 @ 2.00GHz
RAM: 4.00GB
System type: 64-bit Operating System

Another data set composed of large samples.  This one was run on the research assistant's computer.

Similarly to what was done with the NDD data set, two R sessions were run simultaneously to take advantage of the two CPUs in this system.  Unfortunately, we were not able to process all 13 of these relatively large samples.

Again, there is a possibility that the random number generator would be in slightly different states than it would have been if the data had been processed in one long R session.  So, the results from a subsequent run may differ slightly from the results obtained here.
