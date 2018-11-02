In the R scripts, you will probably need to either modify the line
.libPaths("/home/jmm97/lib/lib64/R")
to point to whereever you have installed the NMF package, OR perhaps
even simply comment out the line, depending on your R installation.

The .csh C-shell script demonstrates how the .sh Bourne shell scripts
might be invoked.  If you try this script, you'll need to modify the
variables mainInputFolder, mainOutputFolder, and nccFolder, to
reflect the locations where stuff is kept on your particular system.
