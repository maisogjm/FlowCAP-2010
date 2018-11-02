#!/bin/csh

# Directory containing the input CSV files.
set mainInputFolder = /home/jmm97/flowCAP/csv

# Desired output directory.  This should already exist.
set mainOutputFolder = /home/jmm97/flowCAP/Test
if ( ! -e $mainOutputFolder ) then
    echo "$mainOutputFolder does not exist"
    exit 1
endif

# The folder that contains the three CSV files containing number of
# estimated populations.
set nccFolder = /home/jmm97/flowCAP/Challenge3/NCC

foreach a ( 0.01 0.05 0.01 )
    set dirName1 = tclust_alpha_$a

    foreach nRun ( 50 100 200 )
        set dirName2 = Runs_$nRun

        foreach k ( 10 20 )
            set dirName3 = kSteps_$k

            # Determine name of output directory and Bourne shell script.
            set outDirName  = ${dirName1}_${dirName2}_${dirName3}
            set outDirPath  = $mainOutputFolder/$outDirName

            foreach dataSet ( GvHD Lymph StemCell )

                # Determine input arguments
                if ( $dataSet == "Lymph" ) then
                    set nccFile = DLBCL.csv
                else
                    set nccFile = $dataSet.csv
                endif
                set inputDir  = $mainInputFolder/$dataSet/CSV
                set outputDir = $outDirPath/$dataSet
                set nccFile   = $nccFolder/$nccFile

                set runCMD = "georgetown_Challenge3_tcluster.sh $inputDir $outputDir $nccFile $a $nRun $k"
                echo $runCMD
                $runCMD

            end
        end
    end
end
