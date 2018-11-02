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

foreach nmfMethod ( brunet snmfl snmfr )
    set dirName1 = method_${nmfMethod}

    foreach varNorm ( With Without )
        set dirName2 = VarNorm_${varNorm}

        foreach nRun ( 1 2 )
            set dirName3 = Runs_${nRun}

            # Determine name of output directory and Bourne shell script.
            set outDirName  = ${dirName1}_${dirName2}_${dirName3}_seed_12345
            set outDirPath  = $mainOutputFolder/$outDirName
            set shellScript = georgetown_Challenge3_${nmfMethod}_${varNorm}VarNorm.sh
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

                # Invoke Bourne shell script.
                set runCMD = "$shellScript $inputDir $outputDir $nccFile $nRun"
                echo $runCMD
                $runCMD

            end; # FOREACH dataSet
        end; # FOREACH nRun
    end; # FOREACH varNorm
end; # FOREACH nmfMethod
