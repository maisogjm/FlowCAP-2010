#!/bin/csh
foreach a ( 0.01 0.05 0.1 )
    foreach r ( 50 100 200 )
        foreach k ( 10 20 )

            set studyDir = tclust_alpha_${a}_Runs_${r}_kSteps_${k}

            # Check GvHD.
            set subDir = $studyDir/GvHD
            set n      = `ls $subDir | wc -w`
            echo "$subDir : $n"
            if ( $n != 12 ) echo ERROR

            set fileSize = `ls -l $subDir | tail -12 | cut -d ' ' -f17`
            foreach fs ( $fileSize )
                if ( $fs == 0 ) echo EMPTYFILE
            end


            # Check Lymph.
            set subDir = $studyDir/Lymph
            set n      = `ls $subDir | wc -w`
            echo "$subDir : $n"
            if ( $n != 30 ) echo ERROR

            set fileSize = `ls -l $subDir | tail -12 | cut -d ' ' -f17`
            foreach fs ( $fileSize )
                if ( $fs == 0 ) echo EMPTYFILE
            end


            # Check StemCell.
            set subDir = $studyDir/StemCell
            set n      = `ls $subDir | wc -w`
            echo "$subDir : $n"
            if ( $n != 30 ) echo ERROR

            set fileSize = `ls -l $subDir | tail -12 | cut -d ' ' -f17`
            foreach fs ( $fileSize )
                if ( $fs == 0 ) echo EMPTYFILE
            end

        end
    end
end
