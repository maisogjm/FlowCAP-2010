#!/bin/csh
foreach m ( brunet snmfl snmfr)
    foreach v ( TRUE FALSE )
        foreach r ( 1 20 100 )

            set studyDir = method_${m}_VarNorm_${v}_Runs_${r}_seed_12345

            #-----------------------------------------------
            # Check GvHD.
            set subDir = $studyDir/GvHD
            set n      = `ls $subDir | wc -w`
            echo "$subDir : $n"
            if ( $n != 12 ) echo ERROR

            set fileSize = `ls -l $subDir | tail -12 | cut -d ' ' -f17`
            foreach fs ( $fileSize )
                if ( $fs == 0 ) echo EMPTYFILE
            end

            #-----------------------------------------------
            # Check Lymph.
            set subDir = $studyDir/Lymph
            set n      = `ls $subDir | wc -w`
            echo "$subDir : $n"
            if ( $n != 30 ) echo ERROR

            set fileSize = `ls -l $subDir | tail -12 | cut -d ' ' -f17`
            foreach fs ( $fileSize )
                if ( $fs == 0 ) echo EMPTYFILE
            end

            #-----------------------------------------------
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
