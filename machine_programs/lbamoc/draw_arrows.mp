#MOC LBAMOC "0<.>"

// This program draws left pointing arrows to the left of and right pointing
// arrows to the right of the starting position, which is marked by a dot. The
// tape needs to only consist of zeroes at the start.
#PROGRAM FILL
Start:
    putdot

putdot / W. :
    LE
        goleft
        goright

goleft / L :
    LE
        putlangle
        skipright

putlangle / W< :
    goleft

skipright / R :
    =.
        RE
            skipright
            End
        onemoreright

onemoreright / R :
    RE
        putrangle
        End

putrangle / W> :
    RE
        goright
        End

goright / R :
    RE
        putrangle
        End
