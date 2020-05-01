// <<<<<<<.>>, . is starting position
#MOC LBA "0<.>"

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
