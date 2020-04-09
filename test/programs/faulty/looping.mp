#MOC CM 1

// decrease register by 1, then call P2
#PROGRAM P1
Start:
    R1=0
        Dec1
        End

Dec1 / R1-1:
    R1=0
        gotoP2
        End

gotoP2 / $P2:
    End

// decrease register by 1, then call P1
#PROGRAM P2
Start:
    R1=0
        Dec1
        End

Dec1 / R1-1:
    R1=0
        gotoP1
        End

gotoP1 / $P1:
    End
