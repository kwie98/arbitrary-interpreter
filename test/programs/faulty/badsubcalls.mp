#MOC CMMOC 3
// bad order of programs, MAIN calls other programs before they are defined
// R2 = R1 + R3
#PROGRAM MAIN
Start:
    S1
S1 / $MOVE3TO2:
    S2
S2 / $ADD:
    End

// R3 = R1 + R2
#PROGRAM ADD
Start:
    R3=0
        decR3
        R1=0
            decR1
            R2=0
                decR2
                End
decR3 / R3-1:
    R3=0
        decR3
        R1=0
            decR1
            R2=0
                decR2
                End
decR1 / R1-1:
    incR3
decR2 / R2-1:
    incR3
incR3 / R3+1:
    R1=0
        decR1
        R2=0
            decR2
            End

// R2 = R3
#PROGRAM MOVE3TO2
Start:
    R2=0
        decR2
        R3=0
            decR3
            End
decR2 / R2-1:
    R2=0
        decR2
        R3=0
            decR3
            End
decR3 / R3-1:
    incR2
incR2 / R2+1:
    R3=0
        decR3
        End
