#MOC CMMOC 4

#PROGRAM ACCUMULATE3

Start:
    R4=0
        decR4
        R1=0
            decR1
            R2=0
                decR2
                R3=0
                    decR3
                    End

decR4 / R4-1:
    R4=0
        decR4
        R1=0
            decR1
            R2=0
                decR2
                R3=0
                    decR3
                    End


decR1 / R1-1:
    incR4

decR2/ R2-1:
    incR4

decR3 / R3-1:
    incR4

decR3 / R3+1: // second occurance
    incR4

incR4 / R4+1:
    R1=0
        decR1
        R2=0
            decR2
            R3=0
                decR3
                End
