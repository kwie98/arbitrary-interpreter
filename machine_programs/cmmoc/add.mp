#MOC CMMOC 2

// R1 <- R1 + R2
#PROGRAM ADD
Start:
    R2=0
        decR2
        End

decR2 / R2-1:
    incR1

incR1 / R1+1:
    R2=0
        decR2
        End
