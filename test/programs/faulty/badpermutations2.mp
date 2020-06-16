#MOC CMMOC 4

// R1 <- R1 + R2
#PROGRAM ADD
Start:
    R2=0
        decR2
        End

decR2 / R2-1 1 2 3 4:
    incR1

incR1 / R1+1:
    R2=0
        decR2
        End

#PROGRAM MAIN
Start:
    Z1

Z1 / $ADD 1 2:
    Z2

Z2 / $ADD 3 4:
    Z3

Z3 / $ADD 1 3:
    End
