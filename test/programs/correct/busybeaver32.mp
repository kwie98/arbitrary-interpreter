// https://en.wikipedia.org/wiki/Busy_beaver
#MOC TMMOC "01"

#PROGRAM BUSYBEAVER32
Start:
    A

A / NOP:
    =1
        AB0
        AH0
AB0 / W1:
    AB1
AB1 / R:
    B
AH0 / W1:
    AH1
AH1 / R:
    End

B / NOP:
    =1
        BC0
        BB0
BC0 / W0:
    BC1
BC1 / R:
    C
BB0 / W1:
    BB1
BB1 / R:
    B

C / NOP:
    =1
        CC0
        CA0
CC0 / W1:
    CC1
CC1 / L:
    C
CA0 / W1:
    CA1
CA1 / L:
    A
