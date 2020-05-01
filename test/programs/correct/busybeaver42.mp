// https://en.wikipedia.org/wiki/Busy_beaver
#MOC TM "01"

#PROGRAM BUSYBEAVER42
Start:
    A

A / NOP:
    =1
        AB0
        AB0x
AB0 / W1:
    AB1
AB1 / R:
    B
AB0x / W1:
    AB1x
AB1x / L:
    B

B / NOP:
    =1
        BA0
        BC0
BA0 / W1:
    BA1
BA1 / L:
    A
BC0 / W0:
    BC1
BC1 / L:
    C

C / NOP:
    =1
        CH0
        CD0
CH0 / W1:
    CH1
CH1 / R:
    End
CD0 / W1:
    CD1
CD1 / L:
    D

D / NOP:
    =1
        DD0
        DA0
DD0 / W1:
    DD1
DD1 / R:
    D
DA0 / W0:
    DA1
DA1 / R:
    A
