// https://en.wikipedia.org/wiki/Busy_beaver
#MOC TMMOC "01"

#PROGRAM BUSYBEAVER42
Start:
    =1
        A0
        A1

A0 / W1R:
    =1
        B0
        B1

A1 / W1L:
    =1
        B0
        B1

B0 / W1L:
    =1
        A0
        A1

B1 / W0L:
    =1
        C0
        C1

C0 / W1R:
    CRASH

C1 / W1L:
    =1
        D0
        D1

D0 / W1R:
    =1
        D0
        D1

D1 / W0R:
    =1
        A0
        A1

CRASH / W0N:
    CRASH
