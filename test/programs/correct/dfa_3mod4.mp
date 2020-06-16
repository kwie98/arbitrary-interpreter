// ~ L = {#a - #b = 3 mod 4}
#MOC DFAMOC "ab"

#PROGRAM MAIN
Start:
    I=
        I=a
            z3
            z1
        End

z0 / NXT:
    I=
        I=a
            z3
            z1
        End

z1 / NXT:
    I=
        I=a
            z0
            z2
        End

z2 / NXT:
    I=
        I=a
            z1
            z3
        End

z3 / NXT:
    I=
        I=a
            z2
            z0
        acc

acc / ACC:
    End
