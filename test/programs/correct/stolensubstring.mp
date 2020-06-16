// https://upsl.uber.space/aws19/mp_b_ss.txt
#MOC SMMOC 4 "ABC"

#PROGRAM SUBSTR
Start:
    Lila

Lila / NOP:
    R1=_
        R3=_
            R4=_
                Z1
                Gruen
            R4=_
                Z1
                End
        R4=_
            Z1
            End

Gruen / NOP:
    R1=A
        R1=B
            Z2C
            Z2B
        Z2A

Orange / NOP:
    R4=A
        R4=B
            Z6C
            Z6B
        Z6A

Pink / NOP:
    R1=A
        R1=B
            Z10C
            Z10B
        Z10A

Braun / NOP:
    R2=A
        R2=B
            Z12C
            Z12B
        Z12A

Z1 / R4-:
    Lila

Z2A / R4+A:
    Z3

Z2B / R4+B:
    Z3

Z2C / R4+C:
    Z3

Z3 / R1-:
    R1=_
        Gruen
        R2=_
            Z4
            Z5

Z4 / R4-:
    Z5

Z5 / R2-:
    R2=_
        Z4
        Orange

Z6A / R1+A:
    Z7

Z6B / R1+B:
    Z7

Z6C / R1+C:
    Z7

Z7 / R4-:
    Z8

Z8 / R3-:
    R3=_
        Orange
        Z9

Z9 / R4-:
    R4=_
        Z9
        Pink

Z10A / R2+A:
    Z11

Z10B / R2+B:
    Z11

Z10C / R2+C:
    Z11

Z11 / R1-:
    R1=_
        Pink
        Braun

Z12A / R4+A:
    Z13

Z12B / R4+B:
    Z13

Z12C / R4+C:
    Z13

Z13 / R2-:
    R2=_
        Braun
        End
