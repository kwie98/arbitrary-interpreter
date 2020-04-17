// https://upsl.uber.space/aws19/mp_a_mult.txt
#MOC CM 4

#PROGRAM MULT
Start:
    R3=0
        Z7
        R4=0
            Z8
            R1=0
                R2=0
                    Z1
                    End
                End

Z1 / R1-1:
    Z2

Z2 / R2-1:
    Z3

Z3 / R3+1:
    Z4

Z4 / R4+1:
    R2=0
        Z2
        Z5

Z5 / R4-1:
    Z6

Z6 / R2+1:
    R4=0
        Z5
        R1=0
            Z1
            End

Z7 / R3-1:
    R3=0
        Z7
        Z8

Z8 / R4-1:
    R4=0
        Z8
        R1=0
            R2=0
                Z1
                End
            End
