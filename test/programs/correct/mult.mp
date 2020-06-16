// R3 <- R1 * R2
#MOC CMMOC 4

#PROGRAM MULT
Start:
    R3=0
        clrR3
        R1=0
            R2=0
                R4=0
                    clrR4
                    mult
                End
            End

clrR3 / R3-1:
    R3=0
        clrR3
        R1=0
            R2=0
                R4=0
                    clrR4
                    mult
                End
            End


clrR4 / R4-1:
    R3=0
        clrR3
        R1=0
            R2=0
                R4=0
                    clrR4
                    mult
                End
            End

mult / R1-1:
    pushR2_1

mult2 / R1-1:
    pushR4_1

pushR2_1 / R2-1:
    pushR2_2
pushR2_2 / R3+1:
    pushR2_3
pushR2_3 / R4+1:
    R2=0
        pushR2_1
        R1=0
            mult2
            End

pushR4_1 / R4-1:
    pushR4_2
pushR4_2 / R3+1:
    pushR4_3
pushR4_3 / R2+1:
    R4=0
        pushR4_1
        R1=0
            mult
            End
