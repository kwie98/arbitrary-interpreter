#MOC PDA "abA_"
#PROGRAM P
Start: // S=
    I=
        I=a
            End
            z0aB
        End

ACC / ACC :
    End

z0aB / W_ : // S=_
    I=
        I=a
            z0b_
            z0a_
        End

z0a_ / WA_ : // S=A
    I=
        I=a
            z0bA
            z0aA
        End

z0aA / WAA : // S=A
    I=
        I=a
            z0bA
            z0aA
        End

z0bA / W :
    I=
        I=b
            End
            S=A
                S=_
                    End
                    z1b_
                z1bA
        End

z0b_ / W :
    I=
        End
        ACC

z1bA / W :
    I=
        I=b
            End
            S=A
                S=_
                    End
                    z1b_
                z1bA
        End

z1b_ / W :
    I=
        End
        ACC
