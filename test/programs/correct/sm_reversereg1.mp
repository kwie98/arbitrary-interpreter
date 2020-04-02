#MOC SM 2 "ABC"

#PROGRAM REVERSEREG1
Start: // this is a comment
    R2=_
        Z1
        R1=_
            R1=A
                R1=B
                    R1=C
                        UN
                        Z2C
                    Z2B
                Z2A
            End

// comments are great!
// this one describes the state below:
Z1 / R2-:
    R2=_
        Z1
        R1=_
            R1=A
                R1=B
                    R1=C
                        UN
                        Z2C
                    Z2B
                Z2A
            End

Z2A / R2+A:
    Z3

Z2B / R2+B:
    Z3

Z2C / R2+C:
    Z3

Z3 / R1-:
    R1=_
        R1=A
            R1=B
                R1=C
                    UN
                    Z2C
                Z2B
            Z2A
        End

UN / NOP:
    End
