#MOC SMMOC 2 "ABC"

// This program reverses the contents of register 1 and writes the result into
// the other register.
#PROGRAM REVERSEREG1
Start:
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
