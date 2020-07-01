#MOC CMMOC 1

// subtracts 2 from the register
#PROGRAM sub2
Start:
    S1

S1 / R1-1:
    S2

S2 / R1-1:
    End

// subtracts 3 from the register
#PROGRAM sub3
Start:
    S1

S1 / R1-1:
    S2

S2 / R1-1:
    S3

S3 / R1-1:
    End

// subtracts 4 from the register using program calls
#PROGRAM sub4
Start:
    S1

S1 / $sub2:
    S2

S2 / $sub2:
    End

// subtracts 7 from the register using program calls
#PROGRAM sub7
Start:
    S1

S1 / $sub3:
    S2

S2 / $sub4:
    End

// subtracts 10 from the register using program calls
#PROGRAM sub10
Start:
    S1

S1 / $sub3:
    S2

S2 / $sub7:
    End
