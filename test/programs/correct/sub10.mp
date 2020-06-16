// subtracts 10 (or whatever programs name is) from R1
#MOC CMMOC 1

#PROGRAM sub2
Start:
    S1

S1 / R1-1:
    S2

S2 / R1-1:
    End

#PROGRAM sub3
Start:
    S1

S1 / R1-1:
    S2

S2 / R1-1:
    S3

S3 / R1-1:
    End

#PROGRAM sub4
Start:
    S1

S1 / $sub2:
    S2

S2 / $sub2:
    End

#PROGRAM sub7
Start:
    S1

S1 / $sub3:
    S2

S2 / $sub4:
    End

#PROGRAM sub10
Start:
    S1

S1 / $sub3:
    S2

S2 / $sub7:
    End
