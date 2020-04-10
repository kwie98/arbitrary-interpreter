#MOC CM 1

#PROGRAM sub4
Start:
    S1

S1 / $sub2:
    S2

S2 / $sub2:
    End

#PROGRAM sub3
Start:
    S1

S1 / $sub2:
    S2

S2 / R1-1:
    End

#PROGRAM sub2
Start:
    S1

S1 / $sub1:
    S2

S2 / $sub1:
    End

#PROGRAM sub1
Start:
    S1

S1 / R1-1:
    End
