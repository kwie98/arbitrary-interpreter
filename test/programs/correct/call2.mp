// program call examples
#MOC CM 4

// increment R1 by 2
#PROGRAM p2
Start:
    Z1

Z1 / R1+1:
    Z2

Z2 / R1+1:
    End

// increment R2 by 2 and decrement R3 by 1
#PROGRAM p3
Start:
    Z1

Z1 / R2+1:
    Z2

Z2 / R2+1:
    Z3

Z3 / R3-1:
    End

#PROGRAM p1

Start:
	R1=0
		Z2
		Z1

Z1 / $p2:
	Z3

Z2 / $p3:
	Z3

Z3 / R4+1:
	End



#PROGRAM p1a

Start:
	R1=0
		Z2
		Z1


Z1 / $p2 3: //equivalent with '$p2 3 1 2 4'
	Z3

Z2 / $p3 4 3 2: //equivalent with '$p2 4 3 2 1'
	Z3

Z3 / R4+1:
	End
