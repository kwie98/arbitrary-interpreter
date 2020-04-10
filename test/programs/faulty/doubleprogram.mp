
#MOC ISM 1 "AAABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

#PROGRAM EMPTY1
Start:
  R1=_
    TakeOne
    End

TakeOne / R1-:
  R1=_
    TakeOne
    End

// actually pushes 'A' twice
#PROGRAM EMPTY1
Start:
    Z1

Z1 / R1+A:
    Z2

Z2 / R1+A:
    End
