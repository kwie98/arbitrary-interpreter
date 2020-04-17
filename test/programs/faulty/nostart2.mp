#MOC ISM 1 "AAABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

#PROGRAM EMPTY1
Estart / R1-: // manipulated Start state name with operation
  R1=_
    TakeOne
    End

TakeOne / R1-:
  R1=_
    TakeOne
    End
