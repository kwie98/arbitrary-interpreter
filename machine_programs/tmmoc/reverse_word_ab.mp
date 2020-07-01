#MOC TMMOC "Bab"

// This program accepts the language { ww^R | w in {a, b}* }
#PROGRAM P
Start:
    =B
        =a
            =b
                crash
                q0#b
            q0#a
        q0#B

crash / WBN:
    End

q0#B / WBN:
    End

q0#a / WBR:
    =B
        =a
            =b
                crash
                qa#b
            qa#a
        qa#B

q0#b / WBR:
    =B
        =a
            =b
                crash
                qb#b
            qb#a
        qb#B

qa#B / WBL:
    =a
        crash
        qa2#a

qa#a / WaR:
    =B
        =a
            =b
                crash
                qa#b
            qa#a
        qa#B

qa#b / WbR:
    =B
        =a
            =b
                crash
                qa#b
            qa#a
        qa#B

qa2#a / WBL:
    =B
        =a
            =b
                crash
                qL#b
            qL#a
        qL#B

qb#B / WBL:
    =b
        crash
        qb2#b

qb#a / WaR:
    =B
        =a
            =b
                crash
                qb#b
            qb#a
        qb#B

qb#b / WbR:
    =B
        =a
            =b
                crash
                qb#b
            qb#a
        qb#B

qb2#b / WBL:
    =B
        =a
            =b
                crash
                qL#b
            qL#a
        qL#B

qL#B / WBR:
    =B
        =a
            =b
                crash
                q0#b
            q0#a
        q0#B

qL#a / WaL:
    =B
        =a
            =b
                crash
                qL#b
            qL#a
        qL#B

qL#b / WbL:
    =B
        =a
            =b
                crash
                qL#b
            qL#a
        qL#B
