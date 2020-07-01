#MOC TMMOC "_01"

// This program adds 1 to the initial binary number.
#PROGRAM INCREMENT
Start:
    =_
        goright
        stepleft

goright / R:
    =_
        goright
        stepleft

stepleft / L:
    =_
        =1
            add0
            add1
        add_

add0 / W1L:
    =_
        goleft
        stepright

add1 / W0L:
    =_
        =1
            add0
            add1
        add_

add_ / W1N:
    End

goleft / L:
    =_
        goleft
        stepright

stepright / R:
    End
