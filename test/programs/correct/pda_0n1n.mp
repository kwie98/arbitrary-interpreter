#MOC RDPAMOC "01AZ"

#PROGRAM MAIN
Start:
    I=0
        I=1
            I=
                End
                S=Z
                    End
                    acc
            S=A
                End
                pushEPSq
        S=A
            pushZAp
            pushAAp

pushZAp / WZA:
    I=0
        I=1
            I=
                End
                S=Z
                    End
                    acc
            S=A
                End
                pushEPSq
        S=A
            pushZAp
            pushAAp

pushAAp / WAA:
    I=0
        I=1
            I=
                End
                S=Z
                    End
                    acc
            S=A
                End
                pushEPSq
        S=A
            pushZAp
            pushAAp

pushEPSq / W:
    I=1
        I=
            End
            S=Z
                End
                acc
        S=A
            End
            pushEPSq

acc / ACC:
    End
