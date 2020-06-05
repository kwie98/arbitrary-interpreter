// accepts language {0^n1^n | n >= 0} with starting symbol Z
// inspired by https://en.wikipedia.org/wiki/Pushdown_automaton, although this
// PDA works slightly differently (it always reads a symbol from the input in
// the 'W'-operation and writing symbols is flipped)
#MOC PDA "01AZ"

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
