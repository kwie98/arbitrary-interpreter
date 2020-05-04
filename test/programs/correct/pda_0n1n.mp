// accepts language {0^n1^n | n >= 0} with starting symbol Z
// inspired by https://en.wikipedia.org/wiki/Pushdown_automaton, although this
// PDA works slightly differently (it always ready a symbol from the input in
// the 'W'-operation and writing symbols is flipped)
#MOC PDA "01AZ"

#PROGRAM MAIN
Start:
    p

p / NOP:
    I=0
        q
        S=A
            pushZAp
            pushAAp

pushZAp / WZA:
    p

pushAAp / WAA:
    p

q / NOP:
    I=1
        I=
            End
            S=Z
                End
                acc
        S=A
            End
            pushEPSq

pushEPSq / W:
    q

acc / ACC:
    End
