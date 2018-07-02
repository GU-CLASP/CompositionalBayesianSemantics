import Lib

modelDumbo :: P Prop
modelDumbo = do
    elephant <- newClass
    mouse <- newClass
    small <- newScalarA
    large <- newScalarA

    -- fixme: this does not imply that small and large are disjoint for every class!
    hypUniv (disjoint (is (adjAP small)) (is (adjAP large)))

    hypUniv (most (isClass elephant) (is (adjAP large)))
    hypUniv (most (isClass mouse) (is (adjAP small)))

    -- mickey <- newIndSuch [subsective large mouse]
    dumbo <- newIndSuch [subsective small elephant]

    -- return (is (adjAP large) dumbo)
    -- return (is (adjAP (forClass elephant large)) dumbo)
    return (is (forClass elephant small) dumbo)
 
main = run modelDumbo
