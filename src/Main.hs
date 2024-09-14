import AIH.Loss

main :: IO()
main = do
    print (maeLoss [1,2,3] [1,6,3])
    print (mseLoss [1,2,3] [1,6,3])
    print (zeroOneLoss [1,2,3] [1,6,3])