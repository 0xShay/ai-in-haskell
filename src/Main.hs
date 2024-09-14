import AIH.Loss
import AIH.Clustering

main :: IO()
main = do
    print (maeLoss [1,2,3] [1,6,3])
    print (mseLoss [1,2,3] [1,6,3])
    print (zeroOneLoss [1,2,3] [1,6,3])

    let clusteringStructure = [[[1,2],[22,3],[3,4]],[[64,5],[5,6],[6,7]]]
    print (centroid (head clusteringStructure))
    print (distanceEuc [1,2,3] [2,4,6])
    print (inertia (head clusteringStructure))
    print (wcss clusteringStructure)