##### kNN (Nearest Neighbor) Example #####
## Import libraries
library(class)

## Load signs dataset
signs <- read.csv('~/Documents/Code/R_Programming/Data Science/Nearest_Neighbors/signs.csv',
                  stringsAsFactors = FALSE)[,3:51]

## Sign to classify
next_sign <- data.frame('r1' = 204,'g1' = 227,'b1' = 220,'r2' = 196,'g2' = 59,
                        'b2' = 51,'r3' = 202,'g3' = 67,'b3' = 59,'r4' = 204,
                        'g4' = 227,'b4' = 220,'r5' = 236,'g5' = 250,'b5' = 234,
                        'r6' = 242,'g6' = 252,'b6' = 235,'r7' = 205,'g7' = 148,
                        'b7' = 131,'r8' = 190,'g8' = 50,'b8' = 43,'r9' = 179,
                        'g9' = 70,'b9' = 57,'r10' = 242,'g10' = 229,'b10' = 212,
                        'r11' = 190,'g11' = 50,'b11' = 43,'r12' = 193,'g12' = 51,
                        'b12' = 44,'r13' = 170,'g13' = 197,'b13' = 196,'r14' = 190,
                        'g14' = 50,'b14' = 43,'r15' = 190,'g15' = 47,'b15' = 41,
                        'r16' = 165,'g16' = 195,'b16' = 196)

## Segment out the labels
sign_types <- signs$sign_type

## Test varying values of k - Not working
## Classify the next sign observed - Capture the results
sign_preds <- knn(train = signs[-1], test = next_sign, cl = sign_types)

## Compute the accuracy of the baseline model (default k = 1)
k_1 <- knn(train = signs[-1], test = signs_test[-1], cl = sign_types)
mean(k_1 == signs_actual)

## Modify the above to set k = 7
k_7 <- knn(train = signs[-1], test = signs_test[-1], cl = sign_types, k = 7)
mean(k_7 == signs_actual)

## Set k = 15 and compare to the above
k_15 <- knn(train = signs[-1], test = signs_test[-1], cl = sign_types, k = 15)
mean(k_15 == signs_actual)
