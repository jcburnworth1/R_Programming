## Sankey diagram example 1
library(networkD3)
nodes = data.frame("name" = 
                     c("Node A", # Node 0
                       "Node B", # Node 1
                       "Node C", # Node 2
                       "Node D"))# Node 3
links = as.data.frame(matrix(c(
  0, 1, 10, # Each row represents a link. The first number
  0, 2, 20, # represents the node being conntected from. 
  1, 3, 30, # the second number represents the node connected to.
  2, 3, 40),# The third number is the value of the node
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)

## Sankey diagram example 2
nodes = data.frame("name" = c('NPPES Only',
                              'CAQH - NPPES Agree',
                              'MED - NPPES Agree',
                              'Multiple Specialties Suggested',
                              'MED Only',
                              'CAQH Only',
                              'CAQH - MED - NPPES Agree',
                              'CAQH - MED Agree',
                              'No Data',
                              'LN - NPPES Agree',
                              'CAQH - LN Agree',
                              'LN Only',
                              'CAQH - LN - NPPES Agree',
                              'LN - MED Agree',
                              'CAQH - LN - MED - NPPES Agree',
                              'LN - MED - NPPES Agree',
                              'CAQH - LN - MED Agree',
                              'ASSC - CAQH - NPPES Agree',
                              'ASSC - NPPES Agree',
                              'ASSC Only',
                              'ASSC - LN - NPPES Agree',
                              'ASSC - CAQH - MED - NPPES Agree',
                              'ASSC - CAQH - MED Agree',
                              'ASSC - CAQH Agree',
                              'ASSC - MED Agree'))

links = as.data.frame(matrix(c(0,3,5,
                               0,18,944,
                               0,19,343,
                               1,17,394,
                               1,18,31,
                               1,20,2,
                               1,23,18,
                               1,24,3,
                               2,18,46,
                               2,19,77,
                               2,20,214,
                               2,24,2,
                               3,17,29,
                               3,18,343,
                               3,19,72,
                               3,20,6,
                               3,23,11,
                               3,24,3,
                               4,18,42,
                               4,19,8,
                               4,24,50,
                               5,3,3,
                               5,18,150,
                               5,19,24,
                               5,23,54,
                               6,21,73,
                               7,17,4,
                               7,18,7,
                               7,22,14,
                               8,19,31),
  byrow = TRUE, ncol = 3))

names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)