#graph is a data.frame with v1, v2 and w
#init node is numeric scalar that exists in the graph
#return list of distances between inital node and remaining nodes
dijkstra <- function(graph, init_node){
  unvisited <- matrix(c(graph[1,1],Inf,NA_integer_),ncol=3)
  visited <- c()
  
  for (v1 in graph[,1]){
    s1 <- c(unvisited[,1])
    if(! v1 %in% s1){
      unvisited <- rbind(unvisited, matrix(c(v1,Inf,NA_integer_),ncol=3))
    }
  }
  unvisited[init_node,2] <- 0
  
  curr_node = init_node
  while(length(unvisited[,1]>0)){ #could use length(unvisited) as well
    print("current node:")
    print(curr_node)
    # get the indices of all occurences of our current node in v1
    vertex_indices <- graph$v1 == curr_node
    # use these indices to get all nearest neighbours
    neighbour_nodes <- graph$v2[vertex_indices]
    # use these indices to get all weights of nearest neighbours
    distances <- graph$w[vertex_indices]
    # make a matrix with nearest neigbours and their distances
    neighbours <- matrix(c(neighbour_nodes,distances), nrow=3)
    # only keep the unvisited neighbours
    neighbours <- neighbours[neighbours[,1] %in% unvisited[,1],]
    
    #change everything downwards from here, because neighbours is a matrix now!
    
    print("neighbours:")
    print(neighbours)
    
    neighbours <- neighbours[neighbours %in% unvisited[,1]]
    #print(distances)
    
    #loop through neighbours, update distances
    unvisited[neighbours,2] <- distances
    #print(unvisited)
    visited <- c(visited,unvisited[curr_node,])
    print("visited:")
    print(visited)
    unvisited <- unvisited[-curr_node,]
    print("unvisited:")
    print(unvisited)
    #change i to nearest vertex
    curr_node = neighbours[distances == min(distances)]
    #print(curr_node)
  }
}
