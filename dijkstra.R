#graph is a data.frame with v1, v2 and w
#init node is numeric scalar that exists in the graph
#return list of distances between inital node and remaining nodes
dijkstra <- function(graph, init_node){
  stopifnot(is.numeric(init_node), is.atomic(init_node))
  stopifnot(is.data.frame(graph),ncol(graph)==3)
  
  #create a matrix for the unvisited nodes
  unvisited <- matrix(c(graph[1,1],Inf,NA_integer_),ncol=3)
  
  for (v1 in graph[,1]){
    s1 <- c(unvisited[,1])
    if(! v1 %in% s1){
      unvisited <- rbind(unvisited, matrix(c(v1,Inf,NA_integer_),ncol=3))
    }
  }
  unvisited[init_node,2] <- 0
  
  #create a matrix for the visited nodes with the same dimensions
  dimensions <- dim(unvisited)
  visited <- matrix(ncol=dimensions[2], nrow=dimensions[1])
  print("empty visited matrix:")
  print(visited)
  
  #initialize current node with init_node
  curr_node = init_node
  
  #"visit" all nodes
  while(FALSE %in% is.na(unvisited)){ #could use length(unvisited) as well
    print("current node:")
    print(curr_node)
    # get the indices of all occurences of our current node in v1
    vertex_indices <- graph$v1 == curr_node
    # use these indices to get all nearest neighbours
    neighbour_nodes <- graph$v2[vertex_indices]
    # use these indices to get all weights of nearest neighbours
    distances <- graph$w[vertex_indices]
    # create a matrix with the same dimensions as the other matrices
    neighbours <- matrix(ncol=2, nrow=dimensions[1])
    # add nearest neigbours and their distances
    neighbours[neighbour_nodes,] <- matrix(c(neighbour_nodes,distances), ncol=2)
    # only keep the unvisited neighbours
    neighbours[neighbours[,1] %in% visited[,1],] <- c(NA,NA,NA)
    
    print("neighbours:")
    print(neighbours)
    
    #TODO: calculate distances and that stuff
    print("unvisited with distances:")
    print(unvisited)
    
    visited [curr_node,]<- unvisited[curr_node,]
    print("visited:")
    print(visited)
    unvisited [curr_node,]<- c(NA,NA,NA)
    print("unvisited:")
    print(unvisited)
    #change current node to nearest vertex
    curr_node <- neighbours[min(neighbours[,2], na.rm = TRUE),1]#TODO:change calculation
    #print(curr_node)
  }
}
