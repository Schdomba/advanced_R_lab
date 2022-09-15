#' @title Implementation of the Dijkstra Algorithm
#' 
#' @description The function \code{dijkstra} takes a graph and an initial node
#'     and calculates the shortest distance from the initial to all other nodes.
#'
#' @param graph A \code{data.frame} containing three variables (\code{v1}, 
#'     \code{v2} and \code{w}).
#'     Where \code{v1} and \code{v2} contain nodes and \code{w} contains the 
#'     distances between each of the \code{v1} and \code{v2} nodes.
#' @param init_node A numeric scalar representing the selected node from which
#'     the distances to all other nodes are being calculated. \code{init_node}
#'     has to exist in the graph.
#'
#' @return A vector containing the shortest distance from the initial to all
#'     other nodes.
#'     
#' @export
#' 
#' @examples 
#' wiki_graph <-
#' data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'           v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'           w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#' 
#' 
#' @references{
#'  \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#'  }


#graph is a data.frame with v1, v2 and w
#init node is numeric scalar that exists in the graph
#return list of distances between initial node and remaining nodes
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
  
  #initialize current node with init_node
  curr_node <- init_node
  
  #set last distance to 0
  curr_dist <- 0
  
  #"visit" all nodes
  while(any(!is.na(unvisited))){ 
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
    # set visited neighbours to NA NA
    neighbours[neighbours[,1] %in% visited[,1],] <- c(NA,NA)
    
    #calculate and update distances
    for(node_num in neighbours[!is.na(neighbours[,1]),1]){
      
      if(unvisited[node_num,2] > (neighbours[node_num,2] + curr_dist)){
        unvisited[node_num,2] <- neighbours[node_num,2] + curr_dist
        unvisited[node_num,3] <- curr_node
      }
    }
    
    visited [curr_node,]<- unvisited[curr_node,]
    unvisited [curr_node,]<- c(NA,NA,NA)
    
    #change current node to nearest vertex
    curr_node <- which.min(unvisited[,2])
    curr_dist <- unvisited[curr_node,2]
    
  }
  return(c(visited[,2]))
}
