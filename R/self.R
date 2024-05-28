library(R6)
self<-R6Class(
  classname = "self",
  public = list(
    vector=c(),
    structure=matrix(),
    graph=NULL,
    initialize=function(vector){
      self$vector=vector
      self$structure=private$ini_matrix(self$vector)
      self$graph=private$chart(self$vector,self$structure)
    },
    get_vector=function(){
      return(self$vector)
    },
    get_structure=function(){
      return(self$structure)
    },
    get_path_distance=function(path){
      return(private$path_distance(path))
    },
    get_all_path=function(start,end){
      return(private$all_path(start,end))
    },
    get_all_path_distance=function(start,end){
      return(private$all_path_distance(start,end))
    },
    draw_graph=function(){
      plot(self$graph, 
           layout = layout_nicely(self$graph), 
           edge.label = E(self$graph)$weight, 
           vertex.size = 30, 
           vertex.label.cex = 1.5,
           edge.arrow.size = 0.1,
           edge.curved=0.1)
    }
    
  ),
  private = list(
    chart=function(vectors,structure){
      g=igraph::graph.empty(n = length(vectors), directed = TRUE)
      for (i in 1:length(vectors)) {
        for (j in 1:length(vectors)) {
          if (structure[i, j] != 0) {
            g = igraph::add_edges(g, c(i, j), weight = structure[i, j])
          }
        }
      }
      V(g)$name = vectors
      return(g)
    },
    path_distance=function(path){
      path=unlist(strsplit(path,"->"))
      dis=slider::slide2_dbl(path[-length(path)], path[-1], private$distance)
      p=sum(dis)
      return(p)
    },
    all_path=function(start,end){
      path=igraph::all_simple_paths(self$graph,from = start,to=end)%>%
        purrr::map(~paste(names(.x),collapse = "->"))%>%
        unlist()
      return(path)
    },
    all_path_distance=function(start,end){
      path=private$all_path(start,end)
      p=purrr::map_dbl(path,private$path_distance)
      names(p)=path
      return(p)
    },
    distance=function(start=NA,end=NA){
      # 检查 start 和 end 是否存在
      if (is.na(start) || is.na(end)) {
        return(NA)
      }
      # 计算 start 和 end 之间的距离
      return(self$structure[start,end])
    },
    ini_matrix=function(v){
      l=length(v)
      structure=matrix(NA,nrow=l,ncol=l)
      diag(structure)=0
      dimnames(structure)=list(v,v)
      structure[,"self"]=0
      for (i in 1:l){
        substructure=structure[i,]
        na.index=which(is.na(substructure))
        na.l=length(na.index)
        structure[i,na.index]=round(
          # c(gtools::rdirichlet(1,rep(1,na.l))),
          1,
          2)
      }
      return(structure)
      }
  )
)
if (interactive()) {
  v=c("self",paste0("v",1:8))
  test=self$new(vector =v)
  test$get_structure()
  path=test$get_all_path("v1","v3")[1]
  test$get_path_distance(path)
  test$get_all_path_distance("v1","v3")
  test$draw_graph()
}
