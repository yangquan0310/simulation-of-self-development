library(R6)
rm(list=ls())
self<-R6Class(
  classname = "self",
  public = list(
    vector=c(),
    structure=matrix(),
    graph=NULL,
    current=NULL,
    goal=NULL,
    initialize=function(vector){
      self$vector=vector
      self$structure=private$make_structure(self$vector)
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
    },
    tsk=function(start,goal){
      self$current=start
      self$goal=goal
      private$train()
    }
  ),
  private = list(
    .learning_rate = 0.1,
    .discount_factor = 0.9,
    .exploration_rate = 1.0,
    .exploration_decay = 0.99,
    make_structure=function(v){
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
    },
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
    choose_action = function(state, neighbors_list) {
      if (runif(1) < private$.exploration_rate) {
        return(sample(neighbors_list, 1))
      } else {
        q_values <- self$structure[state, neighbors_list]
        return(neighbors_list[which.max(q_values)])
      }
    },
    action_result=function(action,current_state){
      result=action
      return(result)
    },
    result_reward=function(result){
      if (result == self$goal) {
        return(1)
      } else {
        return(-0.1)
      }
    },
    learn = function(current_state, action, reward, next_state) {
      predict = self$structure[current_state, action]
      target = reward + private$.discount_factor * max(self$structure[next_state, ])
      self$structure[current_state, action] <- predict + private$.learning_rate * (target - predict)
    },
    update_exploration_rate = function() {
      private$.exploration_rate <- private$.exploration_rate * private$.exploration_decay
    },
    train=function(){
      done = FALSE
      while (!done) {
        neighbors_list = igraph::neighbors(self$graph, self$current, mode = "out")$name
        action = private$choose_action(self$current, neighbors_list)
        next_state = private$action_result(action, self$current)
        reward = private$result_reward(next_state)
        cat("goal: ",self$goal,"\tcurrent state: ",self$current,"->","next state: ",next_state,"\n")
        private$learn(self$current, action, reward, next_state)
        if(next_state==self$goal){
          done=TRUE
        }
        self$current <- next_state
        self$graph=private$chart(self$vector,self$structure)
        self$draw_graph()
        Sys.sleep(0.5) # 可视化时暂停一段时间
      }
      private$update_exploration_rate()
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
    }
  ),
  active= list(
    learning_rate = function(value) {
      if (missing(value)) {
        return(private$.learning_rate)
      }
      private$.learning_rate <- value
    },
    discount_factor = function(value) {
      if (missing(value)) {
        return(private$.discount_factor)
      }
      private$.discount_factor <- value
    },
    exploration_rate = function(value) {
      if (missing(value)) {
        return(private$.exploration_rate)
      }
      private$.exploration_rate <- value
    },
    exploration_decay = function(value) {
      if (missing(value)) {
        return(private$.exploration_decay)
      }
      private$.exploration_decay <- value
    }
  )
)
if (interactive()) {
  v=c("self",paste0("v",1:8))
  test=self$new(vector =v)
  test$tsk("v1","v3")
  test$tsk("v1","v4")
}
