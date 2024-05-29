library(R6)
rm(list=ls())
self <- R6Class(
  classname = "self",
  public = list(
    vector = c(), # 向量
    adjacency_matrix = matrix(), # 邻接矩阵
    graph = NULL, # 图
    current_state = NULL, # 当前状态
    goal_state = NULL, # 目标状态
    
    initialize = function(vector) { # 初始化函数
      self$vector = vector
      self$adjacency_matrix = private$create_adjacency_matrix(self$vector)
      self$graph = private$create_graph(self$vector, self$adjacency_matrix)
    },
    
    get_vector = function() { # 获取向量
      return(self$vector)
    },
    
    get_adjacency_matrix = function() { # 获取邻接矩阵
      return(self$adjacency_matrix)
    },
    
    plot_graph = function() { # 绘制图形
      plot(self$graph,
           layout = layout_nicely(self$graph),
           edge.label = E(self$graph)$weight,
           vertex.size = 30,
           vertex.label.cex = 1.5,
           edge.arrow.size = 0.1,
           edge.curved = 0.1)
    },
    
    task_train = function(start, goal) { # 训练模型
      self$current_state = start
      self$goal_state = goal
      private$run_training()
    }
  ),
  
  private = list(
    .learning_rate = 0.1, # 学习率
    .discount_factor = 0.9, # 折扣因子
    .exploration_rate = 1.0, # 探索率
    .exploration_decay = 0.99, # 探索衰减
    
    create_adjacency_matrix = function(v) { # 创建邻接矩阵
      l = length(v)
      matrix = matrix(NA, nrow = l, ncol = l)
      diag(matrix) = 0
      dimnames(matrix) = list(v, v)
      matrix[,"self"] = 0
      for (i in 1:l) {
        sub_matrix = matrix[i, ]
        na_index = which(is.na(sub_matrix))
        na_length = length(na_index)
        matrix[i, na_index] = round(
          c(gtools::rdirichlet(1,rep(1,na_length))),
          # 1,
          2)
      }
      return(matrix)
    },
    
    create_graph = function(vectors, matrix) { # 创建图
      g = igraph::graph.empty(n = length(vectors), directed = TRUE)
      for (i in 1:length(vectors)) {
        for (j in 1:length(vectors)) {
          if (matrix[i, j] != 0) {
            g = igraph::add_edges(g, c(i, j), weight = matrix[i, j])
          }
        }
      }
      V(g)$name = vectors
      return(g)
    },
    
    select_action = function(state, neighbors_list) { # 选择动作
      if (runif(1) < private$.exploration_rate) {
        return(sample(neighbors_list, 1))
      } else {
        q_values = self$adjacency_matrix[state, neighbors_list]
        return(neighbors_list[which.max(q_values)])
      }
    },
    
    get_action_result = function(action, current_state) { # 获取动作结果
      result = action
      return(result)
    },
    
    calculate_reward = function(result) { # 计算奖励
      if (result == self$goal_state) {
        return(1)
      } else {
        return(-0.1)
      }
    },
    
    compute_time_optimal = function(action, current_state) { # 计算时间最优
      return(exp(self$adjacency_matrix[current_state, action]))
    },
    
    update_q_value = function(current_state, action, reward, next_state) { # 更新Q值
      predict = self$adjacency_matrix[current_state, action]
      target = reward + private$.discount_factor * max(self$adjacency_matrix[next_state, ])
      self$adjacency_matrix[current_state, action] <- predict + private$.learning_rate * (target - predict)
    },
    
    reduce_exploration_rate = function() { # 降低探索率
      private$.exploration_rate <- private$.exploration_rate * private$.exploration_decay
    },
    
    run_training = function() { # 运行训练
      done = FALSE
      while (!done) {
        neighbors_list = igraph::neighbors(self$graph, self$current_state, mode = "out")$name
        action = private$select_action(self$current_state, neighbors_list)
        next_state = private$get_action_result(action, self$current_state)
        reward = private$calculate_reward(next_state)
        cat("goal: ", self$goal_state, "\tcurrent state: ", self$current_state, "->", "next state: ", next_state, "\n")
        private$update_q_value(self$current_state, action, reward, next_state)
        if (next_state == self$goal_state) {
          done = TRUE
        }
        self$current_state <- next_state
        self$graph = private$create_graph(self$vector, self$adjacency_matrix)
        self$plot_graph()
        Sys.sleep(0.5) # 可视化时暂停一段时间
      }
      private$reduce_exploration_rate()
    }
  ),
  active = list(
    active_learning_rate = function(value) { # 学习率
      if (missing(value)) {
        return(private$.learning_rate)
      }
      private$.learning_rate <- value
    },
    
    active_discount_factor = function(value) { # 折扣因子
      if (missing(value)) {
        return(private$.discount_factor)
      }
      private$.discount_factor <- value
    },
    
    active_exploration_rate = function(value) { # 探索率
      if (missing(value)) {
        return(private$.exploration_rate)
      }
      private$.exploration_rate <- value
    },
    
    active_exploration_decay = function(value) { # 探索衰减
      if (missing(value)) {
        return(private$.exploration_decay)
      }
      private$.exploration_decay <- value
    }
  )
)

# 测试
if (interactive()) {
  v=c("self",paste0("v",1:8))
  test=self$new(vector =v)
  test$task_train("v1","v3")
  test$task_train("v1","v4")
}
