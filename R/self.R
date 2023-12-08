library(R6)
self<-R6Class(
  classname = "self",
  public = list(
    trait=c(),
    structure=matrix(),
    initialize=function(trait,structure){
      self$trait=trait
      self$structure=structure
      dimnames(self$structure)=list(
        self$trait,
        self$trait
      )
      
    },
    get_self_structure=function(){
      return(self$structure)
    }
  ),
  private = list()
)
if (interactive()) {
  test=self$new(
    trait=c("happy","sad"),
    structure = matrix(
      c(
        1,0,
        0,1
      ),
      nrow=2,ncol=2
    )
  )
  test$get_self_structure()
}
