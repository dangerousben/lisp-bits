(defmacro compose (&rest functions)
  (unless functions (error "No functions to compose"))
  (let ((args (gensym)))
    (labels ((comp (functions)
               (let ((head (car functions))
                     (tail (cdr functions)))
                 (if tail
                     `(multiple-value-call #',head ,(comp tail))
                     `(apply #',head ,args)))))
      `(lambda (&rest ,args) ,(comp functions)))))
