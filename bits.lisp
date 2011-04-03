(defmacro compose (&rest functions)
  (let ((args (gensym)))
    (labels ((comp (functions)
               (let ((head (car functions))
                     (tail (cdr functions)))
                 (if tail
                     `(funcall #',head ,(comp tail))
                     `(apply #',head ,args)))))
      `(lambda (&rest ,args) ,(comp functions)))))
