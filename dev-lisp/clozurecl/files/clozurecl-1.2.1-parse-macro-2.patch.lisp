
(in-package "CCL")

(defun parse-macro-2 (name arglist body env &aux default-initial-value)
  "Special form of parse-macro-internal for expanding compiler macros to
handle the CLHS funcall case."
  (unless (verify-lambda-list arglist t t t)
    (error "Invalid lambda list ~s" arglist))
  (multiple-value-bind (lambda-list whole environment)
      (normalize-lambda-list arglist t t)
    (multiple-value-bind (body local-decs doc)
        (parse-body body env t)
      (let ((whole-var (gensym "WHOLE"))
            (env-var (gensym "ENVIRONMENT")))
        (multiple-value-bind (bindings binding-decls)
            (%destructure-lambda-list lambda-list whole-var nil nil
                                      :cdr-p t
                                      :whole-p nil
                                      :use-whole-var t
                                      :default-initial-value default-initial-value)
          (when environment
            (setq bindings (nconc bindings (list `(,environment ,env-var)))))
          (when whole
            (setq bindings (nconc bindings (list `(,whole ,whole-var)))))
          (values
            `(lambda (,whole-var ,env-var)
               (declare (ignorable ,whole-var ,env-var))
	       (let ((form ,whole-var)
		     (block-name ',name))
		 (if (and (listp form)
			  (eq (first form) 'funcall)
			  (listp (second form))
			  (eq 'function (first (second form)))
			  (eq block-name (second (second form))))
		     (setq ,whole-var `(block-name ,@(cddr form)))))
               (block ,name
                 (let* ,(nreverse bindings)
                   ,@(when binding-decls `((declare ,@binding-decls)))
                   ,@local-decs
                   ,@body)))
            doc))))))
