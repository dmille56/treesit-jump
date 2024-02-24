(function_statement (function_name) (script_block (_)) @function.inner) @function.outer
(class_method_definition (simple_name) (script_block (_)) @function.inner) @function.outer

(class_statement "class" (simple_name) (_) @class.inner) @class.outer

;; :TODO: figure out how to make comment.inner work correctly
(comment) @comment.inner
(comment) @comment.outer

;; :TODO: add conditional support
;; :TODO: add loop support
;; :TODO: add parameter support

;; Examples from python:
;;
;; (function_definition
;;   body: (block)? @function.inner) @function.outer
;; 
;; (class_definition
;;   body: (block)? @class.inner) @class.outer
;; 
;; (parameters
;;   ((_) @parameter.inner . ","? @parameter.outer) @parameter.outer)
;;   
;; (lambda_parameters
;;   ((_) @parameter.inner . ","? @parameter.outer) @parameter.outer)
;; 
;; (argument_list
;;   ((_) @parameter.inner . ","? @parameter.outer) @parameter.outer)
;; 
;; (comment) @comment.inner
;; 
;; (comment)+ @comment.outer
;; 
;; ((function_definition
;;    name: (identifier) @_name
;;    body: (block)? @test.inner) @test.outer)
;; 
;; (for_statement
;;  body: (_) @loop.inner) @loop.outer
;; 
;; (while_statement
;;  body: (_) @loop.inner) @loop.outer
;; 
;; (if_statement
;;  consequence: (_) @conditional.inner) @conditional.outer
