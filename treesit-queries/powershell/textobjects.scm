;; Functions
(function_statement (function_name) (script_block (_)) @function.inner) @function.outer
(class_method_definition (simple_name) (script_block (_)) @function.inner) @function.outer

;; Classes
(class_statement "class" (simple_name) (_) @class.inner) @class.outer

;; Comments
;; :TODO: figure out how to make comment.inner work correctly
(comment) @comment.inner
(comment) @comment.outer

;; Conditionals
(if_statement "if" (_) (statement_block (_)) @conditional.inner) @conditional.outer
(else_clause "else" (_) @conditional.inner) @conditional.outer
(elseif_clause "elseif" (_) (statement_block (_)) @conditional.inner) @conditional.outer
(switch_statement "switch" (_) (switch_body (_)) @conditional.inner) @conditional.outer
(switch_clause (_) (statement_block (_)) @conditional.inner) @conditional.outer

;; Loops
(foreach_statement "foreach" (_) (statement_block (_)) @loop.inner) @loop.outer
(for_statement "for" (_) (statement_block (_)) @loop.inner) @loop.outer
(while_statement "while" (_) (statement_block (_)) @loop.inner) @loop.outer
(do_statement "do" (statement_block (_)) @loop.inner) @loop.outer

;; Parameters
;; :TODO: add parameter support

;; :TODO: add support for try/catch/finally
;; :TODO: add support for begin/process/end
