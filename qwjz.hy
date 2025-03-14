(import unittest)


(defclass Reserved []
    (setv if 'if)
    (setv proc 'proc)
    (setv declare 'declare)
    (setv in 'in))

; Define abstract syntax & values
(defclass ExprC [])
(defclass Value [])
(defclass NumC [ExprC]
    (defn __init__ [self n]
        (setv self.n n))
    (defn __eq__ [self other]
        (= self.n other.n)))
(defclass StrC [ExprC]
    (defn __init__ [self s]
        (setv self.s s))
    (defn __eq__ [self other]
        (= self.s other.s)))    
(defclass IdC [ExprC]
    (defn __init__ [self id]
        (setv self.id id))
    (defn __eq__ [self other]
        (= self.id other.id)))
(defclass AppC [ExprC]
    (defn __init__ [self id args]
        (setv self.id id)
        (setv self.args args))
    (defn __eq__ [self other]
        (& (= self.id other.id)
           (= self.args other.args))))
(defclass LamC [ExprC]
    (defn __init__ [self args body]
        (setv self.args args)
        (setv self.body body))
    (defn __eq__ [self other]
        (& (= self.args other.args)
           (= self.body other.body))))
(defclass CondC [ExprC]
    (defn __init__ [self cond t f]
        (setv self.cond cond)
        (setv self.t t)
        (setv self.f f))
    (defn __eq__ [self other]
        (= self.b other.b)))
(defclass NumV [Value]
    (defn __init__ [self n]
        (setv self.n n))
    (defn __eq__ [self other]
        (= self.n other.n)))
(defclass StrV [Value]
    (defn __init__ [self s]
        (setv self.s s))
    (defn __eq__ [self other]
        (= self.s other.s)))    
(defclass BoolV [Value]
    (defn __init__ [self b]
        (setv self.b b))
    (defn __eq__ [self other]
        (= self.b other.b)))
(defclass PrimV [Value]
    (defn __init__ [self op]
        (setv self.op op))
    (defn __eq__ [self other]
        (= self.op other.op)))
(defclass CloV [ExprC]
    (defn __init__ [self args body env]
        (setv self.args args)
        (setv self.body body)
        (setv self.env env))
    (defn __eq__ [self other]
        (and
            (= self.args other.args)
            (= self.body other.body)
            (= self.env other.env))))
(defclass Binding []
    (defn __init__ [self id val]
        (setv self.id id)
        (setv self.val val)))

; Initialize primordial environment
(setv tl-env [
    (Binding 'true (BoolV True))
    (Binding 'false (BoolV False))
    (Binding '+ (PrimV '+))
    (Binding '- (PrimV '-))
    (Binding '* (PrimV '*))
    (Binding '/ (PrimV '/))
    (Binding '<= (PrimV '<=))
    (Binding 'equal? (PrimV 'equal?))
    (Binding 'error (PrimV 'error))])

; === top-interp ===
; parse and interpret the given Sexp
(defn top-interp [exp]
    (serialize (interp (parse exp) tl-env)))

; === interp ===
; Interprets the given AST in the given environment
(defn interp [ast env]
    (match ast
        (NumC) (NumV ast.n)
        (StrC) (StrV ast.s)
        (IdC) (lookup ast.id env)
        (CondC) (do
                    (setv interp-cond (interp ast.cond env))
                    (match interp-cond
                        (BoolV) (if (= interp-cond.b True)
                                    (interp ast.t env)
                                    (interp ast.f env))
                        other (raise (Exception "Conditionals must be boolean"))))
        (LamC) (CloV ast.args ast.body env)
        (AppC) (do
                    (setv id-val (interp ast.id env))
                    (setv arg-vals (list (map (fn [arg] (interp arg env)) ast.args)))
                    (match id-val
                        (CloV) (if (= (len arg-vals) (len id-val.args))
                                    (interp
                                        id-val.body
                                        (+ (list 
                                                (map (fn [arg val] (Binding arg val))
                                                    id-val.args
                                                    arg-vals))
                                            id-val.env))
                                    (raise (Exception "Incorrect argument count")))
                        (PrimV) (interp-prim id-val.op arg-vals)
                        other (raise (Exception "Runtime Error"))))
        other (raise (Exception "Runtime Error"))))

; === lookup ===
; Gets the value of an IdC in a given environment
(defn lookup [id env]
    (match env
        [] (raise (Exception f"Unbound indentifier: {id}"))
        other (do
                (setv fbind (get env 0))
                (if (= id fbind.id)
                    fbind.val
                    (lookup id (cut env 1 None))))))

; === interp-prim ===
; Interprets a primitive
(defn interp-prim [operation args]
   (cond 
   (= operation '+) (NumV (+ (. (get args 0) n) (. (get args 1) n)))
   (= operation '-) (NumV (- (. (get args 0) n) (. (get args 1) n)))
   (= operation '*) (NumV (* (. (get args 0) n) (. (get args 1) n)))
   (= operation '/) (NumV (/ (. (get args 0) n) (. (get args 1) n)))
   (= operation '<=) (BoolV (<= (. (get args 0) n) (. (get args 1) n)))
   (= operation 'equal?) (BoolV (= (. (get args 0) n) (. (get args 1) n)))
   (= operation 'error) (raise (Exception (get args 0)))
    True (raise ( Exception "Unhandled operation"))))
  
; === serialize ===
; Serializes a value as a string
(defn serialize [v]
    (match v
        (NumV) (str v.n)
        (StrV) v.s
        (BoolV) (if v.b
                    "true"
                    "false")
        (PrimV) "#<primop>"
        (CloV) "#<procedure>"
        other (raise (Exception "Not a value"))))

; === parse ===
; parse QWJZ from an S-expression to ExprC
(defn parse [sexp]
    (match sexp
        (| (float x)
           (int x))
            (NumC x)
        (hy.models.Symbol s)
            (match s
                (| "if" "proc" "declare" "in")
                    (raise (Exception "QWJZ invalid identifier"))
                id (IdC id))
        (hy.models.String str)
            (StrC str)
        [Reserved.if c t f]
            (CondC (parse c) (parse t) (parse f))
        [Reserved.proc [#* a] b]
            (if (args-valid? a)
                (LamC a (parse b))
                (raise (Exception "QWJZ invalid argument names in lambda")))
        ;[Reserved.declare [[a d] [c e]] :as o Reserved.in s]
        ;    (print "test" )
        [op #* args]
            (AppC (parse op) (list (map parse args)))))

; === args-valid? ===
; confirm that a list of symbols contains only unique, non-reserved identifiers
(defn args-valid? [args]
    (and (= (len args) (len (set args)))
        (not (in Reserved.if args))
        (not (in Reserved.proc args))
        (not (in Reserved.declare args))
        (not (in Reserved.in args))))

; === tests ===
(defclass TestQWJZ [unittest.TestCase]
    (defn setUp [self]
        False)

    (defn test_interp_1 [self]
        (self.assertEqual
            (interp (NumC 2) tl-env)
            (NumV 2)))
    (defn test_interp_mult [self]
        (self.assertEqual
            (interp (AppC (IdC '*) [(NumC 12) (NumC 12)]) tl-env)
            (NumV 144)))
    (defn test_interp_add [self]
        (self.assertEqual
            (interp (AppC (IdC '+) [(NumC 12) (NumC 12)]) tl-env)
            (NumV 24)))
    (defn test_interp_minus [self]
        (self.assertEqual
            (interp (AppC (IdC '-) [(NumC 12) (NumC 13)]) tl-env)
            (NumV -1)))
    (defn test_interp_divide [self]
        (self.assertEqual
            (interp (AppC (IdC '/) [(NumC 12) (NumC 6)]) tl-env)
            (NumV 2)))
    (defn test_interp_conditional [self]
        (self.assertEqual
            (interp (CondC (AppC (IdC '<=) [(NumC 5) (NumC 3)]) (NumC 1) (NumC 0)) tl-env)
            (NumV 0)))    
    (defn test_interp_equals [self]
        (self.assertEqual
            (interp (AppC (IdC 'equal?) [(NumC 3) (NumC 3)]) tl-env)
            (BoolV True)))
    (defn test_interp_error_message [self]
        (with [(self.assertRaisesRegex Exception "Unbound indentifier: fail")]
            (interp (AppC (IdC 'fail) [(StrC "placeholder")]) tl-env)))
    (defn test_top-interp [self]
        (self.assertEqual
            (top-interp '[[proc [a b c] [a [+ b c]]]
                          [proc [x] [+ [* 2 x] [* x x]]]
                          7 5])
            "168")))
    
            

(if (= __name__ "__main__")
    (unittest.main)
    False)
