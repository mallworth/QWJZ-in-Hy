; Define abstract syntax & values
(defclass ExprC [])
(defclass Value [])
(defclass NumC [ExprC]
    (defn __init__ [self n]
        (setv self.n n)))
(defclass StrC [ExprC]
    (defn __init__ [self s]
        (setv self.s s)))    
(defclass IdC [ExprC]
    (defn __init__ [self id]
        (setv self.id id)))
(defclass AppC [ExprC]
    (defn __init__ [self id args]
        (setv self.id id)
        (setv self.args args)))
(defclass LamC [ExprC]
    (defn __init__ [self args body]
        (setv self.args args)
        (setv self.body body)))
(defclass CondC [ExprC]
    (defn __init__ [self cond t f]
        (setv self.cond cond)
        (setv self.t t)
        (setv self.f f)))
(defclass NumV [Value]
    (defn __init__ [self n]
        (setv self.n n)))
(defclass StrV [Value]
    (defn __init__ [self s]
        (setv self.s s)))    
(defclass BoolV [Value]
    (defn __init__ [self b]
        (setv self.b b)))
(defclass PrimV [Value]
    (defn __init__ [self op]
        (setv self.op op)))
(defclass CloV [ExprC]
    (defn __init__ [self args body env]
        (setv self.args args)
        (setv self.body body)
        (setv self.env env)))
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
