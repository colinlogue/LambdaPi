
def id : forall (a : *), a -> a :=
  (fun _ x . x)
end

def const : forall (a : *) (b : *), a -> b -> a :=
  (fun _ _ x y . x) 
end


--------------
-- Naturals --
--------------

def plus : nat -> nat -> nat :=
  natElim
    (fun _ . nat -> nat)
    (fun n . n)
    (fun _ f n . succ (f n))
end

def mult : nat -> nat -> nat :=
  natElim
    (fun _ . nat -> nat)
    (fun _ . 0)
    (fun _ f n . f (succ n))
end