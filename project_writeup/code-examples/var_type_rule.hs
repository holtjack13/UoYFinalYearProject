check e (Var x) T
  | lookupVar e x == Just T = Ok True
  | otherwise = Bad "No variable with name" ++ 
                     x ++ " in scope."

lookupVar :: Context -> Ident -> Maybe Type
lookupVar c v = lookup v c
