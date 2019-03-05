check :: Env -> Exp -> Type -> Err Bool
check e (And a b) Bool = check e a Bool && check e b Bool 
check e (TrueLiteral) Bool = Ok True
check e (FalseLiteral) Bool = Ok True
