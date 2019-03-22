check :: Env -> Exp -> Type -> Err Bool
check e (Or a b) TBool = check e a TBool `safeAnd` 
                         check e b TBool
check e (TrueLiteral) TBool = Ok True
check e (FalseLiteral) TBool = Ok True
