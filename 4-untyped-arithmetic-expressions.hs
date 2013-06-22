module TAPL4 where
	data T =
		  TTrue
		| TFalse
		| TIfThenElse T T T
		| TZero
		| TSucc T
		| TPred T
		| TIsZero T
		deriving(Show, Eq)

	eval_small :: T -> T
	eval_small t = let t' = eval_small1 t in
		if t == t' then t
		else eval_small t'

	eval_small1 :: T -> T
	eval_small1 t = case t of
			  TIfThenElse TTrue t2 t3  -> t2
			  TIfThenElse TFalse t2 t3 -> t3
			  TIfThenElse t1 t2 t3     -> TIfThenElse (eval_small1 t1) t2 t3
			  TSucc t1                 -> TSucc (eval_small1 t1)
			  TPred TZero              -> TZero
			  TPred (TSucc nv1)        -> nv1
			  TPred t1                 -> TPred (eval_small1 t1)
			  TIsZero TZero            -> TTrue
			  TIsZero (TSucc nv1)      -> TFalse
			  TIsZero t1               -> TIsZero (eval_small1 t1)
			  t                        -> t

	eval_large :: T -> T
	eval_large t = case t of
					TTrue -> TTrue
					TFalse -> TFalse
					TZero -> TZero
					TIfThenElse t1 t2 t3 -> case eval_large t1 of
								TTrue -> eval_large t2
								TFalse -> eval_large t3
					TSucc t1 -> TSucc (eval_large t1)
					TPred t1 -> case eval_large t1 of
								TZero -> TZero
								TSucc nv1 -> nv1
					TIsZero t1 -> case eval_large t1 of
						TZero -> TTrue
						TSucc nv1 -> TFalse



