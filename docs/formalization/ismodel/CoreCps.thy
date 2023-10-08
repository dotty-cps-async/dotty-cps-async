theory CoreCps
  imports Main
begin



datatype  typeexpr = AnyTp
     |
      ConstTp
     |
      ArrowTp typeexpr typeexpr
     |
      MonadTp typeexpr
     |
      ErrorTp



datatype expr = ConstantExpr int
  |
   Let int typeexpr expr
  |
   Block "expr list"
  |
   If expr expr expr
  |
   While expr expr 
  |
   Lambda int typeexpr  expr 
  |
   App expr expr
  |
   Error string



fun isError :: "expr \<Rightarrow> bool" where
   "isError (Error s) = True" |
   "isError other = False"

fun lub :: "typeexpr \<Rightarrow> typeexpr \<Rightarrow> typeexpr" where
   "lub ConstTp ConstTp = ConstTp"
  |
   "lub (ArrowTp a1 b1) (ArrowTp a2 b2) = 
    (if (a1 = a2) then (ArrowTp a1 (lub b1 b2)) else AnyTp)
   " 
  |
   "lub ErrorTp x = ErrorTp"
  |
   "lub x ErrorTp = ErrorTp"
  |
   "lub AnyTp x = AnyTp" 
  |
   "lub x AnyTp = AnyTp"
  |
   "lub ConstTp (ArrowTp x y) = AnyTp"
  |
   "lub (ArrowTp x y) ConstTp = AnyTp"

type_synonym varIndex = int
type_synonym typeVarState = "varIndex \<Rightarrow> typeexpr"

fun typeExpr :: "expr \<Rightarrow> typeVarState \<Rightarrow> typeexpr" where
    "typeExpr (ConstantExpr x) s = ConstTp"
  |
    "typeExpr (Let v vt body) s = (typeExpr body (s(v:=vt)) )"
  |
    "typeExpr (Block list) s = (typeExpr (last(list)) s)"
  |
    "typeExpr (If cond ifTrue ifFalse) s = (lub (typeExpr ifTrue s) (typeExpr ifFalse s))"
  |
    "typeExpr (While e1 e2) s = ConstTp"
  |
    "typeExpr (Lambda  i tp body) s = (ArrowTp tp (typeExpr body (s(i:=tp))))"
  |
    "typeExpr (App x arg) s =
               (if ((typeExpr x s) = (ArrowTp xt yt)) then
                       (if (xt = (typeExpr arg s)) then yt else ErrorTp)
                else
                  ErrorTp)"
  | 
    "typeExpr (Error e) s = ErrorTp"


end
