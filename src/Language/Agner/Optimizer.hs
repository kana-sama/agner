module Language.Agner.Optimizer (optimize) where

import Language.Agner.Syntax

optimize :: Module -> Module
optimize = 
  resolveTailCalls

resolveTailCalls :: Module -> Module
resolveTailCalls = module_
  where
    module_ m =
      m{decls = map decl m.decls}

    decl = \case
      FunDecl{funid, clauses} -> FunDecl{funid, clauses = [clause funid c | c <- clauses]}
      d -> d

    clause f MkClause{pats, guards, body} =
      MkClause{pats, guards, body = exprs f body}

    expr f = \case
      Apply funid es | f == funid -> TailApply funid es
      Case e bs -> Case e (map (branch f) bs)
      Receive bs -> Receive (map (branch f) bs)
      Begin es -> Begin (exprs f es)
      e -> e

    exprs f es =
      init es ++ [expr f (last es)]

    branch f (CaseBranch p gs es) =
      CaseBranch p gs (exprs f es)
