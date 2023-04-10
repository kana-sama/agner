module Language.Agner.Optimizer where

import Language.Agner.Syntax

optimize :: Module -> Module
optimize = 
  resolveTailCalls

resolveTailCalls :: Module -> Module
resolveTailCalls = module_
  where
    module_ m = m{decls = map decl m.decls}
    decl d = d{clauses = map clause d.clauses}
    clause c = let ?funid = c.funid in c{body = exprs c.body}
    
    exprs :: (?funid :: FunId) => _
    exprs [] = []
    exprs [e] = [expr e]
    exprs (e:es) = e : exprs es

    expr :: (?funid :: FunId) => Expr -> Expr
    expr = \case
      Apply _ funid es | ?funid == funid -> Apply TailCall funid es
      Receive cases -> Receive [(p, exprs es) | (p, es) <- cases]
      AndAlso e1 e2 -> AndAlso e1 (expr e2)
      OrElse e1 e2 -> OrElse e1 (expr e2)
      Begin es -> Begin (exprs es)
      e -> e
