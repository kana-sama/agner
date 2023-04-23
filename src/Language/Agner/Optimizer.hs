module Language.Agner.Optimizer where

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
      FunDecl{funid, body} -> FunDecl{funid, body = expr funid body}
      d -> d

    expr f = \case
      Apply funid es | f == funid -> TailApply funid es
      Case e bs -> Case e (map (branch f) bs)
      Receive bs -> Receive (map (branch f) bs)
      Seq a b -> Seq a (expr f b)
      e -> e

    branch f (CaseBranch p gs e) =
      CaseBranch p gs (expr f e)
