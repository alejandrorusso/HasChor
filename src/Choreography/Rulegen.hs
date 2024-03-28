{-# LANGUAGE TemplateHaskell #-}

module Choreography.Rulegen where

import Language.Haskell.TH.Syntax

compileFor :: Int -> [(String, (String, Int))] -> Q [Dec]
compileFor endpoint config = do
  r <- mkRun endpoint config
  return $ rewriteConvenience ++ mkProxies endpoints ++ mkLocallyRules endpoint' endpoints ++ mkSendRules endpoint' endpoints ++ mkCondRules endpoint' endpoints ++ r
  where
    endpoints :: [String]
    endpoints = map fst config

    endpoint' :: String
    endpoint' = endpoints !! endpoint

mkRun :: Int -> [(String, (String, Int))] -> Q [Dec]
mkRun endpoint config = [d| run' choreo = runChoreography (mkHttpConfig $(lift config)) choreo $( lift $ fst (config !! endpoint)) >> return () |]


-- run' choreo = runChoreography cfg choreo "server" >> return ()
--   where
--     cfg = mkHttpConfig [ ("person1", ("localhost", 4242))
--                        , ("person2", ("localhost", 4343))
--                        , ("person3", ("localhost", 4344))
--                        , ("server",  ("localhost", 4345))
--                        ]

-- | Given a list such as ["server", "person1"], will generate
--
-- @@@
-- server :: Proxy "server"
-- server = Proxy
--
-- person1 :: Proxy "person1"
-- person1 = Proxy
-- @@@
mkProxies :: [String] -> [Dec]
mkProxies = concatMap (\ep -> [noinline ep, mkSig ep, mkTerm ep])
  where
    noinline :: String -> Dec
    noinline ep = PragmaD $ InlineP (mkName ep) NoInline FunLike AllPhases

    mkSig :: String -> Dec
    mkSig endpoint = SigD (mkName endpoint) (AppT (ConT (mkName "Proxy")) (LitT (StrTyLit endpoint)))

    mkTerm :: String -> Dec
    mkTerm endpoint = FunD (mkName endpoint) [Clause [] (NormalB (ConE (mkName "Proxy"))) []]

-- | Generates rules for locally
mkLocallyRules :: String -> [String] -> [Dec]
mkLocallyRules endpoint endpoints = properRule : otherRules
  where
    properRule :: Dec
    properRule = PragmaD $ RuleP (locallyRulename endpoint) Nothing [f endpoint] (locallyLHS endpoint) locallyRHS AllPhases

    otherRules :: [Dec]
    otherRules = map (\endpoint -> PragmaD $ RuleP (locallyRulename endpoint) Nothing [f endpoint] (locallyLHS endpoint) otherRHS AllPhases) (filter ((/=) endpoint) endpoints)

    locallyRulename :: String -> String
    locallyRulename endpoint = concat ["CHOREORULES locally", endpoint]

    locallyLHS :: String -> Exp
    locallyLHS endpoint = UInfixE (VarE $ mkName endpoint) (VarE $ mkName "locally") ((VarE $ mkName "f"))

    locallyRHS :: Exp
    locallyRHS = UInfixE (VarE $ mkName "wrap") (VarE $ mkName "<$>") (AppE (VarE $ mkName "run") (AppE (VarE $ mkName "f") (VarE $ mkName "unwrap")))

    otherRHS :: Exp
    otherRHS = AppE (VarE $ mkName "return") (VarE $ mkName "mkEmpty")

    f :: String -> RuleBndr
    f endpoint = TypedRuleVar (mkName "f") (ForallT [] [] (locallyForallQuantifier endpoint))

    locallyForallQuantifier :: String -> Type
    locallyForallQuantifier endpoint = ForallT [] [] (AppT (AppT ArrowT (ForallT [PlainTV (mkName "a") SpecifiedSpec] [] (AppT (AppT ArrowT (AppT (AppT (ConT (mkName "At")) (VarT $ mkName "a")) (LitT (StrTyLit endpoint)))) (VarT $ mkName "a")))) (AppT (VarT $ mkName "m") (VarT $ mkName "a")))

-- | Generates send rules
mkSendRules :: String -> [String] -> [Dec]
mkSendRules endpoint endpoints = properRule ++ [otherRule from to | from <- endpoints', to <- endpoints', from /= to]
  where
    endpoints' :: [String]
    endpoints' = filter ((/=) endpoint) endpoints

    properRule :: [Dec]
    properRule =
      [ PragmaD $ RuleP fromProperEP Nothing [RuleVar $ mkName "v", RuleVar $ mkName "recipient"] (lhs endpoint "recipient") (rhsTo "recipient") AllPhases,
        PragmaD $ RuleP toProperEP Nothing [RuleVar $ mkName "from", RuleVar $ mkName "v"] (lhs "from" endpoint) (rhsFrom "from") AllPhases
      ]
      where
        fromProperEP :: String
        fromProperEP = concat ["CHOREORULES sendfrom", endpoint]

        toProperEP :: String
        toProperEP = concat ["CHOREORULES sendto", endpoint]

        lhs :: String -> String -> Exp
        lhs from to = UInfixE (TupE [Just $ VarE $ mkName from, Just $ VarE $ mkName "v"]) (VarE $ mkName "~>") (VarE $ mkName to)

        rhsTo :: String -> Exp
        rhsTo to = UInfixE lhs (VarE $ mkName ">>") rhs
          where
            lhs = AppE (AppE (VarE $ mkName "send") (AppE (VarE $ mkName "unwrap") (VarE $ mkName "v"))) (AppE (VarE $ mkName "toLocTm") (VarE $ mkName to))
            rhs = AppE (VarE $ mkName "return") (VarE $ mkName "mkEmpty")

        rhsFrom :: String -> Exp
        rhsFrom from = UInfixE lhs (VarE $ mkName "<$>") rhs
          where
            lhs = VarE $ mkName "wrap"
            rhs = AppE (VarE $ mkName "recv") (AppE (VarE $ mkName "toLocTm") (VarE $ mkName from))

    otherRule :: String -> String -> Dec
    otherRule from to = PragmaD $ RuleP name Nothing [RuleVar $ mkName "v"] (lhs from to) rhs AllPhases
      where
        name :: String
        name = concat ["CHOREORULES send", from, to]

        lhs :: String -> String -> Exp
        lhs from to = UInfixE (TupE [Just $ VarE $ mkName from, Just $ VarE $ mkName "v"]) (VarE $ mkName "~>") (VarE $ mkName to)

        rhs :: Exp
        rhs = AppE (VarE $ mkName "return") (VarE $ mkName "mkEmpty")

mkCondRules :: String -> [String] -> [Dec]
mkCondRules endpoint endpoints = properRule : otherRules
  where
    properRule :: Dec
    properRule = PragmaD $ RuleP (ruleName endpoint) Nothing foralls (lhs endpoint) rhs AllPhases
      where
        rhs :: Exp
        rhs = UInfixE left (VarE $ mkName ">>") right
          where
            left = (AppE (VarE $ mkName "broadcast") (AppE (VarE $ mkName "unwrap") (VarE $ mkName "v")))
            right = (AppE (VarE $ mkName "c") (AppE (VarE $ mkName "unwrap") (VarE $ mkName "v")))

    lhs :: String -> Exp
    lhs endpoint = UInfixE (TupE [Just $ VarE $ mkName endpoint, Just $ VarE $ mkName "v"]) (VarE $ mkName "cond") (VarE $ mkName "c")

    otherRules :: [Dec]
    otherRules = map (\endpoint -> PragmaD $ RuleP (ruleName endpoint) Nothing foralls (lhs endpoint) (rhs endpoint) AllPhases) (filter ((/=) endpoint) endpoints)
      where
        rhs :: String -> Exp
--        rhs endpoint = UInfixE (AppE (VarE $ mkName "recv") (AppE (VarE $ mkName "toLocTm") (VarE $ mkName endpoint))) (VarE $ mkName ">>=") (LamE [VarP $ mkName "x"] (AppE (VarE $ mkName "c") (VarE $ mkName "x")))
        rhs endpoint = UInfixE (AppE (VarE $ mkName "recv") (AppE (VarE $ mkName "toLocTm") (VarE $ mkName endpoint))) (VarE $ mkName ">>=") (LamE [VisAP $ VarP $ mkName "x"] (AppE (VarE $ mkName "c") (VarE $ mkName "x")))

    ruleName :: String -> String
    ruleName endpoint = concat ["CHOREORULES cond", endpoint]

    foralls :: [RuleBndr]
    foralls = [RuleVar $ mkName "v", RuleVar $ mkName "c"]

rewriteConvenience :: [Dec]
rewriteConvenience = [doubleArrow, condprime]
  where
    doubleArrow :: Dec
    doubleArrow = PragmaD $ RuleP "CHOREORULES locallyandsend" Nothing foralls lhsDoubleArrow rhsDoubleArrow AllPhases

    condprime :: Dec
    condprime = PragmaD $ RuleP "CHOREORULES condprime" Nothing foralls lhsCondPrime rhsCondPrime AllPhases

    foralls :: [RuleBndr]
    foralls = [RuleVar $ mkName "l", TypedRuleVar (mkName "m") (ForallT [] [] (locallyForallQuantifier "l")), RuleVar $ mkName "l'"]

    locallyForallQuantifier :: String -> Type
    locallyForallQuantifier endpoint = ForallT [] [] (AppT (AppT ArrowT (ForallT [PlainTV (mkName "a") SpecifiedSpec] [] (AppT (AppT ArrowT (AppT (AppT (ConT (mkName "At")) (VarT $ mkName "a")) (VarT $ mkName endpoint))) (VarT $ mkName "a")))) (AppT (VarT $ mkName "m") (VarT $ mkName "a")))

    lhsDoubleArrow :: Exp
    lhsDoubleArrow = UInfixE (TupE [Just $ VarE $ mkName "l", Just $ VarE $ mkName "m"]) (VarE $ mkName "~~>") (VarE $ mkName "l'")

    lhsCondPrime :: Exp
    lhsCondPrime = UInfixE (TupE [Just $ VarE $ mkName "l", Just $ VarE $ mkName "m"]) (VarE $ mkName "cond'") (VarE $ mkName "l'")

    rhsDoubleArrow :: Exp
    rhsDoubleArrow = DoE Nothing [stm1, stm2]

    rhsCondPrime :: Exp
    rhsCondPrime = DoE Nothing [stm1, stm3]

    stm1 :: Stmt
    stm1 = BindS (VarP $ mkName "x") (UInfixE (VarE $ mkName "l") (VarE $ mkName "locally") (VarE $ mkName "m"))

    stm2 :: Stmt
    stm2 = NoBindS $ UInfixE (TupE [Just $ VarE $ mkName "l", Just $ VarE $ mkName "x"]) (VarE $ mkName "~>") (VarE $ mkName "l'")

    stm3 :: Stmt
    stm3 = NoBindS $ UInfixE (TupE [Just $ VarE $ mkName "l", Just $ VarE $ mkName "x"]) (VarE $ mkName "cond") (VarE $ mkName "l'")

-- (~~>) (l, m) l' = do
--   x <- l `locally` m
--   (l, x) ~> l'

-- cond' (l, m) c = do
--   x <- l `locally` m
--   cond (l, x) c

-- "CHOREORULES cond server"  forall v c . cond (server, v)  c = broadcast (unwrap v) >> c (unwrap v)
-- "CHOREORULES cond person1" forall v c . cond (person1, v) c = recv (toLocTm person1) >>= \x -> c x