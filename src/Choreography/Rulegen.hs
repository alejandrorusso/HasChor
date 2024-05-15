{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Choreography.Rulegen where

import Data.List
import Control.Monad

import Language.Haskell.TH.Syntax

compileFor :: Int -> [(String, (String, Int))] -> Q [Dec]
compileFor endpoint config = do
  r <- mkRun endpoint config
  return $ endpointlist endpoints ++ rewriteConvenience ++ mkProxies endpoints ++ mkLocallyRules endpoint' endpoints ++ mkSendRules endpoint' endpoints ++ mkCondRules endpoint' endpoints ++ r
  where
    endpoints :: [String]
    endpoints = map fst config

    endpoint' :: String
    endpoint' = endpoints !! endpoint

handle :: String
handle = "handle"

endpointlist :: [String] -> [Dec]
endpointlist endpoints =
  [ SigD (mkName handle) eptype
  , FunD (mkName handle) [Clause [] (NormalB (AppE (VarE (mkName "error")) (LitE $ StringL $ handle <> " should not be evaluated"))) []]
  ]
  where
    epsAsTypes :: [Type]
    epsAsTypes = map (\ep -> AppT (ConT (mkName "Proxy")) (LitT (StrTyLit ep))) endpoints

    eptype :: Type
    eptype | length epsAsTypes > 1 = foldr (\a b -> Arrow a b) (head epsAsTypes) (tail epsAsTypes)

mkRun :: Int -> [(String, (String, Int))] -> Q [Dec]
mkRun endpoint config = [d| run' choreo = runChoreography (mkHttpConfig $(lift config)) choreo $( lift $ fst (config !! endpoint)) >> return () |]

mkProxies :: [String] -> [Dec]
mkProxies = concatMap (\ep -> [mkSig ep, mkTerm ep])
  where
    mkSig :: String -> Dec
    mkSig endpoint = SigD (mkName endpoint) (AppT (ConT (mkName "Proxy")) (LitT (StrTyLit endpoint)))

    mkTerm :: String -> Dec
    mkTerm endpoint = FunD (mkName endpoint) [Clause [] (NormalB (ConE (mkName "Proxy"))) []]

-- | Generates rules for locally
mkLocallyRules :: String -> [String] -> [Dec]
mkLocallyRules endpoint endpoints = properRule : otherRules
  where
    properRule :: Dec
    properRule = PragmaD $ RuleP (locallyRulename endpoint) Nothing [f endpoint, projected endpoint] (locallyLHS endpoint) locallyRHS AllPhases

    otherRules :: [Dec]
    otherRules = map (\endpoint -> PragmaD $ RuleP (locallyRulename endpoint) Nothing [f endpoint, projected endpoint] (locallyLHS endpoint) otherRHS AllPhases) (filter ((/=) endpoint) endpoints)

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

    projected :: String -> RuleBndr
    projected endpoint = TypedRuleVar (mkName endpoint) (ForallT [] [] (AppT (ConT (mkName "Proxy")) (LitT (StrTyLit endpoint))))

    locallyForallQuantifier :: String -> Type
    locallyForallQuantifier endpoint = ForallT [] [] (AppT (AppT ArrowT (ForallT [PlainTV (mkName "a") SpecifiedSpec] [] (AppT (AppT ArrowT (AppT (AppT (ConT (mkName "At")) (VarT $ mkName "a")) (LitT (StrTyLit endpoint)))) (VarT $ mkName "a")))) (AppT (VarT $ mkName "m") (VarT $ mkName "a")))

-- | Generates send rules
mkSendRules :: String -> [String] -> [Dec]
mkSendRules endpoint endpoints = properRule ++ [otherRule from to | from <- endpoints', to <- endpoints', from /= to] ++ [idrules endpoint]
  where
    endpoints' :: [String]
    endpoints' = filter ((/=) endpoint) endpoints

    properRule :: [Dec]
    properRule =
      [ PragmaD $ RuleP fromProperEP Nothing [RuleVar $ mkName "v", RuleVar $ mkName "recipient", projected endpoint] (lhs endpoint "recipient") (rhsTo "recipient") AllPhases,
        PragmaD $ RuleP toProperEP Nothing [RuleVar $ mkName "from", RuleVar $ mkName "v", projected endpoint] (lhs "from" endpoint) (rhsFrom "from") AllPhases
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

    projected :: String -> RuleBndr
    projected endpoint = TypedRuleVar (mkName endpoint) (ForallT [] [] (AppT (ConT (mkName "Proxy")) (LitT (StrTyLit endpoint))))

    otherRule :: String -> String -> Dec
    otherRule from to = PragmaD $ RuleP name Nothing [RuleVar $ mkName "v", projected from, projected to] (lhs from to) rhs AllPhases
      where
        name :: String
        name = concat ["CHOREORULES send", from, to]

        lhs :: String -> String -> Exp
        lhs from to = UInfixE (TupE [Just $ VarE $ mkName from, Just $ VarE $ mkName "v"]) (VarE $ mkName "~>") (VarE $ mkName to)

        rhs :: Exp
        rhs = AppE (VarE $ mkName "return") (VarE $ mkName "mkEmpty")

    idrules :: String -> Dec
    idrules endpoint = PragmaD $ RuleP name Nothing [RuleVar $ mkName "v", projected endpoint] lhs rhs AllPhases
      where
        name :: String
        name = concat ["CHOREORULES sendId", endpoint]

        lhs :: Exp
        lhs = UInfixE (TupE [Just $ VarE $ mkName endpoint, Just $ VarE $ mkName "v"]) (VarE $ mkName "~>") (VarE $ mkName endpoint)

        rhs :: Exp
        rhs = AppE (VarE $ mkName "return") (AppE (VarE $ mkName "wrap") (AppE (VarE $ mkName "unwrap") (VarE $ mkName "v")))

mkCondRules :: String -> [String] -> [Dec]
mkCondRules endpoint endpoints = properRule : otherRules
  where
    properRule :: Dec
    properRule = PragmaD $ RuleP (ruleName endpoint) Nothing (foralls <> [projected endpoint]) (lhs endpoint) rhs AllPhases
      where
        rhs :: Exp
        rhs = UInfixE left (VarE $ mkName ">>") right
          where
            left = (AppE (VarE $ mkName "broadcast") (AppE (VarE $ mkName "unwrap") (VarE $ mkName "v")))
            right = (AppE (VarE $ mkName "c") (AppE (VarE $ mkName "unwrap") (VarE $ mkName "v")))

    lhs :: String -> Exp
    lhs endpoint = UInfixE (TupE [Just $ VarE $ mkName endpoint, Just $ VarE $ mkName "v"]) (VarE $ mkName "cond") (VarE $ mkName "c")

    otherRules :: [Dec]
    otherRules = map (\endpoint -> PragmaD $ RuleP (ruleName endpoint) Nothing (foralls <> [projected endpoint]) (lhs endpoint) (rhs endpoint) AllPhases) (filter ((/=) endpoint) endpoints)
      where
        rhs :: String -> Exp
        rhs endpoint = UInfixE (AppE (VarE $ mkName "recv") (AppE (VarE $ mkName "toLocTm") (VarE $ mkName endpoint))) (VarE $ mkName ">>=") (LamE [VarP $ mkName "x"] (AppE (VarE $ mkName "c") (VarE $ mkName "x")))
--        rhs endpoint = UInfixE (AppE (VarE $ mkName "recv") (AppE (VarE $ mkName "toLocTm") (VarE $ mkName endpoint))) (VarE $ mkName ">>=") (LamE [VisAP $ VarP $ mkName "x"] (AppE (VarE $ mkName "c") (VarE $ mkName "x")))

    projected :: String -> RuleBndr
    projected endpoint = TypedRuleVar (mkName endpoint) (ForallT [] [] (AppT (ConT (mkName "Proxy")) (LitT (StrTyLit endpoint))))

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

-- Generate Specialisation

genSpec :: String -> Q [Dec]
genSpec function = do
  i <- reify (mkName function)
  specialise (mkName function)
  when (not $ isVarI i) (error "reify returned something that was not a value variable")
  let (VarI _ (ForallT _ ctx t) _) = i
  runIO $ putStrLn "===== all parameters ====="
  runIO $ putStrLn $ unlines $ map show $ allArgs t
  runIO $ putStrLn "===== only proxies ===== "
  runIO $ putStrLn $ unlines $ map show $ filter (isProxy . snd) $ allArgs t --i
  runIO $ putStrLn  "===== Only KnownSymbol Constraints ====="
  runIO $ putStrLn $ unlines $ map show $ filter (isKnownSymbol) $ ctx
  runIO $ putStrLn "===== To Specialise ====="
  runIO $ putStrLn $ unlines $ map show $ whichArgumentsToSpecialise i

  exps <- allendpoints
  runIO $ putStrLn $ show exps

  undefined

specialise :: Name -> Q [Dec]
specialise n = do
  i <- reify n
  when (not $ isVarI i) (error "reify returned something that was not a value variable")
  let all = allArgsI i
      tospec = whichArgumentsToSpecialise i
  endpoints <- allendpoints
  let combinations = enumer (length tospec) endpoints

  -- TODO FIXME: this is where we need to continue when the TH support exists for the new syntax.
  -- all is all parameters, whereas tospec are those that should be replaced with all
  -- enumerations in combinations.
  undefined

-- | Take an Info value (assuming it is a VarI), and return a list of the positional parameters
-- that should be specialised.
whichArgumentsToSpecialise :: Info -> [(Int, Type)]
whichArgumentsToSpecialise (VarI _ (ForallT vars ctx t) _) =
  let proxyargs       = filter (isProxy . snd) $ allArgs t -- fetch all proxy parameters
      knownsymbols    = filter isKnownSymbol ctx           -- fetch the known symbol constraints, indicating which are polymorphic
      knownsymbolvars = map (\(KnownSymbolType v) -> v) knownsymbols
      allproxies = filter (\(i, ProxyType v) -> v `elem` knownsymbolvars) proxyargs -- return those proxy parameters that should be specialised
  in nubBy (\e1 e2 -> snd e1 == snd e2) allproxies

-- | Returns all potential endpoints as expressions
allendpoints :: Q [Exp]
allendpoints = do
  i <- reify (mkName handle) -- we previously created a dummy function whose type will tell us what possible endpoints there are
                             -- and here we read its type to retrieve the information
  when (not $ isVarI i) (error "reify could not properly manage handle")

  let VarI _ t _ = i
      args = map snd $ allArgs t
      argsAsExps = map (\(AppT _ (LitT (StrTyLit s))) -> s) args -- turn each endpoint (currently a type) into an expression
  return $ map (VarE . mkName) argsAsExps

isVarI :: Info -> Bool
isVarI (VarI _ _ _) = True
isVarI _            = False

isProxy :: Type -> Bool
isProxy (ProxyType _) = True
isProxy _             = False

isKnownSymbol :: Type -> Bool
isKnownSymbol (KnownSymbolType _) = True
isKnownSymbol _                   = False

-- | Pattern synonym to have a nicer time working with function types
pattern Arrow t1 t2 = AppT (AppT ArrowT t1) t2

allArgsI :: Info -> [(Int, Type)]
allArgsI (VarI _ t _) = allArgs t

-- | Traverse a type and return a list of all the arguments to the function, as well as its
-- position in the argument list
allArgs :: Type -> [(Int, Type)]
allArgs t = case t of
  ForallT _ _ t -> allArgs' t 0
  t -> allArgs' t 0
  where
    allArgs' (Arrow t1 t2) n = (n,t1) : allArgs' t2 (n+1)
    allArgs' t n = [(n,t)]

-- | Pattern to see if something is a Proxy-something
pattern ProxyType v = AppT (ConT (Name (OccName "Proxy") (NameG TcClsName (PkgName "ghc-internal") (ModName "GHC.Internal.Data.Proxy")))) (VarT v)

-- | Pattern to match against a KnownSymbol constraint
pattern KnownSymbolType v = AppT (ConT (Name (OccName "KnownSymbol") (NameG TcClsName (PkgName "ghc-internal") (ModName "GHC.Internal.TypeLits")))) (VarT v)

enumer :: Int -> [a] -> [[a]]
enumer 0 _ = []
enumer 1 xs = map (\x -> [x]) xs
enumer n xs = let ys = enumer (n-1) xs
              in [ x : ys' | x <- xs, ys' <- ys]

{-@


merge ::
  KnownSymbol a =>
  Proxy a ->
  KnownSymbol b =>
  Proxy b ->
  KnownSymbol c =>
  Proxy c ->
  [Int] @ b ->
  [Int] @ c ->
  Choreo IO ([Int] @ a)


builds this AST


ForallT [ KindedTV a_6989586621679025463 SpecifiedSpec (ConT GHC.Types.Symbol)
        , KindedTV b_6989586621679025464 SpecifiedSpec (ConT GHC.Types.Symbol)
        , KindedTV c_6989586621679025465 SpecifiedSpec (ConT GHC.Types.Symbol)]
        [AppT (ConT GHC.Internal.TypeLits.KnownSymbol) (VarT a_6989586621679025463)]
        (AppT
          (AppT ArrowT (AppT (ConT GHC.Internal.Data.Proxy.Proxy) (VarT a_6989586621679025463)))
          (ForallT [] [AppT
                         (ConT GHC.Internal.TypeLits.KnownSymbol)
                         (VarT b_6989586621679025464)]
                      (AppT
                        (AppT
                          ArrowT
                          (AppT
                            (ConT GHC.Internal.Data.Proxy.Proxy)
                            (VarT b_6989586621679025464))
                        )
                        (ForallT [] [AppT
                                      (ConT GHC.Internal.TypeLits.KnownSymbol)
                                      (VarT c_6989586621679025465)]
                                    (AppT
                                      (AppT
                                        ArrowT
                                        (AppT
                                          (ConT GHC.Internal.Data.Proxy.Proxy)
                                          (VarT c_6989586621679025465))
                                      )
                                      (AppT
                                        (AppT
                                          ArrowT
                                          (AppT
                                            (AppT
                                              (ConT Choreography.Location.@)
                                              (AppT
                                                ListT
                                                (ConT GHC.Types.Int)
                                              )
                                            )
                                            (VarT b_6989586621679025464)
                                          )
                                        )
                                        (AppT
                                          (AppT
                                            ArrowT
                                            (AppT
                                              (AppT
                                                (ConT Choreography.Location.@)
                                                (AppT
                                                  ListT
                                                  (ConT GHC.Types.Int)
                                                )
                                              )
                                              (VarT c_6989586621679025465)
                                            )
                                          )
                                          (AppT
                                            (AppT
                                              (ConT Choreography.Choreo.Choreo)
                                              (ConT GHC.Types.IO)
                                            )
                                            (AppT
                                              (AppT
                                                (ConT Choreography.Location.@)
                                                (AppT
                                                  ListT
                                                  (ConT GHC.Types.Int)
                                                )
                                              )
                                              (VarT a_6989586621679025463)
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )






merge ::
  (KnownSymbol a, KnownSymbol b, KnownSymbol c) =>
  Proxy a ->
  Proxy b ->
  Proxy c ->
  [Int] @ b ->
  [Int] @ c ->
  Choreo IO ([Int] @ a)

builds this AST


(ForallT [ KindedTV a_6989586621679025463 SpecifiedSpec (ConT GHC.Types.Symbol)
         , KindedTV b_6989586621679025464 SpecifiedSpec (ConT GHC.Types.Symbol)
         , KindedTV c_6989586621679025465 SpecifiedSpec (ConT GHC.Types.Symbol)
         ]
         [ AppT (ConT GHC.Internal.TypeLits.KnownSymbol) (VarT a_6989586621679025463)
         , AppT (ConT GHC.Internal.TypeLits.KnownSymbol) (VarT b_6989586621679025464)
         , AppT (ConT GHC.Internal.TypeLits.KnownSymbol) (VarT c_6989586621679025465)
         ]
         (AppT
           (AppT
             ArrowT 
             (AppT
               (ConT GHC.Internal.Data.Proxy.Proxy)
               (VarT a_6989586621679025463)
             )
           )
           (AppT
             (AppT
               ArrowT
               (AppT
                 (ConT GHC.Internal.Data.Proxy.Proxy)
                 (VarT b_6989586621679025464)
               )
             )
             (AppT
               (AppT
                 ArrowT
                 (AppT
                   (ConT GHC.Internal.Data.Proxy.Proxy)
                   (VarT c_6989586621679025465)
                 )
               )
               (AppT
                 (AppT
                   ArrowT
                   (AppT
                     (AppT
                       (ConT Choreography.Location.@)
                       (AppT
                         ListT
                         (ConT GHC.Types.Int)
                       )
                     )
                     (VarT b_6989586621679025464)
                   )
                 )
                 (AppT
                   (AppT
                     ArrowT
                     (AppT
                       (AppT
                         (ConT Choreography.Location.@)
                         (AppT
                           ListT
                           (ConT GHC.Types.Int)
                         )
                       )
                       (VarT c_6989586621679025465)
                     )
                   )
                   (AppT
                     (AppT
                       (ConT Choreography.Choreo.Choreo)
                       (ConT GHC.Types.IO)
                     )
                     (AppT
                       (AppT
                         (ConT Choreography.Location.@)
                         (AppT
                           ListT
                           (ConT GHC.Types.Int)
                         )
                       )
                       (VarT a_6989586621679025463)
                     )
                   )
                 )
               )
             )
           )
         )
       )

The second one seems much easier to work with, and is the preferred one. We should write a little function that takes such a type, analyses it, and generates the specialisation rules.

@-}