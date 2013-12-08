module Osc
	( tau
	, ampl
	, shift
	, freq
	, zipW
	, zipWL
	, mapW
	, constW
	, cW
	, sinW
	, sinWA
	, sinWF
	, sinWAF
	, sawW
	, sawWA
	, sawWF
	, sawWAF
	, triW
	, triWA
	, triWF
	, triWAF
	, sqrW
	, sqrWA
	, sqrWF
	, sqrWAF
	, merge
	) where

import Types

import Data.Fixed (mod')

tau = pi * 2

ampl :: Osc -> Osc -> Osc
ampl = zipW (*)

campl :: Ampl -> Osc -> Osc
campl a = ampl (cW a)

shift :: Osc -> Osc -> Osc
shift shift inner = Osc (\t -> sample inner (sample shift t + t))

freq :: Osc -> Osc -> Osc
freq fr inner = Osc (\t -> sample inner (sample fr t * t))

cfreq :: Freq -> Osc -> Osc
cfreq fr = freq (cW fr)

zipW :: (Ampl -> Ampl -> Ampl) -> Osc -> Osc -> Osc
zipW f o1 o2 = Osc (\t -> f (sample o1 t) (sample o2 t))

zipWL :: ([Ampl] -> Ampl) -> [Osc] -> Osc
zipWL f os = Osc $ \t -> f (map (\o -> sample o t) os)

mapW :: (Ampl -> Ampl) -> Osc -> Osc
mapW f inner = Osc (f . sample inner)

constW :: Ampl -> Osc
constW a = Osc (\t -> a)

cW :: Ampl -> Osc
cW = constW

sinW, sawW, triW, sqrW :: Osc
sinW = Osc (\t -> sin (t * tau))
sawW = Osc (\t -> ((t + 1) `mod'` 2) - 1)
triW = Osc undefined
sqrW = Osc (\t -> if (t + 1) `mod'` 2 > 1 then 1 else -1)

sinWA, sawWA, triWA, sqrWA :: Ampl -> Osc
sinWA a = campl a sinW
sawWA a = campl a sawW
triWA a = campl a triW
sqrWA a = campl a sqrW

sinWF, sawWF, triWF, sqrWF :: Freq -> Osc
sinWF f = cfreq f sinW
sawWF f = cfreq f sawW
triWF f = cfreq f triW
sqrWF f = cfreq f sqrW

sinWAF, sawWAF, triWAF, sqrWAF :: Ampl -> Freq -> Osc
sinWAF a f = cfreq f $ campl a $ sinW
sawWAF a f = cfreq f $ campl a $ sawW
triWAF a f = cfreq f $ campl a $ triW
sqrWAF a f = cfreq f $ campl a $ sqrW

merge :: [Osc] -> Osc
merge os = zipWL (\as -> sum as / l) os
	where l = fromIntegral $ length os
