module Types
	( Time(..)
	, Ampl(..)
	, Freq(..)
	, Osc(..)
	) where

type Time = Double
type Ampl = Double
type Freq = Double

newtype Osc = Osc { sample :: Time -> Ampl }
