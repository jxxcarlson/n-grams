module Utility where


(|>) ::  a -> (a -> b)-> b
(|>) a f = f a