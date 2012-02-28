-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import MyImp(test1, test2a, test2b, test3, test4a, test4b, test5)
import DimTest

main = do
    putStrLn "Hello World!\n"
    putStrLn "** Test1 **"
    putStrLn (show test1)
    putStrLn "** Test2 **"
    putStrLn (show test2a)
    putStrLn (show test2b)
    putStrLn "** Test3 **"
    putStrLn (show test3)
    putStrLn "** Test4 **"
    putStrLn (show test4a)
    putStrLn (show test4b)
    putStrLn "** Test5 **"
    putStrLn (show test5)
    putStrLn "** DimTest **"
    putStrLn (show dimTest1)
    putStrLn (show dimTest2)
    putStrLn (show dimTest3)
    putStrLn (show dimTest4)
    putStrLn (show dimTest5)
    putStrLn (show dimTest6)
    putStrLn (show dimTest7)
    putStrLn (show dimTest8)
    putStrLn (show dimTest9)

