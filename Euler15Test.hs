module Euler15Test where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Euler15
import Data.List
import Data.Set

areEqual list1 list2 = sort list1 == sort list1

main :: IO ()
main = hspec $ do

  let grid = fromList [
            fromList [1, 1], fromList [1, 2], fromList [1, 3], fromList [1, 4], fromList [1, 5],
            fromList [1, 6], fromList [1, 7], fromList [1, 8], fromList [1, 9], fromList [1, 10],
            fromList [1, 11], fromList [1, 12], fromList [1, 13], fromList [1, 14], fromList [1, 15],
            fromList [1, 16], fromList [1, 17], fromList [1, 18], fromList [1, 19], fromList [1, 20]
            -- fromList [ 2]9, fromList [29], fromList [29], fromList [20], fromList [27], fromList [21], fromList [28], fromList [27], fromList [20], fromList [27], fromList [27], fromList [20], fromList [28], fromList [23], fromList [29], fromList [28], fromList [24], fromList [26], fromList [22], fromList [20],
            -- fromList [ 3]1, fromList [39], fromList [31], fromList [33], fromList [35], fromList [39], fromList [34], fromList [39], fromList [33], fromList [31], fromList [30], fromList [37], fromList [33], fromList [38], fromList [30], fromList [33], fromList [39], fromList [33], fromList [36], fromList [35],
            -- fromList [ 4]2, fromList [40], fromList [45], fromList [43], fromList [44], fromList [40], fromList [41], fromList [42], fromList [49], fromList [44], fromList [48], fromList [46], fromList [41], fromList [42], fromList [46], fromList [41], fromList [47], fromList [42], fromList [46], fromList [41],
            -- fromList [ 5]2, fromList [51], fromList [56], fromList [51], fromList [51], fromList [57], fromList [53], fromList [59], fromList [51], fromList [52], fromList [56], fromList [54], fromList [52], fromList [50], fromList [50], fromList [58], fromList [56], fromList [53], fromList [53], fromList [50],
            -- fromList [ 6]4, fromList [67], fromList [62], fromList [60], fromList [69], fromList [63], fromList [65], fromList [62], fromList [64], fromList [65], fromList [63], fromList [63], fromList [68], fromList [66], fromList [64], fromList [60], fromList [65], fromList [67], fromList [62], fromList [60],
            -- fromList [ 7]2, fromList [78], fromList [71], fromList [78], fromList [74], fromList [73], fromList [77], fromList [70], fromList [76], fromList [78], fromList [70], fromList [77], fromList [79], fromList [74], fromList [70], fromList [76], fromList [78], fromList [78], fromList [74], fromList [70],
            -- fromList [ 8]7, fromList [86], fromList [80], fromList [88], fromList [82], fromList [82], fromList [82], fromList [80], fromList [85], fromList [83], fromList [84], fromList [89], fromList [83], fromList [88], fromList [80], fromList [81], fromList [86], fromList [89], fromList [84], fromList [81],
            -- fromList [ 9]4, fromList [95], fromList [98], fromList [95], fromList [96], fromList [93], fromList [99], fromList [96], fromList [97], fromList [97], fromList [98], fromList [98], fromList [96], fromList [93], fromList [94], fromList [98], fromList [94], fromList [99], fromList [93], fromList [92],
            -- fromList [ 2]1, fromList [36], fromList [23], fromList [09], fromList [75], fromList [00], fromList [76], fromList [44], fromList [20], fromList [45], fromList [35], fromList [14], fromList [00], fromList [61], fromList [33], fromList [97], fromList [34], fromList [31], fromList [33], fromList [95],
            -- fromList [ 7]8, fromList [17], fromList [53], fromList [28], fromList [22], fromList [75], fromList [31], fromList [67], fromList [15], fromList [94], fromList [03], fromList [80], fromList [04], fromList [62], fromList [16], fromList [14], fromList [09], fromList [53], fromList [56], fromList [92],
            -- fromList [ 1]6, fromList [39], fromList [05], fromList [42], fromList [96], fromList [35], fromList [31], fromList [47], fromList [55], fromList [58], fromList [88], fromList [24], fromList [00], fromList [17], fromList [54], fromList [24], fromList [36], fromList [29], fromList [85], fromList [57],
            -- fromList [ 8]6, fromList [56], fromList [00], fromList [48], fromList [35], fromList [71], fromList [89], fromList [07], fromList [05], fromList [44], fromList [44], fromList [37], fromList [44], fromList [60], fromList [21], fromList [58], fromList [51], fromList [54], fromList [17], fromList [58],
            -- fromList [ 1]9, fromList [80], fromList [81], fromList [68], fromList [05], fromList [94], fromList [47], fromList [69], fromList [28], fromList [73], fromList [92], fromList [13], fromList [86], fromList [52], fromList [17], fromList [77], fromList [04], fromList [89], fromList [55], fromList [40],
            -- fromList [ 0]4, fromList [52], fromList [08], fromList [83], fromList [97], fromList [35], fromList [99], fromList [16], fromList [07], fromList [97], fromList [57], fromList [32], fromList [16], fromList [26], fromList [26], fromList [79], fromList [33], fromList [27], fromList [98], fromList [66],
            -- fromList [ 8]8, fromList [36], fromList [68], fromList [87], fromList [57], fromList [62], fromList [20], fromList [72], fromList [03], fromList [46], fromList [33], fromList [67], fromList [46], fromList [55], fromList [12], fromList [32], fromList [63], fromList [93], fromList [53], fromList [69],
            -- fromList [ 0]4, fromList [42], fromList [16], fromList [73], fromList [38], fromList [25], fromList [39], fromList [11], fromList [24], fromList [94], fromList [72], fromList [18], fromList [08], fromList [46], fromList [29], fromList [32], fromList [40], fromList [62], fromList [76], fromList [36],
            -- fromList [ 2]0, fromList [69], fromList [36], fromList [41], fromList [72], fromList [30], fromList [23], fromList [88], fromList [34], fromList [62], fromList [99], fromList [69], fromList [82], fromList [67], fromList [59], fromList [85], fromList [74], fromList [04], fromList [36], fromList [16],
            -- fromList [ 2]0, fromList [73], fromList [35], fromList [29], fromList [78], fromList [31], fromList [90], fromList [01], fromList [74], fromList [31], fromList [49], fromList [71], fromList [48], fromList [86], fromList [81], fromList [16], fromList [23], fromList [57], fromList [05], fromList [54],
            -- fromList [ 0]1, fromList [70], fromList [54], fromList [71], fromList [83], fromList [51], fromList [54], fromList [69], fromList [16], fromList [92], fromList [33], fromList [48], fromList [61], fromList [43], fromList [52], fromList [01], fromList [89], fromList [19], fromList [67], fromList [48]
         ]

describe "findDivisors" $ do
  it "gives [1, 3] for findDivisors 3" $ do
    areEqual [1, 3] [1, 3] `shouldBe` True
  -- it "gives [1, 3, 9] for findDivisors 9" $ do
    -- areEqual (findDivisors 9) [1, 3, 9] `shouldBe` True
  -- it "gives [1, 2, 3, 4, 6, 12] for findDivisors 12" $ do
    -- areEqual (findDivisors 12) [1, 2, 3, 4, 6, 12] `shouldBe` True

-- describe "triangularNumberWithDivisor" $ do
  -- it "gives 28 for 5" $ do
    -- triangularNumberWithDivisor 5 `shouldBe` 28

  -- it "gives 28 for 500" $ do
    -- triangularNumberWithDivisor 500 `shouldBe` 28
