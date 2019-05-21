{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "Smoke tests" [
            testCase "fromList" $
                let map = fromList [(1, "a"), (2, "b")] :: m Int String in do
                size map @?= 2
                Map.lookup 1 map @?= Just "a"
                Map.lookup 2 map @?= Just "b"

            ,testCase "toAscList . fromList sorts list" $
                let tr = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                Map.toAscList tr @?= [(1, "x"), (2, "a"), (3, "c")]

            ,testCase "insert" $
                let map = insert 3 "c" empty :: m Int String in
                Map.lookup 3 map @?= Just "c"

            ,testCase "insertWith" $
                let map = insertWith (++) 3 "d" (singleton 3 "c") :: m Int String in
                Map.lookup 3 map @?= Just "dc"

            ,testCase "insertWithKey" $
                let map = insertWithKey (\k new old -> show k ++ new ++ old) 3 "d" (singleton 3 "c") :: m Int String in
                Map.lookup 3 map @?= Just "3dc"

            ,testCase "delete" $
                let map = delete 3 (singleton 3 "c") :: m Int String in
                Map.null map @?= True

            ,testCase "adjust" $
                let map = adjust ("a" ++) 3 (singleton 3 "c") :: m Int String in
                Map.lookup 3 map @?= Just "ac"

            ,testCase "adjustWithKey" $
                let map = adjustWithKey (\k x -> show k ++ x) 3 (singleton 3 "c") :: m Int String in
                Map.lookup 3 map @?= Just "3c"

            ,testCase "update" $
                let map = update (const Nothing) 3 (singleton 3 "c") :: m Int String in
                Map.null map @?= True

            ,testCase "updateWithKey" $
                let map = updateWithKey (\k v -> Just $ show k ++ v) 3 (singleton 3 "c") :: m Int String in
                Map.lookup 3 map @?= Just "3c"

            ,testCase "member" $
                let map = (singleton 3 "c") :: m Int String in
                member 1 map  @?= False

            ,testCase "notMember" $
                let map = (singleton 3 "c") :: m Int String in
                notMember 1 map @?= True

            ,testCase "null" $
                let map = (singleton 3 "c") :: m Int String in
                Map.null map @?= False
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]