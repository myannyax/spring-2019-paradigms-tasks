import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        walter_a = robot "Walter" 20 50
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"
        , testCase "Test for getAttack" $
            getAttack walter @?= 50
        , testCase "Test for getHealth" $
            getHealth walter @?= 50
        , testCase "Test for setName" $
            setName "NewName" walter @?= robot "NewName" 50 50
        , testCase "Test for setAttack" $
            setAttack 30 walter @?= robot "Walter" 30 50
        , testCase "Test for setHealth" $
            setHealth 30 walter @?= robot "Walter" 50 30
        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"
        , testCase "Test for damage" $
            damage walter 10 @?= robot "Walter" 50 40
        , testCase "Test for isAlive" $
            isAlive walter @?= True
        , testCase "Test for fight" $
            fight walter_a walter @?= setHealth 30 walter
        , testCase "Test for threeRoundFight" $
            threeRoundFight walter_a walter @?= setHealth 30 walter
        , testCase "Test for neueAttack" $
            survivors @?= roboter
        ]
