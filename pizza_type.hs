-- Author: Paul McELroy
-- Date: 9/23/2016
-- A demonstation of a "pizza" type synonym and a simple program that uses it.
-- GitHub Repository: https://github.com/Greendogo/EECS_776_HW1

type Size = Int
type Topping = String
type Crusty = Bool
type Pizza = ([Topping], Size, Crusty)

addTopping :: Pizza -> String -> Pizza
addTopping (toppings, size, crusty) topping = (toppings ++ [topping], size, crusty)

setSize :: Pizza -> Int -> Pizza
setSize (toppings, size, crusty) newSize = (toppings, newSize, crusty)

setCrusty :: Pizza -> Crusty -> Pizza
setCrusty (toppings, size, crusty) isCrusty = (toppings, size, isCrusty)

printPizza :: Pizza -> IO ()
printPizza (toppings, size, crusty) = do
  putStrLn ("\nYour pizza is a " ++ show size ++ " inch pie with a " ++ ((\x -> if x then "crusty" else "soft") crusty) ++ " crust and these toppings:")
  mapM_ putStrLn (map (\x -> " -" ++ x) toppings)
  putStrLn ""

price :: Pizza -> Int
price (toppings, size, crusty) = 3 * length toppings + (quot (3 * size * size) 60) + (\x -> if x then 2 else 0) crusty

calories :: Pizza -> Int
calories (toppings, size, crusty) = 300 * length toppings + (12 * size * size) -- Each topping is 300 calories, while each square inch contributes about 16 calories.

main = do
  putStrLn "Pizza Builder!\n---------------------\n"
  menu (["Cheese", "Sauce"], 8, False)

menu :: Pizza -> IO Pizza
menu pizza =
  do
    putStrLn "\nMain Menu:"
    putStrLn "1) Select Size"
    putStrLn "2) Add a Topping"
    putStrLn "3) Enter Crust Type"
    putStrLn "4) Print Pizza"
    putStrLn "5) Get Pizza Price"
    putStrLn "6) Get Pizza Calories"
    putStrLn "7) Clear Pizza"
    putStrLn "8) Demo"
    putStrLn "9) Quit"
    putStrLn "---------------------"
    putStrLn "Please enter selection: "
    getOption pizza

getOption :: Pizza -> IO Pizza
getOption pizza =
  do
    option <- getLine
    if option `notElem` ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
      then
        do
          putStrLn "\nPlease enter a valid selection: "
          getOption pizza
    else if option == "9"
      then
        do
          putStrLn "\nGoodbye!"
          return pizza
    else
        do
          (runTask pizza option)

runTask :: Pizza -> String -> IO Pizza
runTask pizza option
  | option == "1" = do
    putStrLn "\nPlease enter a size in inches: "
    temp <- getLine
    menu (setSize pizza (read temp :: Int))
  | option == "2" = do
    putStrLn "\nPlease enter a topping to add: "
    temp <- getLine
    menu (addTopping pizza temp)
  | option == "3" = do
    putStrLn "\nEnter \"y\" for Crusty, \"n\" for not Crusty: "
    temp <- getLine
    if temp == "Y" || temp == "y" then m True
    else if temp == "N" || temp == "y" then m False
    else do
      putStrLn "\nNot an option! No change made!"
      menu pizza
  | option == "4" = do
    printPizza pizza
    menu pizza
  | option == "5" = do
    putStrLn ("\nThe price of your pizza will be $" ++ p ++ ".00")
    putStrLn "Add $0.07 per square inch, plus $3 for each topping."
    putStrLn "Crusty crust is $2 extra"
    menu pizza
  | option == "6" = do
    putStrLn ("\nYour pizza will have " ++ c ++ " calories.")
    putStrLn "Each topping is 300 calories, while\neach square inch contributes about 16 calories."
    menu pizza
  | option == "7" = do
    putStrLn "\nPizza reset to default!"
    menu (["Cheese", "Sauce"], 8, False)
  | option == "8" = do
    putStrLn "Demo: "
    putStrLn "1) Small Default Pizza with soft crust\n  ([\"Cheese\", \"Sauce\"], 8, False):"
    putStrLn ("    Price: $" ++ (show (price defaultPizza)) ++ ".00")
    putStrLn ("    Calories: " ++ (show  (calories defaultPizza)))
    printPizza defaultPizza

    putStrLn "2) Medium Italian Pizza with crusty crust\n ([\"Cheese\", \"Sauce\", \"Pepperoni\", \"Olives\"], 14, True):"
    putStrLn ("    Price: $" ++ (show (price italianPizza)) ++ ".00")
    putStrLn ("    Calories: " ++ (show  (calories italianPizza)))
    printPizza defaultPizza

    putStrLn "3) Large Hawaiian Pizza with crusty crust\n  ([\"Cheese\", \"Sauce\", \"Ham\", \"Pineapple\"], 16, True):"
    putStrLn ("    Price: $" ++ (show (price hawaiianPizza)) ++ ".00")
    putStrLn ("    Calories: " ++ (show  (calories hawaiianPizza)))
    printPizza defaultPizza

    menu pizza
  where m t = menu (setCrusty pizza t)
        p = show (price pizza)
        c = show (calories pizza)
        defaultPizza = (["Cheese", "Sauce"], 8, False)
        italianPizza = (["Cheese", "Sauce", "Pepperoni", "Olives"], 14, True)
        hawaiianPizza = (["Cheese", "Sauce", "Ham", "Pineapple"], 16, True)
