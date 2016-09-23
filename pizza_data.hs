-- Author: Paul McELroy
-- Date: 9/23/2016
-- A demonstation of a "pizza" datatype and a simple program that uses it.
-- GitHub Repository: https://github.com/Greendogo/EECS_776_HW1

type Toppings = [Topping]

data Topping
  = Ham
  | Pineapple
  | Pepperoni
  | Olives
  | Cheese
  | Sauce
  deriving (Show)

data Crust
  = Crusty
  | Soft
  deriving (Show)

data Size
  = Small
  | Medium
  | Large
  deriving (Show, Read, Eq)

data Pizza = Pizza {toppings :: Toppings, crust :: Crust, size :: Size} deriving (Show)

addTopping :: Pizza -> String -> Pizza
addTopping pizza topping = pizza {toppings = (toppings pizza ++ (f topping))}
  where f "1" = [Cheese]
        f "2" = [Sauce]
        f "3" = [Pepperoni]
        f "4" = [Olives]
        f "5" = [Pineapple]
        f "6" = [Ham]
        f _ = []

setSize :: Pizza -> String -> Pizza
setSize pizza newSize = pizza {size = f newSize}
  where f "1" = Small
        f "2" = Medium
        f "3" = Large
        f _   = size pizza

setCrusty :: Pizza -> String -> Pizza
setCrusty pizza newCrust = pizza {crust = f newCrust}
  where f "Y" = Crusty
        f "y" = Crusty
        f "N" = Soft
        f "n" = Soft
        f _   = crust pizza

printPizza :: Pizza -> IO ()
printPizza pizza = do
  putStrLn ("\nYour pizza is a " ++ show (size pizza) ++ " pie with a " ++ show (crust pizza) ++ " crust and these toppings:")
  mapM_ putStrLn (map (\x -> " -" ++ show x) (toppings pizza))
  putStrLn ""

price :: Pizza -> Int
price pizza = 3 * length (toppings pizza) + (quot (3 * (f (size pizza)) * (f (size pizza))) 60) + g (crust pizza)
  where f Small = 8
        f Medium = 12
        f Large = 16
        g Soft = 0
        g Crusty = 2

calories :: Pizza -> Int
calories pizza = 300 * length (toppings pizza) + (12 * (f (size pizza) * f (size pizza))) -- Each topping is 300 calories, while each square inch contributes about 16 calories.
  where f Small = 8
        f Medium = 12
        f Large = 16

main = do
  putStrLn "Pizza Builder!\n---------------------\n"
  menu Pizza {toppings = [Cheese, Sauce], size = Small, crust = Soft}

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
    putStrLn "\nPlease select a size (enter a number):"
    putStrLn "  1) Small"
    putStrLn "  2) Medium"
    putStrLn "  3) Large"
    temp <- getLine
    menu (setSize pizza temp)
  | option == "2" = do
    putStrLn "\nPlease select a topping to add (enter a number):"
    putStrLn "  1) Cheese"
    putStrLn "  2) Sauce"
    putStrLn "  3) Pepperoni"
    putStrLn "  4) Olives"
    putStrLn "  5) Pineapple"
    putStrLn "  6) Ham"
    temp <- getLine
    menu (addTopping pizza temp)
  | option == "3" = do
    putStrLn "\nEnter \"y\" for Crusty, \"n\" for not Crusty: "
    temp <- getLine
    menu (setCrusty pizza temp)
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
    menu Pizza {toppings = [Cheese, Sauce], size = Small, crust = Soft}
  | option == "8" = do
    putStrLn "Demo: "
    putStrLn "1) Small Default Pizza with soft crust:\n"
    putStrLn ("    Price: $" ++ (show (price defaultPizza)) ++ ".00")
    putStrLn ("    Calories: " ++ (show  (calories defaultPizza)))
    printPizza defaultPizza

    putStrLn "2) Medium Italian Pizza with crusty crust:\n"
    putStrLn ("    Price: $" ++ (show (price italianPizza)) ++ ".00")
    putStrLn ("    Calories: " ++ (show  (calories italianPizza)))
    printPizza defaultPizza

    putStrLn "3) Large Hawaiian Pizza with crusty crust:\n"
    putStrLn ("    Price: $" ++ (show (price hawaiianPizza)) ++ ".00")
    putStrLn ("    Calories: " ++ (show  (calories hawaiianPizza)))
    printPizza defaultPizza

    menu pizza
  where m t = menu (setCrusty pizza t)
        p = show (price pizza)
        c = show (calories pizza)
        defaultPizza = Pizza {toppings = [Cheese, Sauce], size = Small, crust = Soft}
        italianPizza = Pizza {toppings = [Cheese, Sauce, Pepperoni, Olives], size = Medium, crust = Crusty}
        hawaiianPizza = Pizza {toppings = [Cheese, Sauce, Ham, Pineapple], size = Large, crust = Crusty}
