module Data exposing(..)

import Math exposing(..)

type MediumType = Air | Ground
type Conductors = One | Two | Three

 

type alias TableItem =
  {
    crossSection : Float,
    air_1 : Int,
    ground_1 : Int,
    air_2 : Int,
    ground_2 : Int,
    air_3 : Int,
    ground_3 : Int
  }

table_cu_pva : List TableItem
table_cu_pva =
  [  TableItem 1.5 29 32 24 33 21 28
    ,TableItem 2.5 40 42 33 44 28 37
    ,TableItem 4 53 54 44 56 37 48
    ,TableItem  6 67 67 56 71 49 58
    ,TableItem 10 91 89 76 94 66 77
    ,TableItem 16 121 116 101 123 87 100
    ,TableItem 25 160 148 134 157 115 130
    ,TableItem 35 197 178 166 190 141 158
    ,TableItem 50 247 217 208 230 177 192
    ,TableItem 70 318 265 0 0 226 237
    ,TableItem 95 386 314 0 0 274 280
    ,TableItem 120 450 358 0 0 321 321
    ,TableItem 150 521 406 0 0 370 363
    ,TableItem 185 594 455 0 0 421 406
    ,TableItem 240 704 525 0 0 499 468 ]

table_al_pva : List TableItem
table_al_pva =
  [  TableItem 2.5 30 32 25 33 51 28
    ,TableItem  4 40 41 34 42 29 37
    ,TableItem  6 51 52 43 54 37 44
    ,TableItem 10 69 68 58 72 50 59
    ,TableItem 16 93 83 77 94 67 77
    ,TableItem 25 122 113 103 120 88 100
    ,TableItem 35 151 136 127 145 106 121
    ,TableItem 50 189 166 159 176 136 147
    ,TableItem 70 233 200 0 0 167 178
    ,TableItem 95 284 237 0 0 204 212
    ,TableItem 120 330 269 0 0 236 241
    ,TableItem 150 380 305 0 0 273 278
    ,TableItem 185 436 343 0 0 313 308
    ,TableItem 240 515 396 0 0 369 355 ]


type TableSelector = TableSelector MediumType Conductors


getColumn : TableSelector -> (TableItem -> Int)
getColumn selector =
  case selector of
    TableSelector Air One -> .air_1
    TableSelector Air Two -> .air_2
    TableSelector Air Three -> .air_3
    TableSelector Ground One -> .ground_1
    TableSelector Ground Two -> .ground_2
    TableSelector Ground Three -> .ground_3

getItemByCurrent : MetalType -> MediumType -> Conductors -> Int -> Maybe TableItem
getItemByCurrent metal medium conductors current =
  let
    table = if metal == Cu then table_cu_pva else table_al_pva
    f = getColumn (TableSelector medium conductors) 
  in
    table
      |> List.filter (\ rec -> f rec >= current )
      |> List.head
