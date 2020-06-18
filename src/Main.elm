module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html


main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    { userWalletGBP : Int
    , shopWalletGBP : Int

    --, userInv : List ( Cage, Maybe Dragon )
    , noOfShopCages : Int
    , noOfShopDragons : Int
    , cages : List ( Cage, Maybe Dragon )

    --, dragon : Dragon
    , storedDragon : Maybe Dragon
    }


type alias Cage =
    { id : Int
    , price : Int
    , name : String
    , selectable : Bool
    }


type alias Dragon =
    { id : Int
    , price : Int
    , name : String
    , dragonKind : DragonKind
    }


newDragonRed =
    Dragon 1 15 "A new Dragon" Red


newDragonGreen =
    Dragon 1 15 "A new Dragon" Green


newDragonBlue =
    Dragon 1 15 "A new Dragon" Blue


newDragon =
    Dragon 1 15 "A new Dragon" NoType


type DragonKind
    = Red
    | Green
    | Blue
    | NoType


init : Model
init =
    { userWalletGBP = 100
    , shopWalletGBP = 100

    --, userInv = []
    , noOfShopCages = 10
    , noOfShopDragons = 10
    , cages = []

    --, dragon = { id = 0, name = "Dragon", price = 15, dragonKind = NoType }
    , storedDragon = Nothing
    }


type Msg
    = NoOp
    | BuyingCage
    | BuyingDragon Dragon
    | SellDragon Dragon
    | MovingDragon Dragon
    | ChooseCageForBoughtDragon Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        BuyingCage ->
            { model
                | cages = ( Cage (List.length model.cages) 10 "Steel Cage" False, Nothing ) :: model.cages
                , userWalletGBP = model.userWalletGBP - 10
                , shopWalletGBP = model.shopWalletGBP + 10
                , noOfShopCages = model.noOfShopCages - 1
            }

        BuyingDragon dragonOne ->
            { model
                | storedDragon = Just dragonOne
                , userWalletGBP = model.userWalletGBP - 10
                , shopWalletGBP = model.shopWalletGBP + 10
                , noOfShopDragons = model.noOfShopDragons - 1
                , cages = List.map (\( cage, maybeDragon ) -> ( { cage | selectable = True }, maybeDragon )) model.cages
            }

        SellDragon aDragon ->
            { model
                | userWalletGBP = model.userWalletGBP + 10
                , shopWalletGBP = model.shopWalletGBP - 10
                , noOfShopDragons = model.noOfShopDragons + 1
                , cages =
                    List.map
                        (\( cage, maybeDragon ) ->
                            let
                                maybeDragonUnpack =
                                    maybeDragon |> Maybe.withDefault (Dragon 99999 15 "dragon" NoType)
                            in
                            if maybeDragonUnpack.id == aDragon.id then
                                --Remove the dragon from the cage.
                                ( cage, Nothing )

                            else
                                --Leave the cage as it is.
                                ( cage, maybeDragon )
                        )
                        model.cages
            }

        MovingDragon aDragon ->
            { model
                | storedDragon = Just aDragon
                , cages = List.map (\( cage, maybeDragon ) -> ( { cage | selectable = True }, maybeDragon )) model.cages
            }

        ChooseCageForBoughtDragon cageId ->
            let
                newCages =
                    List.map
                        (\( cage, maybeDragon ) ->
                            if cage.id == cageId then
                                ( cage, model.storedDragon )

                            else
                                ( cage, maybeDragon )
                        )
                        model.cages
            in
            { model | cages = newCages, storedDragon = Nothing }


view : Model -> Html.Html Msg
view model =
    layout
        [ centerX
        , Font.shadow { offset = ( 1.0, 0 ), blur = 1, color = rgb255 255 255 255 }
        , paddingEach { top = 100, right = 0, bottom = 0, left = 0 }
        , Background.image "../imgs/bg.png"
        ]
        (viewShop model)


viewShop : Model -> Element Msg
viewShop model =
    let
        isMaxCages =
            List.length model.cages >= 5
    in
    if isMaxCages then
        row
            [ centerX
            , Background.color (rgb255 70 149 190)
            , Background.image "../imgs/shopbg.png"
            ]
            [ column
                [ centerX
                , centerY
                , padding 25
                , spacing 25
                , Background.color (rgba255 211 74 40 0.9)
                ]
                [ el
                    [ Font.family
                        [ Font.external
                            { name = "Metal Mania"
                            , url = "https://fonts.googleapis.com/css2?family=Metal+Mania&display=swap"
                            }
                        , Font.sansSerif
                        ]
                    , Font.size 38
                    ]
                    (text "CRAZY NICKS TINY CHICKEN DRAGONS ZOO")
                , viewShopBuyDragon model
                , viewShopChooseCage model
                ]
            ]

    else
        row
            [ centerX
            , Background.image "../imgs/shopbg.png"
            , Background.color (rgb255 70 149 190)
            ]
            [ column
                [ centerX
                , centerY
                , padding 25
                , spacing 25
                , Background.color (rgba255 211 74 40 0.9)
                ]
                [ el
                    [ Font.family
                        [ Font.external
                            { name = "Metal Mania"
                            , url = "https://fonts.googleapis.com/css2?family=Metal+Mania&display=swap"
                            }
                        , Font.sansSerif
                        ]
                    , Font.size
                        38
                    ]
                    (text "CRAZY NICKS TINY CHICKEN DRAGONS ZOO")
                , Input.button
                    [ padding 50 ]
                    { onPress = Just BuyingCage
                    , label = text "Buy a Steel Cage!"
                    }
                , viewShopBuyDragon model
                , viewShopChooseCage model
                ]
            ]


viewShopBuyDragon : Model -> Element Msg
viewShopBuyDragon model =
    let
        emptyCageFound =
            model.cages |> List.filter (\( cage, maybeDragon ) -> maybeDragon == Nothing) |> List.isEmpty |> not

        emptyCageFoundButHaveADragon =
            model.cages |> List.filter (\( cage, maybeDragon ) -> maybeDragon == Nothing) |> List.isEmpty |> not
    in
    if emptyCageFound then
        row []
            [ column [ padding 10, Background.color (rgb255 226 38 51) ]
                [ Input.button
                    [ padding 10 ]
                    { onPress = Just (BuyingDragon newDragonRed), label = text "Buy a Red Dragon" }
                ]
            , column [ padding 10, Background.color (rgb255 38 164 226) ]
                [ Input.button
                    [ padding 10 ]
                    { onPress = Just (BuyingDragon newDragonBlue), label = text "Buy a Blue Dragon" }
                ]
            , column [ padding 10, Background.color (rgb255 38 226 85) ]
                [ Input.button
                    [ padding 10 ]
                    { onPress = Just (BuyingDragon newDragonGreen), label = text "Buy a Green Dragon" }
                ]
            ]

    else if emptyCageFoundButHaveADragon then
        row []
            [ column [ padding 25 ]
                [ Input.button
                    [ padding 25 ]
                    { onPress = Just (MovingDragon newDragon), label = text "Move to this cage a Dragon" }
                ]
            ]

    else
        column [] []


viewShopChooseCage : Model -> Element Msg
viewShopChooseCage model =
    row []
        [ column [] <|
            (model.cages
                |> List.map
                    (\( cage, maybeDragon ) ->
                        case maybeDragon of
                            Just d ->
                                viewCage d

                            Nothing ->
                                case model.storedDragon of
                                    Just _ ->
                                        viewChooseCage cage.id

                                    Nothing ->
                                        column [] []
                    )
            )
        ]


viewCage : Dragon -> Element Msg
viewCage d =
    let
        dragonColour =
            case d.dragonKind of
                Red ->
                    rgb255 226 38 51

                Blue ->
                    rgb255 38 164 226

                Green ->
                    rgb255 38 226 85

                NoType ->
                    rgb 0.238 0.238 0.238
    in
    row []
        [ column
            [ Background.color dragonColour
            , padding 25
            , spacing 25
            ]
            [ text "Dragon here"
            , Input.button [ padding 10, Background.color (rgb255 102 168 135), Font.size 12 ]
                { onPress = Just (SellDragon d), label = text "Sell This dragon" }
            ]
        ]


viewChooseCage : Int -> Element Msg
viewChooseCage cageId =
    row [ centerX ]
        [ column
            [ Background.color (rgb 0.238 0.238 0.238)
            , padding 50
            ]
            [ Input.button
                []
                { onPress = Just (ChooseCageForBoughtDragon cageId), label = text "Select this cage" }
            ]
        ]
