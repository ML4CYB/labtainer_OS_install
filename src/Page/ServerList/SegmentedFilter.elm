module Page.ServerList.SegmentedFilter exposing (FilterType(..), view)

import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icons
import Helpers.String
import Html
import Html.Attributes
import Html.Events
import Set
import Style.Helpers as SH
import Style.Types as ST exposing (ExoPalette)
import Style.Widgets.Icon as Icon
import Style.Widgets.Popover.Popover exposing (popover)
import Style.Widgets.Spacer exposing (spacer)
import View.Types exposing (Context)


type FilterType
    = Radio
    | Dropdown (List String) String


view :
    { selected : String
    , options : List { value : String, label : String }
    , onChange : List String -> msg
    , palette : ExoPalette
    , filterType : FilterType
    , dropdownMsgMapper : String -> msg
    , showPopovers : Set.Set String
    , labelPrefix : String
    }
    -> Element msg
view config =
    case config.filterType of
        Radio ->
            radioView
                { selected = config.selected
                , options = config.options
                , onChange = \value -> config.onChange [ value ]
                , palette = config.palette
                , labelPrefix = config.labelPrefix
                }

        Dropdown selectedValues placeholder ->
            dropdownView
                { selected = config.selected
                , options = config.options
                , onChange = config.onChange
                , palette = config.palette
                , filterType = config.filterType
                , dropdownMsgMapper = config.dropdownMsgMapper
                , showPopovers = config.showPopovers
                , placeholder = placeholder
                , labelPrefix = config.labelPrefix
                }
                selectedValues


truncated : Int -> String -> Element msg
truncated atMostChars string =
    Element.html <|
        Html.span
            [ Html.Attributes.style "display" "inline-block"
            , Html.Attributes.style "max-width" (String.fromInt atMostChars ++ "ch")
            , Html.Attributes.style "overflow" "hidden"
            , Html.Attributes.style "text-overflow" "ellipsis"
            , Html.Attributes.style "white-space" "nowrap"
            , Html.Attributes.title string
            ]
            [ Html.text string ]


radioView :
    { selected : String
    , options : List { value : String, label : String }
    , onChange : String -> msg
    , palette : ExoPalette
    , labelPrefix : String
    }
    -> Element msg
radioView config =
    let
        pillAttributes index isSelected isLast =
            [ Border.widthEach
                { top = 1
                , bottom = 1
                , left = 1
                , right = if isLast then 1 else 0
                }
            , Element.paddingXY spacer.px12 spacer.px6
            , Element.htmlAttribute <| Html.Attributes.style "transition" "all 0.3s cubic-bezier(0.4, 0, 0.2, 1)"
            , Element.htmlAttribute <| Html.Attributes.style "transform" "translateZ(0)"
            , Element.htmlAttribute <| Html.Attributes.style "transform-origin" "center"
            , Element.htmlAttribute <| Html.Attributes.style "position" "relative"
            , Element.htmlAttribute <| Html.Attributes.style "z-index" (if isSelected then "2" else "1")
            ]
                ++ (if isSelected then
                        [ Background.color <| SH.toElementColor config.palette.primary
                        , Font.color <| SH.toElementColor config.palette.muted.background
                        , Element.htmlAttribute <| Html.Attributes.style "transform" "translateZ(0) scale(1.05)"
                        ]

                    else
                        [ Background.color <| SH.toElementColor config.palette.menu.background
                        , Border.color <| SH.toElementColor config.palette.neutral.border
                        , Font.color <| SH.toElementColor config.palette.neutral.text.default
                        , Element.htmlAttribute <| Html.Attributes.style "transform" "translateZ(0) scale(1)"
                        ]
                   )
                ++ (if index == 0 && isLast then
                        [ Border.rounded 8 ]

                    else if index == 0 then
                        [ Border.roundEach
                            { topLeft = 8
                            , topRight = 0
                            , bottomLeft = 8
                            , bottomRight = 0
                            }
                        ]

                    else if isLast then
                        [ Border.roundEach
                            { topLeft = 0
                            , topRight = 8
                            , bottomLeft = 0
                            , bottomRight = 8
                            }
                        ]

                    else
                        [ Border.rounded 0 ]
                   )

        pill index option isLast =
            Input.button
                (pillAttributes index (List.member option.value [ config.selected ]) isLast)
                { onPress = Just <| config.onChange option.value
                , label =
                    Element.row [ Element.spacing spacer.px4 ]
                        [ Element.el
                            [ Font.size 12
                            , Font.color <| SH.toElementColor config.palette.neutral.text.subdued
                            ]
                            (Element.text config.labelPrefix)
                        , Element.text option.label
                        ]
                }

        lastIndex =
            List.length config.options - 1
    in
    Element.row
        [ Element.spacing 0
        , Element.htmlAttribute <| Html.Attributes.style "background-color" "transparent"
        , Element.htmlAttribute <| Html.Attributes.style "padding" "4px"
        , Element.htmlAttribute <| Html.Attributes.style "border-radius" "10px"
        , Background.color <| SH.toElementColor config.palette.neutral.background.backLayer
        ]
        (List.indexedMap
            (\index option ->
                pill index option (index == lastIndex)
            )
            config.options
        )


dropdownView :
    { selected : String
    , options : List { value : String, label : String }
    , onChange : List String -> msg
    , palette : ExoPalette
    , filterType : FilterType
    , dropdownMsgMapper : String -> msg
    , showPopovers : Set.Set String
    , placeholder : String
    , labelPrefix : String
    }
    -> List String
    -> Element msg
dropdownView config selectedValues =
    let
        dropdownId =
            Helpers.String.hyphenate [ "segmented-filter-dropdown", config.selected ]

        target togglePopover popoverIsShown =
            Input.button
                [ Border.rounded 4
                , Border.width 1
                , Border.color <| SH.toElementColor config.palette.neutral.border
                , Background.color <| SH.toElementColor config.palette.menu.background
                , Element.paddingXY spacer.px8 spacer.px4
                , Element.spacing spacer.px4
                ]
                { onPress = Just togglePopover
                , label =
                    Element.row [ Element.spacing spacer.px4 ]
                        [ Element.row [ Element.spacing spacer.px4 ]
                            (if config.labelPrefix /= "" && not (List.isEmpty selectedValues) then
                                [ Element.el
                                    [ Font.size 12
                                    , Font.color <| SH.toElementColor config.palette.neutral.text.subdued
                                    ]
                                    (Element.text config.labelPrefix)
                                ]

                             else
                                []
                            )
                        , if List.isEmpty selectedValues then
                            Element.text config.placeholder

                          else
                            let
                                findText : String -> List { value : String, label : String } -> Maybe String
                                findText value optionsList =
                                    optionsList
                                        |> List.filter (\opt -> opt.value == value)
                                        |> List.head
                                        |> Maybe.map .label

                                selectedTexts =
                                    selectedValues
                                        |> List.filterMap (\v -> findText v config.options)

                                truncatedElements =
                                    case selectedTexts of
                                        [ single ] ->
                                            [ truncated 20 single ]

                                        [ first, second ] ->
                                            [ truncated 10 first, truncated 10 second ]

                                        first :: second :: _ ->
                                            let
                                                remainingCount =
                                                    List.length selectedValues - 2
                                            in
                                            [ truncated 10 first, truncated 10 second, Element.text <| "and " ++ String.fromInt remainingCount ++ " more" ]

                                        [] ->
                                            []

                                joinedElements =
                                    case truncatedElements of
                                        [] ->
                                            Element.none

                                        [ single ] ->
                                            single

                                        first :: rest ->
                                            Element.row [ Element.spacing 0 ] <|
                                                List.intersperse (Element.text ", ") (List.map (\e -> Element.row [] [ e ]) truncatedElements)
                            in
                            joinedElements
                        , Icon.sizedFeatherIcon 16 <|
                            if popoverIsShown then
                                Icons.chevronUp

                            else
                                Icons.chevronDown
                        ]
                }

        content closePopover =
            Element.column
                [ Element.spacing spacer.px4
                , Element.padding spacer.px8
                , Background.color <| SH.toElementColor config.palette.menu.background
                , Border.rounded 4
                , Border.width 1
                , Border.color <| SH.toElementColor config.palette.neutral.border
                ]
                (List.map
                    (\option ->
                        Input.checkbox
                            [ Element.width Element.fill ]
                            { onChange =
                                \checked ->
                                    let
                                        newSelectedValues =
                                            if checked then
                                                -- Add the value to the list if not already present
                                                if List.member option.value selectedValues then
                                                    selectedValues

                                                else
                                                    selectedValues ++ [ option.value ]

                                            else
                                                -- Remove the value from the list
                                                List.filter ((/=) option.value) selectedValues
                                    in
                                    config.onChange newSelectedValues
                            , icon = Input.defaultCheckbox
                            , checked = List.member option.value selectedValues
                            , label = Input.labelRight [] (Element.text option.label)
                            }
                    )
                    config.options
                )
    in
    popover
        { palette = config.palette
        , showPopovers = config.showPopovers
        }
        config.dropdownMsgMapper
        { id = dropdownId
        , content = \_ -> content ()
        , contentStyleAttrs = []
        , position = ST.PositionBottomLeft
        , distanceToTarget = Nothing
        , target = target
        , targetStyleAttrs = []
        }
