-- Copyright 2019 Ju Liu
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


module Chart exposing
    ( Config
    , view, viewWith
    )

{-| Export a chord voicing to SVG

@docs Config


# Charts

@docs view, viewWith

-}

import Chords exposing (Voicing)
import List.Extra
import Svg exposing (Attribute, Svg)
import Svg.Attributes as Attr


{-| Contains options to customize SVG.

  - `height`: relative height within svg viewbox
  - `width`: relative width within svg viewbox
  - `hPaddingPct`: percentage of width to use as horizontal padding
  - `vPaddingPct`: percentage of height to use as vertical padding
  - `minFrets`: minimum number of frets to display in a chart

-}
type alias Config =
    { height : Int
    , width : Int
    , hPaddingPct : Float
    , vPaddingPct : Float
    , minFrets : Int
    }


{-| Render a chord voicing and a label to SVG.
-}
view : String -> Voicing -> Svg msg
view label voicing =
    viewWith
        { height = 200
        , width = 150
        , hPaddingPct = 0.2
        , vPaddingPct = 0.2
        , minFrets = 4
        }
        label
        voicing


{-| Render a chord voicing and a label to SVG passing along customizations.
-}
viewWith : Config -> String -> Voicing -> Svg msg
viewWith config label voicing =
    let
        { height, width, hPaddingPct, vPaddingPct, minFrets } =
            config

        height_ =
            toFloat height

        width_ =
            toFloat width

        horizontalPadding =
            width_ * hPaddingPct

        verticalPadding =
            height_ * vPaddingPct

        numStrings =
            List.length voicing

        chartHeight =
            height_ - (2 * verticalPadding)

        chartWidth =
            width_ - (2 * horizontalPadding)

        columnWidth =
            chartWidth
                / toFloat (numStrings - 1)

        fretValues =
            List.foldl
                (\elem acc ->
                    case elem of
                        Just ( fret, note ) ->
                            fret :: acc

                        Nothing ->
                            acc
                )
                []
                voicing

        highestFret =
            List.maximum fretValues
                |> Maybe.withDefault 0

        lowestFret =
            List.minimum fretValues
                |> Maybe.withDefault 0

        fretRange =
            if lowestFret == 0 then
                highestFret

            else
                highestFret - lowestFret + 1

        numFrets =
            1 + max minFrets fretRange

        atBeginning =
            highestFret - numFrets < 0

        rowHeight =
            chartHeight
                / toFloat (numFrets - 1)

        strings =
            voicing
                |> List.indexedMap
                    (\index _ ->
                        let
                            horizontalOffset =
                                horizontalPadding
                                    + (columnWidth * toFloat index)

                            dVal =
                                d
                                    ( horizontalOffset, verticalPadding )
                                    ( horizontalOffset, verticalPadding + chartHeight )
                        in
                        Svg.path [ Attr.d dVal ] []
                    )

        frets =
            List.range 0 (numFrets - 1)
                |> List.map
                    (\relativeFret ->
                        let
                            verticalOffset =
                                verticalPadding
                                    + (rowHeight * toFloat relativeFret)

                            strokeWidth =
                                if atBeginning && relativeFret == 0 then
                                    "4"

                                else
                                    "1"

                            dVal =
                                d
                                    ( horizontalPadding, verticalOffset )
                                    ( horizontalPadding + chartWidth, verticalOffset )
                        in
                        Svg.path
                            [ Attr.strokeWidth strokeWidth
                            , Attr.d dVal
                            ]
                            []
                    )

        notes =
            voicing
                |> List.indexedMap
                    (\index elem ->
                        case elem of
                            Just ( fret, note ) ->
                                let
                                    relativeFret =
                                        if atBeginning then
                                            fret

                                        else
                                            fret - lowestFret + 1

                                    verticalOffset =
                                        verticalPadding
                                            + (rowHeight * toFloat relativeFret)
                                            - (rowHeight / 2)

                                    horizontalOffset =
                                        horizontalPadding
                                            + (columnWidth * toFloat index)

                                    fillColor =
                                        if atBeginning && relativeFret == 0 then
                                            "white"

                                        else
                                            ""
                                in
                                Svg.circle
                                    [ Attr.cx (String.fromFloat horizontalOffset)
                                    , Attr.cy (String.fromFloat verticalOffset)
                                    , Attr.r <| String.fromFloat (rowHeight / 5)
                                    , Attr.fill fillColor
                                    ]
                                    []

                            Nothing ->
                                let
                                    verticalOffset =
                                        verticalPadding - (rowHeight / 2)

                                    horizontalOffset =
                                        horizontalPadding
                                            + (columnWidth * toFloat index)
                                in
                                centeredText
                                    { x = horizontalOffset
                                    , y = verticalOffset
                                    , fontSize = rowHeight / 2
                                    }
                                    []
                                    "X"
                    )

        startFret =
            if atBeginning then
                []

            else
                [ centeredText
                    { x = horizontalPadding / 2
                    , y = rowHeight / 2 + verticalPadding
                    , fontSize = rowHeight / 2
                    }
                    []
                    (String.fromInt lowestFret)
                ]

        label_ =
            centeredText
                { x = horizontalPadding + (chartWidth / 2)
                , y = verticalPadding * 1.5 + chartHeight
                , fontSize = rowHeight * 0.6
                }
                [ Attr.fontWeight "bold" ]
                label
    in
    Svg.svg
        [ Attr.height "100%"
        , Attr.width "100%"
        , Attr.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
        ]
        (label_
            :: (strings
                    ++ frets
                    ++ notes
                    ++ startFret
               )
        )


d : ( Float, Float ) -> ( Float, Float ) -> String
d ( startX, startY ) ( endX, endY ) =
    "M"
        ++ String.fromFloat startX
        ++ ","
        ++ String.fromFloat startY
        ++ " L"
        ++ String.fromFloat endX
        ++ ","
        ++ String.fromFloat endY


centeredText :
    { x : Float, y : Float, fontSize : Float }
    -> List (Attribute msg)
    -> String
    -> Svg msg
centeredText { x, y, fontSize } attrs text =
    Svg.text_
        ([ Attr.x <| String.fromFloat x
         , Attr.y <| String.fromFloat y
         , Attr.fontFamily "Helvetica, Arial, sans-serif"
         , Attr.fontSize <| String.fromFloat fontSize
         , Attr.textAnchor "middle"
         ]
            ++ attrs
        )
        [ Svg.tspan
            [ Attr.alignmentBaseline "central"
            ]
            [ Svg.text text
            ]
        ]
