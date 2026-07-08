module UIText
  ( drawUITextLeft,
    drawUITextCentered,
    uiTextWidth,
  )
where

import Data.Char (toUpper)
import Graphics.Gloss

drawUITextLeft :: Float -> Float -> Float -> Color -> String -> Picture
drawUITextLeft x y pixel colorValue textValue =
  Translate x y $
    Pictures
      [ Translate (fromIntegral i * advance) 0 (drawGlyph pixel colorValue ch)
        | (i, ch) <- zip [0 :: Int ..] textValue
      ]
  where
    advance = pixel * 4.6

drawUITextCentered :: Float -> Float -> Float -> Color -> String -> Picture
drawUITextCentered x y pixel colorValue textValue =
  drawUITextLeft (x - uiTextWidth pixel textValue / 2) y pixel colorValue textValue

uiTextWidth :: Float -> String -> Float
uiTextWidth pixel textValue =
  max 0 (fromIntegral (length textValue) * pixel * 4.6 - pixel * 0.6)

drawGlyph :: Float -> Color -> Char -> Picture
drawGlyph pixel colorValue rawChar =
  Pictures
    [ Color (withAlpha 0.22 black) $
        Translate (pixel * 0.18) (-pixel * 0.14) glyphBody,
      Color colorValue glyphBody
    ]
  where
    glyphBody =
      Pictures
        (points ++ horizontals ++ verticals)
    rowsData = glyphRows rawChar
    rowWidth = case rowsData of
      [] -> 0
      firstRow : _ -> length firstRow
    occupied row col = inBounds row col && (rowsData !! row !! col == '#')
    inBounds row col = row >= 0 && col >= 0 && row < length rowsData && col < rowWidth
    cellCenter row col =
      (fromIntegral col * pixel, negate (fromIntegral row * pixel))
    points =
      [ uncurry Translate (cellCenter row col) $
          circleSolid (pixel * 0.16)
        | (row, rowPattern) <- zip [0 :: Int ..] rowsData,
          (col, marker) <- zip [0 :: Int ..] rowPattern,
          marker == '#'
        ]
    horizontals =
      [ let (cx, cy) = cellCenter row col
         in Translate (cx + pixel * 0.5) cy $
              rectangleSolid (pixel * 1.08) (pixel * 0.22)
        | (row, rowPattern) <- zip [0 :: Int ..] rowsData,
          (col, marker) <- zip [0 :: Int ..] rowPattern,
          marker == '#',
          occupied row (col + 1)
        ]
    verticals =
      [ let (cx, cy) = cellCenter row col
         in Translate cx (cy - pixel * 0.5) $
              rectangleSolid (pixel * 0.22) (pixel * 1.08)
        | (row, rowPattern) <- zip [0 :: Int ..] rowsData,
          (col, marker) <- zip [0 :: Int ..] rowPattern,
          marker == '#',
          occupied (row + 1) col
        ]

glyphRows :: Char -> [String]
glyphRows rawChar = case toUpper rawChar of
  'A' -> rows [".###.", "#...#", "#####", "#...#", "#...#"]
  'B' -> rows ["####.", "#...#", "####.", "#...#", "####."]
  'C' -> rows [".####", "#....", "#....", "#....", ".####"]
  'D' -> rows ["###..", "#..#.", "#...#", "#..#.", "###.."]
  'E' -> rows ["#####", "#....", "####.", "#....", "#####"]
  'F' -> rows ["#####", "#....", "####.", "#....", "#...."]
  'G' -> rows [".####", "#....", "#.###", "#...#", ".###."]
  'H' -> rows ["#...#", "#...#", "#####", "#...#", "#...#"]
  'I' -> rows ["#####", "..#..", "..#..", "..#..", "#####"]
  'J' -> rows ["..###", "...#.", "...#.", "#..#.", ".##.."]
  'K' -> rows ["#..#.", "#.#..", "##...", "#.#..", "#..#."]
  'L' -> rows ["#....", "#....", "#....", "#....", "#####"]
  'M' -> rows ["#...#", "##.##", "#.#.#", "#...#", "#...#"]
  'N' -> rows ["#...#", "##..#", "#.#.#", "#..##", "#...#"]
  'O' -> rows [".###.", "#...#", "#...#", "#...#", ".###."]
  'P' -> rows ["####.", "#...#", "####.", "#....", "#...."]
  'Q' -> rows [".###.", "#...#", "#...#", "#..##", ".####"]
  'R' -> rows ["####.", "#...#", "####.", "#.#..", "#..##"]
  'S' -> rows [".####", "#....", ".###.", "....#", "####."]
  'T' -> rows ["#####", "..#..", "..#..", "..#..", "..#.."]
  'U' -> rows ["#...#", "#...#", "#...#", "#...#", ".###."]
  'V' -> rows ["#...#", "#...#", "#...#", ".#.#.", "..#.."]
  'W' -> rows ["#...#", "#...#", "#.#.#", "##.##", "#...#"]
  'X' -> rows ["#...#", ".#.#.", "..#..", ".#.#.", "#...#"]
  'Y' -> rows ["#...#", ".#.#.", "..#..", "..#..", "..#.."]
  'Z' -> rows ["#####", "...#.", "..#..", ".#...", "#####"]
  '0' -> rows [".###.", "#..##", "#.#.#", "##..#", ".###."]
  '1' -> rows ["..#..", ".##..", "..#..", "..#..", ".###."]
  '2' -> rows [".###.", "#...#", "...#.", "..#..", "#####"]
  '3' -> rows ["####.", "....#", ".###.", "....#", "####."]
  '4' -> rows ["#...#", "#...#", "#####", "....#", "....#"]
  '5' -> rows ["#####", "#....", "####.", "....#", "####."]
  '6' -> rows [".###.", "#....", "####.", "#...#", ".###."]
  '7' -> rows ["#####", "...#.", "..#..", ".#...", ".#..."]
  '8' -> rows [".###.", "#...#", ".###.", "#...#", ".###."]
  '9' -> rows [".###.", "#...#", ".####", "....#", ".###."]
  ':' -> rows [".....", "..#..", ".....", "..#..", "....."]
  '.' -> rows [".....", ".....", ".....", "..#..", "....."]
  ',' -> rows [".....", ".....", ".....", "..#..", ".#..."]
  '/' -> rows ["....#", "...#.", "..#..", ".#...", "#...."]
  '-' -> rows [".....", ".....", "#####", ".....", "....."]
  '+' -> rows ["..#..", "..#..", "#####", "..#..", "..#.."]
  '|' -> rows ["..#..", "..#..", "..#..", "..#..", "..#.."]
  '(' -> rows ["...#.", "..#..", "..#..", "..#..", "...#."]
  ')' -> rows [".#...", "..#..", "..#..", "..#..", ".#..."]
  '!' -> rows ["..#..", "..#..", "..#..", ".....", "..#.."]
  '?' -> rows [".###.", "#...#", "...#.", ".....", "..#.."]
  '>' -> rows ["#....", ".#...", "..#..", ".#...", "#...."]
  '<' -> rows ["....#", "...#.", "..#..", "...#.", "....#"]
  '=' -> rows [".....", "#####", ".....", "#####", "....."]
  ' ' -> rows [".....", ".....", ".....", ".....", "....."]
  _ -> rows [".###.", "#...#", "...#.", ".....", "..#.."]
  where
    rows = map (map normalize)
    normalize '.' = '.'
    normalize _ = '#'
