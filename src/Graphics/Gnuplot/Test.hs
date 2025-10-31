{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Graphics.Gnuplot.Test where


import Graphics.Gnuplot.QQ

test1 :: IO ()
test1 =
  runGnuplot
    [gnuplot|
      set term qt
      plot [][-2:2] sin(x), x, x-(x**3)/6
      pause -1
    |]
    []


test2 :: IO ()
test2 = do
  let times   = [0,1..10] :: [Double]
      values1 = map (\t -> t + 0.5*sin t) times
      values2 = map (\t -> 0.8*t + 0.2*cos t) times
      err1    = map (const 0.3) times
      err2    = map (const 0.2) times
      pts1 = zipWith (\t y -> [t,y]) times values1
      pts2 = zipWith (\t y -> [t,y]) times values2
      errPts1 = zipWith3 (\t y e -> [t, y, e]) times values1 err1
      errPts2 = zipWith3 (\t y e -> [t, y, e]) times values2 err2
  runGnuplot
    [gnuplot|
        set term qt
        set title "Demo: Multiple datasets with error bars"
        set xlabel "Time"
        set ylabel "Value"
        set grid
        set key outside
        # Plot data with lines and points, plus error bars
        plot {{pts1}} using 1:2 with linespoints lc rgb "blue" title "Data 1", \
             {{pts2}} using 1:2 with linespoints lc rgb "red" title "Data 2", \
             {{err1}} using 1:2:3 with yerrorbars lc rgb "blue" notitle, \
             {{err2}} using 1:2:3 with yerrorbars lc rgb "red" notitle
        pause -1
    |]
    [ DataSet "pts1" pts1
    , DataSet "pts2" pts2
    , DataSet "err1" errPts1
    , DataSet "err2" errPts2
    ]


test3 :: IO ()
test3 = do
  let xs = [0,0.5..5]
      ys = [0,0.5..5]
      -- z = sin(x)*cos(y)
      points3D = [ [x,y,sin x * cos y] | x <- xs, y <- ys ]
  runGnuplot
    [gnuplot|
      set term qt
      set title "3D Surface Demo"
      set xlabel "X-axis"
      set ylabel "Y-axis"
      set zlabel "Z-axis"
      set grid
      set key outside

      # Use splot for 3D surface
      splot {{pts}} using 1:2:3 with points lc rgb "blue" title "Points", \
            {{pts}} using 1:2:3 with lines lc rgb "red" notitle
      pause -1
    |]
    [ DataSet "pts" points3D ]
