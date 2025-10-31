{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Graphics.Gnuplot.Test where


import Graphics.Gnuplot.QQ

test1 :: IO ()
test1 =
  runGnuplot
    [gnuplot|
        set view equal xy
        set zzeroaxis; set xzeroaxis; set yzeroaxis
        set xyplane at 0
        unset border
        unset key
        unset xtics
        unset ytics
        set ztics axis

        set arrow 1 from 0,0,0 to 1,0,0 head filled lw 1.5
        set label 1 at 1.2,0,0 "X" center
        set arrow 2 from 0,0,0 to 0,1,0 head filled lw 1.5
        set label 2 at 0,1.2,0 "Y" center
        set arrow 3 from 0,0,0 to 0,0,21 head filled lw 1.5
        set label 3 at 0,0,23 "Z" center

        set view 60, 30, 1., 1.75

        set multiplot layout 1,3

        set view azimuth 0.
        set title 'azimuth 0' offset 0,2
        splot sample [t=0:20] '+' using (cos($1)):(sin($1)):($1) with lines lw 2

        set title '{s:t1}' offset 0,2
        # set title 'azimuth 10' offset 0,2
        set view azimuth 10.
        replot

        set title 'azimuth {i:t2}' offset 0,2
        # set title 'azimuth 60' offset 0,2
        set view azimuth 60.
        replot
      unset multiplot
      pause -1
    |]
  where
    t1 = "azimuth 10" :: String
    t2 = 60 :: Int


test2 :: IO ()
test2 = do
  let times   = [0,1..10] :: [Double]
      values1 = map (\t -> t + 0.5*sin t) times
      values2 = map (\t -> 0.8*t + 0.2*cos t) times
      err1    = map (const 0.3) times
      err2    = map (const 0.2) times
      pts1    = zipWith (\t y -> [t,y]) times values1
      pts2    = zipWith (\t y -> [t,y]) times values2
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
        plot {d:pts1} using 1:2 with linespoints lc rgb "blue" title "Data 1", \
             {d:pts2} using 1:2 with linespoints lc rgb "red" title "Data 2", \
             {d:errPts1} using 1:2:3 with yerrorbars lc rgb "blue" notitle, \
             {d:errPts2} using 1:2:3 with yerrorbars lc rgb "red" notitle
        pause -1
    |]


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
      splot {d:points3D} using 1:2:3 with points lc rgb "blue" title "Points", \
            {d:points3D} using 1:2:3 with lines lc rgb "red" notitle
      pause -1
    |]
