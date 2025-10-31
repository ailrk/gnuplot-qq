# gnuplot-qq

A quasi quoter for gnuplot script.


### Motivation

Plotting code is simple, short, and disposable, the code quality is rarely an issue. You want fast iteration and immediate feedback.

I’ve tried several plotting libraries: [gnuplot](https://hackage.haskell.org/package/gnuplot), [matplotlib](https://hackage.haskell.org/package/matplotlib), and [Chart](https://hackage.haskell.org/package/Chart), each comes with some limitations. `matplotlib` requires the full Python runtime and matplotlib dependencies, but only exposes a subset of its features; `Chart` relies heavily on lenses and has a large, complex API; `gnuplot` works, but sometimes you need to work around its abstraction. All three approaches involve extra effort and dependencies just to produce a simple plot.

This is a simple quasi quoter that lets you write gnuplot directly in Haskell. You can generate data in Haskell and interpolate it straight into your gnuplot script. Don't worry about gnuplot script, it is very simple. It is so simple that it takes less time to learn it than figuring out how to use the `gnuplot` library. The dependency is about 4MB, and the library itself is about a hundred lines of Haskell.


### Usage

Use the `[gnuplot||]` quasi quoter to write the gnuplot script, Code within the `{}` bracket will be interpolated. You have to specify the type of the interpolated value. Currently there are 4 types available.

- `{i:_}`: integer
- `{r:_}`: real number
- `{d:_}`: data set of type `[[Double]]`
- `{s:_}`: string

Run the plot with `runGnuplot`.

Use `ghci` to iteratively test the plot.


### Example

```haskell
module Main where
import Graphics.Gnuplot.QQ

main :: IO ()
main = do
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
```


### Roadmap

- [ ] Make `Gnuplot` a monoid so we can combine plots
- [ ] Add a newtype wrapper `Multiplot`, a monoid that combines plots into a mulitplot
- [ ] Support polymorphic types
