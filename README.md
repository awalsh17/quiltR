# quiltR

Code to design quilts in R. This is work in progress and needs more documentation.

## 2-point perspective polygons

This code generates a landscape with a specified number of rectangular polygons.

<img src="./examples/simple_cubes2.png" width="500"/>

See the [tutorial_polygons.Rmd](./how_to_run/tutorial_polygons.Rmd) Rmd and rendered html.

## Random Foundation Paper piecing blocks

This code generates random foundation paper piecing (FPP) patterns
including the order to add the sections. Can use any starting shape
and be run recursively to create a large design with smaller blocks.

<img src="./examples/random_fpp_design.png" width="500"/>

[wip/make_random_fpp.R](./wip/make_random_fpp.R) script.

I also started some code that takes some design features as input.
For example, if you have an input shape, you can generate the pattern 
to match it.

[wip/make_constrained_fpp.R](./wip/make_constrained_fpp.R) script.

## Using an image as a starting point

Code to take a photo (or image like an ombre color fade) and separate in to blocks for piecing.

[wip/design_from_image.R](./wip/design_from_image.R) script.

## Support for splitting a design into blocks

For one example, see the [tutorial_polygons.Rmd](./how_to_run/tutorial_polygons.Rmd) section on creating
individual blocks.

## Support for choosing colors

This is a work in progress. Some data on the colors available in Kona cotton
or Art Gallery Fabrics is in [colors]("./colors").

## Notes

Also documented at <https://awalsh17.github.io/posts/2022-03-24-quilting-with-r/>

For more on foundation paper piecing, see <https://web.stanford.edu/~mleake/projects/paperpiecing/>

Features in progress:

-   Match to Kona Solids and Art Gallery Fabrics by color
-   Plan the yardage of each color needed
-   Complete the other design types (2-point perspective polygons, pixelated image, random FPP)
-   Allow for hand selection of block shapes and not a uniform grid
-   Add example results and document properly, rename things, etc.
