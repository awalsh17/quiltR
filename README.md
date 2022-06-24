# quiltR

Code to design quilts in R. This is work in progress and needs more documentation.

## 2-point perspective polygons

This code generates a landscape with a specified number of rectangular polygons.

<img src="./examples/simple_cubes2.png" width="1000"/>

See the [tutorial_polygons.Rmd](./how_to_run/tutorial_polygons.Rmd) Rmd and rendered html.

## Random Foundation Paper piecing blocks

This code generates a random FPP pattern including the order to add the sections.

[wip/make_random_fpp.R](./wip/make_random_fpp.R) script.

## Using an image as a starting point

Code to take a photo (or image like an ombre color fade) and separate in to blocks for piecing.

[wip/design_from_image.R](./wip/design_from_image.R) script.

## Support for creating blocks from a design
TBA

## Support for choosing colors
TBA

## Notes

Also documented at <https://awalsh17.github.io/posts/2022-03-24-quilting-with-r/>

For more on foundation paper piecing, see <https://web.stanford.edu/~mleake/projects/paperpiecing/>

Features in progress:

-   Match to Kona Solids and Art Gallery Fabrics by color
-   Plan the yardage of each color needed
-   Complete the other design types (2-point perspective polygons, pixelated image, random FPP)
-   Allow for hand selection of block shapes and not a uniform grid
-   Add example results and document properly, rename things, etc.
