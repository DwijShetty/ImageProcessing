//
// F# image processing functions.
//
// More details?
//
// Name? School? Date?
// Dwij Shetty, University of Illinois Chicago, Spring '23

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Pixels in grayscale
  // have the same value for each of the Red, Green and Blue
  // values in the RGB value.  Conversion to grayscale is done
  // by using a WEIGHTED AVERAGE calculation.  A normal average
  // (adding the three values and dividing by 3) is not the best,
  // since the human eye does not perceive the brightness of 
  // red, green and blue the same.  The human eye perceives 
  // green as brighter than red and it perceived red as brighter
  // than blue.  Research has shown that the following weighted
  // values should be used when calculating grayscale.
  //  - the green value should account for 58.7% of the grayscale.
  //  - the red value should account for   29.9% of the grayscale.
  //  - the blue value should account for  11.4% of the grayscale.
  //
  // So if the RGB values were (25, 75, 250), the grayscale amount 
  // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  // and then all three RGB values would become 80 or (80, 80, 80).
  // We will use truncation to cast from the floating point result 
  // to the integer grayscale value.
  //
  // Returns: updated image.
  //
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    // for now, just return the imag
    // Function to calculate the grayscale value for a pixel.
    let grayscalePixel (r, g, b) =
        int (float r * 0.299 + float g * 0.587 + float b * 0.114)
    
    // Map over the pixels in the image and convert each to grayscale.
    let grayscaleImage = List.map (fun row -> List.map grayscalePixel row) image
    
    // Create a new image with the grayscale pixels.
    List.init height (fun i -> List.init width (fun j -> grayscaleImage.[i].[j], grayscaleImage.[i].[j], grayscaleImage.[i].[j]))
      

  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    // Function to apply threshold to a single RGB value.
    let thresholdPixel p =
        if p > threshold then depth else 0
    
    // Map over the pixels in the image and apply the threshold to each RGB value.
    let thresholdImage = List.map (fun row -> List.map (fun (r, g, b) -> thresholdPixel r, thresholdPixel g, thresholdPixel b) row) image
    
    // Create a new image with the thresholded pixels.
    List.init height (fun i -> List.init width (fun j -> thresholdImage.[i].[j]))


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    let flipRow (row:(int*int*int) list) =
        List.rev row
    List.map flipRow image


  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  // Define a function to calculate the Euclidean distance between two 3D points
  let distance (p1:int*int*int) (p2:int*int*int) =
    let (x1, y1, z1) = p1
    let (x2, y2, z2) = p2
    sqrt(float ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)))

  // Define a function to get neighboring pixels for a given pixel in an image
  let getNeighboringPixels (x:int) (y:int) (image:(int*int*int) list list) =
    let rightPixel = if x < List.length (List.head image) - 1 then Some(List.nth (List.nth image y) (x+1)) else None
    let bottomPixel = if y < List.length image - 1 then Some(List.nth (List.nth image (y+1)) x) else None
    (rightPixel, bottomPixel)
  
  // Define a function to check if the Euclidean distance between two pixels is greater than a given threshold
  let isSignificantlyDifferent (p1:int*int*int) (p2:int*int*int) (threshold:int) =
    distance p1 p2 > float threshold

  // Define a function to get the color of a pixel based on whether it is an edge or not
  let getEdgePixel (x:int) (y:int) (image:(int*int*int) list list) (threshold:int) =
    let pixel = List.nth (List.nth image y) x
    let (rightPixel, bottomPixel) = getNeighboringPixels x y image
    let isEdge = 
        match rightPixel, bottomPixel with
        | Some r, Some b -> isSignificantlyDifferent pixel r threshold || isSignificantlyDifferent pixel b threshold
        | Some r, None -> isSignificantlyDifferent pixel r threshold
        | None, Some b -> isSignificantlyDifferent pixel b threshold
        | None, None -> false
    if isEdge then (0, 0, 0) else (255, 255, 255)

  // Define a recursive function to perform edge detection on an image
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
      let updatedImage = 
        List.init (height-1) (fun y -> 
            List.init (width-1) (fun x -> 
                getEdgePixel x y image threshold))
      updatedImage

  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    // Transpose the image
    let transposed = List.transpose image
    
    // Reverse each row to rotate the image to the right
    let rotated = List.map (fun row -> List.rev row) transposed
    
    // Return the rotated image
    rotated