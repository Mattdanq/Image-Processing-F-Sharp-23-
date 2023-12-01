// Work on your solution here

// Do not change the API (module/namespace/function signatures)

// Refer to the PDF for more instructions on how to implement
// each function

// As of now, each function simply takes an image as input
// and returns it as it is

(*
Project 2: Image Processing F#
Project Desc: 
  The program is supposed to process the image and change it in many ways a simple image editing program can by rotating the image in different ways and changing the hues
Matthew Danque
mdanq2
10/28 ?
*)
namespace ImageLibrary

module Operations =

//-------------------------------GRAYSCALE----------------------------
  
  //adds the calculation and adds to new list
  let rec grayCalc (mini:(int*int*int) list) 
                   (newMini:(int*int*int) list) =
    match mini with
    |[] -> List.rev newMini
    |h1::t1 -> 
      match h1 with
      |(a, b, c) -> 
        let temA = float a * 0.299
        let temB = float b * 0.587
        let temC = float c * 0.114
        let temAvg = temA + temB + temC
        let avg = int temAvg
        let newTup = (avg, avg, avg)
        grayCalc t1 (newTup::newMini)

  //turns the image to grayscale(very much gray hahah) by averging the numbers together, but down
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) =
    //explores the list and traverses it
    let rec grayscaler (img:(int*int*int) list list)
                    (newImg:(int*int*int) list list) =
      match img with
      |[] -> List.rev newImg
      |h1::t1 -> grayscaler t1 (grayCalc h1 []::newImg)
    grayscaler image []

  //-------------------------------TRESHHOLD----------------------------
  //given that if an pixel RGB value is <= thresh we set RGB to 0, otherwise to depth
  let rec threshCalc (depth:int)
                     (thresh:int)
                     (mini:(int*int*int) list) 
                     (newMini:(int*int*int) list) =
    match mini with
    |[] -> List.rev newMini
    |h1::t1 -> 
      match h1 with
      |(a, b, c) -> 
        let newA = 
          if a <= thresh then 0
          else depth
          
        let newB = 
          if b <= thresh then 0
          else depth

        let newC = 
          if c <= thresh then 0
          else depth
          
        let newTup = (newA, newB, newC)
        threshCalc depth thresh t1 ((newA, newB, newC)::newMini)
  //the function that is actually called
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    //explores the list and traverses it
    let rec treshExp (img:(int*int*int) list list)
                     (newImg:(int*int*int) list list) =
      match img with
      |[] -> List.rev newImg
      |h1::t1 -> treshExp t1 (threshCalc depth threshold h1 []::newImg)
    treshExp image []
    
//-------------------------------FLIP HORIZONTAL----------------------------
  //flips each list in the image list list and outputs a flipped image more or less
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    
    List.map(fun e -> List.rev e) image

//-------------------------------EDGE DETECT----------------------------
  //calculates the pixel distance of the origin - right and origin - below
  let rec edgeCalc (width:int)
                   (thresh:int)
                   (wc:int)
                   (mini:(int*int*int) list)
                   (mini2:(int*int*int) list)
                   (newMini:(int*int*int) list) =
    match mini, mini2 with
    |_, [] -> List.rev newMini
    |h1::t1, h2::t2 when (wc = width) -> edgeCalc width thresh (wc + 1) t1 t2 newMini
    |h1::t1, h2::t2 -> 
      match h1, h2, t1.Head with
      |(a, b, c), (d, e, f), (g, h, i) ->
        //we check with to BELOW
        let temA = (a - d) * (a - d)
        let temB = (b - e) * (b - e)
        let temC = (c - f) * (c - f)
        let combine1 = temA + temB + temC
        let combineABC = float combine1
        let downCheck = sqrt combineABC
        //we check LEFT
        let temD = (a - g) * (a - g)
        let temE = (b - h) * (b - h)
        let temF = (c - i) * (c - i)
        let combine2 = temD + temE + temF
        let combineDEF = float combine2
        let leftCheck = sqrt combineDEF
        //----------------------------------
        let floatThresh = float thresh
        let newTup = 
          if (downCheck > floatThresh || leftCheck > floatThresh) then (0, 0, 0)
          else (255,255,255)
        edgeCalc width thresh (wc + 1) t1 t2 (newTup::newMini)
        
  //function that is actually called 
  let rec EdgeDetect (width:int)
                     (height:int)
                     (depth:int)
                     (image:(int*int*int) list list)
                     (threshold:int) =
    //traverses the list
    let rec edgeExp (lc: int)
                    (img:(int*int*int) list list)
                    (row2:(int*int*int) list list)
                    (newImg:(int*int*int) list list) =
      match img, row2 with
      |[], _ -> List.rev newImg
      |_, [] -> List.rev newImg
      |h1::t1, h2::t2 -> edgeExp (lc + 1) t1 t2 (edgeCalc width threshold 1 h1 h2 []::newImg)
    edgeExp 1 image image.Tail []
  
//-------------------------------ROTATE 90----------------------------
  // the original function in which is supposed to be called to rotate the image
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
                        
  //"explores" the list given to them and plucks out an element at a certain index of stop
    let rec explorer (eList:(int*int*int) list)
                   (count:int)
                   (stop:int) =
      match eList with
      |[] -> (0,0,0)
      |h1::t1 when (count = stop) -> h1
      |h1::t1 -> explorer t1 (count + 1) stop
    
    //recursive with more paramters
    //is supposed to allow the rotation of the image by traversing the list of lists many times and take certain elements and shove it into a new list
    let rec rotate90 (wc:int)
                   (image:(int*int*int) list list)
                   (imageRef:(int*int*int) list list)
                   (small:(int*int*int) list)
                   (newImg:(int*int*int) list list) =
      match image with
      |[] when (wc = width)-> List.rev newImg
      |[] -> rotate90 (wc + 1) imageRef imageRef [] (small::newImg)
      |h1::t1 when (wc > 0) -> rotate90 wc t1 imageRef (explorer h1 0 wc::small) newImg
      |h1::t1 -> rotate90 wc t1 imageRef (h1.Head::small) newImg

    rotate90 0 image image [] [] 