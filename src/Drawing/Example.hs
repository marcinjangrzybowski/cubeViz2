module Drawing.Example where


import Drawing.Base



square1 :: DrawingGL
square1 = combineDrawings $ [
            Drawing [( unitHyCube 2  , SShape ( Rgba 1.0 1.0 0.0 1.0 )  )]
             ,
             (translate [0.2 , 0.2] $ scale 0.6 (Drawing [ ( unitHyCube 2  , Mask  ) ]))
             , 
             (translate [0 , 0] $ scale 0.4 (Drawing [ ( unitHyCube 2  , MShape ( Rgba 1.0 0.0 0.0 1.0 )  ) ]))
             ,
             (translate [0 , 0.5] $ scale 0.4 (Drawing [ ( unitHyCube 2  , MShape ( Rgba 1.0 0.0 1.0 1.0 )  ) ]))
             ,
             (translate [0.5 , 0.5] $ scale 0.4 (Drawing [ ( unitHyCube 2  , SShape ( Rgba 0.0 1.0 1.0 1.0 )  ) ]))
            ]

