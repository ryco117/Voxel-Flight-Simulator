﻿module Helpers

open Vulkan

let deviceSizeZero = DeviceSize.op_Implicit 0

let random = System.Random ()
let randomFloat () = random.NextDouble () |> float32

let rectFromFourNumbers x y width height =
    Rect2D(Offset = Offset2D(X = x, Y = y), Extent = Extent2D(Width = width, Height = height))