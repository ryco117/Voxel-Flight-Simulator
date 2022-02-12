module Helpers

open System.Runtime.InteropServices

open Vulkan

let deviceSizeZero = DeviceSize.op_Implicit 0

let random = System.Random ()
let randomFloat () = random.NextDouble () |> float32

let rectFromFourNumbers x y width height =
    Rect2D (Offset = Offset2D (X = x, Y = y), Extent = Extent2D (Width = width, Height = height))

let MarshalArrayOfStruct (array: 'a[]) (pointer: nativeint) =
    let elementSize = sizeof<'a>
    let mutable offset = 0
    for i = 0 to array.Length - 1 do
        Marshal.StructureToPtr (array[i], (pointer + nativeint offset), false)
        offset <- offset + elementSize