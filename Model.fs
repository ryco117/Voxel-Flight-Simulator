module LightModel

open System.Runtime.InteropServices
open Vulkan

open Helpers
open LightDevice

module Vertex =
    let bindingDescriptions =
        [|VertexInputBindingDescription (Binding = 0u, Stride = 2u * (uint32 sizeof<float32>), InputRate = VertexInputRate.Vertex)|]

    let attributeDescriptions =
        [|VertexInputAttributeDescription (Binding = 0u, Location = 0u, Format = Format.R32G32Sfloat, Offset = 0u)|]

type LightModel (device: LightDevice, vertices: (float32*float32)[]) =
    let mutable disposed = false
    let vertexBuffer, vertexBufferMemory =
        let count = 2 * vertices.Length
        assert (count >= 3)
        let buffSize = DeviceSize.op_Implicit (sizeof<float32> * count)
        let transferToPtr (memPtr: nativeint) =
            let data = Array.init count (fun i ->
                match i % 2 with
                | 0 -> fst vertices[i/2]
                | 1 -> snd vertices[i/2]
                | e -> raise (System.ArithmeticException $"Modulo operator failed with impossibility %i{e}"))
            Marshal.Copy (data, 0, memPtr, data.Length)
        device.CreateLocalBufferWithTransfer buffSize BufferUsageFlags.VertexBuffer transferToPtr

    member _.Bind (commandBuffer: CommandBuffer) =
        commandBuffer.CmdBindVertexBuffer (0u, vertexBuffer, deviceSizeZero)

    member _.Draw (commandBuffer: CommandBuffer) =
        commandBuffer.CmdDraw (uint32 vertices.Length, 1u, 0u, 0u)

    interface System.IDisposable with
        override _.Dispose () =
            if not disposed then
                disposed <- true
                device.Device.DestroyBuffer vertexBuffer
                device.Device.FreeMemory vertexBufferMemory
    override self.Finalize () = (self :> System.IDisposable).Dispose ()