module LightRenderer

open Vulkan

open LightVulkanWindow
open LightDevice
open LightSwapchain

type LightRenderer (window: LightVulkanWindow, device: LightDevice) =
    let mutable swapchain = new LightSwapchain (device, None)
    let createCommandBuffers () =
        let info = new CommandBufferAllocateInfo (Level = CommandBufferLevel.Primary, CommandPool = device.CommandPool, CommandBufferCount = uint32 maxFramesInFlight)
        device.Device.AllocateCommandBuffers info
    let mutable commandBuffers = createCommandBuffers ()

    let mutable disposed = false
    let mutable isFrameStarted = false
    let mutable currentImageIndex = 0
    let mutable currentFrameIndex = 0

    let freeCommandBuffers () =
        device.Device.FreeCommandBuffers (device.CommandPool, commandBuffers)

    let recreateSwapchain () =
        let size = window.Extent
        if size.Width = 0u || size.Height = 0u then
            ()
        else
            device.Device.WaitIdle ()
            let oldSwapchain = swapchain
            swapchain <- new LightSwapchain (device, Some oldSwapchain)

            if not (oldSwapchain.CompareSwapFormats swapchain) then
                // TODO: recreate pipeline layout if new render pass is incompatible
                raise (System.Exception "Created a new swpachain with an incompatible render pass to the previous")

    let handleResize () =
        recreateSwapchain ()
        window.Resized <- false

    member _.IsFrameInProgress = isFrameStarted

    member _.SwapchainRenderPass = swapchain.RenderPass

    member _.AspectRatio = swapchain.ExtentAspectRatio

    member _.CurrentCommandBuffer =
        assert isFrameStarted
        commandBuffers[currentFrameIndex]

    member _.CurrentFrameIndex =
        assert isFrameStarted
        currentFrameIndex

    member _.BeginFrame () =
        assert (not isFrameStarted)
        if window.Resized then
            handleResize ()
            None
        else
            try
                currentImageIndex <- swapchain.AcquireNextImageAsIndex ()
                isFrameStarted <- true
                let commandBuffer = commandBuffers[currentFrameIndex]
                let beginInfo = new CommandBufferBeginInfo ()
                commandBuffer.Begin beginInfo
                Some commandBuffer
            with
            | :? ResultException as e when (e.Result = Result.ErrorOutOfDateKhr || e.Result = Result.SuboptimalKhr) ->
                handleResize ()
                None

    member self.EndFrame () =
        let commandBuffer = self.CurrentCommandBuffer
        try
            commandBuffer.End ()
            swapchain.SubmitCommandBuffers commandBuffer currentImageIndex
        with
        | :? ResultException as e when (e.Result = Result.ErrorOutOfDateKhr || e.Result = Result.SuboptimalKhr) -> handleResize ()
        isFrameStarted <- false
        currentFrameIndex <- (currentFrameIndex + 1) % maxFramesInFlight

    member self.BeginSwapchainRenderPass (commandBuffer: CommandBuffer) =
        assert (commandBuffer = self.CurrentCommandBuffer)  // TODO: Support multiple render passes (making the parameterized command buffer useful)
        let renderPassInfo =
            let clearValues = [|
                new ClearValue (Color = new ClearColorValue [|0.1f; 0.1f; 0.1f; 1.f|]);
                new ClearValue (DepthStencil = ClearDepthStencilValue (Depth = 1.f, Stencil = 0u))|]
            new RenderPassBeginInfo (
                RenderPass = swapchain.RenderPass,
                Framebuffer = swapchain.GetFramebuffer currentImageIndex,
                RenderArea = Helpers.rectFromFourNumbers 0 0 swapchain.Extent.Width swapchain.Extent.Height,
                ClearValues = clearValues)
        commandBuffer.CmdBeginRenderPass (renderPassInfo, SubpassContents.Inline)

        let size = swapchain.Extent
        let viewport =
            Viewport (
                X = 0.f,
                Y = 0.f,
                Width = float32 size.Width,
                Height = float32 size.Height,
                MinDepth = 0.f,
                MaxDepth = 1.f)
        let scissor = Helpers.rectFromFourNumbers 0 0 size.Width size.Height
        commandBuffer.CmdSetViewport (0u, viewport)
        commandBuffer.CmdSetScissor (0u, scissor)

    member self.EndSwapchainRenderPass (commandBuffer: CommandBuffer) =
        assert (commandBuffer = self.CurrentCommandBuffer)  // TODO: Support multiple render passes (making the parameterized command buffer useful)
        commandBuffer.CmdEndRenderPass ()

    interface System.IDisposable with
        override _.Dispose () =
            if not disposed then
                disposed <- true
                freeCommandBuffers ()
    override self.Finalize () = (self :> System.IDisposable).Dispose ()