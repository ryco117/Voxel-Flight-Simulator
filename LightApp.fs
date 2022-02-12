module LightApp

open System.Windows.Forms

open LightVulkanWindow
open LightDevice
open LightRenderer
open LightRenderSystem
open LightModel
open LightObject
open LightState

let defaultSpeed = 0.245f
let initialPosition = System.Numerics.Vector3 (0.f, 0.125f, -2.f)

let newDefaultState () = {
    demoControls = true
    keyForwardInput = 0.f
    keyLeftInput = 0.f
    keyRightInput = 0.f
    keyBackInput = 0.f
    keyStrafeLeftInput = 0.f
    keyStrafeRightInput = 0.f
    lastFrameTime = 0.
    lastSpeed = 0.f
    playerPosition = initialPosition
    playerQuaternion = Maths.Vector4.UnitQuaternion
    upTime = System.Diagnostics.Stopwatch.StartNew ()
    gameObjects = Array.empty}

let resetPlayer state =
    state.upTime.Restart ()
    {state with
        demoControls = true
        playerPosition = initialPosition
        playerQuaternion = Maths.Vector4.UnitQuaternion
        lastFrameTime = 0.
        lastSpeed = 0.f}

let mutable portalRandom = System.Random 0
let takePortal state =
    state.upTime.Restart ()
    {state with
        playerPosition = initialPosition
        playerQuaternion = Maths.Vector4.UnitQuaternion
        lastFrameTime = 0.
        lastSpeed = defaultSpeed}

type LightApp () =
    let window = new LightVulkanWindow (600, 400, "Voxel Flight Simulator")
    let device = new LightDevice (window)
    let renderer = new LightRenderer (window, device)

    let mutable state =
        { newDefaultState () with
            gameObjects =
                let quadObject =
                    let vertices = [|
                        (-1.f, -1.f)
                        ( 1.f, -1.f)
                        (-1.f,  1.f)
                        ( 1.f,  1.f)|]
                    let model = new LightModel (device, vertices)
                    LightObject (Model = Some model)
                [|quadObject|]}

    let mutable disposed = false

    member _.Run () =
        let renderSystem = new LightRenderSystem (device, renderer.SwapchainRenderPass)
        let updateState () =
            if state.demoControls then
                state <- {state with lastFrameTime = state.upTime.Elapsed.TotalSeconds}
            else
                match Voxels.octreeScaleAndCollisionOfPoint state.playerPosition renderSystem.VoxelData with
                | _, Voxels.Intersection.Collision -> state <- resetPlayer state
                | _, Voxels.Intersection.Portal index ->
                    let t = portalRandom.Next () ^^^ int index in (if t = System.Int32.MinValue then t + 1 else t)
                    |> System.Random
                    |> renderSystem.RegenerateWorld
                    state <- takePortal state
                | currentScale, Voxels.Intersection.EmptySpace ->
                    let deltaTime =
                        state.upTime.Elapsed.TotalSeconds - state.lastFrameTime
                        |> float32
                    let newSpeed =
                        let tempNew = defaultSpeed * System.MathF.Pow (currentScale, 0.65f)
                        let frac = exp ((if tempNew > state.lastSpeed then -0.375f else -2.75f) * deltaTime)
                        frac * state.lastSpeed + (1.f - frac) * tempNew
                    let forward =
                        newSpeed * System.Numerics.Vector3.UnitZ
                        |> state.playerQuaternion.RotateVectorAsQuaternion
                    let roll = Maths.Vector4.BuildQuaternion System.Numerics.Vector3.UnitZ (deltaTime * (state.keyLeftInput - state.keyRightInput))
                    let pitch = Maths.Vector4.BuildQuaternion System.Numerics.Vector3.UnitX (0.8f * deltaTime * (state.keyForwardInput - state.keyBackInput))
                    let yaw = Maths.Vector4.BuildQuaternion System.Numerics.Vector3.UnitY (0.375f * deltaTime * (state.keyStrafeRightInput - state.keyStrafeLeftInput))
                    let deltaP = deltaTime*forward
                    state <-
                        {state with
                            lastFrameTime = state.upTime.Elapsed.TotalSeconds
                            lastSpeed = newSpeed
                            playerPosition = state.playerPosition + deltaP
                            playerQuaternion =
                                pitch.QuaternionMultiply roll
                                |> yaw.QuaternionMultiply
                                |> state.playerQuaternion.QuaternionMultiply}
        window.TockFunction <- Some updateState
        let drawFunc () =
            match renderer.BeginFrame () with
            | Some commandBuffer ->
                renderer.BeginSwapchainRenderPass commandBuffer
                renderSystem.RenderGameObjects commandBuffer state renderer.AspectRatio
                renderer.EndSwapchainRenderPass commandBuffer
                renderer.EndFrame ()
            | None -> ()
            window.Invalidate ()
        window.DrawFunction <- Some drawFunc
        let handleKeyDown (args: KeyEventArgs) =
            match args.KeyCode with
            | Keys.Escape -> exit 0
            | Keys.F5 ->
                let portalSeed, worldGen =
                    if args.Shift then
                        0, System.Random 0
                    else
                        Helpers.random.Next (), Helpers.random
                portalRandom <- System.Random portalSeed
                renderSystem.RegenerateWorld worldGen
                state <- resetPlayer state
            | Keys.R when args.Control ->
                state <- resetPlayer state
            | Keys.W when state.demoControls ->
                state <-
                    let time = float32 state.upTime.Elapsed.TotalSeconds
                    {state with
                        demoControls = false
                        playerPosition = demoCameraPosition time
                        playerQuaternion = demoCameraQuaternion time
                        keyForwardInput = 1.f}
            | Keys.S when state.demoControls ->
                state <-
                    let time = float32 state.upTime.Elapsed.TotalSeconds
                    {state with
                        demoControls = false
                        playerPosition = demoCameraPosition time
                        playerQuaternion = demoCameraQuaternion time
                        keyBackInput = 1.f}
            | Keys.D when state.demoControls ->
                state <-
                    let time = float32 state.upTime.Elapsed.TotalSeconds
                    {state with
                        demoControls = false
                        playerPosition = demoCameraPosition time
                        playerQuaternion = demoCameraQuaternion time
                        keyRightInput = 1.f}
            | Keys.A when state.demoControls ->
                state <-
                    let time = float32 state.upTime.Elapsed.TotalSeconds
                    {state with
                        demoControls = false
                        playerPosition = demoCameraPosition time
                        playerQuaternion = demoCameraQuaternion time
                        keyLeftInput = 1.f}
            | Keys.Q when state.demoControls ->
                state <-
                    let time = float32 state.upTime.Elapsed.TotalSeconds
                    {state with
                        demoControls = false
                        playerPosition = demoCameraPosition time
                        playerQuaternion = demoCameraQuaternion time
                        keyStrafeLeftInput = 1.f}
            | Keys.E when state.demoControls ->
                state <-
                    let time = float32 state.upTime.Elapsed.TotalSeconds
                    {state with
                        demoControls = false
                        playerPosition = demoCameraPosition time
                        playerQuaternion = demoCameraQuaternion time
                        keyStrafeRightInput = 1.f}
            | Keys.W -> state <- {state with keyForwardInput = 1.f}
            | Keys.S -> state <- {state with keyBackInput = 1.f}
            | Keys.D -> state <- {state with keyRightInput = 1.f}
            | Keys.A -> state <- {state with keyLeftInput = 1.f}
            | Keys.Q -> state <- {state with keyStrafeLeftInput = 1.f}
            | Keys.E -> state <- {state with keyStrafeRightInput = 1.f}
            | Keys.F11 -> window.ToggleFullscreen ()
            | _ -> ()
        window.HandleKeyDown <- Some handleKeyDown
        let handleKeyUp (args: KeyEventArgs) =
            match args.KeyCode with
            | Keys.W -> state <- {state with keyForwardInput = 0.f}
            | Keys.S -> state <- {state with keyBackInput = 0.f}
            | Keys.D -> state <- {state with keyRightInput = 0.f}
            | Keys.A -> state <- {state with keyLeftInput = 0.f}
            | Keys.Q -> state <- {state with keyStrafeLeftInput = 0.f}
            | Keys.E -> state <- {state with keyStrafeRightInput = 0.f}
            | _ -> ()
        window.HandleKeyUp <- Some handleKeyUp
        let handleController = function
        | None -> ()
        | Some (args: SharpDX.XInput.Gamepad) ->
            if args.Buttons.HasFlag SharpDX.XInput.GamepadButtonFlags.Back then
                exit 0
            elif args.Buttons.HasFlag SharpDX.XInput.GamepadButtonFlags.B then
                state <- resetPlayer state
            elif args.Buttons.HasFlag SharpDX.XInput.GamepadButtonFlags.Start then
                renderSystem.RegenerateWorld portalRandom
                state <- resetPlayer state
            else
                let mutable noInput = state.demoControls
                let i16ToF32 (i: int16) =
                    if i > 0s then
                        if i < 3000s then 0.f else noInput <- false; float32 i / 32767.f
                    else
                        if i > -3000s then 0.f else noInput <- false; float32 i / -32768.f
                let byteToF32 (i: byte) = if i < 8uy then 0.f else noInput <- false; float32 i / 255.f
                let forward, backward =
                    let t = max (i16ToF32 args.LeftThumbY) 0.f
                    if args.LeftThumbY > 0s then
                        t, 0.f
                    else
                        0.f, t
                let right, left =
                    let t = max (i16ToF32 args.LeftThumbX) 0.f
                    if args.LeftThumbX > 0s then
                        t, 0.f
                    else
                        0.f, t
                let strafeRight = byteToF32 args.RightTrigger
                let strafeLeft = byteToF32 args.LeftTrigger
                if not noInput then
                    state <-
                        if state.demoControls then
                            let time = float32 state.upTime.Elapsed.TotalSeconds
                            {state with
                                demoControls = false
                                playerPosition = demoCameraPosition time
                                playerQuaternion = demoCameraQuaternion time
                                keyForwardInput = forward
                                keyBackInput = backward
                                keyRightInput = right
                                keyLeftInput = left
                                keyStrafeLeftInput = strafeLeft
                                keyStrafeRightInput = strafeRight}
                        else
                            {state with
                                keyForwardInput = forward
                                keyBackInput = backward
                                keyRightInput = right
                                keyLeftInput = left
                                keyStrafeLeftInput = strafeLeft
                                keyStrafeRightInput = strafeRight}
        window.HandleController <- Some handleController

        System.Windows.Forms.Application.Run window
        device.Device.WaitIdle ()

    interface System.IDisposable with
        override _.Dispose () =
            if not disposed then
                disposed <- true
    override self.Finalize () = (self :> System.IDisposable).Dispose ()

[<EntryPoint>]
let main _ =
    use app = new LightApp ()
    app.Run ()
    0