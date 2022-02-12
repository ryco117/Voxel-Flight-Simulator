module Voxels

open Maths

type RecursiveRef =
| Self
| Parent of RecursiveRef

type GraphRef<'a> =
| Ref of 'a
| Recurse of RecursiveRef
| Empty

type Voxel = {
    averageColour: Vector4
    nodeFTL: GraphRef<Voxel> // Front-Top-Left
    nodeFTR: GraphRef<Voxel> // Front-Top-Right
    nodeFBL: GraphRef<Voxel> // Front-Bottom-Left
    nodeFBR: GraphRef<Voxel> // Front-Bottom-Right
    nodeBTL: GraphRef<Voxel> // Back-Top-Left
    nodeBTR: GraphRef<Voxel> // Back-Top-Right
    nodeBBL: GraphRef<Voxel> // Back-Bottom-Left
    nodeBBR: GraphRef<Voxel> // Back-Bottom-Right
    flags: uint32}

let ftlCell = System.Numerics.Vector3 (-0.5f, 0.5f, -0.5f)
let ftrCell = System.Numerics.Vector3 (0.5f, 0.5f, -0.5f)
let fblCell = System.Numerics.Vector3 (-0.5f, -0.5f, -0.5f)
let fbrCell = System.Numerics.Vector3 (0.5f, -0.5f, -0.5f)
let btlCell = System.Numerics.Vector3 (-0.5f, 0.5f, 0.5f)
let btrCell = System.Numerics.Vector3 (0.5f, 0.5f, 0.5f)
let bblCell = System.Numerics.Vector3 (-0.5f, -0.5f, 0.5f)
let bbrCell = System.Numerics.Vector3 (0.5f, -0.5f, 0.5f)

let leafVoxel = {
    averageColour = Vector4.Zero
    nodeFTL = Recurse Self; nodeFTR = Recurse Self; nodeFBL = Recurse Self; nodeFBR = Recurse Self
    nodeBTL = Recurse Self; nodeBTR = Recurse Self; nodeBBL = Recurse Self; nodeBBR = Recurse Self
    flags = 1u}

[<System.Runtime.InteropServices.StructLayout (System.Runtime.InteropServices.LayoutKind.Explicit)>]
type VoxelCompact =
    struct
        [<System.Runtime.InteropServices.FieldOffset 0>]
        val averageColour: Vector4
        [<System.Runtime.InteropServices.FieldOffset 16>]
        val nodeFTL: uint32 // Front-Top-Left
        [<System.Runtime.InteropServices.FieldOffset 20>]
        val nodeFTR: uint32 // Front-Top-Right
        [<System.Runtime.InteropServices.FieldOffset 24>]
        val nodeFBL: uint32 // Front-Bottom-Left
        [<System.Runtime.InteropServices.FieldOffset 28>]
        val nodeFBR: uint32 // Front-Bottom-Right
        [<System.Runtime.InteropServices.FieldOffset 32>]
        val nodeBTL: uint32 // Back-Top-Left
        [<System.Runtime.InteropServices.FieldOffset 36>]
        val nodeBTR: uint32 // Back-Top-Right
        [<System.Runtime.InteropServices.FieldOffset 40>]
        val nodeBBL: uint32 // Back-Bottom-Left
        [<System.Runtime.InteropServices.FieldOffset 44>]
        val nodeBBR: uint32 // Back-Bottom-Right
        [<System.Runtime.InteropServices.FieldOffset 48>]
        val flags: uint32 // 32-bit mask
        [<System.Runtime.InteropServices.FieldOffset 52>]
        val alignmentSpace0: uint32 // EMPTY SPACE IN BUFFER - ALIGNMENT
        [<System.Runtime.InteropServices.FieldOffset 56>]
        val alignmentSpace1: uint64 // EMPTY SPACE IN BUFFER - ALIGNMENT
        new (colour', nodeFTL', nodeFTR', nodeFBL', nodeFBR', nodeBTL', nodeBTR', nodeBBL', nodeBBR', flags') =
            {
                averageColour = colour'
                nodeFTL = nodeFTL'
                nodeFTR = nodeFTR'
                nodeFBL = nodeFBL'
                nodeFBR = nodeFBR'
                nodeBTL = nodeBTL'
                nodeBTR = nodeBTR'
                nodeBBL = nodeBBL'
                nodeBBR = nodeBBR'
                flags = flags'
                alignmentSpace0 = 0u
                alignmentSpace1 = 0UL}
    end

let nullVoxelIndex: uint32 = 0xFFFFFFFFu

let compactOctreeFromRoot rootVoxel =
    let mutable nodeIndex = 0u
    let dictionary = System.Collections.Generic.Dictionary<Voxel, uint32> ()
    let rec breadthFirstSearchOctree acc selfIndex = function
    | [] -> acc
    | (voxel, parentList)::tail ->
        let refToIndex toExplore = function
        | Recurse Self -> selfIndex, toExplore
        | Recurse (Parent vxls) ->
            let rec func = function
            | _::indexTail, Parent vxls -> func (indexTail, vxls)
            | pIndex::_, Self -> pIndex, toExplore
            | [], _ -> 0u, toExplore    // Default to root index if we requested a parent past the root
            func (parentList, vxls)
        | Empty -> nullVoxelIndex, toExplore
        | Ref voxel ->
            if dictionary.ContainsKey voxel then
                dictionary.Item voxel, toExplore
            else
                nodeIndex <- nodeIndex + 1u
                dictionary.Item voxel <- nodeIndex
                nodeIndex, (voxel, selfIndex::parentList)::toExplore

        let colour = voxel.averageColour
        let ftl, bfs = refToIndex [] voxel.nodeFTL
        let ftr, bfs = refToIndex bfs voxel.nodeFTR
        let fbl, bfs = refToIndex bfs voxel.nodeFBL
        let fbr, bfs = refToIndex bfs voxel.nodeFBR
        let btl, bfs = refToIndex bfs voxel.nodeBTL
        let btr, bfs = refToIndex bfs voxel.nodeBTR
        let bbl, bfs = refToIndex bfs voxel.nodeBBL
        let bbr, bfs = refToIndex bfs voxel.nodeBBR
        let compactVoxel = VoxelCompact (colour, ftl, ftr, fbl, fbr, btl, btr, bbl, bbr, voxel.flags)
        breadthFirstSearchOctree (compactVoxel::acc) (selfIndex + 1u) (List.append (List.rev bfs) tail)
    breadthFirstSearchOctree [] 0u [rootVoxel, []]
    |> List.rev
    |> List.toArray

let randomColour (random: System.Random) =
    let randF () = random.NextDouble () |> float32
    Vector4 (randF (), randF (), randF (), 1.f)

let randomLeaf random =
    let colour = randomColour random
    if random.NextDouble () > 0. then
        {leafVoxel with averageColour = colour}
    else
        {leafVoxel with averageColour = colour; flags = 4u}

let generateRecursiveVoxelOctree (random: System.Random) n =
    let rec randomRecurse maxDepth =
        if maxDepth > 0 && random.NextDouble () > 0.8 then
            Parent (randomRecurse (maxDepth - 1))
        else
            Self
    let maxRecurseDepth =
        log (float n) / log 2.
        |> int
    let queue = System.Collections.Generic.Queue ()
    for _ = 0 to (int n - 1) do
        randomLeaf random
        |> queue.Enqueue
    while queue.Count > 1 do
        let rec rollVoxel () =
            let mutable popped = false
            let mutable colour = Vector4 (0.f)
            let mutable weight = 0.f
            let popNodeOption () =
                let randomType = random.NextDouble ()
                if queue.Count = 0 then
                    if randomType > 0.675 then
                        Recurse (randomRecurse maxRecurseDepth)
                    else
                        Empty
                else
                    if randomType > 0.45 then
                        if randomType > 0.825 then
                            Recurse (randomRecurse maxRecurseDepth)
                        else
                            let voxel = queue.Dequeue ()
                            weight <- weight + 1.f
                            colour.AddInPlace voxel.averageColour
                            popped <- true
                            Ref voxel
                    else
                        Empty

            let ftl = popNodeOption ()
            let ftr = popNodeOption ()
            let fbl = popNodeOption ()
            let fbr = popNodeOption ()
            let btl = popNodeOption ()
            let btr = popNodeOption ()
            let bbl = popNodeOption ()
            let bbr = popNodeOption ()
            if popped then
                {
                    averageColour = colour / weight
                    nodeFTL = ftl; nodeFTR = ftr
                    nodeFBL = fbl; nodeFBR = fbr
                    nodeBTL = btl; nodeBTR = btr
                    nodeBBL = bbl; nodeBBR = bbr
                    flags = 0u
                }
            else
                rollVoxel ()
        rollVoxel ()
        |> queue.Enqueue
    queue.Dequeue ()
    |> compactOctreeFromRoot

type Intersection =
| EmptySpace
| Collision
| Portal of uint32

let octreeScaleAndCollisionOfPoint (v: System.Numerics.Vector3) (octree: VoxelCompact[]) =
    let maxDepth = 15   // TODO: Unify maxDepth between shader and cpu programatically
    let rec f (scale: float32) iter (p: System.Numerics.Vector3) = function
    | _ when iter = maxDepth -> scale, Collision
    | index when index = nullVoxelIndex -> scale, EmptySpace
    | index ->
        let i = int index
        let voxel = octree[i]
        if voxel.flags &&& 5u > 0u then
            scale, Collision
        elif voxel.flags &&& 2u > 0u then
            scale, if System.Numerics.Vector3.Dot (p, p) < 0.75f then Portal index else EmptySpace
        else
            let func p =
                f (scale + scale) (iter + 1) (p+p)
            if p.X > 0.f then
                if p.Y > 0.f then
                    if p.Z > 0.f then
                        func (p - btrCell) voxel.nodeBTR
                    else
                        func (p - ftrCell) voxel.nodeFTR
                else
                    if p.Z > 0.f then
                        func (p - bbrCell) voxel.nodeBBR
                    else
                        func (p - fbrCell) voxel.nodeFBR
            else
                if p.Y > 0.f then
                    if p.Z > 0.f then
                        func (p - btlCell) voxel.nodeBTL
                    else
                        func (p - ftlCell) voxel.nodeFTL
                else
                    if p.Z > 0.f then
                        func (p - bblCell) voxel.nodeBBL
                    else
                        func (p - fblCell) voxel.nodeFBL
    if abs v.X > 1.f || abs v.Y > 1.f || abs v.Z > 1.f then
        1.f, EmptySpace
    else
        let scaleInv, intersection = f 1.f 0 v 0u
        1.f / scaleInv, intersection

let addRandomGoals (random: System.Random) n minDepth (octree: VoxelCompact[]) =
    let mutable parentSet = Collections.Set<int> []
    let rec replaceVoxel selfIndex parent depth =
        let selfI = int selfIndex
        let voxel = octree[selfI]
        let setVoxel () =
            octree[int selfIndex] <- VoxelCompact (randomColour random, selfIndex, selfIndex, selfIndex, selfIndex, selfIndex, selfIndex, selfIndex, selfIndex, 2u)
            parentSet <- parentSet.Add parent
            true
        if voxel.flags &&& 6u > 0u then
            false
        elif voxel.flags &&& 1u > 0u then
            if depth < minDepth || parentSet.Contains parent then
                false
            else
                setVoxel ()
        else
            if depth < minDepth || random.NextDouble () > 0.925 then
                let randChild () =
                    match random.Next 8 with
                    | 0 -> voxel.nodeFTL
                    | 1 -> voxel.nodeFTR
                    | 2 -> voxel.nodeFBL
                    | 3 -> voxel.nodeFBR
                    | 4 -> voxel.nodeBTL
                    | 5 -> voxel.nodeBTR
                    | 6 -> voxel.nodeBBL
                    | 7 -> voxel.nodeBBR
                    | _ -> raise (System.Exception "Invalid octree child index")
                let mutable newIndex = randChild ()
                while newIndex = nullVoxelIndex || newIndex <= selfIndex do
                    newIndex <- randChild ()
                replaceVoxel newIndex selfI (depth + 1)
            elif not (parentSet.Contains parent) then
                setVoxel ()
            else
                false
    let mutable count = 0
    while count < n do
        if replaceVoxel 0u 0 0 then
            count <- count + 1