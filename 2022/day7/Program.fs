open ElfFs

let root =
    System.IO.File.ReadAllLines("input.txt")
    |> Folder.createRootFromCapturedOutput

printfn "Total size: %d" (root.GetSize())

let atLeastSizeLimit = 100000

let sumOfSmallerFolders =
    root.GetAllFolders()
    |> Seq.map (fun f -> f.GetSize())
    |> Seq.filter (fun s -> s <= atLeastSizeLimit)
    |> Seq.sum

printfn "Sum of smaller folders: %d" sumOfSmallerFolders


// total disk size is fixed at 70000000
let totalDiskSize = 70000000

let unusedSpace = totalDiskSize - root.GetSize()

printfn "Remaining free space: %d" unusedSpace

let spaceNeeded = 30000000

let toDelete = spaceNeeded - unusedSpace

printfn "Space to delete: %d" toDelete

// find all folders that are larger than the space to delete

let smallestDirectorySizeThatFreesUpEnoughSpace =
    root.GetAllFolders()
    |> Seq.filter (fun f -> f.GetSize() > toDelete)
    |> Seq.map (fun f -> f.GetSize())
    |> Seq.min


printfn "Smallest directory size that frees up enough space: %d" smallestDirectorySizeThatFreesUpEnoughSpace
