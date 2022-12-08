module ElfFs

    type CreateFolderCommand = { Name:string }
    type EnterFolderCommand = { Name:string }
    type ListCommand = struct end
    type MoveOutCommand = struct end
    type CreateFileCommand = { Name:string; Size:int }
    type EnterRootCommand = struct end

    type Command =
        | CreateFolderCommand of CreateFolderCommand
        | EnterFolderCommand of EnterFolderCommand
        | ListCommand of ListCommand
        | MoveOutCommand of MoveOutCommand
        | CreateFileCommand of CreateFileCommand
        | EnterRootCommand of EnterRootCommand

    type File(name:string, size:int) = 
        member this.Name = name
        member this.Size = size
        override this.ToString() = sprintf "%s (%d bytes)" name size

    type Folder(name:string, parent:option<Folder>) = 
        let files = new System.Collections.Generic.List<File>()
        let folders = new System.Collections.Generic.List<Folder>()

        override this.ToString() = 
            let sb = new System.Text.StringBuilder()
            sb.AppendLine(sprintf "Folder: %s" name) |> ignore
            sb.AppendLine("Files:") |> ignore
            for file in files do
                sb.AppendLine(sprintf "  %s" (file.ToString())) |> ignore
            sb.AppendLine("Folders:") |> ignore
            for folder in folders do
                sb.AppendLine(sprintf "  %s" (folder.ToString())) |> ignore
            sb.ToString()

        member this.Name = name
        member this.AddFile(file:File) = files.Add(file)
        member this.AddFolder(folder:Folder) = folders.Add(folder)
        member this.Files = files
        member this.Folders = folders
        member this.Parent = parent

        member this.GetSize() =
            let filesSize = files |> Seq.map (fun f -> f.Size) |> Seq.sum
            let foldersSize = folders |> Seq.map (fun f -> f.GetSize()) |> Seq.sum
            filesSize + foldersSize

        member this.GetAllFolders() =
            let rec getAllFolders (folder:Folder) =
                let subfolders = folder.Folders |> Seq.map getAllFolders |> Seq.concat
                Seq.append [folder] subfolders
            getAllFolders this

        static member mapInputToCommand (line:string) : Command =
            match line with
            | txt when txt.Equals("$ cd ..") ->
                MoveOutCommand (new MoveOutCommand())
            | txt when txt.Equals("$ cd /") ->
                EnterRootCommand (new EnterRootCommand())
            | txt when txt.StartsWith("$ cd ") -> 
                let folderName = txt.Substring(5)
                EnterFolderCommand {Name = folderName}
            | txt when txt.StartsWith("dir ") -> 
                let folderName = txt.Substring(4)
                CreateFolderCommand {Name = folderName}
            | txt when txt.Equals("$ ls") ->
                ListCommand (new ListCommand())
            | _ -> 
                // assume it's a file in this case
                let parts = line.Split(' ')
                let size = int(parts[0])
                let name = parts[1]
                CreateFileCommand {Name = name; Size = size}

        static member commandProcessor (currentFolder:Folder) (command:Command) : Folder =
        // printfn "Processing command: %s" (command.ToString())

            match command with
            | CreateFolderCommand cmd ->
                let newFolder = new Folder(cmd.Name, Some currentFolder)
                currentFolder.AddFolder(newFolder)
                currentFolder
            | EnterFolderCommand cmd ->
                let folderToEnter = currentFolder.Folders |> Seq.find (fun f -> f.Name.Equals(cmd.Name))
                folderToEnter
            | ListCommand _ ->
                currentFolder
            | MoveOutCommand _ ->
                currentFolder.Parent.Value
            | CreateFileCommand cmd ->
                let newFile = new File(cmd.Name, cmd.Size)
                currentFolder.AddFile(newFile)
                currentFolder
            | EnterRootCommand _ ->
                currentFolder
        static member createRootFromCapturedOutput (lines:string[]) =
            let root = Folder("/", None)

            lines
                |> Array.map Folder.mapInputToCommand
                |> Array.fold Folder.commandProcessor root |> ignore

            root