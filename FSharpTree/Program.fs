open System
open System.IO

// to enable a more point-free approach
let split f g x = (f x, g x)

// a file system item is either a file with a name
// or a folder with name and a sequence of items
type FileSystemItem = 
    | File of string
    | Folder of (string * FileSystemItem seq)
    
let getFolderNameFromPath path =
    let di = new DirectoryInfo(path)
    di.Name

let getFileNameFromPath path = 
    let fi = new FileInfo(path)
    fi.Name

let rec readFileSystemItemsUnder path = 
    let files = Directory.GetFiles path |> Seq.map File
    let folders = path |> Directory.EnumerateDirectories 
                       |> Seq.map (split getFolderNameFromPath readFileSystemItemsUnder)
                       |> Seq.map Folder
    Seq.append files folders

let readFileSystemFromPath path =
    if File.Exists path then
        File (getFileNameFromPath path)
    else if Directory.Exists path then
        Folder (getFolderNameFromPath path, readFileSystemItemsUnder path)
    else
        failwith "Invalid path"

let tabSep = "|  "
let nameSep = "|-- "
let newLine = Environment.NewLine

//now we need to visit the tree structure and mark each item with its level within the hierarchy
let rec prettyPrint level item =
    let prefix = if level = 0 then "" else String.replicate (level-1) tabSep
    match item with
    | File x -> 
        prefix + nameSep + x
    | Folder(x, items) ->
        let nextLevel = level+1
        let rest = items |> Seq.map (prettyPrint nextLevel)
        prefix + nameSep + x + (Seq.fold (fun acc x -> acc + newLine + tabSep + x) "" rest)


[<EntryPoint>]
let main args = 
    let path = args.[0]
    let fileStructure = readFileSystemFromPath path
    Console.WriteLine (fileStructure |> prettyPrint 0)
    0