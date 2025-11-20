module TrieSet

type PrefixTreeSetNode<'T when 'T: comparison> =
    { children: Map<'T, PrefixTreeSetNode<'T>>
      isEndOfSequence: bool }

type PrefixTreeSet<'T when 'T: comparison> = { root: PrefixTreeSetNode<'T> }

// Basic Operations
let empty: PrefixTreeSet<'T> =
    { root =
        { children = Map.empty
          isEndOfSequence = false } }

let add (seq: seq<'T>) (set: PrefixTreeSet<'T>) : PrefixTreeSet<'T> =
    let rec insertSeq seqList node =
        match seqList with
        | [] -> { node with isEndOfSequence = true }
        | head :: tail ->
            let child =
                Map.tryFind head node.children
                |> Option.defaultValue
                    { children = Map.empty
                      isEndOfSequence = false }

            let newChild = insertSeq tail child

            { node with
                children = Map.add head newChild node.children }

    { set with
        root = insertSeq (Seq.toList seq) set.root }

let contains (seq: seq<'T>) (set: PrefixTreeSet<'T>) : bool =
    let rec findNode seqList node =
        match seqList with
        | [] -> node.isEndOfSequence
        | head :: tail ->
            match Map.tryFind head node.children with
            | Some child -> findNode tail child
            | None -> false

    findNode (Seq.toList seq) set.root

let hasPrefix (prefix: seq<'T>) (set: PrefixTreeSet<'T>) : bool =
    let rec findNode seqList node =
        match seqList with
        | [] -> true
        | head :: tail ->
            match Map.tryFind head node.children with
            | Some child -> findNode tail child
            | None -> false

    findNode (Seq.toList prefix) set.root

// Additional Operations
let isEmpty (set: PrefixTreeSet<'T>) : bool =
    set.root.children.IsEmpty && not set.root.isEndOfSequence

let remove (seq: seq<'T>) (set: PrefixTreeSet<'T>) : PrefixTreeSet<'T> =
    let rec removeSeq seqList node =
        match seqList with
        | [] ->
            if node.children.IsEmpty then
                None
            else
                Some { node with isEndOfSequence = false }
        | head :: tail ->
            match Map.tryFind head node.children with
            | Some child ->
                match removeSeq tail child with
                | Some newChild ->
                    let newChildren = Map.add head newChild node.children
                    Some { node with children = newChildren }
                | None ->
                    let remainingChildren = Map.remove head node.children

                    if remainingChildren.IsEmpty && not node.isEndOfSequence then
                        None
                    else
                        Some
                            { node with
                                children = remainingChildren }
            | None -> Some node

    match removeSeq (Seq.toList seq) set.root with
    | Some newRoot -> { set with root = newRoot }
    | None -> empty

let size (set: PrefixTreeSet<'T>) : int =
    let rec count node =
        let childCount = node.children |> Map.values |> Seq.sumBy count
        if node.isEndOfSequence then childCount + 1 else childCount

    count set.root

let toList (set: PrefixTreeSet<'T>) : 'T list list =
    let rec collect acc currentPath node =
        let newPath =
            if node.isEndOfSequence then
                List.rev currentPath :: acc
            else
                acc

        node.children
        |> Map.fold (fun acc' key child -> collect acc' (key :: currentPath) child) newPath

    collect [] [] set.root |> List.filter (fun x -> x <> [])

let fold (folder: 'State -> 'T list -> 'State) (state: 'State) (set: PrefixTreeSet<'T>) : 'State =
    let rec collect acc currentPath node =
        let newAcc =
            if node.isEndOfSequence && currentPath <> [] then
                folder acc (List.rev currentPath)
            else
                acc

        node.children
        |> Map.fold (fun acc' key child -> collect acc' (key :: currentPath) child) newAcc

    collect state [] set.root

let filter (predicate: 'T list -> bool) (set: PrefixTreeSet<'T>) : PrefixTreeSet<'T> =
    set
    |> toList
    |> List.filter predicate
    |> List.fold (fun s seq -> add seq s) empty

let map (mapping: 'T list -> 'U list) (set: PrefixTreeSet<'T>) : PrefixTreeSet<'U> =
    set |> toList |> List.map mapping |> List.fold (fun s seq -> add seq s) empty

let exists (predicate: 'T list -> bool) (set: PrefixTreeSet<'T>) : bool =
    let rec check node currentPath =
        if node.isEndOfSequence && currentPath <> [] && predicate (List.rev currentPath) then
            true
        else
            node.children |> Map.exists (fun key child -> check child (key :: currentPath))

    check set.root []

let forall (predicate: 'T list -> bool) (set: PrefixTreeSet<'T>) : bool =
    let allSequences = toList set
    List.forall predicate allSequences

let iter (action: 'T list -> unit) (set: PrefixTreeSet<'T>) : unit =
    let rec traverse currentPath node =
        if node.isEndOfSequence && currentPath <> [] then
            action (List.rev currentPath)

        node.children |> Map.iter (fun key child -> traverse (key :: currentPath) child)

    traverse [] set.root

// Helper for string sequences
let addString (str: string) (set: PrefixTreeSet<char>) = add str set
let containsString (str: string) (set: PrefixTreeSet<char>) = contains str set
let hasPrefixString (prefix: string) (set: PrefixTreeSet<char>) = hasPrefix prefix set


// Monoid operation: Union of two prefix tree sets
let union (set1: PrefixTreeSet<'T>) (set2: PrefixTreeSet<'T>) : PrefixTreeSet<'T> =
    let rec mergeNodes (node1: PrefixTreeSetNode<'T>) (node2: PrefixTreeSetNode<'T>) =
        // Optimization: Skip empty nodes
        let isEmptyNode (n: PrefixTreeSetNode<'T>) =
            n.children.IsEmpty && not n.isEndOfSequence

        if isEmptyNode node1 then
            node2
        elif isEmptyNode node2 then
            node1
        else
            let combinedEnd = node1.isEndOfSequence || node2.isEndOfSequence
            let keys1 = node1.children |> Map.keys |> Set.ofSeq
            let keys2 = node2.children |> Map.keys |> Set.ofSeq
            let allKeys = Set.union keys1 keys2

            let mergedChildren =
                Set.fold
                    (fun acc key ->
                        let child1 =
                            Map.tryFind key node1.children
                            |> Option.defaultValue
                                { children = Map.empty
                                  isEndOfSequence = false }

                        let child2 =
                            Map.tryFind key node2.children
                            |> Option.defaultValue
                                { children = Map.empty
                                  isEndOfSequence = false }

                        let mergedChild = mergeNodes child1 child2
                        Map.add key mergedChild acc)
                    Map.empty
                    allKeys

            { children = mergedChildren
              isEndOfSequence = combinedEnd }

    { root = mergeNodes set1.root set2.root }
