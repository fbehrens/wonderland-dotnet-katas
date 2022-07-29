namespace alphabet_cipher

module coder =

    let num (c : char) = int c - 97

    let zeichen (i : int) =
        (i + 26) % 26 + 97 |> char |> string
    // zeichen 0

    let repeat (s : string) =
        seq {
            while (true) do
                for c in s -> c
        }

    let encode (key : string) (message : string) : string =
        let enc (m : char, k : char) = num m + num k |> zeichen
        // enc ('b','b') // c
        let ckey = repeat key
        Seq.zip message ckey
        |> Seq.map enc
        |> String.concat ""

    let decode (key : string) (message : string) : string =
        let ckey = repeat key
        let dec (m : char, k : char) = num m - num k |> zeichen
        // dec ('c','b') // 'b'
        Seq.zip message ckey
        |> Seq.map dec
        |> String.concat ""

    let decipher (cipher : string) (message : string) : string =
        let shorten (s : string) =
            let len =
                String.length (s)
            seq { 0 .. len - 1 }
            |> Seq.map (fun i -> s.[0..i])
            |> Seq.find (fun s1 ->
                let padded =
                    repeat s1
                    |> Seq.take len
                    |> Seq.map string
                    |> String.concat ""
                padded = s
            )
        decode message cipher |> shorten
