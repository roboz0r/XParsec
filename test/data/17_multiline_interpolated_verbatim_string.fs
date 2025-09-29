// Multiline interpolated verbatim string
let version = "1.2.3"
let s = $@"
<config>
    <version>{version}</version>
</config>
"
printfn "%s" s