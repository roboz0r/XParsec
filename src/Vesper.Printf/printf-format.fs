namespace Vesper

type PrintfFormat<'Printer, 'State, 'Residue, 'Result>(value: string) =
    member _.Value = value
