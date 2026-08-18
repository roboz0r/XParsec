namespace Vesper

type PrintfFormat<'Printer, 'State, 'Residue, 'Result>(value: string) =
    member _.Value = value

type Format<'Printer, 'State, 'Residue, 'Result> = PrintfFormat<'Printer, 'State, 'Residue, 'Result>
