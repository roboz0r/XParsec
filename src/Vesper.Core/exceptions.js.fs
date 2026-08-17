namespace System

open Vesper

type SystemException(message: string) =
    inherit exn(message)

type InvalidOperationException(message: string) =
    inherit SystemException(message)

type ArgumentException(message: string) =
    inherit SystemException(message)

type ArgumentNullException(message: string) =
    inherit ArgumentException(message)

type NotSupportedException(message: string) =
    inherit SystemException(message)

type IndexOutOfRangeException(message: string) =
    inherit SystemException(message)

type FormatException(message: string) =
    inherit SystemException(message)
