namespace XParsec.Shared

open System

[<RequireQualifiedAccess>]
type ParseSyntax = | Json

type ParseRequest =
    {
        Id: Guid
        Input: string
        Syntax: ParseSyntax
    }

type ParseResponse =
    {
        RequestId: Guid
        Success: bool
        Message: string
    }
