module Test

// Heritable external reference base: `(# class "..." #)` (tagged) vs the opaque
// value repr `(# "..." #)` (untagged).
type Attribute = (# class "System.Attribute" #)
type int = (# "System.Int32" #)
