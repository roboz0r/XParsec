/// Tests for TOML date/time parsing.
/// Covers offset date-time, local date-time, local date, and local time.
module XParsec.Toml.Tests.DateTimeTests

open Expecto
open XParsec.Toml

[<Tests>]
let offsetDateTimeTests =
    testList
        "Offset Date-Time"
        [
            test "UTC datetime" {
                let input = "key = 1979-05-27T07:32:00Z"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.OffsetDateTime dt) ->
                    Expect.equal dt.Year 1979 "Year"
                    Expect.equal dt.Month 5 "Month"
                    Expect.equal dt.Day 27 "Day"
                    Expect.equal dt.Hour 7 "Hour"
                    Expect.equal dt.Minute 32 "Minute"
                    Expect.equal dt.Second 0 "Second"
                    Expect.equal dt.OffsetMinutes (ValueSome 0) "UTC offset"
                | _ -> failtest "Expected OffsetDateTime"
            }

            test "datetime with positive offset" {
                let input = "key = 1979-05-27T07:32:00+05:30"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.OffsetDateTime dt) -> Expect.equal dt.OffsetMinutes (ValueSome 330) "+05:30 offset"
                | _ -> failtest "Expected OffsetDateTime"
            }

            test "datetime with negative offset" {
                let input = "key = 1979-05-27T07:32:00-08:00"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.OffsetDateTime dt) -> Expect.equal dt.OffsetMinutes (ValueSome -480) "-08:00 offset"
                | _ -> failtest "Expected OffsetDateTime"
            }

            test "datetime with fractional seconds" {
                let input = "key = 1979-05-27T07:32:00.999999Z"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.OffsetDateTime dt) -> Expect.equal dt.Nanosecond 999999000 "Fractional seconds"
                | _ -> failtest "Expected OffsetDateTime"
            }
        ]

[<Tests>]
let localDateTimeTests =
    testList
        "Local Date-Time"
        [
            test "local datetime" {
                let input = "key = 1979-05-27T07:32:00"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.LocalDateTime dt) ->
                    Expect.equal dt.Year 1979 "Year"
                    Expect.equal dt.Month 5 "Month"
                    Expect.equal dt.Day 27 "Day"
                    Expect.equal dt.Hour 7 "Hour"
                    Expect.equal dt.Minute 32 "Minute"
                    Expect.equal dt.Second 0 "Second"
                    Expect.equal dt.OffsetMinutes ValueNone "No offset"
                | _ -> failtest "Expected LocalDateTime"
            }

            test "local datetime with space separator" {
                let input = "key = 1979-05-27 07:32:00"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.LocalDateTime dt) -> Expect.equal dt.Hour 7 "Hour"
                | _ -> failtest "Expected LocalDateTime"
            }
        ]

[<Tests>]
let localDateTests =
    testList
        "Local Date"
        [
            test "local date" {
                let input = "key = 1979-05-27"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.LocalDate d) ->
                    Expect.equal d.Year 1979 "Year"
                    Expect.equal d.Month 5 "Month"
                    Expect.equal d.Day 27 "Day"
                | _ -> failtest "Expected LocalDate"
            }
        ]

[<Tests>]
let localTimeTests =
    testList
        "Local Time"
        [
            test "local time" {
                let input = "key = 07:32:00"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.LocalTime t) ->
                    Expect.equal t.Hour 7 "Hour"
                    Expect.equal t.Minute 32 "Minute"
                    Expect.equal t.Second 0 "Second"
                | _ -> failtest "Expected LocalTime"
            }

            test "local time with fractional" {
                let input = "key = 07:32:00.123"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.LocalTime t) -> Expect.equal t.Nanosecond 123000000 "Fractional"
                | _ -> failtest "Expected LocalTime"
            }
        ]
