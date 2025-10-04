// Test 8: Type Definitions
type Person =
    { Name: string
      Age: int }

type Result<'TSuccess, 'TError> =
    | Success of 'TSuccess
    | Failure of 'TError