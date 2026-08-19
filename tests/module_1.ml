let test =
  0

type t1 =
  | C1
  | C2

type t2 =
  { f1: int
  ; f2: int
  }

type t3 =
  { mutable f1: int
  ; mutable f2: int
  }

module M = struct
  let test =
    test

  type t1 =
    | C1
    | C2

  type t2 =
    { f1: int
    ; f2: int
    }

  type t3 =
    { mutable f1: int
    ; mutable f2: int
    }
end

let test =
  test

let test =
  M.test
