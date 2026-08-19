let test =
  0

type t1 =
  | C1
  | C2

type t2 =
  { t2_1: int
  ; t2_2: int
  }

type t3 =
  { mutable t3_1: int
  ; mutable t3_2: int
  }

module M1 = struct
  let test =
    test

  type t1 =
    | C1
    | C2

  type t2 =
    { t2_1: int
    ; t2_2: int
    }

  type t3 =
    { mutable t3_1: int
    ; mutable t3_2: int
    }

  module M2 = struct
    let test =
      test

    let test =
      test

    type t1 =
      | C1
      | C2

    type t2 =
      { t2_1: int
      ; t2_2: int
      }

    type t3 =
      { mutable t3_1: int
      ; mutable t3_2: int
      }
  end

  let test =
    test

  let test =
    M2.test
end

let test =
  test

let test =
  M1.test

let test =
  M1.M2.test
