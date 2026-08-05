let test =
  0

module M1 = struct
  let test =
    test

  module M2 = struct
    let test =
      test

    let test =
      test
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
