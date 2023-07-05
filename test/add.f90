program test_add
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_arithmetic_basic_core_add
    use :: strith_arithmetic_basic_add
    use :: strith_arithmetic_op_unary_negate
    use :: strith_util_toString
    implicit none

    character(len=digits) :: a, b, add
    character(:), allocatable :: expr

    add = add_core(weights_of_digits(3), weights_of_digits(4))
    expr = "8+16=24"
    if (add == "+000000000000000000000000000000000000024") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    add = add_core(weights_of_digits(127), weights_of_digits(126))
    expr = "170141183460469231731687303715884105728+85070591730234615865843651857942052864&
           &=255211775190703847597530955573826158592"
    if (add == "+255211775190703847597530955573826158592") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    a = "+000000000000000000000000007420738134810"
    b = "+000000000000000000000000152125131763605"
    add = add_core(a, b)
    expr = "7420738134810+152125131763605=159545869898415"
    if (add == "+000000000000000000000000159545869898415") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    !---a>=0, b>=0
    add = weights_of_digits(3) + weights_of_digits(4)
    expr = "8+16=24"
    if (add == "+000000000000000000000000000000000000024") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    add = weights_of_digits(127) + weights_of_digits(126)
    expr = "170141183460469231731687303715884105728+85070591730234615865843651857942052864&
           &=255211775190703847597530955573826158592"
    if (add == "+255211775190703847597530955573826158592") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    a = "+000000000000000000000000007420738134810"
    b = "+000000000000000000000000152125131763605"
    add = a + b
    expr = "7420738134810+152125131763605=159545869898415"
    if (add == "+000000000000000000000000159545869898415") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    !---a<0, b>=0
    a = -(to_string(101))
    b = to_string(7)
    add = a + b
    expr = "-101+7=94"
    if (add == "-000000000000000000000000000000000000094") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    add = -weights_of_digits(127) + weights_of_digits(126)
    expr = "-170141183460469231731687303715884105728+85070591730234615865843651857942052864&
           &=-85070591730234615865843651857942052864"
    if (add == "-085070591730234615865843651857942052864") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    a = "-000000000000000000000000007420738134810"
    b = "+000000000000000000000000152125131763605"
    add = a + b
    expr = "-7420738134810+152125131763605=144704393628795"
    if (add == "+000000000000000000000000144704393628795") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    !---a>=0, b<0
    a = to_string(7)
    b = -(to_string(101))
    add = a + b
    expr = "7+-101=-94"
    if (add == "-000000000000000000000000000000000000094") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    add = weights_of_digits(127) + (-weights_of_digits(126))
    expr = "170141183460469231731687303715884105728+-85070591730234615865843651857942052864&
           &=85070591730234615865843651857942052864"
    if (add == "+085070591730234615865843651857942052864") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    a = "+000000000000000000000000007420738134810"
    b = "-000000000000000000000000152125131763605"
    add = a + b
    expr = "7420738134810+-152125131763605=144704393628795"
    if (add == "-000000000000000000000000144704393628795") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    !---a<0, b<0
    a = -(to_string(101))
    b = -(to_string(7))
    add = a + b
    expr = "-7+-101=-108"
    if (add == "-000000000000000000000000000000000000108") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    add = (-weights_of_digits(127)) + (-weights_of_digits(126))
    expr = "-170141183460469231731687303715884105728+-85070591730234615865843651857942052864&
           &=-255211775190703847597530955573826158592"
    if (add == "-255211775190703847597530955573826158592") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    a = "-000000000000000000000000007420738134810"
    b = "-000000000000000000000000152125131763605"
    add = a + b
    expr = "-7420738134810+-152125131763605=159545869898415"
    if (add == "-000000000000000000000000159545869898415") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if
end program test_add
