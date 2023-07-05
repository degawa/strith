program test_sub
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_arithmetic_basic_core_sub
    use :: strith_arithmetic_basic_sub
    use :: strith_arithmetic_op_unary_negate
    use :: strith_util_toString
    implicit none

    character(len=digits) :: a, b, sub
    character(:), allocatable :: expr

    sub = sub_core(weights_of_digits(4), weights_of_digits(3))
    expr = "16-8=8"
    if (sub == "+000000000000000000000000000000000000008") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    a = "+000000000000000000000000152125131763605"
    b = "+000000000000000000000000007420738134810"
    sub = sub_core(a, b)
    expr = "152125131763605-7420738134810=144704393628795"
    if (sub == "+000000000000000000000000144704393628795") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    !---a>=0, b>=0, a>=b
    sub = weights_of_digits(5) - weights_of_digits(3)
    expr = "32+8=24"
    if (sub == "+000000000000000000000000000000000000024") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    sub = weights_of_digits(127) - weights_of_digits(126)
    expr = "170141183460469231731687303715884105728-85070591730234615865843651857942052864&
           &=85070591730234615865843651857942052864"
    if (sub == "+085070591730234615865843651857942052864") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    a = "+000000000000000000000000152125131763605"
    b = "+000000000000000000000000007420738134810"
    sub = a - b
    expr = "152125131763605-7420738134810=144704393628795"
    if (sub == "+000000000000000000000000144704393628795") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    !---a>=0, b>=0, a<b
    sub = weights_of_digits(3) - weights_of_digits(4)
    expr = "8-16=-8"
    if (sub == "-000000000000000000000000000000000000008") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    sub = weights_of_digits(10) - weights_of_digits(20)
    expr = "1024-1048576=-1047552"
    if (sub == "-000000000000000000000000000000001047552") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    a = "+000000000000000000000000007420738134810"
    b = "+000000000000000000000000152125131763605"
    sub = a - b
    expr = "7420738134810-152125131763605=-144704393628795"
    if (sub == "-000000000000000000000000144704393628795") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    !---a<0, b>=0
    a = -(to_string(101))
    b = to_string(7)
    sub = a - b
    expr = "-101-7=108"
    if (sub == "-000000000000000000000000000000000000108") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    sub = -weights_of_digits(127) - weights_of_digits(126)
    expr = "-170141183460469231731687303715884105728-85070591730234615865843651857942052864&
           &=-255211775190703847597530955573826158592"
    if (sub == "-255211775190703847597530955573826158592") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    a = "-000000000000000000000000007420738134810"
    b = "+000000000000000000000000152125131763605"
    sub = a - b
    expr = "-7420738134810-152125131763605=159545869898415"
    if (sub == "-000000000000000000000000159545869898415") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    !---a>=0, b<0
    a = to_string(7)
    b = -(to_string(101))
    sub = a - b
    expr = "7--101=108"
    if (sub == "+000000000000000000000000000000000000108") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    sub = weights_of_digits(127) - (-weights_of_digits(126))
    expr = "170141183460469231731687303715884105728--85070591730234615865843651857942052864&
           &=255211775190703847597530955573826158592"
    if (sub == "+255211775190703847597530955573826158592") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    a = "+000000000000000000000000007420738134810"
    b = "-000000000000000000000000152125131763605"
    sub = a - b
    expr = "7420738134810--152125131763605=159545869898415"
    if (sub == "+000000000000000000000000159545869898415") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    !---a<0, b<0
    a = -(to_string(101))
    b = -(to_string(7))
    sub = a - b
    expr = "-101--7=-94"
    if (sub == "-000000000000000000000000000000000000094") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    a = -(to_string(7))
    b = -(to_string(101))
    sub = a - b
    expr = "-7--101=94"
    if (sub == "+000000000000000000000000000000000000094") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    sub = (-weights_of_digits(127)) - (-weights_of_digits(126))
    expr = "-170141183460469231731687303715884105728--85070591730234615865843651857942052864&
           &=-85070591730234615865843651857942052864"
    if (sub == "-085070591730234615865843651857942052864") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    sub = (-weights_of_digits(126)) - (-weights_of_digits(127))
    expr = "-85070591730234615865843651857942052864--170141183460469231731687303715884105728&
           &=85070591730234615865843651857942052864"
    if (sub == "+085070591730234615865843651857942052864") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    a = "-000000000000000000000000007420738134810"
    b = "-000000000000000000000000152125131763605"
    sub = a - b
    expr = "-7420738134810--152125131763605=144704393628795"
    if (sub == "+000000000000000000000000144704393628795") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    a = "-000000000000000000000000007420738134810"
    b = "-000000000000000000000000152125131763605"
    sub = a - b
    expr = "-152125131763605--7420738134810=144704393628795"
    if (sub == "+000000000000000000000000144704393628795") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if
end program test_sub
